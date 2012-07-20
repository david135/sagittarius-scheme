// -*- mode:c++; coding: utf-8; -*-
/*
 * jit.cpp
 *
 *   Copyright (c) 2010  Takashi Kato <ktakashi@ymail.com>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *  $Id: $
 */
#include <string>
#include <stdexcept>
#include "xbyak/xbyak.h"
#define LIBSAGITTARIUS_BODY
#include "sagittarius/closure.h"
#include "sagittarius/code.h"
#include "sagittarius/gloc.h"
#include "sagittarius/identifier.h"
#include "sagittarius/instruction.h"
#include "sagittarius/library.h"
#include "sagittarius/number.h"
#include "sagittarius/vm.h"

//#define JIT_DEBUG

struct WalkContext
{
  bool noExternalCall;		// not to access given VM.
  WalkContext() 
    : noExternalCall(true)
  {}
};

// For different level of optimisation
class Walker
{
public:
  virtual void walk(SgClosure *closure, WalkContext &ctx) = 0;
};

class DefaultWalker : public Walker
{
public:
  virtual void walk(SgClosure *closure, WalkContext &ctx);
};

void DefaultWalker::walk(SgClosure *closure, WalkContext &ctx)
{
  SgCodeBuilder *cb = SG_CODE_BUILDER(closure->code);
  SgWord *code = cb->code;
  for (int i = 0; i < cb->size;) {
    Instruction insn = static_cast<Instruction>(INSN(code[i]));
    InsnInfo *info = Sg_LookupInsnName(insn);
    switch (INSN(code[i])) {
      // for now
    case GREF_CALL:
    case GREF_TAIL_CALL: {
      SgObject p = SG_OBJ(code[i+1]);
      if (SG_IDENTIFIERP(p)) {
	// should not be here but just in case.
	p = Sg_FindBinding(SG_IDENTIFIER_LIBRARY(p),
			   SG_IDENTIFIER_NAME(p),
			   SG_UNBOUND);
	if (SG_UNBOUNDP(p)) {
	  // the closure must throw unbound error, so walk failed.
	  throw std::runtime_error("unbound variable.");
	}
      }
      p = SG_GLOC_GET(SG_GLOC(p));
      if (!SG_PROCEDURE(p)) {
	throw std::runtime_error("call instruction is calling non procedure object.");
      } else if (SG_PROCEDURE_OPTIONAL(p) != 0) {
	throw std::runtime_error("optional arguments are not supported yet.");
      }
      // if the calling procedure is a closure, then we compile this
      // so that the real compiler can simply compile it to native
      // call instruction instead of using Sg_Apply.
      // TODO generic?
      if (!SG_EQ(closure, p) && SG_CLOSUREP(p)) {
	// to make it easier, compile it here
	int r = Sg_JitCompileClosure(p);
	if (!r) {
	  throw std::runtime_error("nested closure jit compile failed.");
	}
      }
    }
    case CALL:
    case TAIL_CALL:
      ctx.noExternalCall = false;
      break;
    default: break;
    }
    i += 1 + info->argc;
  }
}

#ifdef JIT_DEBUG
static int nest_level = 0;
#endif

class JitCompiler : public Xbyak::CodeGenerator
{
private:
  SgClosure * const closure_;
  Walker * const walker_;
  WalkContext context_;
  int label_;
  std::string this_label_;
  // for tail call
  std::string re_entry_label_;
  // need for tail call
  size_t prologue_size_;
public:
  JitCompiler(SgClosure * const closure, Walker * const walker)
    : closure_(closure)
    , walker_(walker)
    , label_(0)
  {}
  
  // compile the closure
  bool compile();

private:
  static const int VM_OFF = 16;
  static const int ARGC_OFF = 12;
  static const int ARGS_OFF = 8;

  // FIXME: the same as vm.c
  static SgObject* shift_args(SgObject *fp, int m, SgObject *sp)
  {
    SgObject *f = fp + m;
    for (int i = m - 1; 0 <= i; i--) {
      INDEX_SET(f, i, INDEX(sp, i));
    }
    return f;
  }

  // walk through the closure to detect possible
  // optimisation options.
  void walk()
  {
    walker_->walk(closure_, context_);
  }
  void prologue()
  {
    // TODO 64 bits
    this_label_ = gen_label();
    re_entry_label_ = gen_label();
    L(this_label_.c_str());
    push(ebp);
    mov(ebp, esp);
    vm(ebx);
    L(re_entry_label_.c_str());
    prologue_size_ = getSize();
#ifdef JIT_DEBUG
    dump_args();
#endif
  }
  void epilogue()
  {
    // only noExternalCall is false, we need to move
    // vm->ac to eax
    if (!context_.noExternalCall) {
      // put vm->ac to eax
      vm(edx);
      mov(eax, ptr[edx + offsetof(SgVM, ac)]);
    }
    pop(ebp);
    ret();
  }
  void stack_access(const Xbyak::Operand &d, int offset)
  {
    mov(d, ptr[ebp + offset]);
  }

  void vm_stack_ref(const Xbyak::Reg32e &d, const Xbyak::Reg32e &vm,
		    int index)
  {
    mov(d, ptr[vm + offsetof(SgVM, sp)]);
    mov(d, ptr[d - (index + 1) * sizeof(void*)]);
  }

  void args(const Xbyak::Operand &d)
  {
    stack_access(d, ARGS_OFF);
  }
  void argc(const Xbyak::Operand &d)
  {
    stack_access(d, ARGC_OFF);
  }
  void vm(const Xbyak::Operand &d)
  {
    stack_access(d, VM_OFF);
  }
  // put the value to vm->ac or eax if noExternalCall is true
  // destination must indicate vm if noExternalCall is false.
  void ac(const Xbyak::Reg32e &vm, const Xbyak::Operand &v)
  {
    if (context_.noExternalCall) {
      mov(eax, v);
    } else {
      mov(ptr[vm + offsetof(SgVM, ac)], v);
    }
  }
  void ac(const Xbyak::Reg32e &vm, const SgObject o)
  {
    if (context_.noExternalCall) {
      mov(eax, (intptr_t)o);
    } else {
      mov(ptr[vm + offsetof(SgVM, ac)], (intptr_t)o);
    }
  }
  void add_offset(const Xbyak::Reg32e &vm, int offset)
  {
    // in case, save eax and edx
    push(eax);
    vm_sp(eax, vm);
    lea(eax, ptr[eax + offset]);
    mov(ptr[ebx + offsetof(SgVM, sp)], eax);
    pop(eax);
  }
  void vm_inc_sp(const Xbyak::Reg32e &vm)
  {
    // Xbyak does not allow us to write like this...
    //add(ptr[ebp + VM_OFF + offsetof(SgVM, sp)], 1*sizeof(void*));
    add_offset(vm, 1*sizeof(void*));
  }
  void vm_dec_sp(const Xbyak::Reg32e &vm)
  {
    // Xbyak does not allow us to write like this...
    //sub(ptr[ebp + VM_OFF + offsetof(SgVM, sp)], 1*sizeof(void*));
    add_offset(vm, -(1*sizeof(void*)));
  }
  void vm_sp(const Xbyak::Reg32e &d, const Xbyak::Reg32e &vm)
  {
    mov(d, ptr[vm + offsetof(SgVM, sp)]);
  }
  void vm_fp(const Xbyak::Reg32e &d, const Xbyak::Reg32e &vm)
  {
    mov(d, ptr[vm + offsetof(SgVM, fp)]);
  }
  void vm_cl(const Xbyak::Reg32e &d, const Xbyak::Reg32e &vm)
  {
    mov(d, ptr[vm + offsetof(SgVM, cl)]);
  }
  void vm_pc(const Xbyak::Reg32e &d, const Xbyak::Reg32e &vm)
  {
    mov(d, ptr[vm + offsetof(SgVM, pc)]);
  }
  void vm_cont(const Xbyak::Reg32e &d, const Xbyak::Reg32e &vm)
  {
    mov(d, ptr[vm + offsetof(SgVM, cont)]);
  }
  void ref_ac(const Xbyak::Reg32e &d, const Xbyak::Reg32e &vm)
  {
    // if noExternalCall, eax is the previous instructions result.
    if (!context_.noExternalCall) {
      mov(d, ptr[vm + offsetof(SgVM, ac)]); // vm->ac = eax
    }
  }

  std::string gen_label()
  {
    std::string l("L");
    char buf[50];
    snprintf(buf, 50, "%d", label_++);
    l.append(buf);
    return l;
  }

  int compile_rec(SgWord *code, int size);
#ifdef JIT_DEBUG
  static void trace_impl(const char *msg)
  {
    SgVM *vm = Sg_VM();
    for (int i = 0; i < nest_level; i++) {
      fputc(' ', stderr);
    }
    fprintf(stderr, "%d: %s(sp:%p fp:%p ac:%p)\n",
	    nest_level, msg, vm->sp, vm->fp, vm->ac);
  }
  static void trace_frame_impl()
  {
    SgVM *vm = Sg_VM();
    SgContFrame *c = CONT(vm);
    // print cont frame
    fprintf(stderr,
	    "pc   %p\n"
	    "cont %p\n"
	    " prev %p\n"
	    " size %d\n"
	    " pc   %p\n"
	    " cl   %p\n"
	    " fp   %p\n"
	    " env  %p\n\n",
	    vm->pc, c, c->prev, c->size, c->pc, c->cl, c->fp, c->env);
  }

  static void inc_nest_impl(int off)
  {
    nest_level += off;
  }
  static void dump_args_impl(void **args, int argc)
  {
    SgVM *vm = Sg_VM();
    fprintf(stderr, "sp: %p, fp: %p, args %d:\n", vm->sp, vm->fp, argc);
    for (int i = 0; i < argc; i++) {
      fprintf(stderr, " %d: %p\n", i, args[i]);
    }
    fprintf(stderr, "\n");
  }

  void dump_args()
  {
    push(eax);
    {
      argc(eax);
      push(eax);
      args(eax);
      push(eax);
      call((void*)JitCompiler::dump_args_impl);
      pop(eax);
      pop(eax);
    }
    pop(eax);
  }
#endif
  void inc_nest(int off)
  {
#ifdef JIT_DEBUG
    push(eax);
    push((uintptr_t)off);
    call((void*)JitCompiler::inc_nest_impl);
    pop(eax);
    pop(eax);
#endif
  }

  void trace(const char *msg)
  {
#ifdef JIT_DEBUG
    push(eax);
    push((uintptr_t)msg);
    call((void*)JitCompiler::trace_impl);
    pop(eax);
    pop(eax);
#endif
  }
  void trace_frame()
  {
#ifdef JIT_DEBUG
    push(eax);
    call((void*)JitCompiler::trace_frame_impl);
    pop(eax);
#endif
  }

  // instructions
  void consti_insn(SgWord *code, int i)
  {
    int val1;
    trace("enter CONSTI");
    INSN_VAL1(val1, code[i]);
    mov(eax, (uintptr_t)SG_MAKE_INT(val1));
    ac(ebx, eax);
    trace("leave CONSTI");
  }
  void push_insn(SgWord *code, int i)
  {
    trace("enter PUSH");
    vm_sp(edx, ebx); // edx = vm->sp
    // push argument to vm stack
    mov(ptr[edx], eax);
    // sp++
    vm_inc_sp(ebx);
    trace("leave PUSH");
  }
};
#define retrive_next_gloc(o,d)						\
  do {									\
    o = SG_OBJ(d);							\
    if (SG_IDENTIFIERP(o)) {						\
      o = Sg_FindBinding(SG_IDENTIFIER_LIBRARY(o),			\
			 SG_IDENTIFIER_NAME(o),				\
			 SG_UNBOUND);					\
      if (SG_UNBOUNDP(o)) {						\
	throw std::runtime_error("unbound variable object is called.");	\
      }									\
    }									\
    o = SG_GLOC_GET(SG_GLOC(o));					\
  } while (0)
  
/*
  Note:
  the generated native code will be called as subr with arguments
  (void **args, int argc, SgVM *vm)

  performance not (32 bit):
  * the third argument vm will be put ebx, so do not change.

  * if the closure does not call other procedure, then we don't use
    vm->ac to store the result, always eax (rare case).
 */
int JitCompiler::compile_rec(SgWord *code, int size)
{
  int i;
  for (i = 0; i < size;) {
    Instruction insn = static_cast<Instruction>(INSN(code[i]));
    InsnInfo *info = Sg_LookupInsnName(insn);
    int val1;
    void *bnproc = NULL;
     // each instruction must put its result to ac(vm->ac or eax)
    // if it has some result. cf) some instruction does not have
    // result such as PUSH.
    switch (insn) {
    case FRAME: {
#if 0
      // now we are using Sg_Apply, so we don't need frame.
      trace("enter FRAME");
      trace_frame();
      SgObject n = SG_OBJ(code[i+1]);
      // *newcont = (SgContFrame*)SP(vm);
      vm_sp(eax, ebx);		// keep eax = vm->sp
      // newcont->prev = vm->cont
      vm_cont(edx, ebx);
      mov(ptr[eax + offsetof(SgContFrame, prev)], edx);
      // newcont->fp = vm->fp
      vm_fp(edx, ebx);
      mov(ptr[eax + offsetof(SgContFrame, fp)], edx);
      // newcont->size = vm->sp - vm->fp
      // fp = edx
      mov(ecx, ptr[ebx + offsetof(SgVM, sp)]);
      sub(ecx, edx);
      mov(ptr[eax + offsetof(SgContFrame, size)], ecx);
      // newcont->pc = next_pc
      vm_pc(edx, ebx);
      lea(edx, ptr[edx + SG_INT_VALUE(n)*sizeof(void*)]);
      mov(ptr[eax + offsetof(SgContFrame, pc)], edx);
      // newcont->cl = vm->cl;
      vm_cl(ecx, ebx);
      mov(ptr[eax + offsetof(SgContFrame, cl)], ecx);
      // vm->cont = newcont
      mov(ptr[ebx + offsetof(SgVM, cont)], eax);
      // eax = vm->sp
      // vm->sp += CONT_FRAME_SIZE
      lea(edx, ptr[eax + sizeof(SgContFrame)]);
      mov(ptr[ebx + offsetof(SgVM, sp)], edx);
      trace_frame();
      trace("leave FRAME");
#endif
      break;
    }
    case LREF_PUSH:
      trace("enter LREF_PUSH");
      INSN_VAL1(val1, code[i]);
      args(eax);  // args
      // ecx = arg_ref[val1]
      mov(eax, ptr[eax + val1 * sizeof(void*)]);
      trace("goto PUSH");
      // fall though
    case PUSH: push_insn(code, i); break;
    case LREF:
      trace("enter LREF");
      INSN_VAL1(val1, code[i]);
      args(eax); // args
      mov(eax, ptr[eax + val1 * sizeof(void*)]); // args[n] -> edx
      ac(ebx, eax);
      trace("leave LREF");
      break;
    case CONSTI: consti_insn(code, i); break;
    case CONSTI_PUSH: consti_insn(code, i); push_insn(code, i); break;
    case BNGE:
      bnproc = (void *)Sg_NumGe;
      goto bnnum_entry;
    case BNGT:
      bnproc = (void *)Sg_NumGt;
      goto bnnum_entry;
    case BNLE: 
      bnproc = (void *)Sg_NumLe;
      goto bnnum_entry;
    case BNLT: 
      bnproc = (void *)Sg_NumLt;
      goto bnnum_entry;
    bnnum_entry:
      {
	SgObject n = SG_OBJ(code[i+1]);
	trace("enter BNLE");
	ref_ac(eax, ebx); // eax = vm->ac
	push(eax);
	vm_stack_ref(eax, ebx, 0);
	push(eax);
	call(bnproc);
	add(esp, 2 * sizeof(void*));
	// vm->sp--
	vm_dec_sp(ebx);
	std::string l = gen_label();
	cmp(eax, 1);
	// FIXME: not all jmp shouldn't be T_SHORT but
	// how much should we take?
	jne(l.c_str(), (SG_INT_VALUE(n) < 5) ? T_AUTO : T_NEAR);
	// we don't set #f or #t to eax, it's useless
	// TODO if jump instruction
	int j= compile_rec(code + i + 2, SG_INT_VALUE(n));
	L(l.c_str());
	j += compile_rec(code + i + 1 + SG_INT_VALUE(n), size - i);
	i += j;
	trace("leave BNLE");
	break;
      }
    case ADD: {
      trace("enter ADD");
      vm_stack_ref(eax, ebx, 0);
      vm_dec_sp(ebx);		// vm->sp--;
      ref_ac(edx, ebx);
      push(eax);
      push(edx);
      call((void*)Sg_Add);
      ac(ebx, eax);
      add(esp, 2*sizeof(void*));
      trace("leave ADD");
      break;
    }
    case ADDI: {
      trace("enter ADDI");
      INSN_VAL1(val1, code[i]);
      // TODO 64 bits
      ref_ac(eax, ebx);		// load vm->ac to eax
      mov(edx, eax);
      // SG_INTP(vm->ac);
      and(edx, 3);
      cmp(edx, 1);
      std::string label = gen_label();
      std::string end_label = gen_label();
      je(label.c_str());
      // ac != int
      push(eax);
      push((uintptr_t)SG_MAKE_INT(val1));
      call((void*)Sg_Add);
      add(esp, 2 * sizeof(void*));
      ac(ebx, eax);
      jmp(end_label.c_str());
      // for now...
      L(label.c_str());
      sar(eax, 2);
      add(eax, val1);
      push(eax);
      call((void*)Sg_MakeInteger);
      add(esp, 1 * sizeof(void*));
      ac(ebx, eax);
      L(end_label.c_str());
      trace("leave ADDI");
      break;
    }
    case GREF_TAIL_CALL: {
      trace("enter GREF_TAIL_CALL");
      //inc_nest(1);
      INSN_VAL1(val1, code[i]);
      SgObject o;
      retrive_next_gloc(o, SG_OBJ(code[i+1]));
      // for proper tail recursive, we can not consume c stack either.
      // so make this simple jump

      // manage vm->sp part
      vm_sp(edx, ebx);
      push(edx);
      push((uintptr_t)val1);
      vm_fp(ecx, ebx);
      push(ecx);
      call((void*)JitCompiler::shift_args);
      add(esp, 3 * sizeof(void*));
      // vm->sp = eax
      mov(ptr[ebx + offsetof(SgVM, sp)], eax);
      // manage fp
      lea(eax, ptr[eax - (val1 *sizeof(void*))]);
      // set vm->fp
      mov(ptr[ebx + offsetof(SgVM, fp)], eax);
      vm_sp(edx, ebx);      
      // now vm stack is ok next is re-use c-stack
      // args = vm->fp
      // argc = val1
      // data = vm or subr data
      mov(ptr[ebp + ARGS_OFF], eax);
      mov(dword[ebp + ARGC_OFF], (uintptr_t)val1);
      // for now
      mov(ptr[ebp + VM_OFF], ebx);
      // ok emit jump
      if (SG_EQ(o, closure_)) {
	// tail recursive self call
	pop(ebp);
	jmp(this_label_.c_str());
	//jmp(re_entry_label_.c_str(), T_NEAR);
      } else if (SG_SUBRP(o)) {
	// FIXME: how should we do this?
	// can we just jmp?
	throw std::runtime_error("tail recursive subr call");
      } else if (SG_CLOSUREP(o)) {
	// if a closure reaches here, it must be compiled already
	// see walker.
	SgObject p = SG_CLOSURE(o)->native;
	// pop it and let the prologue from the beginning.
	pop(ebp);
	jmp((void*)SG_SUBR_FUNC(p), T_NEAR);
      } else {
	throw std::runtime_error("non closure procedures are not supported yet.");
      }
      //inc_nest(-1);
      //trace("leave GREF_TAIL_CALL");
      break;
    }
    case GREF_CALL: {
      trace("enter GREF_CALL");
      inc_nest(1);
      INSN_VAL1(val1, code[i]);
      SgObject o;
      retrive_next_gloc(o, SG_OBJ(code[i+1]));
      if (SG_PROCEDURE_OPTIONAL(o) != 0) {
	throw std::runtime_error("optional argument is not supported yet.");
      }
      // save fp
      // TODO is this legal?
      vm_fp(edx, ebx);
      push(edx);

      vm_sp(edx, ebx);
      void *proc = NULL;	// subr
      if (SG_EQ(o, closure_)) {
	push(ebx);
	// proc must be label of this
      } else if (SG_SUBRP(o)) {
	// retrieve data and push
	proc = (void*)SG_SUBR_FUNC(o);
	push((uintptr_t)SG_SUBR_DATA(o));
      } else if (SG_CLOSUREP(o)) {
	push(ebx);
	proc = (void *)SG_SUBR_FUNC(SG_CLOSURE(o)->native);
      } else {
	throw std::runtime_error("generic functions are not supported yet");
      }
      push(val1);		// argc
      // calculate fp
      lea(edx, ptr[edx - (val1 *sizeof(void*))]);
      // set vm->fp
      mov(ptr[ebx + offsetof(SgVM, fp)], edx);
      push(edx);		// args
      if (proc) {
	call(proc);
      } else {
	call(this_label_.c_str());
      }
      ac(ebx, eax);
      add(esp, 3 * sizeof(void*));
      add_offset(ebx, -(val1 * sizeof(void*)));
      pop(edx);			// restore fp
      mov(ptr[ebx + offsetof(SgVM, fp)], edx);

      inc_nest(-1);
      trace("leave GREF_CALL");
      break;
    }
    case RET:
      trace("RETURN");
      epilogue();
      return i;
    default:
      throw std::runtime_error(info->name);
    }
    i += 1 + info->argc;
  }
  return i;
}

bool JitCompiler::compile()
{
  // walk
  try {
    walk();
    SgCodeBuilder *cb = SG_CODE_BUILDER(closure_->code);
    SgWord *code = cb->code;

    prologue();
    int r = compile_rec(code, cb->size);
    if (r < 0) return false;
  } catch (Xbyak::Error e) {
    const char *m = Xbyak::ConvertErrorToString(e);
    if (m) {
      fprintf(stderr, "%s\n", m);
    }
    return false;
  } catch (const std::runtime_error& e) {
    fprintf(stderr, "%s\n", e.what());
    return false;
  }
  return true;
}

int Sg_JitCompileClosure(SgObject closure)
{
  Walker *walker = new DefaultWalker;
  JitCompiler compiler(SG_CLOSURE(closure), walker);
  SgVM *vm = Sg_VM();

  if (compiler.compile()) {
    SgObject subr
      = Sg_MakeSubr(reinterpret_cast<SgSubrProc *>(compiler.getCode()),
		    NULL,
		    SG_PROCEDURE_REQUIRED(closure),
		    SG_PROCEDURE_OPTIONAL(closure),
		    SG_PROCEDURE_NAME(closure));
    if (SG_VM_LOG_LEVEL(vm, SG_DEBUG_LEVEL)) {
      compiler.dump();
    }
    SG_CLOSURE(closure)->native = subr;
    SG_CLOSURE(closure)->state = SG_NATIVE;
    delete walker;
    return TRUE;
  }
  SG_CLOSURE(closure)->state = SG_INVALID_FOR_NATIVE;
  delete walker;
  return FALSE;
}
