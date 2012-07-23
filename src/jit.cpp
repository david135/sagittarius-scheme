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
#include <map>
#include <stdexcept>
#include <stack>
#include "xbyak/xbyak.h"
#define LIBSAGITTARIUS_BODY
#include "sagittarius/closure.h"
#include "sagittarius/code.h"
#include "sagittarius/error.h"
#include "sagittarius/gloc.h"
#include "sagittarius/identifier.h"
#include "sagittarius/instruction.h"
#include "sagittarius/library.h"
#include "sagittarius/number.h"
#include "sagittarius/pair.h"
#include "sagittarius/vm.h"

#define JIT_DEBUG
typedef std::map<SgWord*, std::string> Labels;
struct WalkContext
{
  bool noExternalCall;		// not to access given VM.
  Labels srcLabels;
  Labels dstLabels;
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
private:
  int label_;
  std::string gen_label()
  {
    char buf[20];
    snprintf(buf, 20, "J%d", label_++);
    return std::string(buf);
  }
public:
  DefaultWalker() : label_(0) {}
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
    case JUMP:{
      int n = SG_INT_VALUE(code[i+1]);
      // store the position to lookup
      std::string label = gen_label();
      // set both from and to
      ctx.srcLabels.insert(Labels::value_type(code+i, label));
      ctx.dstLabels.insert(Labels::value_type(code+i+1+n, label));
      break;
    }
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
      }
      // if the calling procedure is a closure, then we compile this
      // so that the real compiler can simply compile it to native
      // call instruction instead of using Sg_Apply.
      // TODO generic?
      if (!SG_EQ(closure, p) && SG_CLOSUREP(p) && 
	  SG_CLOSURE(p)->state == SG_NON_NATIVE) {
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
  Walker & walker_;
  WalkContext context_;
  int label_;
  std::string this_label_;
  // for tail call
  std::string re_entry_label_;
  // need for tail call
  size_t prologue_size_;
  std::stack<int> saved_argp;
  int argp;
public:
  JitCompiler(SgClosure * const closure, Walker & walker)
    : CodeGenerator(256, Xbyak::AutoGrow)
    , closure_(closure)
    , walker_(walker)
    , label_(0)
    , argp(0)
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
    walker_.walk(closure_, context_);
  }
  void prologue()
  {
    // TODO 64 bits
    this_label_ = gen_label();
    re_entry_label_ = gen_label();
    L(this_label_.c_str());
    push(ebp);
    mov(ebp, esp);
    push(ebx);
    vm(ebx);
    L(re_entry_label_.c_str());
    prologue_size_ = getSize();
#ifdef JIT_DEBUG
    dump_args();
#endif
  }

  void leave()
  {
    mov(esp, ebp);
    pop(ebp);
  }

  void epilogue()
  {
    pop(ebx);
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

  void add_offset(const Xbyak::Reg32e &vm, int offset)
  {
    vm_sp(ecx, vm);
    lea(ecx, ptr[ecx + offset]);
    mov(ptr[ebx + offsetof(SgVM, sp)], ecx);
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
    fprintf(stderr, "%d: %s(sp:%p fp:%p ac:%p *sp:%p)\n",
	    nest_level, msg, vm->sp, vm->fp, vm->ac, *(vm->sp-1));
  }
  static void dump_eax_impl(void *eax)
  {
    fprintf(stderr, "eax: %p\n", eax);
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
  void dump_eax()
  {
#ifdef JIT_DEBUG
    push(eax);
    call((void*)JitCompiler::dump_eax_impl);
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
    trace("leave CONSTI");
  }
  void push_insn()
  {
    trace("enter PUSH");
    dump_eax();
    vm_sp(edx, ebx); // edx = vm->sp
    push_insn(edx, eax);
    trace("leave PUSH");
  }
  void push_insn(const Xbyak::Reg32e &sp, const Xbyak::Reg32e &r)
  {
    // push argument to vm stack
    mov(ptr[sp], r);
    // sp++
    vm_inc_sp(ebx);
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

extern "C" void expand_stack(SgVM *vm);

static void call_adjust_args(SgVM *vm, int argc, SgObject proc)
{
#undef APPLY_CALL
#include "vm-args-adjust.c"
  ADJUST_ARGUMENT_FRAME(proc, argc);
}

static void apply_adjust_args(SgVM *vm, int argc, SgObject proc)
{
#define APPLY_CALL
#include "vm-args-adjust.c"
  ADJUST_ARGUMENT_FRAME(proc, argc);
}

  
/*
  Note:
  the generated native code will be called as subr with arguments
  (void **args, int argc, SgVM *vm)

  performance not (32 bit):
  * the third argument vm will be put ebx, so do not change.

  * if the closure does not call other procedure, then we don't use
    vm->ac to store the result, always eax (rare case).

  * keep eax as clean as possible. we treat it the same as vm->ac.
 */
int JitCompiler::compile_rec(SgWord *code, int size)
{
  int i;
  for (i = 0; i < size;) {
    Instruction insn = static_cast<Instruction>(INSN(code[i]));
    InsnInfo *info = Sg_LookupInsnName(insn);
    int val1, val2;
    void *bnproc = NULL;
    // for gref_call
    SgObject gproc = SG_FALSE;

    // emit jump destination label, if there is
    Labels::iterator itr = context_.dstLabels.find(code+i);
    if (itr != context_.dstLabels.end()) {
      L(itr->second.c_str());
    }
    // if it has some result. cf) some instruction does not have
    // result such as PUSH.
    switch (insn) {
      // we don't have to consider FRAME vm instruction
    case FRAME: break;
      // ENTER vm instruction is more like for sanity...
    case ENTER: break;
    case LREF_PUSH:
      trace("enter LREF_PUSH");
      INSN_VAL1(val1, code[i]);
      // keep eax clean, for heavy call
      args(ecx);  // args
      // ecx = arg_ref[val1]
      mov(ecx, ptr[ecx + val1 * sizeof(void*)]);
      vm_sp(edx, ebx);
      push_insn(edx, ecx);
      trace("leave LREF_PUSH");
      break;
    case PUSH: push_insn(); break;
    case LREF:
      trace("enter LREF");
      INSN_VAL1(val1, code[i]);
      args(eax); // args
      mov(eax, ptr[eax + val1 * sizeof(void*)]); // args[n] -> edx
      trace("leave LREF");
      break;
    case CONST: {
      trace("enter CONST");
      SgObject o = SG_OBJ(code[i+1]);
      mov(eax, (uintptr_t)o);
      trace("leave CONST");
      break;
    }
    case CONST_PUSH: {
      trace("enter CONST_PUSH");
      SgObject o = SG_OBJ(code[i+1]);
      // avoid to use eax.
      mov(ecx, (uintptr_t)o);
      vm_sp(edx, ebx);
      push_insn(edx, ecx);
      trace("leave CONST_PUSH");
      break;
    }
    case CONSTI: consti_insn(code, i); break;
    case CONSTI_PUSH:
      // the same as LREF_PUSH;
      trace("enter CONSTI_PUSH");
      INSN_VAL1(val1, code[i]);
      mov(ecx, (uintptr_t)SG_MAKE_INT(val1));
      vm_sp(edx, ebx);
      push_insn(edx, ecx);
      trace("leave CONSTI");
      break;
    case BNNUME:
      bnproc = (void *)Sg_NumEq;
      goto bnnum_entry;
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
	int n = SG_INT_VALUE(code[i+1]);
	trace("enter BNLE");
	push(eax);
	vm_stack_ref(edx, ebx, 0);
	push(edx);
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
	int j= compile_rec(code + i + 2, n);
	L(l.c_str());
	j += compile_rec(code + i + 1 + n, size - i - j);
	i += j;
	trace("leave BNLE");
	break;
      }
    case ADD: {
      trace("enter ADD");
      vm_stack_ref(edx, ebx, 0);
      vm_dec_sp(ebx);		// vm->sp--;
      push(eax);
      push(edx);
      call((void*)Sg_Add);
      add(esp, 2*sizeof(void*));
      trace("leave ADD");
      break;
    }
    case ADDI: {
      trace("enter ADDI");
      INSN_VAL1(val1, code[i]);
      // TODO 64 bits
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
      jmp(end_label.c_str());
      // for now...
      L(label.c_str());
      sar(eax, 2);
      add(eax, val1);
      push(eax);
      call((void*)Sg_MakeInteger);
      add(esp, 1 * sizeof(void*));
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
      // vm->cl = procedure
      //mov(ptr[ebx + offsetof(SgVM, cl)], (uintptr_t)o);
      // ok emit jump
      if (SG_EQ(o, closure_)) {
	// tail recursive self call
	leave();
	jmp(this_label_.c_str());
      } else if (SG_SUBRP(o)) {
	jmp((void*)SG_SUBR_FUNC(o), T_NEAR);
      } else if (SG_CLOSUREP(o)) {
	// if a closure reaches here, it must be compiled already
	// see walker.
	SgObject p = SG_CLOSURE(o)->native;
	leave();
	jmp((void*)SG_SUBR_FUNC(p), T_NEAR);
      } else {
	throw std::runtime_error("non closure procedures are not supported yet.");
      }
      break;
    }
    case GREF_CALL: 
      trace("enter GREF_CALL");
      inc_nest(1);
      retrive_next_gloc(gproc, SG_OBJ(code[i+1]));
//      if (SG_PROCEDURE_OPTIONAL(gproc) != 0) {
//	throw std::runtime_error("optional argument is not supported yet.");
//      }
      goto call_entry;
      break;
    call_entry:
    case CALL: {
      INSN_VAL1(val1, code[i]);
      // save cl and fp
      vm_cl(edx, ebx);
      push(edx);
      vm_fp(edx, ebx);
      push(edx);

      // adjust arguments
      if (SG_FALSEP(gproc)) {
	push(eax);		// save eax for later call
	push(eax);
      } else {
	push((uintptr_t)gproc);
      }
      push((uintptr_t)val1);
      push(ebx);		// vm
      call((void*)call_adjust_args);
      add(esp, 3 * sizeof(void*));
      if (SG_FALSEP(gproc)) {
	pop(eax);
      }

      // push arguments
      vm_sp(edx, ebx);
      void *proc = NULL;	// subr
      if (!SG_FALSEP(gproc)) {
	if (SG_EQ(gproc, closure_)) {
	  push(ebx);
	  // proc must be label of this
	} else if (SG_SUBRP(gproc)) {
	  // retrieve data and push
	  proc = (void*)SG_SUBR_FUNC(gproc);
	  push((uintptr_t)SG_SUBR_DATA(gproc));
	} else if (SG_CLOSUREP(gproc)) {
	  push(ebx);
	  proc = (void *)SG_SUBR_FUNC(SG_CLOSURE(gproc)->native);
	} else {
	  throw std::runtime_error("generic functions are not supported yet");
	}
      } else {
	// LREF or FREF call
	push(ebx);
      }
      push(val1);		// argc
      vm_fp(edx, ebx);
      push(edx);		// args
      if (SG_FALSEP(gproc)) {
	call(eax);
      } else {
	if (proc) {
	  call(proc);
	} else {
	  call(this_label_.c_str());
	}
	gproc = SG_FALSE;
      }
      add(esp, 3 * sizeof(void*));
      add_offset(ebx, -(val1 * sizeof(void*)));
      pop(edx);			// restore fp
      mov(ptr[ebx + offsetof(SgVM, fp)], edx);
      pop(edx);			// restore cl
      mov(ptr[ebx + offsetof(SgVM, cl)], edx);

      inc_nest(-1);
      trace("leave GREF_CALL");
      break;
    }
    case SHIFTJ: {
      trace("enter SHIFTJ");
      INSN_VAL2(val1, val2, code[i]);
      // TODO do we need this?
      push(eax);
      vm_sp(edx, ebx);
      push(edx);
      push((uintptr_t)val1);
      vm_fp(ecx, ebx);
      lea(ecx, ptr[ecx + val2*sizeof(void*)]);
      push(ecx);
      call((void*)JitCompiler::shift_args);
      add(esp, 3 * sizeof(void*));
      // vm->sp = eax
      mov(ptr[ebx + offsetof(SgVM, sp)], eax);
      pop(eax);
      trace("leave SHIFTJ");
      break;
    }
    case JUMP:
      trace("enter JUMP");
      itr = context_.srcLabels.find(code+i);
      if (itr == context_.srcLabels.end()) {
	// something wrong
	throw std::runtime_error("JUMP does not have destination");
      }
      jmp(itr->second.c_str(), T_NEAR);
      trace("leave JUMP");
      break;
    case RET:
      trace("RETURN");
      epilogue();
      return i+1;
      // builtin procedures
    case CAR:
      push(eax);
      call((void*)Sg_Car);
      add(esp, 1*sizeof(void*));
      break;
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
  } catch (...) {
    fprintf(stderr, "something wrong");
    return false;
  }
  return true;
}

int Sg_JitCompileClosure(SgObject closure)
{
  DefaultWalker walker;
  JitCompiler compiler(SG_CLOSURE(closure), walker);
  SgVM *vm = Sg_VM();

  if (compiler.compile()) {
    compiler.ready();
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
    return TRUE;
  }
  SG_CLOSURE(closure)->state = SG_INVALID_FOR_NATIVE;
  return FALSE;
}
