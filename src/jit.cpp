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
#include "sagittarius/symbol.h"
#include "sagittarius/string.h"
#include "sagittarius/vm.h"

//#define JIT_DEBUG
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

extern "C" void expand_stack(SgVM *vm);

static int call_adjust_args(SgVM *vm, int argc, SgObject proc)
{
#undef APPLY_CALL
#include "vm-args-adjust.c"
  ADJUST_ARGUMENT_FRAME(proc, argc);
  return argc;
}

static int apply_adjust_args(SgVM *vm, int argc, SgObject proc)
{
#define APPLY_CALL
#include "vm-args-adjust.c"
  ADJUST_ARGUMENT_FRAME(proc, argc);
  return argc;
}

static void* convert_proc(SgObject proc)
{
  if (SG_SUBRP(proc)) {
    return (void*)SG_SUBR_FUNC(proc);
  } else if (SG_CLOSUREP(proc)) {
    return (void*)SG_SUBR_FUNC(SG_CLOSURE(proc)->native);
  } else {
    Sg_Error(UC("not supported yet, %S"), proc);
    return NULL;
  }
}

struct JitAllocator : public Xbyak::Allocator
{
  virtual uint8_t * alloc(size_t size)
  {
    return reinterpret_cast<uint8_t*>(SG_MALLOC(size));
  }
  // do nothing
  virtual void free(uint8_t *p) {}
};

static JitAllocator jit_allocator;

class JitCompiler : public Xbyak::CodeGenerator
{
private:
  SgClosure * const closure_;
  Walker & walker_;
  WalkContext context_;
  int label_;
  std::string this_label_;

  // for my convenient and more readable code
#ifdef XBYAK64
  Xbyak::Reg64 vm;
  Xbyak::Reg64 ac;
  Xbyak::Reg64 csp;
#elif  defined(XBYAK32)
  Xbyak::Reg32 vm;		// ebx
  Xbyak::Reg32 ac;		// eax
  Xbyak::Reg32 csp;		// esp (c stack pointer)
#else
  #error "JIT not supported for this architecture"
#endif

public:
  JitCompiler(SgClosure * const closure, Walker & walker)
    : CodeGenerator(1024, Xbyak::AutoGrow, &jit_allocator)
    , closure_(closure)
    , walker_(walker)
    , label_(0)
#ifdef XBYAK64
    , vm(rbx)
    , ac(rax)
    , csp(rsp)
#elif  defined(XBYAK32)
    , vm(ebx)
    , ac(eax)
    , csp(esp)
#endif
  {}
  
  // compile the closure
  bool compile();

private:
  static const int VM_OFF = 16;
  static const int ARGC_OFF = 12;
  static const int ARGS_OFF = 8;

public:
  // FIXME: the same as vm.c
  static SgObject* shift_args(SgObject *fp, int m, SgObject *sp)
  {
    SgObject *f = fp + m;
    for (int i = m - 1; 0 <= i; i--) {
      INDEX_SET(f, i, INDEX(sp, i));
    }
    return f;
  }

private:
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
    L(this_label_.c_str());
    push(ebp);
    mov(ebp, esp);
    push(ebx);
    scheme_vm(vm);
    vm_cl(edx, vm); push(edx);
    vm_fp(edx, vm); push(edx);
#ifdef JIT_DEBUG
    dump_args();
#endif
  }

  void epilogue()
  {
    pop(edx); mov(ptr[vm + offsetof(SgVM, fp)], edx);
    pop(edx); mov(ptr[vm + offsetof(SgVM, cl)], edx);
    pop(ebx);
    pop(ebp);
    ret();
  }
  // common instructions
  void leave()
  {
    pop(edx); pop(edx);
    pop(ebx);
    mov(esp, ebp);
    pop(ebp);
  }
  // calling C API for scheme
  // All scheme procedures are proc(void**, int, void*) signature.
  // Assume edx is argc returned from adjust_argument_frame.
  void scheme_call(SgObject proc, bool tail)
  {
    if (SG_SUBRP(proc)) {
      push((uintptr_t)SG_SUBR_DATA(proc));
    } else {
      push(vm);
    }
    push(edx);
    // TODO 64 bit
    vm_fp(ecx, vm);
    push(ecx);

    if (SG_FALSEP(proc)) {
      // LREF call or apply call
      // shcek if it's subr or not
      push(ac);
      call((void*)convert_proc);
      add(csp, 1 * sizeof(void*));

      if (tail) {
	mov(ptr[ebp + ARGS_OFF], ecx);
	mov(dword[ebp + ARGC_OFF], edx);
	mov(ptr[ebp + VM_OFF], vm);
	leave();
      }
      tail ? jmp(ac) : call(ac);
    } else {
      if (tail) {
	mov(ptr[ebp + ARGS_OFF], ecx);
	mov(dword[ebp + ARGC_OFF], edx);
	mov(ptr[ebp + VM_OFF], vm);
	leave();
      }
      if (SG_EQ(proc, closure_)) {
	// self call.
	tail
	  ? jmp(this_label_.c_str(), T_NEAR)
	  : call(this_label_.c_str());
      } else if (SG_SUBRP(proc)) {
	tail
	  ? jmp((void*)SG_SUBR_FUNC(proc), T_NEAR)
	  : call((void*)SG_SUBR_FUNC(proc));
      } else if (SG_CLOSUREP(proc)) {
	tail
	  ? jmp((void*)SG_SUBR_FUNC(SG_CLOSURE(proc)->native), T_NEAR)
	  : call((void*)SG_SUBR_FUNC(SG_CLOSURE(proc)->native));
      } else {
	throw std::runtime_error("generic methods are not supported yet");
      }
    }
    // avoid making size bigger.
    if (!tail) {
      add(csp, 3 * sizeof(void*));
      vm_fp(edx, vm);
      //add_offset(vm, -(argc * sizeof(void*)));
      mov(ptr[vm + offsetof(SgVM, sp)], edx);
    }
  }
  void adjust_args(SgObject proc, uintptr_t argc, bool apply_p)
  {
    if (SG_FALSEP(proc)) {
      push(ac);		// save ac for later call
      push(ac);
    } else {
      push((uintptr_t)proc);
    }
    if (apply_p) {
      // APPLY instruction set rargc to ecx and we need to check
      // if it's zero or not
      cmp(ecx, 0);
      std::string l = gen_label();
      std::string e = gen_label();
      je(l.c_str());
      // not zero add argc + 1;
      push(argc+1);
      jmp(e.c_str());
      L(l.c_str());
      push(argc);
      L(e.c_str());
    } else {
      push(argc);
    }
    push(vm);
    if (apply_p) {
      call((void*)apply_adjust_args);
    } else {
      call((void*)call_adjust_args);
    }
    add(csp, 3 * sizeof(void*));
    // result argc
    mov(edx, ac);
    if (SG_FALSEP(proc)) {
      pop(ac);
    }
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
  void scheme_vm(const Xbyak::Operand &d)
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

  // Debugging methods 
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
    push(ac);
    {
      argc(ac); push(ac);
      args(ac); push(ac);
      call((void*)JitCompiler::dump_args_impl);
      pop(ac);
      pop(ac);
    }
    pop(ac);
  }
#endif
  void inc_nest(int off)
  {
#ifdef JIT_DEBUG
    push(ac);
    push((uintptr_t)off);
    call((void*)JitCompiler::inc_nest_impl);
    pop(ac);
    pop(ac);
#endif
  }

  void trace(const char *msg)
  {
#ifdef JIT_DEBUG
    push(ac);
    push((uintptr_t)msg);
    call((void*)JitCompiler::trace_impl);
    pop(ac);
    pop(ac);
#endif
  }
  void dump_eax()
  {
#ifdef JIT_DEBUG
    push(ac);
    call((void*)JitCompiler::dump_eax_impl);
    pop(ac);
#endif
  }
public:
  void dump_code()
  {
    fprintf(stderr, "#vu8(\n");
    const uint8_t *p = getCode();
    int size = getSize();
    for (int i = 0; i < size; i++) {
      fprintf(stderr, " #x%02x", p[i]);
      if (((i+1) % 16) == 0) fprintf(stderr, "\n");
    }
    fprintf(stderr, ")\n");
  }
private:
  // instructions
  void consti_insn(SgWord *code, int i)
  {
    int val1;
    trace("enter CONSTI");
    INSN_VAL1(val1, code[i]);
    mov(ac, (uintptr_t)SG_MAKE_INT(val1));
    trace("leave CONSTI");
  }
  void push_insn()
  {
    trace("enter PUSH");
    dump_eax();
    vm_sp(edx, vm); // edx = vm->sp
    push_insn(edx, ac);
    trace("leave PUSH");
  }
  void push_insn(const Xbyak::Reg32e &sp, const Xbyak::Reg32e &r)
  {
    // push argument to vm stack
    mov(ptr[sp], r);
    // sp++
    vm_inc_sp(vm);
  }

  void lref_insn(SgWord code)
  {
    int val1;
    INSN_VAL1(val1, code);
    args(ac); // args
    mov(ac, ptr[ac + val1 * sizeof(void*)]); // args[n] -> edx
  }

  // TODO not call c function.
  void car_insn()
  {
    push(ac);
    call((void*)Sg_Car);
    add(csp, 1*sizeof(void*));
  }
  void cdr_insn()
  {
    push(ac);
    call((void*)Sg_Cdr);
    add(csp, 1*sizeof(void*));
  }

  void pairp_insn()
  {
    // TODO 64 bit
    mov(edx, ac);
    // set #f to eax
    mov(ac, (uintptr_t)SG_FALSE);
    test(dl, (uintptr_t)3);
    std::string l = gen_label();
    jne(l.c_str());
    mov(edx, ptr[edx]);
    and(edx, (uintptr_t)7);
    cmp(edx, (uintptr_t)7);
    mov(edx, (uintptr_t)SG_TRUE);
    cmovne(ac, edx);
    L(l.c_str());
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
  

static SgObject list_fun(int n, SgObject ac, SgVM *vm)
{
  SgObject ret = SG_NIL;
  n -= 1;
  if (n > 0) {
    ret = Sg_Cons(ac, ret);
    for (int i = 0; i < n; i++) {
      ret = Sg_Cons(INDEX(SP(vm), i), ret);
    }
    SP(vm) -= n;
  }
  return ret;
}

static SgObject prepare_apply(SgVM *vm, SgObject ac,
			      int val1, int val2, int rargc)
{
  if (rargc < 0) {
    Sg_AssertionViolation(SG_INTERN("apply"),
			  SG_MAKE_STRING("improper list not allowed"),
			  ac);
  }
  int nargc = val1 - 2;
  SgObject proc = INDEX(SP(vm), nargc);
  SgObject *fp = SP(vm) - (val1 - 1);
  JitCompiler::shift_args(fp, nargc, SP(vm));
  if (rargc == 0) {
    SP(vm)--;
    if (val2) {
      SP(vm) = JitCompiler::shift_args(FP(vm), nargc, SP(vm));
    }
  } else {
    INDEX_SET(SP(vm), 0, ac);
    if (val2) {
      SP(vm) = JitCompiler::shift_args(FP(vm), nargc+1, SP(vm));
    }
  }
  return proc;
}

#if 0
static void push_cont(SgVM *vm, SgWord *code, int n)
{
  SgContFrame *newcont = (SgContFrame*)SP(vm);
  newcont->prev = CONT(vm);
  newcont->fp = FP(vm);
  newcont->size = (int)(SP(vm) - FP(vm));
  newcont->pc = code+n;
  newcont->cl = CL(vm);
  CONT(vm) = newcont;
  SP(vm) += CONT_FRAME_SIZE;
}
#endif

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
    SgObject gproc = SG_FALSE, test_obj = SG_FALSE;
    bool tail = false;
    // emit jump destination label, if there is
    Labels::iterator itr = context_.dstLabels.find(code+i);
    if (itr != context_.dstLabels.end()) {
      L(itr->second.c_str());
    }
    // if it has some result. cf) some instruction does not have
    // result such as PUSH.
    switch (insn) {
    case FRAME: 
#if 0
      // to make vm's stack compatible...
      trace("enter FRAME");
      push((uintptr_t)SG_INT_VALUE(code[i+1]));
      push((uintptr_t)(code+i));
      push(vm);
      call((void*)push_cont);
      add(csp, 3 * sizeof(void*));
      trace("leave FRAME");
#endif
      break;
      // ENTER vm instruction is more like for sanity...
    case ENTER: break;
    case LREF_PUSH:
      trace("enter LREF_PUSH");
      INSN_VAL1(val1, code[i]);
      // keep ac clean, for heavy call
      args(ecx);  // args
      // ecx = arg_ref[val1]
      mov(ecx, ptr[ecx + val1 * sizeof(void*)]);
      vm_sp(edx, vm);
      push_insn(edx, ecx);
      trace("leave LREF_PUSH");
      break;
    case PUSH: push_insn(); break;
    case LREF:
      trace("enter LREF");
      INSN_VAL1(val1, code[i]);
      args(ac); // args
      mov(ac, ptr[ac + val1 * sizeof(void*)]); // args[n] -> edx
      trace("leave LREF");
      break;
    case GREF_PUSH: {
      trace("enter GREF_PUSH");
      SgObject o;
      retrive_next_gloc(o, SG_OBJ(code[i+1]));
      mov(ac, (uintptr_t)o);
      push_insn();
      trace("leave GREF_PUSH");
      break;
    }
    case CONST: {
      trace("enter CONST");
      SgObject o = SG_OBJ(code[i+1]);
      mov(ac, (uintptr_t)o);
      trace("leave CONST");
      break;
    }
    case CONST_PUSH: {
      trace("enter CONST_PUSH");
      SgObject o = SG_OBJ(code[i+1]);
      // avoid to use ac.
      mov(ecx, (uintptr_t)o);
      vm_sp(edx, vm);
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
      vm_sp(edx, vm);
      push_insn(edx, ecx);
      trace("leave CONSTI");
      break;
    case BNNUME:
      bnproc = (void *)Sg_NumEq;
      trace("enter BNNUME");
      goto bnnum_entry;
    case BNGE:
      bnproc = (void *)Sg_NumGe;
      trace("enter BNGE");
      goto bnnum_entry;
    case BNGT:
      bnproc = (void *)Sg_NumGt;
      trace("enter BNGT");
      goto bnnum_entry;
    case BNLE: 
      bnproc = (void *)Sg_NumLe;
      trace("enter BNLE");
      goto bnnum_entry;
    case BNLT: 
      bnproc = (void *)Sg_NumLt;
      trace("enter BNLT");
      goto bnnum_entry;
    bnnum_entry:
      {
	int n = SG_INT_VALUE(code[i+1]);
	push(ac);
	vm_stack_ref(edx, vm, 0);
	push(edx);
	call(bnproc);
	add(csp, 2 * sizeof(void*));
	// vm->sp--
	vm_dec_sp(vm);
	std::string l = gen_label();
	cmp(ac, 1);
	jne(l.c_str(), T_NEAR);
	// we don't set #f or #t to ac, it's useless
	// TODO if jump instruction
	int j= compile_rec(code + i + 2, n);
	L(l.c_str());
	j += compile_rec(code + i + 1 + n, size - i - j);
	i += j;
	trace("leave BNNUM");
	break;
      }
    case TEST:
      test_obj = SG_FALSE;
      goto test_entry;
    case BNNULL: 
      test_obj = SG_NIL;
      goto test_entry;
    test_entry:
      {
	int n = SG_INT_VALUE(code[i+1]);
	trace("enter BNNULL");
	std::string l = gen_label();
	cmp(ac, (uintptr_t)test_obj);
	// FIXME: not all jmp shouldn't be T_SHORT but
	// how much should we take?
	jne(l.c_str(), T_NEAR);
	// we don't set #f or #t to ac, it's useless
	// TODO if jump instruction
	int j= compile_rec(code + i + 2, n);
	L(l.c_str());
	j += compile_rec(code + i + 1 + n, size - i - j);
	i += j;
	trace("leave BNNULL");
	break;
      }
    case ADD: {
      trace("enter ADD");
      vm_stack_ref(edx, vm, 0);
      vm_dec_sp(vm);		// vm->sp--;
      push(ac);
      push(edx);
      call((void*)Sg_Add);
      add(csp, 2*sizeof(void*));
      trace("leave ADD");
      break;
    }
    case ADDI: {
      trace("enter ADDI");
      INSN_VAL1(val1, code[i]);
      // TODO 64 bits
      mov(edx, ac);
      // SG_INTP(vm->ac);
      and(edx, 3);
      cmp(edx, 1);
      std::string label = gen_label();
      std::string end_label = gen_label();
      je(label.c_str());
      // ac != int
      push(ac);
      push((uintptr_t)SG_MAKE_INT(val1));
      call((void*)Sg_Add);
      add(csp, 2 * sizeof(void*));
      jmp(end_label.c_str());
      // for now...
      L(label.c_str());
      sar(ac, 2);
      add(ac, val1);
      push(ac);
      call((void*)Sg_MakeInteger);
      add(csp, 1 * sizeof(void*));
      L(end_label.c_str());
      trace("leave ADDI");
      break;
    }
    case GREF_TAIL_CALL: {
      trace("enter GREF_TAIL_CALL");
      //inc_nest(1);
      INSN_VAL1(val1, code[i]);
      retrive_next_gloc(gproc, SG_OBJ(code[i+1]));
      // for proper tail recursive, we can not consume c stack either.
      // so make this simple jump
      // manage vm->sp part
      vm_sp(edx, vm);
      push(edx);
      push((uintptr_t)val1);
      vm_fp(ecx, vm);
      push(ecx);
      call((void*)JitCompiler::shift_args);
      add(csp, 3 * sizeof(void*));
      // vm->sp = ac
      mov(ptr[vm + offsetof(SgVM, sp)], ac);
      tail = true;
      goto call_entry;
#if 0
      // manage fp
      lea(ac, ptr[ac - (val1 *sizeof(void*))]);
      // set vm->fp
      mov(ptr[vm + offsetof(SgVM, fp)], ac);
      vm_sp(edx, vm);
      // now vm stack is ok next is re-use c-stack
      // args = vm->fp
      // argc = val1
      // data = vm or subr data
      mov(ptr[ebp + ARGS_OFF], ac);
      mov(dword[ebp + ARGC_OFF], (uintptr_t)val1);
      // for now
      mov(ptr[ebp + VM_OFF], vm);
      // vm->cl = procedure
      //mov(ptr[vm + offsetof(SgVM, cl)], (uintptr_t)o);
      // ok emit jump
      leave();
      if (SG_EQ(gproc, closure_)) {
	// tail recursive self call
	jmp(this_label_.c_str());
      } else if (SG_SUBRP(gproc)) {
	jmp((void*)SG_SUBR_FUNC(gproc), T_NEAR);
      } else if (SG_CLOSUREP(gproc)) {
	// if a closure reaches here, it must be compiled already
	// see walker.
	SgObject p = SG_CLOSURE(gproc)->native;
	jmp((void*)SG_SUBR_FUNC(p), T_NEAR);
      } else {
	throw std::runtime_error("generic functions are not supported yet.");
      }
      break;
#endif
    }
    case GREF_CALL: 
      trace("enter GREF_CALL");
      inc_nest(1);
      retrive_next_gloc(gproc, SG_OBJ(code[i+1]));
      goto call_entry;
      break;
    call_entry:
    case CALL: {
      INSN_VAL1(val1, code[i]);
      // adjust arguments
      adjust_args(gproc, val1, false);
      scheme_call(gproc, tail);
      if (!tail) {
	inc_nest(-1);
	trace("leave CALL");
      }
      break;
    }
    case APPLY: {
      trace("enter APPLY");
      INSN_VAL2(val1, val2, code[i]);
      push(ac);
      call((void*)Sg_Length);
      mov(ecx, ac);
      pop(ac);

      push(ecx);		// rargc
      {
	push(val2);
	push(val1);
	push(ac);
	push(vm);
	call((void*)prepare_apply);
	add(csp, 4 * sizeof(void*));
      }
      pop(ecx);			// restore rargc

      push(ac);			// save proc
      push(ecx);		// save (save it for sanity)
      {
	adjust_args(SG_FALSE, val1 - 2, true);
      }
      // do not edit edx until scheme_call is called.
      pop(ecx);
      pop(ac);

      scheme_call(SG_FALSE, val2);
      trace("leave APPLY");
      break;
    }
    case SHIFTJ: {
      trace("enter SHIFTJ");
      INSN_VAL2(val1, val2, code[i]);
      // TODO do we need this?
      push(ac);
      vm_sp(edx, vm);
      push(edx);
      push((uintptr_t)val1);
      vm_fp(ecx, vm);
      lea(ecx, ptr[ecx + val2*sizeof(void*)]);
      push(ecx);
      call((void*)JitCompiler::shift_args);
      add(csp, 3 * sizeof(void*));
      // vm->sp = ac
      mov(ptr[vm + offsetof(SgVM, sp)], ac);
      pop(ac);
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
    case PAIRP:
      trace("enter PAIRP");
      pairp_insn();
      trace("leave PAIRP");
      break;
    case LIST:
      trace("enter LIST");
      INSN_VAL1(val1, code[i]);
      push(vm);
      push(ac);
      push((uintptr_t)val1);
      call((void*)list_fun);
      add(csp, 3 * sizeof(void*));
      trace("leave LIST");
      break;
    case CAR:
      trace("enter CAR");
      car_insn();
      trace("leave CAR");
      break;
    case LREF_CAR:
      trace("enter LREF_CAR");
      lref_insn(code[i]);
      car_insn();
      trace("leave LREF_CAR");
      break;
    case LREF_CAR_PUSH:
      trace("enter LREF_CAR_PUSH");
      lref_insn(code[i]);
      car_insn();
      push_insn();
      trace("leave LREF_CAR_PUSH");
      break;
    case LREF_CDR_PUSH:
      trace("enter LREF_CDR_PUSH");
      lref_insn(code[i]);
      cdr_insn();
      push_insn();
      trace("leave LREF_CDR_PUSH");
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

#ifdef JIT_DEBUG
#include "sagittarius/writer.h"
#include "sagittarius/port.h"
#endif

int Sg_JitCompileClosure(SgObject closure)
{
  DefaultWalker walker;
  JitCompiler compiler(SG_CLOSURE(closure), walker);

  if (compiler.compile()) {
    compiler.ready();
    SgObject subr
      = Sg_MakeSubr(reinterpret_cast<SgSubrProc *>(compiler.getCode()),
		    NULL,
		    SG_PROCEDURE_REQUIRED(closure),
		    SG_PROCEDURE_OPTIONAL(closure),
		    SG_PROCEDURE_NAME(closure));
    /*
#ifdef JIT_DEBUG
    Sg_Printf(SG_PORT(Sg_StandardErrorPort()), UC("%S:%d\n"), closure,
	      compiler.getSize());
    compiler.dump_code();
#endif
    */
    SG_CLOSURE(closure)->native = subr;
    SG_CLOSURE(closure)->state = SG_NATIVE;
    return TRUE;
  }
  SG_CLOSURE(closure)->state = SG_INVALID_FOR_NATIVE;
  return FALSE;
}
