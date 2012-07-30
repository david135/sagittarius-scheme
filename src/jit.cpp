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
// debugging flags
//#define JIT_DEBUG
//#define JIT_DUMP

#include <math.h>
#include <string>
#include <map>
#include <set>
#include <stdexcept>
#include "xbyak/xbyak.h"
#define LIBSAGITTARIUS_BODY
#include "sagittarius/closure.h"
#include "sagittarius/code.h"
#include "sagittarius/compare.h"
#include "sagittarius/error.h"
#include "sagittarius/core.h"
#include "sagittarius/gloc.h"
#include "sagittarius/identifier.h"
#include "sagittarius/instruction.h"
#include "sagittarius/library.h"
#include "sagittarius/number.h"
#include "sagittarius/pair.h"
#include "sagittarius/symbol.h"
#include "sagittarius/string.h"
#include "sagittarius/vm.h"

static int jit_compile_closure_rec(SgObject closure, SgObject set);

typedef std::map<SgWord*, std::string> Labels;
struct WalkContext
{
  bool   noGSET;
  Labels srcLabels;
  Labels dstLabels;
  SgObject seen;		// list of closure
  WalkContext(SgObject sets) 
    : noGSET(false)		// TODO check 
    , seen(sets)
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
  virtual void walk(SgClosure *closure, WalkContext &ctx)
  {
    walk(closure, SG_CODE_BUILDER(closure->code), ctx);
  }
private:
  void walk(SgClosure *closure, SgCodeBuilder *cb, WalkContext &ctx);
};

#define check_bound_gloc(o, d)					\
  do {								\
    d = o;							\
    if (SG_IDENTIFIERP(o)) {					\
      d = Sg_FindBinding(SG_IDENTIFIER_LIBRARY(o),		\
			 SG_IDENTIFIER_NAME(o),			\
			 SG_UNBOUND);				\
      if (SG_UNBOUNDP(d)) {					\
	throw std::runtime_error("unbound variable appeard.");	\
      }								\
    }								\
  } while (0);


void DefaultWalker::walk(SgClosure *closure, SgCodeBuilder *cb,
			 WalkContext &ctx)
{
  SgWord *code = cb->code;
  for (int i = 0; i < cb->size;) {
    Instruction insn = static_cast<Instruction>(INSN(code[i]));
    InsnInfo *info = Sg_LookupInsnName(insn);
    bool allow_only_gloc = false;
    bool check_only_gloc = false;
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
    case CLOSURE: {
      // in case we can't compile internal closure, try to avoid the
      // inconsistency.
      SgObject ncb = SG_OBJ(code[i+1]);
      WalkContext nctx(ctx.seen);
      // the real walking is done in runtime. so we can simply ignore the
      // result, just check if it has an error.
      walk(closure, SG_CODE_BUILDER(ncb), nctx);
      break;
    }
      // for now
    case GSET:
      //allow_only_gloc = true;
    case GREF:
    case GREF_PUSH:
      check_only_gloc = true;
    case GREF_CALL:
    case GREF_TAIL_CALL: {
      SgObject p = SG_OBJ(code[i+1]);
      if (allow_only_gloc && SG_IDENTIFIERP(p)) {
	// FIXME
	// resolving identifier in runtime with GSET instruction is really
	// painful, so for now we don't allow it.
	throw std::runtime_error("given closure contains setting global "
				 "variable. it must be run at lease once.");
      }
      check_bound_gloc(p, p);
      if (check_only_gloc) break;

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
	int r = jit_compile_closure_rec(p, Sg_Cons(closure, ctx.seen));
	if (!r) {
	  throw std::runtime_error("nested closure jit compile failed.");
	}
      }
    }
    case CALL:
    case TAIL_CALL:
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

#include "vm-common.c"

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
    // higher order function can not be specified during walking path.
    // so this path actually exists, let's compile it.
    if (SG_CLOSURE(proc)->state == SG_NON_NATIVE) {
      if (!Sg_JitCompileClosure(proc)) {
	Sg_Error(UC("jit compile failed %S"), proc);
      }
    }
    return (void*)SG_SUBR_FUNC(SG_CLOSURE(proc)->native);
  } else {
    Sg_Error(UC("not supported yet, %S"), proc);
    return NULL;
  }
}

static int check_subr(SgObject proc)
{
  return SG_SUBRP(proc);
}

// push and pop cont
static void push_cont(SgVM *vm, SgWord *code, int n)
{
  PUSH_CONT(vm, code+n);
}

static SgObject pop_cont(SgObject r, SgVM *vm)
{
  AC(vm) = r;
  POP_CONT();
  return AC(vm);
}

static SgObject maybe_pop_cont(SgObject r, SgVM *vm, int pop)
{
  AC(vm) = r;
  if (pop) {
    POP_CONT();
  }
  return AC(vm);
}

struct JitAllocator : public Xbyak::Allocator
{
  virtual uint8_t * alloc(size_t size)
  {
    return Xbyak::Allocator::alloc(size);
  }
  // do nothing. let garbage collector do it
  virtual void free(uint8_t *p) {}

  void collect(void *p)
  {
    Xbyak::Allocator::free((uint8_t *)p);
  }
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
  Xbyak::Reg64 argc;
  Xbyak::Reg64 args;
#elif  defined(XBYAK32)
  Xbyak::Reg32 vm;		// ebx
  Xbyak::Reg32 ac;		// eax
  Xbyak::Reg32 csp;		// esp (c stack pointer)
  Xbyak::Reg32 argc;		// esi
  Xbyak::Reg32 args;		// edi
#else
  #error "JIT not supported for this architecture"
#endif

public:
  JitCompiler(SgClosure * const closure, Walker & walker, SgObject sets)
    : CodeGenerator(((SG_CODE_BUILDER(closure->code)->size * 20)/Xbyak::DEFAULT_MAX_CODE_SIZE + 1) *
#ifdef JIT_DEBUG
		    Xbyak::DEFAULT_MAX_CODE_SIZE * 4
#else
		    Xbyak::DEFAULT_MAX_CODE_SIZE
#endif
		    ,0, &jit_allocator)
    , closure_(closure)
    , walker_(walker)
    , context_(sets)
    , label_(0)
#ifdef XBYAK64
    , vm(rbx)
    , ac(rax)
    , csp(rsp)
#elif  defined(XBYAK32)
    , vm(ebx)
    , ac(eax)
    , csp(esp)
    , argc(esi)
    , args(edi)
#endif
  {}
  
  // compile the closure
  bool compile();

private:
  static const int VM_OFF = 16;
  static const int ARGC_OFF = 12;
  static const int ARGS_OFF = 8;

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
    push(esi);
    push(edi);

    scheme_vm(vm);
    set_args(args);
    set_argc(argc);
#ifdef JIT_DEBUG
    dump_args();
    dump_reg(ebx);
#endif
  }

  void epilogue()
  {
    this->pop_cont();
    trace("leaving (after pop_cont)");
    pop(edi);
    pop(esi);
    pop(ebx);
    pop(ebp);
    ret();
  }
  // common instructions
  void leave(const Xbyak::Reg32e &new_args, const Xbyak::Reg32e &new_argc,
	     const Xbyak::Reg32e &vm, bool need_pop = false)
  {
    //pop(edx); pop(edx);
    mov(ptr[ebp + ARGS_OFF], new_args);
    mov(dword[ebp + ARGC_OFF], new_argc);
    mov(ptr[ebp + VM_OFF], vm);

    pop(edi);
    pop(esi);
    pop(ebx);
    mov(esp, ebp);
    pop(ebp);

    if (need_pop) {
      pop_cont();
    }
  }

  void pop_cont()
  {
    push(vm);
    push(ac);
    call((void*)::pop_cont);
    add(csp, 2 * sizeof(void*));
  }
  // calling C API for scheme
  // All scheme procedures are proc(void**, int, void*) signature.
  // Assume edx is argc returned from adjust_argument_frame.
  void scheme_tail_call(SgObject proc)
  {
    if (SG_FALSEP(proc)) {
      push(ac);
      call((void*)check_subr);
      mov(edx, ac);
      pop(ac);

      push(edx);
      push(ac);
      call((void*)convert_proc);
      add(csp, 1 * sizeof(void*));
      pop(edx);
      // ac is now callable procedure

      // we need to check if the calling procedure is subr or not.
      std::string e_l = gen_label();
      vm_fp(ecx, vm);
      cmp(edx, 1);		// i hope this is unlikely
      je(e_l.c_str());
      leave(ecx, argc, vm);
      jmp(ac);
      L(e_l.c_str());
      // see NOTE (*1) below.
      // TODO get subr data
      push(vm);
      push(argc);
      push(ecx);
      call(ac);
      add(csp, 3 * sizeof(void*));
    } else {
      vm_fp(ecx, vm);
      if (SG_SUBRP(proc)) {
	// NOTE (*1)
	// if the calling procedure is subr, we can't make it tail call,
	// otherwise the call does not pop continuation frame.
	// unless we modify the saved eip, but it's too much I guess.
	push((uintptr_t)SG_SUBR_DATA(proc));
	push(argc);
	push(ecx);
	//mov(ac, (uintptr_t)SG_SUBR_DATA(proc));
	//leave(ecx, argc, ac, true);
      } else {
	leave(ecx, argc, vm);
      }
      if (SG_EQ(proc, closure_)) {
	// TODO this case we can optimise not to pop the saved registers.
	// but later.
	jmp(this_label_.c_str(), T_NEAR);
      } else if (SG_SUBRP(proc)) {
	//jmp((void*)SG_SUBR_FUNC(proc), T_NEAR);
	call((void*)SG_SUBR_FUNC(proc));
	add(csp, 3 * sizeof(void*));
      } else if (SG_CLOSUREP(proc)) {
	jmp((void*)SG_SUBR_FUNC(SG_CLOSURE(proc)->native), T_NEAR);
      } else {
	throw std::runtime_error("generic methods are not supported yet");
      }
    }
  }
  void scheme_call(SgObject proc, bool tail)
  {
    if (SG_FALSEP(proc)) {
      // put proc to cl
      mov(ptr[vm + offsetof(SgVM, cl)], ac);
    } else {
      mov(edx, (uintptr_t)proc);
      mov(ptr[vm + offsetof(SgVM, cl)], edx);
    }

    if (tail) {
      scheme_tail_call(proc);
      return;
    }
    // this check must be the first place(x86 call convention)
    if (SG_FALSEP(proc)) {
      //push(edx);
      push(ac);
      {
	push(ac);
	call((void*)check_subr);
	add(csp, 1 * sizeof(void*));
	mov(ecx, ac);
      }
      pop(ac);
      //pop(edx);
      // used for maybe_pop_cont
      push(ecx);
    }
    if (SG_SUBRP(proc)) {
      push((uintptr_t)SG_SUBR_DATA(proc));
    } else {
      push(vm);
    }
    //push(edx);
    push(argc);
    // TODO 64 bit
    vm_fp(ecx, vm);
    push(ecx);

    if (SG_FALSEP(proc)) {
      // convert subr to native code if necessary
      push(ac);
      call((void*)convert_proc);
      add(csp, 1 * sizeof(void*));

      call(ac);
      add(csp, 3 * sizeof(void*));
      // check if the called procedure was subr or not
      // and if it's subr we need to call pop_cont
      push(vm);
      push(ac);
      call((void*)maybe_pop_cont);
      add(csp, 3 * sizeof(void*));
    } else {
      if (SG_EQ(proc, closure_)) {
	// self call.
	call(this_label_.c_str());
      } else if (SG_SUBRP(proc)) {
	call((void*)SG_SUBR_FUNC(proc));
	this->pop_cont();
      } else if (SG_CLOSUREP(proc)) {
	call((void*)SG_SUBR_FUNC(SG_CLOSURE(proc)->native));
      } else {
	throw std::runtime_error("generic methods are not supported yet");
      }
      add(csp, 3 * sizeof(void*));
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
    //mov(edx, ac);
    mov(this->argc, ac);
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
  void vm_stack_set(const Xbyak::Reg32e &v, const Xbyak::Reg32e &vm,
		    int index,
		    const Xbyak::Reg32e &tmp)
  {
    mov(tmp, ptr[vm + offsetof(SgVM, sp)]);
    mov(ptr[tmp - (index + 1) * sizeof(void*)], v);
  }
  // only for inialisation
  void set_args(const Xbyak::Operand &d)
  {
    stack_access(d, ARGS_OFF);
  }
  void set_argc(const Xbyak::Operand &d)
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
    mov(ptr[vm + offsetof(SgVM, sp)], ecx);
  }
  void vm_inc_sp(const Xbyak::Reg32e &vm)
  {
    add_offset(vm, 1*sizeof(void*));
  }
  void vm_dec_sp(const Xbyak::Reg32e &vm)
  {
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
  static void trace_enter_leave(InsnInfo *info, SgWord code)
  {
    SgVM *vm = Sg_VM();
    if (vm->state != COMPILING) {
      char buf[50] = {0};
      if (code >= 0 && info->instValues != 0) {
	int val1, val2;
	switch (info->instValues) {
	case 1:
	  INSN_VAL1(val1, code);
	  snprintf(buf, 50, "%s(%d)", info->name, val1);
	  break;
	case 2:
	  INSN_VAL2(val1, val2, code);
	  snprintf(buf, 50, "%s(%d %d)", info->name, val1, val2);
	  break;
	}
      } else {
	snprintf(buf, 50, "%s", info->name);
      }
      for (int i = 0; i < nest_level*2; i++) {
	fputc(' ', stderr);
      }
      fprintf(stderr, "%d: %s %s(sp:%p fp:%p ac:%p *sp:%p)\n",
	      nest_level, (code >= 0) ? "enter" : "leave",
	      buf,
	      vm->sp, vm->fp, vm->ac, *(vm->sp-1));
    }
  }
  static void trace_impl(const char *msg)
  {
    SgVM *vm = Sg_VM();
    if (vm->state != COMPILING) {
      for (int i = 0; i < nest_level*2; i++) {
	fputc(' ', stderr);
      }
      fprintf(stderr, "%d: %s(sp:%p fp:%p ac:%p *sp:%p)\n",
	      nest_level, msg, vm->sp, vm->fp, vm->ac, *(vm->sp-1));
    }
  }
  static void dump_eax_impl(void *eax)
  {
    SgVM *vm = Sg_VM();
    if (vm->state != COMPILING) {
      fprintf(stderr, "reg: %p\n", eax);
    }
  }
  static void inc_nest_impl(int off)
  {
    nest_level += off;
  }
  static void dump_args_impl(void **args, int argc)
  {
    SgVM *vm = Sg_VM();
    if (vm->state != COMPILING) {
      fprintf(stderr, "sp: %p, fp: %p, args %d:\n", vm->sp, vm->fp, argc);
      for (int i = 0; i < argc; i++) {
	fprintf(stderr, " %d: %p\n", i, args[i]);
      }
      fprintf(stderr, "\n");
    }
  }

  void dump_args()
  {
    push(ac);
    {
      set_argc(ac), push(ac);
      set_args(ac), push(ac);
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

  void trace_enter(SgWord code, InsnInfo *info)
  {
#ifdef JIT_DEBUG
    push(ac);
    {
      push((uintptr_t)code);
      push((uintptr_t)info);
      call((void*)JitCompiler::trace_enter_leave);
      pop(ac);
      pop(ac);
    }
    pop(ac);
#endif
  }

  void trace_leave(InsnInfo *info)
  {
#ifdef JIT_DEBUG
    push(ac);
    {
      push((uintptr_t)-1);
      push((uintptr_t)info);
      call((void*)JitCompiler::trace_enter_leave);
      pop(ac);
      pop(ac);
    }
    pop(ac);
#endif
  }
  void trace(const char * msg)
  {
#ifdef JIT_DEBUG
    push(ac);
    {
      push((uintptr_t)msg);
      call((void*)JitCompiler::trace_impl);
      pop(ac);
    }
    pop(ac);
#endif
  }

  void dump_eax()
  {
#ifdef JIT_DEBUG
    dump_reg(ac);
#endif
  }
  void dump_reg(const Xbyak::Reg32e &r)
  {
#ifdef JIT_DEBUG
    push(ac);
    {
      push(r);
      call((void*)JitCompiler::dump_eax_impl);
      pop(r);
    }
    pop(ac);
#endif
  }
public:
  void dump_code()
  {
#ifdef JIT_DUMP
    fprintf(stderr, "#vu8(\n");
    const uint8_t *p = getCode();
    int size = getSize();
    for (int i = 0; i < size; i++) {
      fprintf(stderr, " #x%02x", p[i]);
      if (((i+1) % 16) == 0) fprintf(stderr, "\n");
    }
    fprintf(stderr, ")\n");
#endif
  }
private:
  // instructions
  void consti_insn(SgWord *code, int i)
  {
    int val1;
    INSN_VAL1(val1, code[i]);
    mov(ac, (uintptr_t)SG_MAKE_INT(val1));
  }
  void push_insn()
  {
    vm_sp(edx, vm); // edx = vm->sp
    push_insn(edx, ac);
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
    lref_insn(code, ac);
  }

  void lref_insn(SgWord code, const Xbyak::Reg32e &d)
  {
    int val1;
    INSN_VAL1(val1, code);
    //vm_fp(ecx, vm);
    mov(d, ptr[args + val1 * sizeof(void*)]);
  }

  void fref_insn(SgWord code)
  {
    fref_insn(code, ac);
  }

  void fref_insn(SgWord code, const Xbyak::Reg32e &d)
  {
    int val1;
    INSN_VAL1(val1, code);
    vm_cl(d, vm);
    mov(d, ptr[d + offsetof(SgClosure, frees) + val1 * sizeof(void*)]);
  }

  void tail_call_insn(SgWord code)
  {
    int val1;
    INSN_VAL1(val1, code);
    // manage vm->sp part
    vm_sp(edx, vm);
    push(edx);
    push((uintptr_t)val1);
    vm_fp(ecx, vm);
    push(ecx);
    call((void*)::shift_args);
    add(csp, 3 * sizeof(void*));
    // vm->sp = ac
    mov(ptr[vm + offsetof(SgVM, sp)], ac);
  }

  void local_call_insn(SgWord code, bool tail)
  {
    int val1;
    INSN_VAL1(val1, code);
    mov(ptr[vm + offsetof(SgVM, cl)], ac);
    vm_sp(edx, vm);
    lea(edx, ptr[edx - val1 * sizeof(void*)]);
    mov(ptr[vm + offsetof(SgVM, fp)], edx);
    scheme_call(SG_FALSE, tail);
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

  void cons_insn()
  {
    push(ac);
    vm_stack_ref(edx, vm, 0);
    push(edx);
    call((void*)Sg_Cons);
    add(csp, 2 * sizeof(void*));
    vm_dec_sp(vm);
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

  void immediate_calc(SgWord code, void *func)
  {
    int val1;
    INSN_VAL1(val1, code);
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
    call(func);
    add(csp, 2 * sizeof(void*));
    jmp(end_label.c_str());
    // immediate calc
    L(label.c_str());
    sar(ac, 2);
    if (func != (void*)Sg_Add) {
      neg(ac);
    }
    add(ac, val1);
    push(ac);
    call((void*)Sg_MakeInteger);
    add(csp, 1 * sizeof(void*));
    L(end_label.c_str());
  }

  SgObject gref_insn(SgObject o)
  {
    check_bound_gloc(o, o);
    if (context_.noGSET) {
      o = SG_GLOC_GET(SG_GLOC(o));
      mov(ac, (uintptr_t)o);
      return o;
    } else {
      mov(ac, (uintptr_t)o);
      mov(ac, ptr[ac + offsetof(SgGloc, value)]);
      return SG_FALSE;
    }
  }
  
  void gref_car_insn(SgObject o)
  {
    o = gref_insn(o);
    if (SG_FALSEP(o)) {
      car_insn();
    } else {
      mov(ac, (uintptr_t)Sg_Car(o));
      add(csp, 1 * sizeof(void*));
    }
  }
  void gref_cdr_insn(SgObject o)
  {
    o = gref_insn(o);
    if (SG_FALSEP(o)) {
      cdr_insn();
    } else {
      mov(ac, (uintptr_t)Sg_Cdr(o));
      add(csp, 1 * sizeof(void*));
    }
  }
};

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
  ::shift_args(fp, nargc, SP(vm));
  if (rargc == 0) {
    SP(vm)--;
    if (val2) {
      SP(vm) = ::shift_args(FP(vm), nargc, SP(vm));
    }
  } else {
    INDEX_SET(SP(vm), 0, ac);
    if (val2) {
      SP(vm) = ::shift_args(FP(vm), nargc+1, SP(vm));
    }
  }
  return proc;
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
    void *bnproc = NULL, *calcproc = NULL;
    // for gref_call
    SgObject gproc = SG_FALSE, test_obj = SG_FALSE;
    bool tail = false, reverse = false;
    // emit jump destination label, if there is
    Labels::iterator itr = context_.dstLabels.find(code+i);
    if (itr != context_.dstLabels.end()) {
      L(itr->second.c_str());
    }
    // if it has some result. cf) some instruction does not have
    // result such as PUSH.
    trace_enter(code[i], info);
    switch (insn) {
    case NOP: nop();
    case FRAME: 
      // to make vm's stack compatible...
      push((uintptr_t)SG_INT_VALUE(code[i+1]));
      push((uintptr_t)(code+i));
      push(vm);
      call((void*)push_cont);
      add(csp, 3 * sizeof(void*));
      break;
      // ENTER vm instruction is more like for sanity...
    case ENTER: break;
    case UNDEF: 
      mov(ac, (uintptr_t)SG_UNDEF);
      break;
    case PUSH: push_insn(); break;
    case LREF: lref_insn(code[i]); break;
    case LREF_PUSH:
      lref_insn(code[i], ecx);
      vm_sp(edx, vm);
      push_insn(edx, ecx);
      break;
    case FREF: fref_insn(code[i]); break;
    case FREF_PUSH:
      fref_insn(code[i], ecx);
      vm_sp(edx, vm);
      push_insn(edx, ecx);
      break;
    case GREF: gref_insn(SG_OBJ(code[i+1])); break;
    case GREF_PUSH: gref_insn(SG_OBJ(code[i+1])); push_insn(); break;
    case CONST: {
      SgObject o = SG_OBJ(code[i+1]);
      mov(ac, (uintptr_t)o);
      break;
    }
    case CONST_PUSH: {
      SgObject o = SG_OBJ(code[i+1]);
      // avoid to use ac.
      mov(ecx, (uintptr_t)o);
      vm_sp(edx, vm);
      push_insn(edx, ecx);
      break;
    }
    case CONST_RET: {
      mov(ac, (uintptr_t)code[i+1]);
      epilogue();
      return i+2;
    }
    case CONSTI: consti_insn(code, i); break;
    case CONSTI_PUSH:
      // the same as LREF_PUSH;
      INSN_VAL1(val1, code[i]);
      mov(ecx, (uintptr_t)SG_MAKE_INT(val1));
      vm_sp(edx, vm);
      push_insn(edx, ecx);
      break;
    case BOX:
      INSN_VAL1(val1, code[i]);
      // save ac just in case
      push(ac);
      {
	vm_stack_ref(edx, vm, val1);
	push(edx);
	call((void*)::make_box);
	add(csp, 1 * sizeof(void*));
	vm_stack_set(ac, vm, val1, edx);
      }
      pop(ac);
      break;
    case UNBOX:
      mov(ac, ptr[ac + offsetof(SgBox, value)]);
      break;
      // SET related
    case LSET:
      lref_insn(code[i], ecx);
      goto lfset_entry;
    case FSET:
      fref_insn(code[i], ecx);
    lfset_entry:
      mov(ptr[ecx + offsetof(SgBox, value)], ac);
      mov(ac, (uintptr_t)SG_UNDEF);
      break;
    case GSET: {
      SgObject o = SG_OBJ(code[i+1]);
      if (SG_IDENTIFIERP(o)) {
	// make gloc here
	SgObject old;
	check_bound_gloc(o, old);
	// FIXME, this this path should not be allowed.
	push((uintptr_t)0);
	push(ac);
	push((uintptr_t)SG_IDENTIFIER_NAME(o));
	push((uintptr_t)SG_IDENTIFIER_LIBRARY(o));
	call((void*)Sg_MakeBinding);
	add(csp, 4 * sizeof(void*));
      } else {
	mov(edx, (uintptr_t)o);
	mov(ptr[edx + offsetof(SgGloc, value)], ac);
      }
      trace("after GSET");

      mov(ac, (uintptr_t)SG_UNDEF);
      break;
    }
    case NOT: {
      cmp(ac, (uintptr_t)SG_FALSE);
      std::string n = gen_label();
      std::string e = gen_label();
      je(n.c_str());
      mov(ac, (uintptr_t)SG_FALSE);
      jmp(e.c_str());
      L(n.c_str());
      mov(ac, (uintptr_t)SG_TRUE);
      L(e.c_str());
      break;
    }
    case BNEQ:
      bnproc = (void*)Sg_EqP;	// FIXME
      goto bnnum_entry;
    case BNEQV:
      bnproc = (void*)Sg_EqvP;	// FIXME
      goto bnnum_entry;
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
	break;
      }
    case TEST:
      test_obj = SG_FALSE;
      reverse = true;
      goto test_entry;
    case BNNULL: 
      test_obj = SG_NIL;
      goto test_entry;
    test_entry:
      {
	int n = SG_INT_VALUE(code[i+1]);
	std::string l = gen_label();
	cmp(ac, (uintptr_t)test_obj);
	// FIXME: not all jmp shouldn't be T_SHORT but
	// how much should we take?
	if (reverse)
	  je(l.c_str(), T_NEAR);
	else
	  jne(l.c_str(), T_NEAR);
	// we don't set #f or #t to ac, it's useless
	// TODO if jump instruction
	int j= compile_rec(code + i + 2, n);
	L(l.c_str());
	j += compile_rec(code + i + 1 + n, size - i - j);
	i += j;
	break;
      }
    case CLOSURE: {
      // closure instruction needs to create a closure with free variables,
      // hence we need to make it in runtime and compile it in runtime!!
      // Note: the code builder itself has been check by now. see walker.
      SgObject cb = SG_OBJ(code[i+1]);
      vm_sp(edx, vm);
      // vm->sp -= SG_CODE_BUILDER_FREEC(cb);
      lea(edx, ptr[edx - SG_CODE_BUILDER_FREEC(cb) * sizeof(void*)]);
      mov(ptr[vm + offsetof(SgVM, sp)], edx);
      push(edx);
      push((uintptr_t)cb);
      call((void*)Sg_MakeClosure);
      add(csp, 2 * sizeof(void*));
      // convert_proc treats the jit compilation. so let's make it delay
      // until it's needed. (well, usually it's next call...)
      //push(ac);
      //call((void*)Sg_JitCompileClosure);
      //pop(ac);
      break;
    }
    case NEG:
      push(ac);
      call((void *)Sg_Negate);
      add(csp, 1 * sizeof(void*));
      break;
    case ADD:
      calcproc = (void *)Sg_Add;
      goto calc_entry;
    case SUB:
      calcproc = (void *)Sg_Sub;
      goto calc_entry;
    case MUL:
      calcproc = (void *)Sg_Mul;
      goto calc_entry;
    case DIV:
      calcproc = (void *)Sg_Div;
      // fall thought
    calc_entry: 
      vm_stack_ref(edx, vm, 0);
      vm_dec_sp(vm);		// vm->sp--;
      push(ac);
      push(edx);
      call(calcproc);
      add(csp, 2*sizeof(void*));
      break;
    case ADDI: immediate_calc(code[i], (void *)Sg_Add); break;
    case SUBI: immediate_calc(code[i], (void *)Sg_Sub); break;
    case MULI:
      calcproc = (void *)Sg_Mul;
      goto icalc_entry;
    case DIVI:
      calcproc = (void *)Sg_Div;
    icalc_entry:
      INSN_VAL1(val1, code[i]);
      push(ac);
      push((uintptr_t)SG_MAKE_INT(val1));
      call(calcproc);
      add(csp, 2*sizeof(void*));
      break;
    case GREF_TAIL_CALL:
      gproc = gref_insn(SG_OBJ(code[i+1]));
      goto tail_call_entry;
    case GREF_CALL: 
      gproc = gref_insn(SG_OBJ(code[i+1]));
      goto call_entry;

    call_entry:
    case CALL: {
      INSN_VAL1(val1, code[i]);
      if (!tail) inc_nest(1);
      // adjust arguments
      adjust_args(gproc, val1, false);
      scheme_call(gproc, tail);
      if (!tail) {
	inc_nest(-1);
      }
      break;
    }
    case TAIL_CALL:
    tail_call_entry:
      // if it's not GREF_TAIL_CALL, then we need to save ac.
      if (SG_FALSEP(gproc)) push(ac);
      tail_call_insn(code[i]);
      if (SG_FALSEP(gproc)) pop(ac);
      tail = true;
      goto call_entry;
      break;
    case LOCAL_TAIL_CALL:
      if (SG_FALSEP(gproc)) push(ac);
      tail_call_insn(code[i]);
      if (SG_FALSEP(gproc)) pop(ac);
      tail = true;
      // fall through
    case LOCAL_CALL:
      // for vm local_call is mere jmp call so call tail_call.
      local_call_insn(code[i], tail);
      break;
    case APPLY: {
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
      break;
    }
    case SHIFTJ: {
      INSN_VAL2(val1, val2, code[i]);
      // do we need this save?
      push(ac);
      {
	vm_sp(edx, vm);
	push(edx);
	push((uintptr_t)val1);
	vm_fp(ecx, vm);
	lea(ecx, ptr[ecx + val2*sizeof(void*)]);
	push(ecx);
	call((void*)::shift_args);
	add(csp, 3 * sizeof(void*));
	// vm->sp = ac
	mov(ptr[vm + offsetof(SgVM, sp)], ac);
      }
      pop(ac);
      break;
    }
    case JUMP:
      itr = context_.srcLabels.find(code+i);
      if (itr == context_.srcLabels.end()) {
	// something wrong
	throw std::runtime_error("JUMP does not have destination");
      }
      jmp(itr->second.c_str(), T_NEAR);
      break;
    case RET:
      epilogue();
      return i+1;
      // builtin procedures
    case PAIRP:
      pairp_insn();
      break;
    case LIST:
      INSN_VAL1(val1, code[i]);
      push(vm);
      push(ac);
      push((uintptr_t)val1);
      call((void*)list_fun);
      add(csp, 3 * sizeof(void*));
      break;
    case CAR:
      car_insn();
      break;
    case CDR:
      cdr_insn();
      break;
    case CAAR:
      car_insn();
      car_insn();
      break;
    case CADR:
      cdr_insn();
      car_insn();
      break;
    case CDAR:
      car_insn();
      cdr_insn();
      break;
    case CDDR:
      cdr_insn();
      cdr_insn();
      break;
    case CONS:
      cons_insn();
      break;
    car_push_entry:
    case CAR_PUSH:
      car_insn();
      push_insn();
      break;
    cdr_push_entry:
    case CDR_PUSH:
      cdr_insn();
      push_insn();
      break;
    case CONS_PUSH:
      cons_insn();
      push_insn();
      break;
    case LREF_CAR:
      lref_insn(code[i]);
      car_insn();
      break;
    case LREF_CDR:
      lref_insn(code[i]);
      cdr_insn();
      break;
    case LREF_CAR_PUSH:
      lref_insn(code[i]);
      goto car_push_entry;
      break;
    case LREF_CDR_PUSH:
      lref_insn(code[i]);
      goto cdr_push_entry;
      break;
    case FREF_CAR:
      fref_insn(code[i]);
      car_insn();
      break;
    case FREF_CDR:
      fref_insn(code[i]);
      cdr_insn();
      break;
    case FREF_CAR_PUSH:
      fref_insn(code[i]);
      goto car_push_entry;
    case FREF_CDR_PUSH:
      fref_insn(code[i]);
      goto cdr_push_entry;

    case GREF_CAR: gref_car_insn(SG_OBJ(code[i+1])); break;
    case GREF_CDR: gref_cdr_insn(SG_OBJ(code[i+1])); break;
    case GREF_CAR_PUSH:
      gref_car_insn(SG_OBJ(code[i+1]));
      push_insn();
      break;
    case GREF_CDR_PUSH:
      gref_cdr_insn(SG_OBJ(code[i+1]));
      push_insn();
      break;

    default:
      throw std::runtime_error(info->name);
    }
    trace_leave(info);
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

#ifdef JIT_DUMP
#include "sagittarius/writer.h"
#include "sagittarius/port.h"
#endif
static void jit_finalizer(SgObject z, void *data)
{
  //puts("gong!");
  jit_allocator.collect((void *)SG_SUBR_FUNC(z));
}

static int jit_compile_closure_rec(SgObject closure, SgObject set)
{
  // check if the target closure is already in compiling
  if (!SG_FALSEP(Sg_Memq(closure, set))) return TRUE;
  try {
    DefaultWalker walker;
    JitCompiler compiler(SG_CLOSURE(closure), walker, set);
    // set dummy subr so that we can decieve cross calling.
    SgObject subr
      = Sg_MakeSubr(reinterpret_cast<SgSubrProc *>(compiler.getCode()),
		    NULL,
		    SG_PROCEDURE_REQUIRED(closure),
		    SG_PROCEDURE_OPTIONAL(closure),
		    SG_PROCEDURE_NAME(closure));
    Sg_RegisterFinalizer(subr, jit_finalizer, NULL);
    SG_CLOSURE(closure)->native = subr;
    if (compiler.compile()) {
#ifdef JIT_DUMP
      Sg_Printf(SG_PORT(Sg_StandardErrorPort()), UC("%S:[%p (%d -> %d)\n"),
		closure,
		compiler.getCode(),
		SG_CODE_BUILDER(SG_CLOSURE(closure)->code)->size,
		compiler.getSize());
      compiler.dump_code();
#endif
      SG_CLOSURE(closure)->state = SG_NATIVE;
      return TRUE;
    }
    SG_CLOSURE(closure)->native = NULL;
    SG_CLOSURE(closure)->state = SG_INVALID_FOR_NATIVE;
    return FALSE;
  } catch (Xbyak::Error e) {
    const char *m = Xbyak::ConvertErrorToString(e);
    if (m) {
      fprintf(stderr, "%s\n", m);
    }
    return FALSE;
  }
}


int Sg_JitCompileClosure(SgObject closure)
{
  return jit_compile_closure_rec(closure, SG_NIL);
}

