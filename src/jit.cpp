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
    case CALL:
    case TAIL_CALL:
    case GREF_CALL:
    case GREF_TAIL_CALL:
      ctx.noExternalCall = false;
      break;
    default: break;
    }
    i += 1 + info->argc;
  }
}

class JitCompiler : public Xbyak::CodeGenerator
{
private:
  SgClosure * const closure_;
  Walker * const walker_;
  WalkContext context_;
  int label_;
public:
  JitCompiler(SgClosure * const closure, Walker * const walker)
    : closure_(closure)
    , walker_(walker)
    , label_(0)
  {}
  
  // compile the closure
  bool compile();

private:
  // walk through the closure to detect possible
  // optimisation options.
  void walk()
  {
    walker_->walk(closure_, context_);
  }
  void prologue()
  {
    // TODO 64 bits
    push(ebp);
    mov(ebp, esp);
    vm(ebx);
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
    stack_access(d, 8);
  }
  void vm(const Xbyak::Operand &d)
  {
    stack_access(d, 16);
  }
  // put the value to vm->ac or eax if noExternalCall is true
  // destination must indicate vm if noExternalCall is false.
  void ac(const Xbyak::Reg32e &d, const Xbyak::Operand &v)
  {
    if (context_.noExternalCall) {
      mov(eax, v);
    } else {
      mov(ptr[d + offsetof(SgVM, ac)], v);
    }
  }
  void ac(const Xbyak::Reg32e &d, const SgObject o)
  {
    if (context_.noExternalCall) {
      mov(eax, (intptr_t)o);
    } else {
      mov(byte[d + offsetof(SgVM, ac)], (intptr_t)o);
    }
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
  void vm_cont(const Xbyak::Reg32e &d, const Xbyak::Reg32e &vm)
  {
    mov(d, ptr[vm + offsetof(SgVM, cont)]);
  }
  void ref_ac(const Xbyak::Reg32e &vm)
  {
    ref_ac(eax, vm);
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
};

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
    int val1, val2;
     // each instruction must put its result to ac(vm->ac or eax)
    // if it has some result. cf) some instruction does not have
    // result such as PUSH.
    switch (insn) {
    case FRAME:
      vm_sp(eax, ebx);
      mov(eax, byte[eax]);	// *newcont = (SgContFrame*)SP(vm);
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
      // newcont->cl = vm->cl;
      vm_cl(ecx, ebx);
      mov(ptr[eax + offsetof(SgContFrame, cl)], ecx);
      // vm->cont = newcont
      mov(ptr[ebx + offsetof(SgVM, cont)], eax);
      // eax = vm->sp
      vm_sp(eax, ebx);
      lea(edx, ptr[eax + sizeof(SgContFrame)]);
      mov(ptr[ebx + offsetof(SgVM, sp)], edx);
      break;
    case LREF_PUSH:
      INSN_VAL1(val1, code[i]);
      args(ecx);  // args
      vm_sp(edx, ebx); // edx = vm->sp
      mov(ecx, ptr[ecx + val1 * sizeof(void*)]);
      mov(byte[edx], ecx);
      // fall though
    case PUSH:
      // push argument to vm stack
      add(edx, 1 * sizeof(void*));
      mov(ptr[ebx + offsetof(SgVM, sp)], edx);
      break;
    case LREF:
      INSN_VAL1(val1, code[i]);
      args(eax); // args
      mov(edx, ptr[eax + val1 * sizeof(void*)]); // args[n] -> edx
      ac(ebx, edx);
      break;
    case CONSTI:
      INSN_VAL1(val1, code[i]);
      ac(ebx, SG_MAKE_INT(val1));
      break;
    case BNLE: {
      SgObject n = SG_OBJ(code[i+1]);
      ac(ebx, eax); // eax = vm->ac
      push(eax);
      vm_stack_ref(eax, ebx, 0);
      push(eax);
      call((void*)Sg_NumLe);
      add(esp, 2 * sizeof(void*));
      cmp(eax, 1);
      std::string l = gen_label();
      jne(l.c_str());
      // we don't set #f or #t to eax, it's useless
      // TODO if jump instruction
      int j= compile_rec(code + i + 2, SG_INT_VALUE(n));
      L(l.c_str());
      j += compile_rec(code + i + 1 + SG_INT_VALUE(n), size - i);
      i += j;
      break;
    }
    case ADD: {
      vm_stack_ref(eax, ebx, 0);
      lea(edx, ptr[eax - 1*sizeof(void*)]);
      mov(edx, ptr[ebx + offsetof(SgVM, sp)]);
      ref_ac(edx, ebx);
      push(eax);
      push(edx);
      call((void*)Sg_Add);
      ac(eax, ebx);
      add(esp, 2*sizeof(void*));
      break;
    }
    case ADDI: {
      INSN_VAL1(val1, code[i]);
      // TODO 64 bits
      ref_ac(ebx);		// load vm->ac to eax
      mov(edx, eax);
      // SG_INTP(vm->ac);
      and(edx, 3);
      cmp(edx, 1);
      std::string label = gen_label();
      std::string end_label = gen_label();
      je(label.c_str());
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
      break;
    }
    case GREF_CALL: {
      SgObject f = SG_OBJ(code[i+1]), o;
      INSN_VAL1(val1, code[i]);
      if (SG_IDENTIFIERP(f)) {
	o = Sg_FindBinding(SG_IDENTIFIER_LIBRARY(f),
			   SG_IDENTIFIER_NAME(f),
			   SG_UNBOUND);
      } else {
	// should be only here
	o = SG_GLOC_GET(SG_GLOC(f));
      }
      // TODO more than one
      switch (val1) {
      case 0:
	call((void*)Sg_Apply0);
	break;
      case 1:
	// extract arguments from vm stack pointer
	vm_stack_ref(eax, ebx, 0);
	push(eax);
	push((uintptr_t)o);
	call((void*)Sg_Apply1);
	add(esp, 2 * sizeof(void*));
	break;
      default:
	throw "not supported yet";
      }
      break;
    }
    case RET:
      epilogue();
      return i;
    default:
      // not supported yet
      throw "not supported yet";
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
  } catch (...) {
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
