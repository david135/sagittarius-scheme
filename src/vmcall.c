/* -*- C -*- */
/*
 * vmcall.c
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
/* This file is included at vm.c */

{
  int argc;
  INSN_VAL1(argc, c);

  if (SG_VM_LOG_LEVEL(vm, SG_DEBUG_LEVEL) && vm->state == RUNNING) {
    Sg_Printf(vm->logPort, UC("calling %S\n"), AC(vm));
    if (SG_VM_LOG_LEVEL(vm, SG_TRACE_LEVEL) && vm->state == RUNNING) {
      print_frames(vm);
    }
  }

  if (SG_SUBRP(AC(vm))) {
    CL(vm) = AC(vm);
    PC(vm) = SG_SUBR_RETURN_CODE(AC(vm));
    SG_SUBR_RETURN_CODE(AC(vm))[0] = SG_WORD(RET);
    CL(vm) = AC(vm);
    SP(vm) = ARGP(vm);
    /* FP(vm) = SP(vm) - argc; */
    SG_PROF_COUNT_CALL(vm, AC(vm));
    AC(vm) = SG_SUBR_FUNC(AC(vm))(ARGP(vm)/* SP(vm) - argc */, argc, SG_SUBR_DATA(AC(vm)));
  } else if (SG_CLOSUREP(AC(vm))) {
    SgClosure *c = SG_CLOSURE(AC(vm));
    SgCodeBuilder *cb = SG_CODE_BUILDER(c->code);
    int required = cb->argc;
    CHECK_STACK(cb->maxStack, vm);
    DC(vm) = AC(vm);
    CL(vm) = AC(vm);
    PC(vm) = cb->code;
    if (cb->optional) {
      int extra = argc - required;
      SgObject *sp;
      if (-1 == extra) {
	sp = unshift_args(SP(vm), 1);
	INDEX_SET(sp, 0, SG_NIL);
	ARGP(vm) = SP(vm) = sp;
	FP(vm) = sp - required;
      } else if (extra >= 0) {
	INDEX_SET(SP(vm), extra, stack_to_pair_args(SP(vm), extra + 1));
	sp = SP(vm) - extra;
	ARGP(vm) = SP(vm) = sp;
	FP(vm) = sp - required;
      } else {
	Sg_WrongNumberOfArgumentsViolation(SG_PROCEDURE_NAME(AC(vm)),
					   required - 1, argc, SG_UNDEF);
      }
    } else if (required == argc) {
      FP(vm) = SP(vm) - argc;
      ARGP(vm) = SP(vm);
    } else {
      SgObject args = SG_NIL;
      int i;
      for (i = 0; i < argc; i++) {
	args = Sg_Cons(INDEX(SP(vm), i), args);
      }
      Sg_WrongNumberOfArgumentsViolation(SG_PROCEDURE_NAME(AC(vm)),
					 required, argc, args);
    }
    SG_PROF_COUNT_CALL(vm, CL(vm));
  } else {
    Sg_AssertionViolation(SG_INTERN("apply"),
			  Sg_MakeString(UC("invalid application"), SG_LITERAL_STRING),
			  AC(vm));
  }
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
