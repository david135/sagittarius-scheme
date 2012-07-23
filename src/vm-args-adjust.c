/* vm-args-adjust.c                                       -*- coding: utf-8; -*-
 *
 *   Copyright (c) 2010-2011  Takashi Kato <ktakashi@ymail.com>
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

/* we need this for JIT */
#undef ADJUST_ARGUMENT_FRAME
#if !defined(APPLY_CALL)
#define ADJUST_ARGUMENT_FRAME(proc, argc)				\
  do {									\
    int required = SG_PROCEDURE_REQUIRED(proc);				\
    int optargs =  SG_PROCEDURE_OPTIONAL(proc);				\
    if (optargs) {							\
      SgObject p = SG_NIL, a;						\
      if (argc < required) {						\
	Sg_WrongNumberOfArgumentsViolation(SG_PROCEDURE_NAME(AC(vm)),	\
					   required, argc, SG_UNDEF);	\
      }									\
      /* fold rest args */						\
      while (argc > required+optargs-1) {				\
	a = POP(SP(vm));						\
	p = Sg_Cons(a, p);						\
	argc--;								\
      }									\
      PUSH(SP(vm), p);							\
      argc++;								\
    } else {								\
      if (argc != required) {						\
	Sg_WrongNumberOfArgumentsViolation(SG_PROCEDURE_NAME(AC(vm)),	\
					   required, argc, SG_UNDEF);	\
      }									\
    }									\
    FP(vm) = SP(vm) - argc;						\
  } while (0)
#else  /* APPLY_CALL */
#define ADJUST_ARGUMENT_FRAME(proc, argc)				\
  do {									\
    int required = SG_PROCEDURE_REQUIRED(proc);				\
    int optargs =  SG_PROCEDURE_OPTIONAL(proc);				\
    int rargc = Sg_Length(INDEX(SP(vm), 0));				\
    SgObject p, a;							\
    if (optargs) {							\
      int __i, req_opt, oargc;						\
      if ((rargc+argc-1) < required) {					\
      	Sg_WrongNumberOfArgumentsViolation(SG_PROCEDURE_NAME(AC(vm)),	\
					   rargc+argc-1, argc, SG_UNDEF); \
      }									\
      req_opt = required+optargs;					\
      p = POP(SP(vm)); /* tail of arglist */				\
      oargc = argc--;							\
      if (oargc > req_opt) {						\
	/* fold rest args */						\
	p = Sg_CopyList(p);						\
	for (__i = oargc; __i > req_opt; __i--) {			\
	  a = POP(SP(vm));						\
	  argc--;							\
	  p = Sg_Cons(a, p);						\
	}								\
	PUSH(SP(vm), p);						\
	argc++;								\
	/* argc -= oargc - __i -1; */					\
      } else {								\
	/* unfold rest arg */						\
	CHECK_STACK(req_opt - oargc, vm);				\
	for (__i = oargc; SG_PAIRP(p) && __i < req_opt; __i++) {	\
	  PUSH(SP(vm), SG_CAR(p));					\
	  argc++;							\
	  p = SG_CDR(p);						\
	}								\
	p = Sg_CopyList(p);						\
	PUSH(SP(vm), p);						\
	argc++;								\
	/* argc += __i - oargc +1; */					\
      }									\
    } else {								\
      /* not optargs */							\
      if ((rargc+argc-1) != required) {					\
	Sg_WrongNumberOfArgumentsViolation(SG_PROCEDURE_NAME(AC(vm)),	\
					   required, rargc, SG_UNDEF);	\
      }									\
      p = POP(SP(vm));							\
      argc--;								\
      if (rargc > 0) {							\
	CHECK_STACK(rargc, vm);						\
	/* argc +=rargc; */						\
	do {								\
	  PUSH(SP(vm), SG_CAR(p));					\
	  argc++;							\
	  p = SG_CDR(p);						\
	} while (--rargc > 0);						\
      }									\
    }									\
    FP(vm) = SP(vm) - argc;						\
  } while (0)
#endif	/* APPLY_CALL */

