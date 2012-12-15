/* regex.h                                                -*- coding: utf-8; -*-
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
#ifndef SAGITTARIUS_REGEX_H_
#define SAGITTARIUS_REGEX_H_

#include <sagittarius.h>

enum PatternFlags {
  /* on Sagittarius Scheme, internal line seperator is lf. */
  /* SG_UNIX_LINE         = (1L << 0), */
  SG_CASE_INSENSITIVE  = (1L << 1),
  SG_DOTALL            = (1L << 2),
  SG_UNICODE_CASE      = (1L << 3),
  SG_COMMENTS          = (1L << 4),
  SG_MULTILINE         = (1L << 5),
  SG_LITERAL           = (1L << 6),
  /* we do not support canonical equivalence */
  /* SG_CANON_EQ          = (1L << 7), */
};

enum Anchor {
  UNANCHORED,         /* No anchoring */
  ANCHOR_START,       /* Anchor at start only */
  ANCHOR_BOTH,        /* Anchor at start and end */
};

typedef struct inst_rec_t inst_t;

typedef union {		 /* arguments for opcode */
  SgChar c;		 /* RX_CHAR: target character */
  unsigned int n;	 /* RX_SAVE: submatch start or end position */
  struct {		 /* RX_SPLIT */
    inst_t *x;		 /*   primary position to jump */
    inst_t *y;		 /*   secondary position to jump */
  } pos;
  SgObject set;		 /* RX_SET: charset */
  unsigned int flags;	 /* RX_FLAGS */
  unsigned int index;	 /* RX_BREF: reference index */
  struct {		 /* RX_BRANCH or RX_BRANCHA */
    inst_t *x;			/* yes-pattern */
    inst_t *y;			/* no-pattern */
    int     n;			/* submatch */
  } cond;
} inst_arg_t;

struct inst_rec_t
{
  unsigned char  opcode;	/* opcode: max 255 */
  unsigned char  flags;
  inst_arg_t     arg;
};

#define INST_OPCODE(i)        ((i)->opcode)
#define INST_FLAG(i)          ((i)->flags)
#define INST_FLAG_SET(i, v)   ((i)->flags|=(v))

typedef struct
{
  inst_t *root;			/* root match code */
  int     rootLength;
} prog_t;

typedef struct SgPatternRec
{
  SG_HEADER;
  SgObject pattern;		/* regex pattern: string or ast */
  SgObject ast;			/* parsed ast */
  int      flags;		/* flags, details are above.
				   this flags are initial condition, if regex
				   has (?imx:...) in its body, matcher
				   overwrites the flags in runtime.*/
  int      groupCount;		/* captured group count */
  int      extendedp;		/* if the compiled code is used possesive match,
				   backreference, lookahead or lookbehind or
				   not*/
  prog_t  *prog;		/* compiled regex */
} SgPattern;

SG_CLASS_DECL(Sg_PatternClass);
#define SG_CLASS_PATTERN   (&Sg_PatternClass)
#define SG_PATTERN(obj)   ((SgPattern *)obj)
#define SG_PATTERNP(obj) SG_XTYPEP(obj, SG_CLASS_PATTERN)

typedef struct match_ctx_rec_t match_ctx_t;

typedef struct SgMatcherRec
{
  SG_HEADER;
  SgPattern *pattern;
  SgString  *text;
  /* privates */
  int        from;
  int        to;
  int        first;
  int        last;
  int        lastAppendPosition;
  match_ctx_t *match_ctx;
  SgChar    *submatch[1];
} SgMatcher;

SG_CLASS_DECL(Sg_MatcherClass);
#define SG_CLASS_MATCHER   (&Sg_MatcherClass)
#define SG_MATCHER(obj)   ((SgMatcher *)obj)
#define SG_MATCHERP(obj) SG_XTYPEP(obj, SG_CLASS_MATCHER)

SG_CDECL_BEGIN
SG_EXTERN SgObject   Sg_CompileRegex(SgString *pattern, int flags,
				     int parseOnly);

SG_EXTERN SgMatcher* Sg_RegexMatcher(SgPattern *pattern, SgString *text);
SG_EXTERN int        Sg_RegexMatches(SgMatcher *m);
SG_EXTERN int        Sg_RegexLookingAt(SgMatcher *m);
SG_EXTERN int        Sg_RegexFind(SgMatcher *m, int start);

SG_EXTERN SgObject   Sg_RegexGroup(SgMatcher *m, int group);

SG_EXTERN SgString*  Sg_RegexReplaceAll(SgMatcher *m, SgObject replacement);
SG_EXTERN SgString*  Sg_RegexReplaceFirst(SgMatcher *m, SgObject replacement);

SG_EXTERN int        Sg_RegexCaptureCount(SgMatcher *m);
/* for debug */
SG_EXTERN void       Sg_DumpRegex(SgPattern *pattern, SgObject port);

SG_CDECL_END

#endif /* SAGITTARIUS_REGEX_H_ */
