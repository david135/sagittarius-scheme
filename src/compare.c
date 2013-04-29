/*  -*- C -*- */
/*
 * compare.c
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
#define LIBSAGITTARIUS_BODY
#include "sagittarius/compare.h"
#include "sagittarius/codec.h"
#include "sagittarius/clos.h"
#include "sagittarius/error.h"
#include "sagittarius/identifier.h"
#include "sagittarius/number.h"
#include "sagittarius/pair.h"
#include "sagittarius/bytevector.h"
#include "sagittarius/record.h"
#include "sagittarius/string.h"
#include "sagittarius/vector.h"
#include "sagittarius/hashtable.h"
#include "sagittarius/vm.h"	/* for box */

int Sg_EqP(SgObject x, SgObject y)
{
  return SG_EQ(x, y);
}

static int compare_double(double dx, double dy)
{
  if (dx == 0.0 && dy == 0.0) {
    /* get sign */
    union { double f64; uint64_t u64; } d1, d2;
    int signx, signy;
    d1.f64 = dx;
    d2.f64 = dy;
    signx = d1.u64 >> 63;
    signy = d2.u64 >> 63;
    return signx == signy;
  } else {
    return dx == dy;
  }  
}

static int eqv_internal(SgObject x, SgObject y, int from_equal_p)
{
  SgClass *cx, *cy;
  if (SG_EQ(x, y)) return TRUE;
  if (SG_NUMBERP(x)) {
    if (SG_NUMBERP(y)) {
      if (SG_FLONUMP(x)) {
	if (SG_FLONUMP(y)) {
	  /* R6RS 11.5 6th item */
	  double dx = SG_FLONUM_VALUE(x);
	  double dy = SG_FLONUM_VALUE(y);
	  return compare_double(dx, dy);
	} else {
	  return FALSE;
	}
      } else if (SG_FLONUMP(y)) {
	return FALSE;
      }
      if (Sg_ExactP(x) && Sg_ExactP(y)) {
	return Sg_NumEq(x, y);
      } else if (Sg_InexactP(x) && Sg_InexactP(y)) {
	/* must be both complex numbers but just in case */
	if (SG_COMPLEXP(x) && SG_COMPLEXP(y)) {
	  /* both imag and ream must be flonum but just in case */
	  double xr = Sg_GetDouble(SG_COMPLEX(x)->real);
	  double xi = Sg_GetDouble(SG_COMPLEX(x)->imag);
	  double yr = Sg_GetDouble(SG_COMPLEX(y)->real);
	  double yi = Sg_GetDouble(SG_COMPLEX(y)->imag);
	  return compare_double(xr, yr) && compare_double(xi, yi);
	} else {
	  return FALSE;
	}
      } else {
	/* exact and inexact */
	return FALSE;
      }
    }
    return FALSE;
  }
  if (SG_CODECP(x)) {
    if (SG_CODECP(y)) {
      /* if these 2 are the same codec, it must use the same putChar and getChar
	 method.
       */
      if ((SG_CODEC(x)->type == SG_BUILTIN_CODEC &&
	   (SG_CODEC_BUILTIN(x)->getc == SG_CODEC_BUILTIN(y)->getc) &&
	   (SG_CODEC_BUILTIN(x)->putc == SG_CODEC_BUILTIN(y)->putc)) ||
	  (SG_CODEC(x)->type == SG_CUSTOM_CODEC &&
	    /* we just compare the name */
	   SG_EQ(SG_CODEC(x)->name, SG_CODEC(y)->name))) {
	return TRUE;
      } else {
	return FALSE;
      }
    } else {
      return FALSE;
    }
  }
  if (!SG_HPTRP(x)) return SG_EQ(x, y);
  /* simple R7RS */
  if (SG_TUPLEP(x)) {
    if (SG_TUPLEP(y)) {
      int xs = Sg_TupleSize(x);
      int ys = Sg_TupleSize(y);
      if (xs == ys) {
	int i;
	for (i = 0; i < xs; i++) {
	  SgObject xe = Sg_TupleRef(x, i, SG_UNBOUND);
	  SgObject ye = Sg_TupleRef(y, i, SG_UNBOUND);
	  if (SG_UNBOUNDP(xe) || SG_UNBOUNDP(ye)) return FALSE;
	  else if (eqv_internal(xe, ye, from_equal_p)) continue;
	  else return FALSE;
	}
	return TRUE;
      } else {
	return FALSE;
      }
    } else {
      return FALSE;
    }
  }

  if (from_equal_p) {
    cx = Sg_ClassOf(x);
    cy = Sg_ClassOf(y);
    if (cx == cy && cx->compare) {
      return (cx->compare(x, y, TRUE) == 0);
    }
  }
  return FALSE;
}

int Sg_EqvP(SgObject x, SgObject y)
{
  return eqv_internal(x, y, FALSE);
}

/* R6RS requires to equal? to stop when the given object were shared object */
#if 0
int Sg_EqualP(SgObject x, SgObject y)
{
  if (SG_EQ(x, y)) return TRUE;
  if (SG_PAIRP(x)) {
    if (!SG_PAIRP(y)) return FALSE;
    do {
      if (!Sg_EqualP(SG_CAR(x), SG_CAR(y))) return FALSE;
      x = SG_CDR(x);
      y = SG_CDR(y);
    } while(SG_PAIRP(x) && SG_PAIRP(y));
    return Sg_EqualP(x, y);
  }
  if (SG_STRINGP(x)) {
    if (!SG_STRINGP(y)) return FALSE;
    return Sg_StringEqual(SG_STRING(x), SG_STRING(y));
  }
  if (SG_NUMBERP(x)) {
    if (SG_NUMBERP(y)) {
      if ((Sg_ExactP(x) && Sg_ExactP(y))
	  && (Sg_InexactP(x) && Sg_InexactP(y))) {
	return Sg_NumEq(x, y);
      }
    }
    return FALSE;
  }
  if (SG_VECTORP(x)) {
    if (SG_VECTORP(y)) {
      int sizex = SG_VECTOR_SIZE(x);
      int sizey = SG_VECTOR_SIZE(y);
      if (sizex == sizey) {
	while (sizex--) {
	  if (!Sg_EqualP(SG_VECTOR_ELEMENT(x, sizex),
			 SG_VECTOR_ELEMENT(y, sizex)))
	    break;
	}
	if (sizex < 0) return TRUE;
      }
    }
  }
  /* TODO: gauche just compare the name. should this be like that? */
  if (SG_IDENTIFIERP(x)) {
    if (SG_IDENTIFIERP(y)) {
      return SG_EQ(SG_IDENTIFIER(x)->name, SG_IDENTIFIER(y)->name)
	&& SG_EQ(SG_IDENTIFIER(x)->library, SG_IDENTIFIER(y)->library);
    }
    return FALSE;
  }
  
  return FALSE;
}

#endif

/* should i make api for box? */
static inline SgObject make_box(SgObject value)
{
  SgBox *b = SG_NEW(SgBox);
  SG_SET_CLASS(b, SG_CLASS_BOX);
  b->value = value;
  return SG_OBJ(b);
}

struct equal_context
{
  SgObject k0;
  SgObject kb;
};

static SgObject pre_p(SgObject x, SgObject y, SgObject k)
{
  if (x == y) return k;
  if (SG_PAIRP(x)) {
    if (!SG_PAIRP(y)) {
      return SG_FALSE;
    }
    ASSERT(SG_INTP(k));
    if (SG_INT_VALUE(k) <= 0) {
      return k;
    } else {
      SgObject k2 = pre_p(SG_CAR(x), SG_CAR(y),
			  SG_MAKE_INT(SG_INT_VALUE(k) - 1));
      if (SG_FALSEP(k2)) {
	return SG_FALSE;
      }
      return pre_p(SG_CDR(x), SG_CDR(y), k2);
    }
  }
  if (SG_VECTORP(x)) {
    if (!SG_VECTORP(y)) {
      return SG_FALSE;
    } else {
      int sizex = SG_VECTOR_SIZE(x);
      int sizey = SG_VECTOR_SIZE(y);
      if (sizex != sizey) {
	return SG_FALSE;
      } else {
	int i;
	ASSERT(SG_INTP(k));
	for (i = 0;; i++) {
	  if (i == sizex || SG_INT_VALUE(k) <= 0) {
	    return k;
	  } else {
	    SgObject k2 = pre_p(SG_VECTOR_ELEMENT(x, i),
				SG_VECTOR_ELEMENT(y, i),
				SG_MAKE_INT(SG_INT_VALUE(k) - 1));
	    if (SG_FALSEP(k2)) {
	      return SG_FALSE;
	    }
	    k = k2;
	  }
	}
      }
    }
  }
  if (SG_STRINGP(x)) {
    if (!SG_STRINGP(y)) {
      return SG_FALSE;
    }
    if (Sg_StringEqual(x, y)) {
      return k;
    } else {
      return SG_FALSE;
    }
  }

  if (SG_BVECTORP(x)) {
    if (!SG_BVECTORP(y)) {
      return SG_FALSE;
    }
    if (Sg_ByteVectorEqP(x, y)) {
      return k;
    } else {
      return SG_FALSE;
    }
  }

  /* R7RS */
  if (SG_TUPLEP(x)) {
    if (!SG_TUPLEP(y)) {
      return SG_FALSE;
    } else {
      int sizex = Sg_TupleSize(x);
      int sizey = Sg_TupleSize(y);
      if (sizex != sizey) {
	return SG_FALSE;
      } else {
	int i;
	for (i = 0;; i++) {
	  if (i == sizex || SG_INT_VALUE(k) <= 0) {
	    return k;
	  } else {
	    SgObject xe = Sg_TupleRef(x, i, SG_UNBOUND);
	    SgObject ye = Sg_TupleRef(y, i, SG_UNBOUND);
	    SgObject k2;
	    if (SG_UNBOUNDP(xe) || SG_UNBOUNDP(ye)) return SG_FALSE;
	    k2 = pre_p(xe, ye, SG_MAKE_INT(SG_INT_VALUE(k) - 1));
	    if (SG_FALSEP(k2)) {
	      return SG_FALSE;
	    }
	    k = k2;
	  }
	}
      }
    }
  }

  if (eqv_internal(x, y, TRUE)) {
    return k;
  } else {
    return SG_FALSE;
  }
}

static SgObject eP(SgHashTable **pht, SgObject x, SgObject y,
		   SgObject k, struct equal_context *ctx);		   

static SgObject fast_p(SgHashTable **pht, SgObject x, SgObject y,
		       SgObject k, struct equal_context *ctx)
{
  if (x == y) return k;
  if (SG_PAIRP(x)) {
    if (!SG_PAIRP(y)) {
      return SG_FALSE;
    }
    k = eP(pht, SG_CAR(x), SG_CAR(y), k, ctx);
    if (SG_FALSEP(k)) {
      return SG_FALSE;
    }
    return k = eP(pht, SG_CDR(x), SG_CDR(y), k, ctx);
  }
  if (SG_VECTORP(x)) {
    if (!SG_VECTORP(y)) {
      return SG_FALSE;
    } else {
      int sizex = SG_VECTOR_SIZE(x);
      int sizey = SG_VECTOR_SIZE(y);
      if (sizex != sizey) {
	return SG_FALSE;
      } else {
	int i;
	for (i = 0;; i++) {
	  if (i == sizex || SG_INT_VALUE(k) <= 0) {
	    return k;
	  } else {
	    k = eP(pht,
		   SG_VECTOR_ELEMENT(x, i),
		   SG_VECTOR_ELEMENT(y, i),
		   k, ctx);
	    if (SG_FALSEP(k)) {
	      return SG_FALSE;
	    }
	  }
	}
      }
    }
  }
  if (SG_STRINGP(x)) {
    if (!SG_STRING(y)) {
      return SG_FALSE;
    }
    if(Sg_StringEqual(x, y)) {
      return k;
    } else {
      return SG_FALSE;
    }
  }

  if (SG_BVECTORP(x)) {
    if (!SG_BVECTORP(y)) {
      return SG_FALSE;
    }
    if (Sg_ByteVectorEqP(x, y)) {
      return k;
    } else {
      return SG_FALSE;
    }
  }
  /* R7RS */
  if (SG_TUPLEP(x)) {
    if (!SG_TUPLEP(y)) {
      return SG_FALSE;
    } else {
      int sizex = Sg_TupleSize(x);
      int sizey = Sg_TupleSize(y);
      if (sizex != sizey) {
	return SG_FALSE;
      } else {
	int i;
	for (i = 0;; i++) {
	  if (i == sizex || SG_INT_VALUE(k) <= 0) {
	    return k;
	  } else {
	    SgObject xe = Sg_TupleRef(x, i, SG_UNBOUND);
	    SgObject ye = Sg_TupleRef(y, i, SG_UNBOUND);
	    if (SG_UNBOUNDP(xe) || SG_UNBOUNDP(ye)) return SG_FALSE;
	    k = eP(pht, xe, ye, k, ctx);
	    if (SG_FALSEP(k)) {
	      return SG_FALSE;
	    }
	  }
	}
      }
    }
  }

  if (eqv_internal(x, y, TRUE)) {
    return k;
  } else {
    return SG_FALSE;
  }
}

static SgObject find(SgObject b)
{
  SgObject n;
  ASSERT(SG_BOXP(b));
  n = SG_BOX(b)->value;
  if (SG_BOXP(n)) {
    for (;;) {
      SgObject nn = SG_BOX(n)->value;
      if (SG_BOXP(nn)) {
	SG_BOX(b)->value = nn;
	b = n;
	n = nn;
      } else {
	return n;
      }
    }
  } else {
    return b;
  }
}

static SgObject union_find(SgHashTable *ht, SgObject x, SgObject y,
			   struct equal_context *ctx)
{
  SgObject bx = Sg_HashTableRef(ht, x, SG_FALSE);
  SgObject by = Sg_HashTableRef(ht, y, SG_FALSE);
  if (SG_FALSEP(bx)) {
    if (SG_FALSEP(by)) {
      SgObject b = make_box(SG_MAKE_INT(1));
      Sg_HashTableSet(ht, x, b, 0);
      Sg_HashTableSet(ht, y, b, 0);
      return SG_FALSE;
    } else {
      SgObject ry = find(by);
      Sg_HashTableSet(ht, x, ry, 0);
      return SG_FALSE;
    }
  } else if (SG_FALSEP(by)) {
    SgObject rx = find(bx);
    Sg_HashTableSet(ht, y, rx, 0);
    return SG_FALSE;
  } else {
    SgObject rx = find(bx);
    SgObject ry = find(by);
    SgObject nx, ny;
    if (rx == ry) {
      return SG_TRUE;
    }
    nx = SG_BOX(rx)->value;
    ny = SG_BOX(ry)->value;
    ASSERT(SG_INTP(nx));
    ASSERT(SG_INTP(ny));
    if (SG_INT_VALUE(nx) > SG_INT_VALUE(ny)) {
      SG_BOX(ry)->value = rx;
      SG_BOX(rx)->value = SG_MAKE_INT(SG_INT_VALUE(nx) + SG_INT_VALUE(ny));
      return SG_FALSE;
    } else {
      SG_BOX(rx)->value = ry;
      SG_BOX(ry)->value = SG_MAKE_INT(SG_INT_VALUE(ny) + SG_INT_VALUE(nx));
      return SG_FALSE;
    }
  }
}

static SgObject call_union_find(SgHashTable **pht, SgObject x,
				SgObject y, struct equal_context *ctx)
{
  if (*pht == NULL) {
    *pht = Sg_MakeHashTableSimple(SG_HASH_EQ, 0);
  }
  return union_find(*pht, x, y, ctx);
}

static SgObject slow_p(SgHashTable **pht, SgObject x, SgObject y,
		       SgObject k, struct equal_context *ctx)
{
  if (x == y) return k;
  if (SG_PAIRP(x)) {
    if (!SG_PAIRP(y)) {
      return SG_FALSE;
    }
    if (!SG_FALSEP(call_union_find(pht, x, y, ctx))) {
      return SG_MAKE_INT(0);
    } else {
      ASSERT(SG_INTP(k));
      k = eP(pht, SG_CAR(x), SG_CAR(y), k, ctx);
      if (SG_FALSEP(k)) {
	return SG_FALSE;
      }
      return k = eP(pht, SG_CDR(x), SG_CDR(y), k, ctx);
    }
  }
  if (SG_VECTORP(x)) {
    int n = SG_VECTOR_SIZE(x);
    int i;
    if (!SG_VECTORP(y)) {
      return SG_FALSE;
    }
    if (n != SG_VECTOR_SIZE(y)) {
      return SG_FALSE;
    }
    if (!SG_FALSEP(call_union_find(pht, x, y, ctx))) {
      return SG_MAKE_INT(0);
    }
    ASSERT(SG_INTP(k));
    k = SG_MAKE_INT(SG_INT_VALUE(k) - 1);
    for (i = 0;; i++) {
      if (i == n) {
	return k;
      } else {
	k = eP(pht,
	       SG_VECTOR_ELEMENT(x, i),
	       SG_VECTOR_ELEMENT(y, i),
	       k, ctx);
	if (SG_FALSEP(k)) {
	  return SG_FALSE;
	}
      }
    }
  }
  if (SG_STRINGP(x)) {
    if (!SG_STRING(y)) {
      return SG_FALSE;
    }
    if(Sg_StringEqual(x, y)) {
      return k;
    } else {
      return SG_FALSE;
    }
  }
  if (SG_BVECTORP(x)) {
    if (!SG_BVECTORP(y)) {
      return SG_FALSE;
    }
    if (Sg_ByteVectorEqP(x, y)) {
      return k;
    } else {
      return SG_FALSE;
    }
  }
  /* R7RS */
  if (SG_TUPLEP(x)) {
    int n, i;
    if (!SG_TUPLEP(y)) {
      return SG_FALSE;
    }
    n = Sg_TupleSize(x);
    if (n != Sg_TupleSize(y)) {
      return SG_FALSE;
    }
    if (!SG_FALSEP(call_union_find(pht, x, y, ctx))) {
      return SG_MAKE_INT(0);
    }
    ASSERT(SG_INTP(k));
    k = SG_MAKE_INT(SG_INT_VALUE(k) - 1);
    for (i = 0;; i++) {
      if (i == n) {
	return k;
      } else {
	SgObject xe = Sg_TupleRef(x, i, SG_UNBOUND);
	SgObject ye = Sg_TupleRef(y, i, SG_UNBOUND);
	if (SG_UNBOUNDP(xe) || SG_UNBOUNDP(ye)) return SG_FALSE;
	k = eP(pht, xe, ye, k, ctx);
	if (SG_FALSEP(k)) {
	  return SG_FALSE;
	}
      }
    }
  }

  if (eqv_internal(x, y, TRUE)) {
    return k;
  } else {
    return SG_FALSE;
  }  
}

#ifdef _WIN32
#define random rand
#endif
static SgObject eP(SgHashTable **pht, SgObject x, SgObject y, SgObject k, struct equal_context *ctx)
{
  ASSERT(SG_INTP(k));
  if (SG_INT_VALUE(k) <= 0) {
    if (k == ctx->kb) {
      return fast_p(pht, x, y, SG_MAKE_INT(random() % (2 * SG_INT_VALUE(ctx->k0))), ctx);
    } else {
      return slow_p(pht, x, y, k, ctx);
    }
  } else {
    return fast_p(pht, x, y, k, ctx);
  }
}

static int interleave_p(SgObject x, SgObject y, SgObject k, struct equal_context *ctx)
{
  SgHashTable *ht = NULL;
  if (SG_FALSEP(eP(&ht, x, y, k, ctx))) {
    return FALSE;
  }
  return TRUE;
}

/*
  (define (precheck/interleave-equal? x y)
    (let ((k (pre? x y k0)))
      (and k (or (> k 0)) (interleave? x y 0))))
 */
static int precheck_interleave_equal_p(SgObject x, SgObject y, struct equal_context *ctx)
{
  SgObject k = pre_p(x, y, ctx->k0);
  if (SG_FALSEP(k)) {
    return FALSE;
  }
  ASSERT(SG_INTP(k));
  if (SG_INT_VALUE(k) > 0) {
    return TRUE;
  }
  return interleave_p(x, y, SG_MAKE_INT(0), ctx);
}

/*
  (define (equal? x y)
    (precheck/interleave-equal? x y))
 */
int Sg_EqualP(SgObject x, SgObject y)
{
  struct equal_context ctx = {SG_MAKE_INT(400), SG_MAKE_INT(-40)};
  return precheck_interleave_equal_p(x, y, &ctx);
}


int SG_EqualM(SgObject x, SgObject y, int mode)
{
  switch (mode) {
  case SG_CMP_EQ:
    return Sg_EqP(x, y);
  case SG_CMP_EQV:
    return Sg_EqvP(x, y);
  case SG_CMP_EQUAL:
    return Sg_EqualP(x, y);
  }
  return FALSE;
}
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
