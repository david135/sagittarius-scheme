/* gc-internal.h                                          -*- coding: utf-8; -*-
 *
 *   Copyright (c) 2010-2013  Takashi Kato <ktakashi@ymail.com>
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
#ifndef SAGITTARIUS_GC_INTERNAL_H_
#define SAGITTARIUS_GC_INTERNAL_H_

#include <sagittarius/config.h>
#include "sagittarius/gencgc.h"
#include <stdint.h>
#include <stddef.h>
#include <limits.h>
#include <inttypes.h>

#define N_WORD_BYTES SIZEOF_VOIDP
#define N_WORD_BITS  (N_WORD_BYTES*8)

/* TODO move this somewhere */
#define CARD_BYTES 0x1000LU
/* 8 byte boundary */
#define LOWTAG_MASK 0x07

extern alloc_region_t boxed_region;
extern alloc_region_t unboxed_region;
extern generation_index_t  from_space, new_space;
extern int            need_gc;

#if CARD_BYTES > USHRT_MAX
# if CARD_BYTES > UINT_MAX
#   error "CARD_BYTES unexpectedly large."
# else
#   define PAGE_BYTES_FMT "u"
    typedef unsigned int page_bytes_t;
# endif
#else
# define PAGE_BYTES_FMT "hu"
  typedef unsigned short page_bytes_t;
#endif

#include "os.h"

/*
  Page
 */
typedef struct page {
  /* the offset from the start of the page to the start of the alloc_region.
   */
  size_t region_start_offset;
  /* the number of bytes of this page that are used. This maybe less than the
     actual bytes used for pages within the current allocation regions. It 
     should be 0 for all unallocated pages.
   */
  page_bytes_t bytes_used;

  /* This is set when the page is write-protected. This should always reflect
     the actual write_protect status of a page. */
  unsigned int write_protected: 1;
  /* This flag is set when the above write_protected flag is cleared.
     TODO do we need this? */
  unsigned int write_protected_cleared: 1;
  /* 0000 free
     0001 boxed data
     0010 unboxed data
     -- 0100 array data
     1000 open region
     If the page is free the following slots are invalid, except for the
     bytes_used which must be zero.
   */
  unsigned int allocated: 3;	/* 4 if we want specific array alloc */
  /* If this page should not be moved during a GC then this flag is set.
     It's only valid during a GC for allocated pages.*/
  unsigned int dont_move: 1;
  /* If the page is part of a large object then this flag is set. No other
     objects should be allocated to these pages. This is only valid when the
     page is allocated.
   */
  unsigned int large_object: 1;
  /* Cleared if the page is known to contain only zeros.
   */
  unsigned int need_to_zero: 1;
  /* the generation that this page belongs to. This should be valid for all
     pages that may have objects allocated, even current allocation region
     pages - this allows the space of an object to be easily determined.
   */
  generation_index_t gen;

  /* counter of number of finalizer in this page
   */
  unsigned int finalizers;
} page_t;

/* values for the page.allocated field */
extern page_index_t page_table_pages;
extern page_t *page_table;

/* allocation starting point 
   TODO we probably want to make READ_ONLY space for constant literals.
 */
extern void *dynamic_space_start;
extern void *dynamic_space_end;
extern size_t dynamic_space_size;
#define DEFAULT_DYNAMIC_SPACE_SIZE (dynamic_space_end - dynamic_space_start)

void add_static_root(void *start, void *end);

/* #define READ_ONLY_SPACE_START read_only_space_start */
#define DYNAMIC_SPACE_START   dynamic_space_start

#define FREE_PAGE_FLAG        0
#define BOXED_PAGE_FLAG       1
#define UNBOXED_PAGE_FLAG     2
#define OPEN_REGION_PAGE_FLAG 4
/* #define ARRAY_PAGE_FLAG       4 */
/* #define OPEN_REGION_PAGE_FLAG 8 */

#define ALLOC_QUICK TRUE

#define CEILING(x,y) (((x) + ((y) - 1)) & (~((y) - 1)))
#define NWORDS(x, bits)							\
  (((bits)<=N_WORD_BITS)						\
   ? CEILING(x, (N_WIDETAG_BITS/(bits)))/(N_WIDETAG_BITS/(bits))	\
   : (x)*(bits/N_WORD_BITS))

/* predicates */
static inline int space_matches_p(void *obj, generation_index_t space)
{
  if (obj >= DYNAMIC_SPACE_START) {
    page_index_t page_index = (obj - DYNAMIC_SPACE_START) / CARD_BYTES;
    return ((page_index < page_table_pages) &&
	    (page_table[page_index].gen == space));
  } else {
    return FALSE;
  }
}

static inline int from_space_p(void *obj)
{
  return space_matches_p(obj, from_space);
}

static inline int new_space_p(void *obj)
{
  return space_matches_p(obj, new_space);
}

extern void * general_alloc(size_t nbytes, int page_type_flag);

/* registers */
#define REG(num) num
#ifdef __i686__
# define reg_EAX  REG( 0)
# define reg_ECX  REG( 2)
# define reg_EDX  REG( 4)
# define reg_EBX  REG( 6)
# define reg_ESP  REG( 8)
# define reg_EBP  REG(10)
# define reg_ESI  REG(12)
# define reg_EDI  REG(14)
# define reg_UESP REG(16)
# define reg_SP   reg_ESP
# define reg_FP   reg_EBP
#elif __x86_64__
# define reg_RAX REG( 0)
# define reg_RCX REG( 2)
# define reg_RDX REG( 4)
# define reg_RBX REG( 6)
# define reg_RSP REG( 8)
# define reg_RBP REG(10)
# define reg_RSI REG(12)
# define reg_RDI REG(14)
# define reg_R8  REG(16)
# define reg_R9  REG(18)
# define reg_R10 REG(20)
# define reg_R11 REG(22)
# define reg_R12 REG(24)
# define reg_R13 REG(26)
# define reg_R14 REG(28)
# define reg_R15 REG(30)
# define reg_SP reg_RSP
# define reg_FP reg_RBP
#else
# error "the architecture is not supported"
#endif

/* TODO:
   The finalizer we need to store somewhere else but in the memory header
   so that invoking process can be delayed. The original SBCL implementation
   using weak pointer to do it. Probably we want to do the same or similar
   way. But for now.
 */
/* we pack the header for better memory usage:
   32 bit model;
   xxxx xxxx xxxx xxxx xxxx xxxx xxxx xxrf
   x = 30 bit finalizer address
   r = 1  bit reserved
   f = 1  bit forwarded pointer flag
   Most probably we can use 3 bits for flags, but i have never
   checked if the function pointer is located on 8 byte boundary
   or not. So let's do it safe way so far.
*/
/* must be unit of 8 */
typedef struct memory_header {
  uintptr_t forwarded : 1;
  uintptr_t has_finalizer: 1;	/* for some sanity check */
  uintptr_t finalizer : N_WORD_BITS-2;
  uintptr_t size;
} memory_header_t;
typedef struct block {
  memory_header_t header;
  uintptr_t body;
} block_t;

#define SET_MEMORY_SIZE(block, size_)			\
  do {							\
    memset(block, 0, size_ + sizeof(memory_header_t));	\
    (block)->header.size = (size_);			\
    (block)->header.forwarded = FALSE;			\
    (block)->header.has_finalizer = FALSE;		\
    (block)->header.finalizer = (uintptr_t)0;		\
  } while (0)
#define BLOCK(obj)               ((block_t *)obj)
#define MEMORY_SIZE(block)       BLOCK(block)->header.size
#define MEMORY_FORWARDED(block)  BLOCK(block)->header.forwarded
#define MEMORY_FINALIZER(block)  BLOCK(block)->header.finalizer
#define GET_MEMORY_FINALIZER(block)					\
  (GC_finalizer_proc)((uintptr_t)BLOCK(block)->header.finalizer<<2)
#define ALLOCATED_POINTER(block) ((void *)&(BLOCK(block)->body))
#define POINTER2BLOCK(p)    ((block_t*)(((uintptr_t)p)-sizeof(memory_header_t)))
#define BLOCK_MAY_VALID_P(block)		\
  (BLOCK(block)->header.has_finalizer		\
   ? BLOCK(block)->header.finalizer != NULL	\
   : BLOCK(block)->header.finalizer == NULL)
/* we can use finalizer location for forwarding pointer */
#define SET_MEMORY_FORWARDED(block, pointer)			\
  do {								\
    MEMORY_FORWARDED(block) = TRUE;				\
    MEMORY_FINALIZER(block) = ((uintptr_t)(pointer))>>2;	\
  } while (0)
#define MEMORY_FORWARDED_VALUE(block)			\
  ((void *)(((uintptr_t)MEMORY_FINALIZER(block))<<2))

#define MEMORY_HAS_FINALIZER(block)  BLOCK(block)->header.has_finalizer
#define SET_MEMORY_FINALIZER(block, proc, data)			\
  do {								\
    MEMORY_FINALIZER(block) = ((uintptr_t)proc >> 2);		\
    /* to make unregister easier */				\
    MEMORY_HAS_FINALIZER(block) = ((void *)proc != NULL);	\
  } while (0)

#define BLOCK_SIZE(block) (MEMORY_SIZE(block) + sizeof(memory_header_t))

/* debugging */
#define QSHOW 1
#if QSHOW
# define FSHOW(args)				\
  do { fprintf args; } while (0)
#else
# define FSHOW(args)
#endif


#endif /* SAGITTARIUS_GC-INTERNAL_H_ */
