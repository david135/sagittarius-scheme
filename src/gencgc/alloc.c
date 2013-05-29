/* alloc.c                                                -*- coding: utf-8; -*-
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
#define LIBSAGITTARIUS_BODY
#include "sagittarius/gencgc.h"
#include "gc-internal.h"
#include <sagittarius/thread.h>

#define BOUNDARY_MASK 0x7
#define ALIGNED_SIZE(n) ((n) + BOUNDARY_MASK) & ~BOUNDARY_MASK

static void * pa_alloc(int bytes, int page_type_flag)
{
  void *result;
  /* 0 byte allocation is not allowed so if it's 0 make it smallest */
  if (bytes == 0) bytes = ALIGNED_SIZE(1);
  result = general_alloc(bytes, page_type_flag);
  /* TODO trigger GC if needed */
  if (need_gc) GC_collect_garbage(0);
  return result;
}

void * GC_malloc(size_t size)
{
  return pa_alloc(ALIGNED_SIZE(size), BOXED_PAGE_FLAG);
}

void * GC_malloc_atomic(size_t size)
{
  return pa_alloc(ALIGNED_SIZE(size), UNBOXED_PAGE_FLAG);
}

void * GC_calloc(size_t num, size_t size)
{
  /* for now */
  return GC_malloc(num * size);
}

