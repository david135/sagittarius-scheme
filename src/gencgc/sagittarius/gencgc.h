/* gencgc.h                                               -*- coding: utf-8; -*-
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
#ifndef SAGITTARIUS_GENCGC_H_
#define SAGITTARIUS_GENCGC_H_

#include <sagittarius/common-macros.h>
#include <stddef.h>

/* TODO this must be defined in gcconfig.h during build process. */
#ifdef __i386__
# define PAGE_BYTES 4096UL
#elif __x86_64__
# define PAGE_BYTES 32768UL
#else
# error "non supported architecture"
#endif
#define GENERATIONS 6

typedef void * (* GC_oom_func)(size_t /* bytes_requested */);

SG_CDECL_BEGIN

/* The GC APIs are similar with Boehm GC for my convenience. 
 */
SG_EXTERN void   GC_init();
SG_EXTERN void * GC_malloc(size_t size);
SG_EXTERN void * GC_malloc_atomic(size_t size);
SG_EXTERN void * GC_calloc(size_t num, size_t size);

SG_EXTERN void * GC_collect_garbage(int generation);

SG_EXTERN void   GC_set_oom_fn(GC_oom_func handler);

SG_CDECL_END

/* compatible macros */
#define GC_INIT() GC_init()

#endif /* SAGITTARIUS_GENCGC_H_ */
