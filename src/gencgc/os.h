/* os.h                                                 -*- coding: utf-8; -*-
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
#ifndef SAGITTARIUS_OS_H_
#define SAGITTARIUS_OS_H_

#include <sagittarius/config.h>
#include "sagittarius/gencgc.h"

#include <target-os.h>

/* compile time definition can be used if there is */
#ifndef DYNAMIC_SPACE_SIZE
# if SIZEOF_VOIDP == 8
#  define DYNAMIC_SPACE_SIZE (1UL<<30)
# else  /* SIZEOF_VOIDP == 4 */
#  define DYNAMIC_SPACE_SIZE (1UL<<29)
# endif
#endif

#define OS_VM_PROT_ALL						\
  (OS_VM_PROT_READ | OS_VM_PROT_WRITE | OS_VM_PROT_EXECUTE)

#define OS_VM_PROT_NONE 0

extern void   os_init();
extern void * os_validate(void *addr, size_t len);
extern void   os_invalidate(void *addr, size_t len);
/* This sets access rights for an area of memory, e.g.
 * write-protecting a page so that the garbage collector can find out
 * whether it's modified by handling the signal. */
extern void os_protect(void *addr, size_t len, int protection);

/* Given a signal context, return the address for storage of the
 * register, of the specified offset, for that context. The offset is
 * defined in the storage class (SC) defined in the Lisp virtual
 * machine (i.e. the file "vm.lisp" for the appropriate architecture). */
os_context_register_t *
os_context_register_addr(os_context_t *context, int offset);

/* Given a signal context, return the address for storage of the
 * program counter for that context. */
os_context_register_t *os_context_pc_addr(os_context_t *context);

/* this is not os but for now */
int thread_init(GC_thread_context_t *context);
int root_context_init(GC_thread_context_t *context);

#endif /* SAGITTARIUS_OS_H_ */
