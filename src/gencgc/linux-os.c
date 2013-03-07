/* linux-os.c                                             -*- coding: utf-8; -*-
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
#include <errno.h>
#include <string.h>
#define LIBSAGITTARIUS_BODY
#include <sagittarius/core.h>
#include "os.h"
#include "gc-internal.h"
#include "linux-os.h"
#include <stdio.h>

size_t os_page_size;
void *dynamic_space_start;
/* FIXME this must be configured in build time */
size_t dynamic_space_size = 0x09000000UL;

void os_init()
{
  os_page_size = PAGE_BYTES;
  /* allocate dynamic space here 
     FIXME the size should not be hard coded.
   */
  dynamic_space_start = mmap(NULL, dynamic_space_size, OS_VM_PROT_ALL,
			     MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (dynamic_space_start == MAP_FAILED) {
    Sg_Panic("dynamic space allocation failed (%s)", strerror(errno));
  }
}

void * os_validate(void *addr, size_t len)
{
  int flags =  MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE;
  void *actual;
  actual = mmap(addr, len, OS_VM_PROT_ALL, flags, -1, 0);
  if (actual == MAP_FAILED) {
    Sg_Panic("mmap failed %p (%s)", addr, strerror(errno));
  }
  if (addr && (addr!=actual)) {
    Sg_Panic("mmap: wanted %lu bytes at %p, actually mapped at %p\n",
	     (unsigned long) len, addr, actual);
  }
  return actual;
}
void os_invalidate(void *addr, size_t len)
{
  if (munmap(addr,len) == -1) {
    Sg_Panic("munmmap failed %p (%s)", addr, strerror(errno));
  }
}

void os_protect(void *address, size_t length, int prot)
{
  if (mprotect(address, length, prot) == -1) {
    Sg_Panic("os_protect failed (%s).", strerror(errno));
  }
}
