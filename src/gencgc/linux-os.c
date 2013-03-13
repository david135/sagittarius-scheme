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
#include <unistd.h>
#define LIBSAGITTARIUS_BODY
#include <sagittarius/core.h>
#include "os.h"
#include "gc-internal.h"
#include "linux-os.h"
#include <stdio.h>

size_t os_page_size;
void *dynamic_space_start;
void *dynamic_space_end;

#ifdef __CYGWIN__
static void init_data_start()
{
#define DATASTART (_data_start__ < _bss_start__ ?			\
		   (void *)_data_start__ : (void *)_bss_start__)
#define DATAEND (_data_end__ > _bss_end__ ?			\
		 (void *)_data_end__ : (void *)_bss_end__)
  int index = current_static_space_index++;
  static_spaces[index].start = DATASTART;
  static_spaces[index].end = DATAEND;
#undef DATASTART
#undef DATAEND
}

#else
# pragma weak __data_start
extern int __data_start[];
# pragma weak data_start
extern int data_start[];
extern int _end[];

static void init_data_start()
{
  int index = current_static_space_index++;
  /* end is always easy... */
  static_spaces[index].end = (void *)_end;
  /* Try the easy approaches first: */
  if (__data_start) {
    static_spaces[index].start = (void *)(__data_start);
    return;
  }
  if (data_start) {
    static_spaces[index].start = (void *)(data_start);
    return;
  }
  /* OK, we don't support this... */
  Sg_Panic("Platform does not have neither __data_start nor data_end");
}
#endif

void os_init()
{
  /* assume there is nothing allocated before this called. */
  void *heap_end;

  /* setup initial static roots */
  init_data_start();
#ifndef __CYGWIN__
  heap_end = sbrk(0);
#else
  /* FIXME on Cygwin sbrk doesn't work as we expected.
     so we use magic number taken from 32 bit SBCL linux. */
  /* heap_end = (void*)0x09000000UL; */
  heap_end = NULL;
#endif
  os_page_size = PAGE_BYTES;
  /* validate the address sbrk(0) returned */
  dynamic_space_start = os_validate(heap_end, DYNAMIC_SPACE_SIZE);
  dynamic_space_end = dynamic_space_start + DYNAMIC_SPACE_SIZE;
}

void * os_validate(void *addr, size_t len)
{
  int flags =  MAP_PRIVATE | MAP_ANONYMOUS
    /* probably only Cygwin requires swap area. */
#ifndef __CYGWIN__
    | MAP_NORESERVE
#endif
    ;
  void *actual;
  actual = mmap(addr, len, OS_VM_PROT_ALL, flags, -1, 0);
  if (actual == MAP_FAILED) {
    Sg_Panic("mmap failed %p (%s)", addr, strerror(errno));
  }
  if (addr && (addr != actual)) {
    Sg_Panic("mmap: wanted %lu bytes at %p, actually mapped at %p\n",
	     (unsigned long)len, addr, actual);
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
