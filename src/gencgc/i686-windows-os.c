/* i686-windows-os.c                                      -*- coding: utf-8; -*-
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
#include "i686-windows-os.h"
#include "sagittarius/gencgc.h"

os_context_register_t *
os_context_register_addr(os_context_t *context, int offset)
{
  static const size_t offsets[8] = {
    offsetof(CONTEXT,Eax),
    offsetof(CONTEXT,Ecx),
    offsetof(CONTEXT,Edx),
    offsetof(CONTEXT,Ebx),
    offsetof(CONTEXT,Esp),
    offsetof(CONTEXT,Ebp),
    offsetof(CONTEXT,Esi),
    offsetof(CONTEXT,Edi),
  };
  return
    (offset >= 0 && offset < 16) ?
    ((void*)(context->win32_context)) + offsets[offset>>1]  : 0;
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
  return (void*)&context->win32_context->Eip; /*  REG_EIP */
}

/* from http://stackoverflow.com/questions/1740888/determining-stack-space-with-visual-studio/1747499#1747499 */
static size_t get_stack_usage(void **stack)
{
  MEMORY_BASIC_INFORMATION mbi;
  VirtualQuery(&mbi, &mbi, sizeof(mbi));
  /* now mbi.AllocationBase = reserved stack memory base address */

  VirtualQuery(mbi.AllocationBase, &mbi, sizeof(mbi));
  /* now (mbi.BaseAddress, mbi.RegionSize) describe reserved 
     (uncommitted) portion of the stack skip it */

  VirtualQuery((char*)mbi.BaseAddress + mbi.RegionSize, &mbi, sizeof(mbi));
  /* now (mbi.BaseAddress, mbi.RegionSize) describe the guard page */
  /* skip it */

  VirtualQuery((char*)mbi.BaseAddress + mbi.RegionSize, &mbi, sizeof(mbi));
  /* now (mbi.BaseAddress, mbi.RegionSize) describe the committed
     (i.e. accessed) portion of the stack */
  *stack = mbi.BaseAddress;
  return mbi.RegionSize;
}

int root_context_init(GC_thread_context_t *context)
{
  void *stack;
  size_t size = get_stack_usage(&stack);
  context->cstackStart = stack;
  context->cstackEnd = (uintptr_t)stack + size;
}
