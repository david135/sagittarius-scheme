/* pthread.c                                              -*- coding: utf-8; -*-
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
#include "os.h"
#include <pthread.h>
#include <sagittarius/gcconfig.h>
#define LIBSAGITTARIUS_BODY
#include <sagittarius/vm.h>


int thread_init(GC_thread_context_t *context)
{
  pthread_attr_t attr;
  void *stack_addr;
  size_t stack_size;
  SgVM *vm = SG_VM(context->thread);
  /* this is pthread_t */
  pthread_getattr_np(vm->thread.thread, &attr);
  pthread_attr_getstack(&attr, &stack_addr, &stack_size);

  context->cstackStart = stack_addr;
  context->cstackEnd = (void *)(((uintptr_t)stack_size) + stack_size);
}

void suspend_thread(GC_thread_context_t *context)
{
#if defined(HAVE_PTHREAD_SUSPEND)
  pthread_suspend(SG_VM(context->thread)->thread.thread);
#elif defined(HAVE_PTHREAD_SUSPEND_NP)
  pthread_suspend_np(SG_VM(context->thread)->thread.thread);
#else
# error "not supported yet!"
#endif
}

void resume_thread(GC_thread_context_t *context)
{
#if defined(HAVE_PTHREAD_RESUME)
  pthread_resume(SG_VM(context->thread)->thread.thread);
#elif defined(HAVE_PTHREAD_RESUME_NP)
  pthread_resume_np(SG_VM(context->thread)->thread.thread);
#elif defined(HAVE_PTHREAD_CONTINUE)
  pthread_continue(SG_VM(context->thread)->thread.thread);
#else
# error "not supported yet!"
#endif
}
