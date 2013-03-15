/* scavenge_dispatcher.c                                  -*- coding: utf-8; -*-
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
#ifdef CONST
#undef CONST
#endif
#include <sagittarius/closure.h>
#include <sagittarius/code.h>
#include <sagittarius/file.h>
#include <sagittarius/gloc.h>
#include <sagittarius/hashtable.h>
#include <sagittarius/instruction.h>
#include <sagittarius/library.h>
#include <sagittarius/port.h>
#include <sagittarius/string.h>
#include <sagittarius/vector.h>
#include <sagittarius/vm.h>


static void scav_hashtable(void **where, SgObject obj)
{
  SgHashIter itr;
  SgHashEntry *e;
  /* does this work? */
  scavenge((void **)obj, sizeof(SgHashTable) / sizeof(SgObject));
  Sg_HashIterInit(SG_HASHTABLE_CORE(obj), &itr);
  while ((e = Sg_HashIterNext(&itr)) != NULL) {
    scavenge(e, sizeof(SgHashEntry) / sizeof(SgObject));
    scavenge(SG_HASH_ENTRY_KEY(e), 1);
    scavenge(SG_HASH_ENTRY_VALUE(e), 1);
  }
}

static int scavenge_general_pointer(void **where, block_t *block);
/*
  dispatch scavenge process to proper procedures.
  the block contains Scheme object so that we can see what we need to
  move.
 */
static int dispatch_pointer(void **where, block_t *block)
{
  SgObject obj = SG_OBJ(ALLOCATED_POINTER(block));
  if (SG_HASHTABLE_P(obj)) {
    scav_hashtable(where, obj);
  }
  /* return MEMORY_SIZE(block); */
  
  /* if (SG_STRINGP(obj)) { */
  /*   SgObject s = gc_alloc_unboxed(sizeof(SgString) +  */
  /* 				  (sizeof(SgChar) * (SG_STRING_SIZE(obj)))); */
  /*   int i; */
  /*   SG_SET_CLASS(s, SG_CLASS_STRING); */
  /*   for (i = 0; i < SG_STRING_SIZE(obj); i++) { */
  /*     SG_STRING_VALUE_AT(s, i) = SG_STRING_VALUE_AT(obj, i); */
  /*   } */
  /*   *where = POINTER2BLOCK(s); */
  /* } */
  else {
    scavenge_general_pointer(where, block);
  }
}

/*
  scavenge general pointer.
  We need to check if the target pointer contains pointer.
 */
void * trans_general_pointer(block_t *block)
{
  return gc_general_copy_object(block, BLOCK_SIZE(block), BOXED_PAGE_FLAG);
}

int scavenge_general_pointer(void **where, block_t *block)
{
  int unboxedp = page_unboxed_p(find_page_index((void *)block));
  if (unboxedp) {
    /* OK block doesn't contain any pointer so we can simply remove it */
    return MEMORY_SIZE(block);
  } else {
    /* now, we might have Scheme pair, so how should we detect it? */
    void *first, **first_pointer;
    first_pointer = ALLOCATED_POINTER(block);
    first = trans_general_pointer(block);
    if (first != block) {
      SET_MEMORY_FORWARDED(block, first);
      *where = first;
    }
    return MEMORY_SIZE(block);
  }
}

/* TODO move this somewhere */
static void preserve_scheme_pointer(void *obj);

static void preserve_hashtable(SgObject obj)
{
  SgHashCore *core = SG_HASHTABLE_CORE(obj);
  SgHashIter itr;
  SgHashEntry *e;
  Sg_HashIterInit(core, &itr);
  preserve_pointer(core->buckets);
  preserve_pointer(core->data);
  preserve_pointer(core->generalHasher);
  preserve_pointer(core->generalCompare);
  while ((e = Sg_HashIterNext(&itr)) != NULL) {
    preserve_pointer(e);
    if (is_scheme_pointer(SG_HASH_ENTRY_KEY(e))) {
      preserve_scheme_pointer(SG_HASH_ENTRY_KEY(e));
    } else {
      preserve_pointer(SG_HASH_ENTRY_KEY(e));
    }
    if (is_scheme_pointer(SG_HASH_ENTRY_VALUE(e))) {
      preserve_scheme_pointer(SG_HASH_ENTRY_VALUE(e));
    } else {
      preserve_pointer(SG_HASH_ENTRY_VALUE(e));
    }
  }
}

static void preserve_port(SgObject obj)
{
  SgBinaryPort *bp;
  SgTextualPort *tp;
  preserve_pointer(SG_PORT(obj)->readtable);
  preserve_scheme_pointer(SG_PORT(obj)->reader);
  preserve_scheme_pointer(SG_PORT(obj)->loadPath);
  preserve_scheme_pointer(SG_PORT(obj)->previousPort);
  if (SG_BINARY_PORTP(obj)) {
    bp = SG_BINARY_PORT(obj);
  binary_port:
    if (!bp) return;
    switch (bp->type) {
    case SG_FILE_BINARY_PORT_TYPE:
      preserve_scheme_pointer(bp->src.file);
      break;
    case SG_BYTE_ARRAY_BINARY_PORT_TYPE:
      if (!SG_INPORTP(obj)) {
	byte_buffer *p;
	for (p = bp->src.obuf.start; p && p != bp->src.obuf.current;
	     p = p->next) 
	  preserve_pointer(p);
      } else {
	/* bytevector doesn't have pointer so just like this is fine */
	preserve_pointer(bp->src.buffer.bvec);
      }
      break;
    case SG_CUSTOM_BINARY_PORT_TYPE:
      preserve_pointer(bp->src.custom.data);
      break;
    }
  } else if (SG_TEXTUAL_PORTP(obj)) {
    tp = SG_TEXTUAL_PORT(obj);
  textual_port:
    if (!tp) return;
    switch (tp->type) {
    case SG_TRANSCODED_TEXTUAL_PORT_TYPE:
      preserve_scheme_pointer(tp->src.transcoded.transcoder);
      preserve_scheme_pointer(tp->src.transcoded.port);
      break;
    case SG_STRING_TEXTUAL_PORT_TYPE:
      if (!SG_INPORTP(obj)) {
	char_buffer *p;
	for (p = tp->src.ostr.start; p && p != tp->src.ostr.current;
	     p = p->next) 
	  preserve_pointer(p);
      } else {
	/* bytevector doesn't have pointer so just like this is fine */
	preserve_pointer(tp->src.buffer.str);
      }
      break;
    case SG_CUSTOM_TEXTUAL_PORT_TYPE:
      preserve_pointer(tp->src.custom.data);
      break;
    }
  } else if (SG_CUSTOM_PORTP(obj)) {
    SgCustomPort *cp = SG_CUSTOM_PORT(obj);
    if (!cp) return;
    preserve_scheme_pointer(cp->id);
    preserve_scheme_pointer(cp->getPosition);
    preserve_scheme_pointer(cp->setPosition);
    preserve_scheme_pointer(cp->close);
    preserve_scheme_pointer(cp->read);
    preserve_scheme_pointer(cp->write);
    preserve_scheme_pointer(cp->ready);
    preserve_pointer(cp->buffer);
    switch (cp->type) {
    case SG_BINARY_CUSTOM_PORT_TYPE:
      bp = SG_CUSTOM_BINARY_PORT(obj);
      goto binary_port;
    case SG_TEXTUAL_CUSTOM_PORT_TYPE:
      tp = SG_CUSTOM_TEXTUAL_PORT(obj);
      goto textual_port;
    }
  } else {
    Sg_Panic("unknown port");
  }
}

void preserve_scheme_pointer(void *obj)
{
 reent:
  if (!obj) {
    return;
  }
  preserve_pointer(obj);
  if (SG_HASHTABLE_P(obj)) {
    preserve_hashtable(SG_OBJ(obj));
  } else if (SG_PORTP(obj)) {
    preserve_port(SG_OBJ(obj));
  } else if (SG_FILEP(obj)) {
    preserve_pointer((void *)SG_FILE(obj)->name);
  } else if (SG_CLOSUREP(obj)) {
    SgObject code = SG_CLOSURE(obj)->code;
    int freec = SG_CODE_BUILDER_FREEC(code), i;
    for (i = 0; i < freec; i++) {
      preserve_scheme_pointer(SG_CLOSURE(obj)->frees[i]);
    }
    obj = code;
    goto reent;
  } else if (SG_CODE_BUILDERP(obj)) {
    /* TODO we need to preserve scheme objects in code */
    int size = SG_CODE_BUILDER(obj)->size, i;
    SgWord *code = SG_CODE_BUILDER(obj)->code;
    preserve_pointer(code);
    for (i = 0; i < size;) {
      InsnInfo *info = Sg_LookupInsnName(INSN(code[i]));
      if (info->argc) {
	SgObject o = SG_OBJ(code[++i]);
	page_index_t index = find_page_index(o);
	if (index >= 0 && 
	    (page_table[index].gen != new_space ||
	     !page_table[index].dont_move)) {
	  /* if ((uintptr_t)o == 0x9108740) { */
	  /*   fprintf(stderr, "wtf?\n"); */
	  /* } */
	  preserve_scheme_pointer(o);
	}
	i += info->argc;
      } else {
	i++;
      }
    }
    preserve_scheme_pointer(SG_CODE_BUILDER_NAME(obj));
    preserve_scheme_pointer(SG_CODE_BUILDER_SRC(obj));
  } else if (SG_VMP(obj)) {
    SgVM *vm = SG_VM(obj);
    void **stack;
    /* TODO we need to preserve a lot but for now */
    /* vm registers */
    preserve_scheme_pointer(vm->ac);
    preserve_scheme_pointer(vm->cl);
    preserve_pointer(vm->stack);
    for (stack = vm->stack; stack < vm->sp; stack++) {
      if (is_scheme_pointer(*stack)) {
	/* TODO it's better to copy */
	preserve_scheme_pointer(*stack);
      } else {
	preserve_pointer(*stack);
      }
    }
    /* preserve continuation frame */
    /* TODO */
  } else if (SG_VECTORP(obj)) {
    int i;
    for (i = 0; i < SG_VECTOR_SIZE(obj);i ++) {
      /* TODO copy */
      page_index_t index = find_page_index(SG_VECTOR_ELEMENT(obj, i));
      /* if it's already preserved we don't go deeper...
	 FIXME: how should we detect the cyclic properly. */
      if (index >= 0 && 
	  (page_table[index].gen != new_space &&
	   !page_table[index].dont_move)) {
	preserve_scheme_pointer(SG_VECTOR_ELEMENT(obj, i));
      }
    }
  } else if (SG_LIBRARYP(obj)) {
    preserve_scheme_pointer(SG_LIBRARY_NAME(obj));
    preserve_hashtable(SG_LIBRARY_TABLE(obj));
    preserve_scheme_pointer(SG_LIBRARY_IMPORTED(obj));
    preserve_scheme_pointer(SG_LIBRARY_EXPORTED(obj));
  } else if (SG_GLOCP(obj)) {
    preserve_scheme_pointer(SG_GLOC_GET(SG_GLOC(obj)));
  }
}
