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
#include <sagittarius/codec.h>
#include <sagittarius/file.h>
#include <sagittarius/gloc.h>
#include <sagittarius/hashtable.h>
#include <sagittarius/identifier.h>
#include <sagittarius/instruction.h>
#include <sagittarius/keyword.h>
#include <sagittarius/library.h>
#include <sagittarius/macro.h>
#include <sagittarius/pair.h>
#include <sagittarius/port.h>
#include <sagittarius/reader.h>
#include <sagittarius/record.h>
#include <sagittarius/string.h>
#include <sagittarius/symbol.h>
#include <sagittarius/transcoder.h>
#include <sagittarius/vector.h>
#include <sagittarius/vm.h>
#include <sagittarius/weak.h>


/* TODO move this somewhere */
static void salvage_scheme_pointer(void **where, void *obj);
static void scavenge_general_pointer(void **where, void *obj);

/*
  dispatch scavenge process to proper procedures.
  the block contains Scheme object so that we can see what we need to
  move.
 */
static void dispatch_pointer(void **where, void *obj)
{
  salvage_scheme_pointer(where, obj);
}

/*
  scavenge general pointer.
  We need to check if the target pointer contains pointer.
 */
void scavenge_general_pointer(void **where, void *obj)
{
  page_index_t index = find_page_index(obj);
  int unboxedp = page_unboxed_p(index);
  int large_p = page_table[index].large_object;
  void *first;

  /* this case it will be scavenged later on. */
  if (index < 0 || page_table[index].dont_move ||
      page_table[index].gen != from_space) return;

  if (MEMORY_FORWARDED(POINTER2BLOCK(obj))) {
    *where = MEMORY_FORWARDED_VALUE(POINTER2BLOCK(obj));
    return;
  }

  if (unboxedp) {
    if (large_p) {
      first = copy_large_unboxed_object(obj);
    } else {
      first = copy_unboxed_object(obj);
    }
  } else {
    if (large_p) {
      first = copy_large_object(obj);
    } else {
      /* now, we might have Scheme pair, so how should we detect it? */
      first = copy_object(obj);
    }
  }
  if (first != obj) {
    SET_MEMORY_FORWARDED(POINTER2BLOCK(obj), first);
    *where = first;
  }
  /* general pointer can contain pointers in it and if the nothing refers it
     but the given object it would be mis collected. To prevent it we need to
     check inside of the object. */
  if (!unboxedp) {
    /* unboxed object doesn't care the inside :-) */
    /* assume object is not on block boundary. see gencgc.c */
    block_t *block = POINTER2BLOCK(first);
    scavenge((intptr_t *)block, MEMORY_SIZE(block)/sizeof(void *));
  }
}

#define copy_root_object(where, obj, copier)			\
  do {								\
    page_index_t index__ = find_page_index(obj);		\
    if (index >= 0 && !page_table[index__].dont_move &&		\
	page_table[index__].gen == from_space) {		\
      block_t *block__ = POINTER2BLOCK(obj);			\
      SgObject z__;						\
      /* basic check */						\
      if (MEMORY_FORWARDED(block__)) {				\
	z__ = MEMORY_FORWARDED_VALUE(block__);			\
      } else {							\
	z__ = copier(obj);					\
	SET_MEMORY_FORWARDED(block__, z__);			\
      }								\
      *where = z__;						\
      (obj) = z__;						\
    }								\
  } while (0)

#define check_forwarded(obj)			\
  do {						\
    block_t *block = POINTER2BLOCK(obj);	\
    if (MEMORY_FORWARDED(block)) return;	\
  } while (0)

#define salvage_entry_key(e)					\
  do {								\
    if (is_scheme_pointer(SG_HASH_ENTRY_KEY(e))) {		\
      salvage_scheme_pointer(&e->key, SG_HASH_ENTRY_KEY(e));	\
    } else {							\
      /* TODO how should we copy this? */			\
      preserve_pointer(SG_HASH_ENTRY_KEY(e));			\
    }								\
  } while (0)

#define salvage_entry_value(e)						\
  do {									\
    if (is_scheme_pointer(SG_HASH_ENTRY_VALUE(e))) {			\
      salvage_scheme_pointer(&e->value, SG_HASH_ENTRY_VALUE(e));	\
    } else {								\
      preserve_pointer(SG_HASH_ENTRY_VALUE(e));				\
    }									\
  } while (0)

static void salvage_hashtable(void **where, SgObject obj)
{
  SgHashCore *core;
  SgHashIter itr;
  SgHashEntry *e;
  /* page_index_t index = find_page_index(obj); */
  copy_root_object(where, obj, copy_object);
  core = SG_HASHTABLE_CORE(obj);
  copy_root_object(&core->buckets, core->buckets, copy_large_object);
  Sg_HashIterInit(core, &itr);
  /* TODO how should we treet this? and do we use this? */
  /* preserve_pointer(core->data); */

  /* We probably don't need following code as long as object is copied. */
  /* salvage_scheme_pointer(&core->generalHasher, core->generalHasher); */
  /* salvage_scheme_pointer(&core->generalCompare, core->generalCompare); */
  while ((e = Sg_HashIterNext(&itr)) != NULL) {
    /* The entry pointer must be already preserved or if it doesn't
       then it's not on stack nor register so we don't have to preserve
       but copy. */
    page_index_t eindex = find_page_index(e);
    /* if the entry is in from space we need to copy it */
    if (page_table[eindex].gen == from_space) {
      /* not on stack so copy it */
      /* NOTE: do not use sizeof(SgHashEntry) the actual entry is
	 not the same size of it. */
      SgHashEntry *z = copy_object(e);
      SET_MEMORY_FORWARDED(POINTER2BLOCK(e), z);
      Sg_HashCoreReplaseEntry(core, (intptr_t)z->key, z);
      e = z;
    }
    /* NOTE: we don't need following so that it will be handled by
       scavenge as long as entry isn't in from space. */
    /* salvage_entry_key(e); */
    /* salvage_entry_value(e); */
  }
  /* it's probably better to check if the real entry has been moved or not
     but for now.*/
  if (SG_HASHTABLE(obj)->type == SG_HASH_EQ) {
    core->rehashNeeded = TRUE;
  }
}

static void salvage_port(void **where, SgObject obj)
{
  SgBinaryPort *bp;
  SgTextualPort *tp;
  copy_root_object(where, obj, copy_object);
  /* FIXME, readtable can contain scheme pointer */
  if (SG_PORT(obj)->readtable) {
    SG_PORT(obj)->readtable = copy_object(SG_PORT(obj)->readtable);
  }
  salvage_scheme_pointer(&SG_PORT(obj)->reader, SG_PORT(obj)->reader);
  salvage_scheme_pointer(&SG_PORT(obj)->loadPath, SG_PORT(obj)->loadPath);
  salvage_scheme_pointer(&SG_PORT(obj)->previousPort,
			 SG_PORT(obj)->previousPort);
  if (SG_BINARY_PORTP(obj)) {
    bp = SG_BINARY_PORT(obj);
  binary_port:
    if (!bp) return;
    switch (bp->type) {
    case SG_FILE_BINARY_PORT_TYPE:
      salvage_scheme_pointer((void **)&bp->src.file, bp->src.file);
      break;
    case SG_BYTE_ARRAY_BINARY_PORT_TYPE:
      if (!SG_INPORTP(obj)) {
	byte_buffer *p;
	for (p = bp->src.obuf.start;
	     p && p != bp->src.obuf.current;
	     p = p->next) {
	  byte_buffer *t;
	  t = (byte_buffer *)copy_object(p);
	  t->next = p->next;
	}
      } else {
	/* bytevector doesn't have pointer so just like this is fine */
	salvage_scheme_pointer((void **)&bp->src.buffer.bvec,
			       bp->src.buffer.bvec);
      }
      break;
    case SG_CUSTOM_BINARY_PORT_TYPE:
      if (bp->src.custom.data) {
	bp->src.custom.data = copy_object(bp->src.custom.data);
      }
      break;
    }
  } else if (SG_TEXTUAL_PORTP(obj)) {
    tp = SG_TEXTUAL_PORT(obj);
  textual_port:
    if (!tp) return;
    switch (tp->type) {
    case SG_TRANSCODED_TEXTUAL_PORT_TYPE:
      salvage_scheme_pointer((void **)&tp->src.transcoded.transcoder,
			     tp->src.transcoded.transcoder);
      salvage_scheme_pointer((void **)&tp->src.transcoded.port,
			     tp->src.transcoded.port);
      break;
    case SG_STRING_TEXTUAL_PORT_TYPE:
      if (!SG_INPORTP(obj)) {
	char_buffer *p;
	for (p = tp->src.ostr.start; p && p != tp->src.ostr.current;
	     p = p->next) {
	  char_buffer *t;
	  t = (char_buffer *)copy_object(p);
	  t->next = p->next;
	}
      } else {
	/* bytevector doesn't have pointer so just like this is fine */
	salvage_scheme_pointer((void **)&tp->src.buffer.str,
			       tp->src.buffer.str);
      }
      break;
    case SG_CUSTOM_TEXTUAL_PORT_TYPE:
      if (tp->src.custom.data) {
	tp->src.custom.data = copy_object(tp->src.custom.data);
      }
      break;
    }
  } else if (SG_CUSTOM_PORTP(obj)) {
    SgCustomPort *cp = SG_CUSTOM_PORT(obj);
    if (!cp) return;
    salvage_scheme_pointer((void **)&cp->id, cp->id);
    salvage_scheme_pointer(&cp->getPosition, cp->getPosition);
    salvage_scheme_pointer(&cp->setPosition, cp->setPosition);
    salvage_scheme_pointer(&cp->close, cp->close);
    salvage_scheme_pointer(&cp->read, cp->read);
    salvage_scheme_pointer(&cp->write, cp->write);
    salvage_scheme_pointer(&cp->ready, cp->ready);
    /* salvage_scheme_pointer((void **)&cp->buffer, cp->buffer); */
    copy_root_object(&cp->buffer, cp->buffer, copy_large_unboxed_object);
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

static void salvage_vm(void **where, void *obj)
{
  SgVM *vm;
  void **stack;
  intptr_t sp_diff, fp_diff;
  copy_root_object(where, obj, copy_object);
  vm = SG_VM(obj);
  /* TODO we need to salvage a lot but for now */
  /* vm registers */
  salvage_scheme_pointer(&vm->ac, vm->ac);
  salvage_scheme_pointer(&vm->cl, vm->cl);

  /* copy stack, I think FP and SP always point on stack,
     if I remember correctly... */
  sp_diff = vm->sp - vm->stack;
  fp_diff = vm->fp - vm->stack;
  copy_root_object(&vm->stack, vm->stack, copy_large_object);
  vm->sp = vm->stack + sp_diff;
  vm->fp = vm->stack + fp_diff;
  for (stack = vm->stack; stack < vm->sp; stack++) {
    if ((uintptr_t)*stack == 0x9161748) {
      fprintf(stderr, "in table\n");
    }
    if (is_scheme_pointer(*stack)) {
      /* TODO it's better to copy */
      salvage_scheme_pointer(stack, *stack);
    } else {
      preserve_pointer(*stack);
    }
  }
  /* source info */
  /* TODO, if we use GENCGC then we can simply remove this
     by implementing annotated pair. */
  if (weak_hashtables) {
    SG_WEAK_HASHTABLE(weak_hashtables)->next = vm->sourceInfos;
  } else {
    weak_hashtables = vm->sourceInfos;
  }
}

static void * trans_list(void *obj)
{
  block_t *block = POINTER2BLOCK(obj);
  SgObject cdr, cons;
  /* copy 'object' */
  /* Following is from SBCL however we have extra in SgPair so need to
     use other one*/
#if 0
#define copy_pair(block, dst, p)					\
  dst = gc_general_alloc(sizeof(SgPair), BOXED_PAGE_FLAG, ALLOC_QUICK); \
  SG_SET_CAR(dst, SG_CAR(p));						\
  SG_SET_CDR(dst, SG_CDR(p));						\
  SET_MEMORY_FORWARDED(block, dst);
#else
  /* copy_object handle object size and contents (by memcpy) */
#define copy_pair(block, dst, p)		\
  dst = copy_object(p);				\
  SET_MEMORY_FORWARDED(block, dst);
#endif
  /* didn't work as I expected. */
#if 0
#define copy_car(p)					\
  do  {							\
    SgObject car = SG_CAR(p);				\
    if (SG_PAIRP(car) && from_space_p(car)) {		\
      block_t *car_block = POINTER2BLOCK(car);		\
      if (!MEMORY_FORWARDED(car_block)) {		\
	salvage_scheme_pointer(&SG_CAR(p), car);	\
      }							\
    }							\
  } while (0)
#else
#define copy_car(p)		/* dummy */
#endif

  copy_pair(block, cons, obj);
  copy_car(cons);

  cdr = SG_CDR(obj);
  while (TRUE) {
    SgObject newcdr;
    block = POINTER2BLOCK(cdr);
    if (!SG_PAIRP(cdr) || !from_space_p(cdr) || MEMORY_FORWARDED(block))
      break;

    copy_pair(block, newcdr, cdr);
    copy_car(newcdr);
    cdr = SG_CDR(cdr);
  }
  return cons;
}

static void salvage_list(void **where, void *obj)
{
  page_index_t index = find_page_index(obj);
  if (page_table[index].gen == from_space) {
    block_t *block = POINTER2BLOCK(obj);
    void *first = trans_list(obj);
    SET_MEMORY_FORWARDED(block, first);
    *where = first;
  }
}

static void salvage_library(void **where, void *obj)
{
  /* if the given library is already forwarded then the rest of the
     objects must be salvaged. */
  check_forwarded(obj);
  copy_root_object(where, obj, copy_object);
  salvage_scheme_pointer(&SG_LIBRARY_NAME(obj), SG_LIBRARY_NAME(obj));
  salvage_scheme_pointer((void **)&SG_LIBRARY_TABLE(obj),
			 SG_LIBRARY_TABLE(obj));
  if (SG_PAIRP(SG_LIBRARY_IMPORTED(obj))) {
    salvage_scheme_pointer(&SG_LIBRARY_IMPORTED(obj),
			   SG_LIBRARY_IMPORTED(obj));
  }
  if (SG_PAIRP(SG_LIBRARY_EXPORTED(obj))) {
    salvage_scheme_pointer(&SG_LIBRARY_EXPORTED(obj),
			   SG_LIBRARY_EXPORTED(obj));
  }
}

#if 1
static int target_p(SgObject o, const char *name)
{
  if (SG_SYMBOLP(o)) {
    return ustrcmp(SG_STRING(SG_SYMBOL(o)->name)->value, name) == 0;
  } else if (SG_STRINGP(o)) {
    return ustrcmp(SG_STRING(o)->value, name) == 0;
  }
  return FALSE;
}
#endif

/* If where is NULL means, it's from the top most position.
   (obj should be pinned)
 */
void salvage_scheme_pointer(void **where, void *obj)
{
  page_index_t index;
  block_t *block;
  if (!obj) {
    return;
  }

  /* immediate value must be ignored. */
  if (!SG_PTRP(obj)) return;
  /* might be static area */
  if (!possibly_valid_dynamic_space_pointer(obj)) return;
  index = find_page_index(obj);
  if (page_table[index].dont_move || 
      page_table[index].gen != from_space) return;
  /* forwarding check.
     Since we are using this salvage process recursively, some pointer
     might have been forwarded already. */
  block = POINTER2BLOCK(obj);
  if (MEMORY_FORWARDED(block)) {
    *where = MEMORY_FORWARDED_VALUE(block);
    return;
  }

  if (SG_PAIRP(obj)) {
    salvage_list(where, obj);
  } else if (SG_HASHTABLE_P(obj)) {
    salvage_hashtable(where, SG_OBJ(obj));
  } else if (SG_STRINGP(obj)) {
    copy_root_object(where, obj, copy_large_unboxed_object);
  } else if (SG_SYMBOLP(obj)) {
    copy_root_object(where, obj, copy_object);
    salvage_scheme_pointer((void **)&SG_SYMBOL(obj)->name,
			   SG_SYMBOL(obj)->name);
  } else if (SG_KEYWORDP(obj)) {
    copy_root_object(where, obj, copy_object);
    salvage_scheme_pointer((void **)&SG_KEYWORD(obj)->name,
			   SG_KEYWORD(obj)->name);
  } else if (SG_IDENTIFIERP(obj)) {
    copy_root_object(where, obj, copy_object);
    salvage_scheme_pointer((void **)&SG_IDENTIFIER_LIBRARY(obj),
			   SG_IDENTIFIER_LIBRARY(obj));
    if (SG_PAIRP(SG_IDENTIFIER_ENVS(obj))) {
      salvage_scheme_pointer((void **)&SG_IDENTIFIER_ENVS(obj),
			     SG_IDENTIFIER_ENVS(obj));
    }
    salvage_scheme_pointer((void **)&SG_IDENTIFIER_NAME(obj),
			   SG_IDENTIFIER_NAME(obj));
  } else if (SG_VMP(obj)) {
    salvage_vm(where, obj);
  } else if (SG_LIBRARYP(obj)) {
    salvage_library(where, obj);
  } else if (SG_GLOCP(obj)) {
    SgGloc *gloc;
    copy_root_object(where, obj, copy_object);
    gloc = SG_GLOC(obj);
    salvage_scheme_pointer((void **)&gloc->name, gloc->name);
    salvage_scheme_pointer((void **)&gloc->library, gloc->library);
    salvage_scheme_pointer((void **)&SG_GLOC_GET(gloc), SG_GLOC_GET(gloc));
  } else if (SG_SUBRP(obj)) {
    copy_root_object(where, obj, copy_object);
    if (SG_SUBR(obj)->data) {
      scavenge_general_pointer(&SG_SUBR(obj)->data, SG_SUBR(obj)->data);
    }
  } else if (SG_CLOSUREP(obj)) {
    SgObject code;
    int freec, i;
    copy_root_object(where, obj, copy_object);
    code = SG_CLOSURE(obj)->code;
    freec = SG_CODE_BUILDER_FREEC(code);
    /* closure's free variables are not independent object thus it can't
       convert to block boundary. so we need to scavenge it manually here. */
    for (i = 0; i < freec; i++) {
      salvage_scheme_pointer(&SG_CLOSURE(obj)->frees[i],
			     SG_CLOSURE(obj)->frees[i]);
    }
    salvage_scheme_pointer(&SG_CLOSURE(obj)->code,
			   SG_CLOSURE(obj)->code);
  } else if (SG_CODE_BUILDERP(obj)) {
    /* TODO we need to salvage scheme objects in code */
    int size = SG_CODE_BUILDER(obj)->size, i;
    SgWord *code;
    copy_root_object(where, obj, copy_object);
    copy_root_object((void **)&SG_CODE_BUILDER(obj)->code,
		     SG_CODE_BUILDER(obj)->code, copy_large_object);
    code = SG_CODE_BUILDER(obj)->code;
    for (i = 0; i < size; i++) {
      InsnInfo *info = Sg_LookupInsnName(INSN(code[i]));
      if (info->argc) {
	SgObject obj = code[++i];
	salvage_scheme_pointer(code+i, obj);
      }
    }
    salvage_scheme_pointer(&SG_CODE_BUILDER_NAME(obj),
			   SG_CODE_BUILDER_NAME(obj));
    salvage_scheme_pointer(&SG_CODE_BUILDER_SRC(obj),
			   SG_CODE_BUILDER_SRC(obj));
  } else if (SG_VECTORP(obj)) {
    int i;
    copy_root_object(where, obj, copy_large_object);
    for (i = 0; i < SG_VECTOR_SIZE(obj);i ++) {
      salvage_scheme_pointer(&SG_VECTOR_ELEMENT(obj, i),
			     SG_VECTOR_ELEMENT(obj, i));
    }
  } else if (SG_PORTP(obj)) {
    salvage_port(where, SG_OBJ(obj));
  } else if (SG_SYNTAXP(obj)) {
    copy_root_object(where, obj, copy_object);
    salvage_scheme_pointer((void **)&SG_SYNTAX_NAME(obj), SG_SYNTAX_NAME(obj));
    salvage_scheme_pointer(&SG_SYNTAX_PROC(obj), SG_SYNTAX_PROC(obj));
  } else if (SG_MACROP(obj)) {
    SgMacro *m = SG_MACRO(obj);
    copy_root_object(where, obj, copy_object);
    salvage_scheme_pointer((void **)&m->name, m->name);
    salvage_scheme_pointer((void **)&m->transformer, m->transformer);
    salvage_scheme_pointer((void **)&m->env, m->env);
    salvage_scheme_pointer((void **)&m->maybeLibrary, m->maybeLibrary);
    salvage_scheme_pointer((void **)&m->extracted, m->extracted);
    /* data must be Scheme object, but I'm not sure */
    salvage_scheme_pointer((void **)&m->data, m->data);
  } else if (SG_TUPLEP(obj)) {
    copy_root_object(where, obj, copy_object);
    salvage_scheme_pointer((void **)&SG_TUPLE(obj)->values, 
			   SG_TUPLE(obj)->values);
    salvage_scheme_pointer((void **)&SG_TUPLE(obj)->printer, 
			   SG_TUPLE(obj)->printer);
  } else if (SG_TRANSCODERP(obj)) {
    copy_root_object(where, obj, copy_object);
    /* do we need to copy codec here? */
  } else if (SG_CODECP(obj)) {
    /* How should we treat? */
    copy_root_object(where, obj, copy_object);
  } else if (SG_FILEP(obj)) {
    copy_root_object(where, obj, copy_object);
  } else if (SG_SHAREDREF_P(obj)) {
    copy_root_object(where, obj, copy_object);
  } else {
    /* for now, need clever solution. */
    /* copy_root_object(where, obj, copy_object); */
    scavenge_general_pointer(where, obj);
    if (debug_flag) {
      fprintf(stderr, "we are missing the object %p\n", obj);
    }
  }
}

/* TODO should we move weak hashtable and how? */
static void scan_weak_hashtable(SgWeakHashTable *table)
{
  SgHashCore *core = SG_WEAK_HASHTABLE_CORE(table);
  SgHashIter itr;
  SgHashEntry *e;
  SgWeakness weakness = table->weakness;

  core->buckets = copy_large_object(core->buckets);
  Sg_HashIterInit(core, &itr);

  /* TODO how should we treet this? */
  preserve_pointer(core->data);
  salvage_scheme_pointer(&core->generalHasher, core->generalHasher);
  salvage_scheme_pointer(&core->generalCompare, core->generalCompare);
  while ((e = Sg_HashIterNext(&itr)) != NULL) {
    /* The entry pointer must be already preserved or if it doesn't
       then it's not on stack nor register so we don't have to preserve
       but copy. */
    page_index_t eindex = find_page_index(e);
    if (!page_table[eindex].dont_move) {
      /* not on stack so copy it */
      e = copy_object(e);
    }
    if (weakness & SG_WEAK_KEY) {
      if (!survived_gc_yet(SG_HASH_ENTRY_KEY(e))) {
	core->entryCount--;
	table->goneEntries++;
	e->key = table->defaultValue;
      } else {
	salvage_entry_key(e);
      }
    } else {
      /* if it's not weak salvage it */
      salvage_entry_key(e);
    }
    if (weakness & SG_WEAK_VALUE) {
      if (!survived_gc_yet(SG_HASH_ENTRY_VALUE(e))) {
	e->value = table->defaultValue;
      } else {
	salvage_entry_value(e);
      }
    } else {
      salvage_entry_value(e);
    }
  }

}

static void scan_weak_hashtables()
{
  SgWeakHashTable *table, *next;
  for (table = SG_WEAK_HASHTABLE(weak_hashtables); table; table = next) {
    next = table->next;
    table->next = NULL;
    scan_weak_hashtable(table);
  }
  /* reset */
  weak_hashtables = NULL;
}

static void scavenge_cont_frame_rec(SgVM *vm, void **where, SgContFrame *cont)
{
  volatile SgContFrame *c = cont;
  if (!cont->cl) return;
  if (!IN_STACK_P((void **)cont, vm)) {
    if (from_space_p(cont)) {
      block_t *block = POINTER2BLOCK(cont);
      if (MEMORY_FORWARDED(block)) {
	*where = MEMORY_FORWARDED_VALUE(block);
      } else {
	SgContFrame *new_cont = copy_object(cont);
	SET_MEMORY_FORWARDED(block, new_cont);
	/* moving program counter causes a lot of trouble so for now we
	   just mark it as preserved pointer. */
	preserve_pointer(new_cont->pc);
	salvage_scheme_pointer((void **)new_cont->cl, new_cont->cl);
	if (new_cont->size) {
	  if (new_cont->fp) {
	    /* if fp is not NULL and size is not 0 then it has some argument
	       frame so collect it. */
	    SgObject *f = (SgObject *)new_cont + CONT_FRAME_SIZE;
	    int i;
	    new_cont->env = f;
	    for (i = 0; i < new_cont->size; i++, f++) {
	      salvage_scheme_pointer(f, *f);
	    }
	  } else {
	    /* this is c cont frame and can contain opaque pointer, so do the
	       safer way. */
	    SgObject *f = (SgObject *)new_cont + CONT_FRAME_SIZE;
	    int i;
	    for (i = 0; i < new_cont->size; i++, f++) {
	      scavenge_general_pointer(f, *f);
	    }
	  }
	}
      }
    }
  }
  scavenge_cont_frame_rec(vm, (void **)&cont->prev, cont->prev);
}

static void scavenge_continuation_frame(SgVM *vm)
{
  scavenge_cont_frame_rec(vm, (void **)&vm->cont, vm->cont);
}
