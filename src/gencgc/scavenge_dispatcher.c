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
#include <sagittarius/generic.h>
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
#include <sagittarius/regex.h>
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
  /* the following code caused stack overflow. probably we don't need this. */
#if 0
  /* general pointer can contain pointers in it and if the nothing refers it
     but the given object it would be mis collected. To prevent it we need to
     check inside of the object. */
  if (!unboxedp) {
    /* unboxed object doesn't care the inside :-) */
    /* assume object is not on block boundary. see gencgc.c */
    block_t *block = POINTER2BLOCK(first);
    scavenge((intptr_t *)block, MEMORY_SIZE(block)/sizeof(void *));
  }
#endif
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
	if (z__ != obj) {					\
	  SET_MEMORY_FORWARDED(block__, z__);			\
	}							\
      }								\
      *(where) = z__;						\
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

static void salvage_hashtable(void **where, SgObject obj)
{
  SgHashCore *core;
  SgHashIter itr;
  SgHashEntry *e;
  int moved = FALSE;
  /* page_index_t index = find_page_index(obj); */
  copy_root_object(where, obj, copy_object);
  core = SG_HASHTABLE_CORE(obj);
  copy_root_object(&core->buckets, core->buckets, copy_large_object);
  /* scavenge_general_pointer(&core->buckets, core->buckets); */
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
      Sg_HashCoreReplaceEntry(core, e, z);
      e = z;
      moved = TRUE;
    }
    scavenge((intptr_t *)POINTER2BLOCK(e), 
	     MEMORY_SIZE(POINTER2BLOCK(e))/N_WORD_BYTES);
  }

  /* it's probably better to check if the real entry has been moved or not
     but for now.*/
  if (SG_HASHTABLE(obj)->type == SG_HASH_EQ && moved) {
    core->rehashNeeded = TRUE;
  }
}

static void salvage_port(void **where, SgObject obj)
{
#if 0
  SgBinaryPort *bp;
  SgTextualPort *tp;
#endif
  copy_root_object(where, obj, copy_object);
  /* FIXME, readtable can contain scheme pointer */
  if (SG_PORT(obj)->readtable) {
    SG_PORT(obj)->readtable = copy_object(SG_PORT(obj)->readtable);
  }
  salvage_scheme_pointer(&SG_PORT(obj)->reader, SG_PORT(obj)->reader);
  salvage_scheme_pointer(&SG_PORT(obj)->loadPath, SG_PORT(obj)->loadPath);
  salvage_scheme_pointer(&SG_PORT(obj)->previousPort,
			 SG_PORT(obj)->previousPort);
#if 0
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
#endif
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
  if (from_space_p(obj)) {
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

static void salvage_procedure(void **where, void *obj)
{
  copy_root_object(where, obj, copy_object);
  switch (SG_PROCEDURE_TYPE(obj)) {
  case SG_PROC_SUBR:
    if (SG_SUBR(obj)->data) {
      scavenge_general_pointer(&SG_SUBR(obj)->data, SG_SUBR(obj)->data);
    }
    break;
  case SG_PROC_CLOSURE: {
    SgObject code;
    int freec, i;
    code = SG_CLOSURE(obj)->code;
    freec = SG_CODE_BUILDER_FREEC(code);
    /* closure's free variables are not independent object thus it can't
       convert to block boundary. so we need to scavenge it manually here.
    */
    for (i = 0; i < freec; i++) {
      salvage_scheme_pointer(&SG_CLOSURE(obj)->frees[i],
			     SG_CLOSURE(obj)->frees[i]);
    }
    salvage_scheme_pointer(&SG_CLOSURE(obj)->code,
			   SG_CLOSURE(obj)->code);
  }
  case SG_PROC_GENERIC: {
    salvage_scheme_pointer(&SG_GENERIC(obj)->methods,
			   SG_GENERIC(obj)->methods);
    if (SG_GENERIC_DATA(obj)) {
      void *data = SG_GENERIC_DATA(obj);
      if (is_scheme_pointer(data)) {
	salvage_scheme_pointer(&SG_GENERIC(obj)->data, data);
      } else {
	scavenge_general_pointer(&SG_GENERIC(obj)->data, data);
      }
    }
    break;
  }
  default:
    break;
  }
}

static void salvage_string(void **where, void *obj)
{
  copy_root_object(where, obj, copy_large_unboxed_object);
}

static void salvage_symbol(void **where, void *obj)
{
  copy_root_object(where, obj, copy_object);
  salvage_scheme_pointer((void **)&SG_SYMBOL(obj)->name,
			 SG_SYMBOL(obj)->name);
}

static void salvage_keyword(void **where, void *obj)
{
  copy_root_object(where, obj, copy_object);
  salvage_scheme_pointer((void **)&SG_KEYWORD(obj)->name,
			 SG_KEYWORD(obj)->name);
}

static void salvage_identifier(void **where, void *obj)
{
  copy_root_object(where, obj, copy_object);
  salvage_scheme_pointer((void **)&SG_IDENTIFIER_LIBRARY(obj),
			 SG_IDENTIFIER_LIBRARY(obj));
  if (SG_PAIRP(SG_IDENTIFIER_ENVS(obj))) {
    salvage_scheme_pointer((void **)&SG_IDENTIFIER_ENVS(obj),
			   SG_IDENTIFIER_ENVS(obj));
  }
  salvage_scheme_pointer((void **)&SG_IDENTIFIER_NAME(obj),
			 SG_IDENTIFIER_NAME(obj));
}

static void salvage_gloc(void **where, void *obj)
{
  SgGloc *gloc;
  copy_root_object(where, obj, copy_object);
  gloc = SG_GLOC(obj);
  salvage_scheme_pointer((void **)&gloc->name, gloc->name);
  salvage_scheme_pointer((void **)&gloc->library, gloc->library);
  salvage_scheme_pointer((void **)&SG_GLOC_GET(gloc), SG_GLOC_GET(gloc));
}

static void salvage_code_builder(void **where, void *obj)
{
  int size = SG_CODE_BUILDER(obj)->size, i;
  SgWord *code = SG_CODE_BUILDER(obj)->code;
  copy_root_object(where, obj, copy_object);
  copy_root_object(&SG_CODE_BUILDER(obj)->code,
		   SG_CODE_BUILDER(obj)->code, copy_large_object);
  code = SG_CODE_BUILDER(obj)->code;
  for (i = 0; i < size; i++) {
    InsnInfo *info = Sg_LookupInsnName(INSN(code[i]));
    if (info->argc) {
      SgObject obj = SG_OBJ(code[++i]);
      salvage_scheme_pointer((void **)(code+i), obj);
    }
  }
  salvage_scheme_pointer(&SG_CODE_BUILDER_NAME(obj),
			 SG_CODE_BUILDER_NAME(obj));
  salvage_scheme_pointer(&SG_CODE_BUILDER_SRC(obj),
			 SG_CODE_BUILDER_SRC(obj));
}

static void salvage_vector(void **where, void *obj)
{
  int i;
  copy_root_object(where, obj, copy_large_object);
  for (i = 0; i < SG_VECTOR_SIZE(obj);i ++) {
    salvage_scheme_pointer(&SG_VECTOR_ELEMENT(obj, i),
			   SG_VECTOR_ELEMENT(obj, i));
  }
}

static void savlage_syntax(void **where, void *obj)
{
  copy_root_object(where, obj, copy_object);
  salvage_scheme_pointer((void **)&SG_SYNTAX_NAME(obj), SG_SYNTAX_NAME(obj));
  salvage_scheme_pointer(&SG_SYNTAX_PROC(obj), SG_SYNTAX_PROC(obj));
}

static void salvage_macro(void **where, void *obj)
{
  SgMacro *m = SG_MACRO(obj);
  copy_root_object(where, obj, copy_object);
  salvage_scheme_pointer((void **)&m->name, m->name);
  salvage_scheme_pointer((void **)&m->transformer, m->transformer);
  salvage_scheme_pointer((void **)&m->env, m->env);
  salvage_scheme_pointer((void **)&m->maybeLibrary, m->maybeLibrary);
  salvage_scheme_pointer((void **)&m->extracted, m->extracted);
  /* data must be Scheme object, but I'm not sure */
  salvage_scheme_pointer((void **)&m->data, m->data);
}

static void salvage_tuple(void **where, void *obj)
{
  copy_root_object(where, obj, copy_object);
  salvage_scheme_pointer((void **)&SG_TUPLE(obj)->values, 
			 SG_TUPLE(obj)->values);
  salvage_scheme_pointer((void **)&SG_TUPLE(obj)->printer, 
			 SG_TUPLE(obj)->printer);
}

/* Regular expression pattern contains REGEX VM's code
   so we need to resolve jump address here, bit pain in the ass though...
 */
#ifdef HAVE_ALLOCA
#define ALLOC_TEMP_BUFFER alloca
#define FREE_TEMP_BUFFER	/* nothing */
#else
#define ALLOC_TEMP_BUFFER malloc
#define FREE_TEMP_BUFFER  free
#endif
void salvage_regex_pattern(void **where, void *obj)
{
  int rootSize, i;
  inst_t *root, *save, *start;
  copy_root_object(where, obj, copy_object);
  salvage_scheme_pointer((void **)&SG_PATTERN(obj)->pattern, 
			 &SG_PATTERN(obj)->pattern);
  salvage_scheme_pointer((void **)&SG_PATTERN(obj)->ast, 
			 &SG_PATTERN(obj)->ast);

  /* here comes the painful part 
     we the following steps
     1. save the current root array into the stack.
     2. scavenge the root instruction.
     3. update the pointer offsets.
  */
  rootSize = SG_PATTERN(obj)->prog->rootLength;
  start = SG_PATTERN(obj)->prog->root;
  /* step 1 save */
  save = ALLOC_TEMP_BUFFER(sizeof(inst_t) * rootSize);
  memcpy(save, start, sizeof(inst_t) * rootSize);
  /* step 2 scavenge */
  scavenge_general_pointer((void **)&SG_PATTERN(obj)->prog->root, start);
  root = SG_PATTERN(obj)->prog->root;
  /* step 3 update */
  for (i = 0; i < rootSize; i++) {
    inst_t *inst = &root[i];
    int op = INST_OPCODE(inst), offset;
#define UPDATE(p)						\
    do {							\
      offset = (&(&save[i])->arg.pos)->p - start;		\
      (&(inst->arg.pos))->p = &root[offset];			\
    } while (0)

    switch (op) {
    case RX_SPLIT:
    case RX_BRANCH:
    case RX_BRANCHA:
      UPDATE(y);
    case RX_JMP:
    case RX_AHEAD:
    case RX_NAHEAD:
    case RX_BEHIND:
    case RX_NBEHIND:
    case RX_ONCE:
      UPDATE(x);
      break;
    default: break;
    }
  }
  FREE_TEMP_BUFFER(save);  
}

/* If where is NULL means, it's from the top most position.
   (obj should be pinned)
 */
typedef void (*scav_func)(SgObject z, void *data);
void salvage_scheme_pointer(void **where, void *obj)
{
  page_index_t index;
  block_t *block;
  
  if (!obj) {
    return;
  }

  /* might be static area */
  if (!search_dynamic_space(obj, TRUE)) return;

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

  /* OK for performance, we need to do some nasty stuff here. */
  /* if an object reaches here, it must be a heap object. so we can get
     its class. But make things easier and not to use magic number we check
     pair with defined macro. */
  if (SG_PAIRP(obj)) {
    salvage_list(where, obj);
  } else {
    SgClass *clazz = SG_CLASS_OF(obj);
    /* class itself might be GCable so we need to save it. */
    salvage_scheme_pointer((void **)&clazz, clazz);
    /* reset the class */
    SG_SET_CLASS(obj, clazz);
    if (clazz->scav_func) {
      scav_func fun = (scav_func)clazz->scav_func;
      fun(where, obj);
    } else {
      scavenge_general_pointer(where, obj);      
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

static void scavenge_bindings(SgObject binding_libraries)
{
  /* must be set but in case */
  if (binding_libraries) {
    SgHashIter itr;
    SgHashCore *core;
    SgHashEntry *e;
    core = SG_HASHTABLE_CORE(binding_libraries);
    Sg_HashIterInit(core, &itr);
    while ((e = Sg_HashIterNext(&itr)) != NULL) {
      /* the entry must be a library */
      /* We can't do keys here must be handled other place. */
#if 0
      SgObject lib = SG_HASH_ENTRY_VALUE(e);
      /* only binding table */
      salvage_hashtable((void **)&SG_LIBRARY_TABLE(lib), SG_LIBRARY_TABLE(lib));
#else
      salvage_scheme_pointer((void **)&e->value, e->value);
#endif

    }

  } else {
    Sg_Panic("Library list is not set!");
  }
}

/* collect top cont frame. */
static void scavenge_cont_frame_rec(SgVM *vm, void **where, SgContFrame *cont)
{
  /* if we move the pointer, then scavenge handle the rest. */
#if 0
  if (!cont->cl) return;
#endif
  /* we might need to follow the frame until we hit the heap located one. */
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
	  /* if fp is not NULL and size is not 0 then it has some argument
	     frame so collect it. */
	  SgObject *f = new_cont->env;
	  int i;
	  for (i = 0; i < new_cont->size; i++, f++) {
	    if (new_cont->fp) {
	      salvage_scheme_pointer(f, *f);
	    } else {
	      scavenge_general_pointer(f, *f);
	    }
	  }
	}
      }
    }
  }
#if 0
  scavenge_cont_frame_rec(vm, (void **)&cont->prev, cont->prev);
#endif
}

static void scavenge_continuation_frame(SgVM *vm)
{
  scavenge_cont_frame_rec(vm, (void **)&vm->cont, vm->cont);
}

static void init_scav_fun()
{
#define SET_CLASS_SCAV(cls, proc)		\
  do {						\
    SgClass *clz = SG_CLASS_##cls;		\
    clz->scav_func = (void *)proc;		\
  } while (0)

  SET_CLASS_SCAV(HASHTABLE, salvage_hashtable);
  SET_CLASS_SCAV(STRING, salvage_string);
  SET_CLASS_SCAV(SYMBOL, salvage_symbol);
  SET_CLASS_SCAV(KEYWORD, salvage_keyword);
  SET_CLASS_SCAV(IDENTIFIER, salvage_identifier);
  SET_CLASS_SCAV(VM, salvage_vm);
  SET_CLASS_SCAV(LIBRARY, salvage_library);
  SET_CLASS_SCAV(GLOC, salvage_gloc);
  SET_CLASS_SCAV(PROCEDURE, salvage_procedure);
  SET_CLASS_SCAV(CODE_BUILDER, salvage_code_builder);
  SET_CLASS_SCAV(VECTOR, salvage_vector);
  SET_CLASS_SCAV(PORT, salvage_port);
  SET_CLASS_SCAV(SYNTAX, savlage_syntax);
  SET_CLASS_SCAV(MACRO, salvage_macro);
  SET_CLASS_SCAV(TUPLE, salvage_tuple);
  SET_CLASS_SCAV(PATTERN, salvage_regex_pattern);
}
