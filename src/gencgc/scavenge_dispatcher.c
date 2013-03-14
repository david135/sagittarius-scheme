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

#include <sagittarius/hashtable.h>
#include <sagittarius/port.h>
#include <sagittarius/file.h>

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

/*
  dispatch scavenge process to proper procedures.
  the block contains Scheme object so that we can see what we need to
  move.
 */
static int dispatch_pointer(void **where, block_t *block)
{
  SgObject obj = SG_OBJ(block->body);
  if (SG_HASHTABLE_P(obj)) {
    scav_hashtable(where, obj);
  }
  return MEMORY_SIZE(block);
}

/*
  scavenge general pointer.
  We need to check if the target pointer contains pointer.
 */
static int scavenge_general_pointer(void **where, block_t *block)
{
  int unboxedp = page_unboxed_p(find_page_index((void *)block));
  if (unboxedp) {
    /* OK block doesn't contain any pointer so we can simply remove it */
    return MEMORY_SIZE(block);
  } else {
    /* now, we might have Scheme pair, so how should we detect it? */
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
    preserve_pointer(SG_HASH_ENTRY_KEY(e));
    preserve_pointer(SG_HASH_ENTRY_VALUE(e));
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
      if (SG_INPORTP(obj)) {
	byte_buffer *p;
	for (p = bp->src.obuf.start; p != bp->src.obuf.current; p++) 
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
      if (SG_INPORTP(obj)) {
	char_buffer *p;
	for (p = tp->src.ostr.start; p != tp->src.ostr.current; p++) 
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
  if (!obj) {
    return;
  } if (SG_HASHTABLE_P(obj)) {
    preserve_hashtable(SG_OBJ(obj));
  } else if (SG_PORTP(obj)) {
    preserve_port(SG_OBJ(obj));
  } else if (SG_FILEP(obj)) {
    preserve_pointer(SG_FILE(obj)->name);
  }
}
