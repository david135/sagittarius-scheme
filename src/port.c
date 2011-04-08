// -*- C -*-
/*
 * port.c
 *
 *   Copyright (c) 2010  Takashi Kato <ktakashi@ymail.com>
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
#include <string.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/port.h"
#include "sagittarius/core.h"
#include "sagittarius/weak.h"
#include "sagittarius/bytevector.h"
#include "sagittarius/file.h"
#include "sagittarius/transcoder.h"
#include "sagittarius/string.h"
#include "sagittarius/error.h"
#include "sagittarius/vector.h"
#include "sagittarius/vm.h"

#define PORT_DEFAULT_BUF_SIZE 8196

static void port_cleanup(SgPort *port)
{
  if (port->closed) return;
  switch (port->type) {
  case SG_BINARY_PORT_TYPE:
    if (SG_BINARY_PORT(port)->type == SG_FILE_BINARY_PORT_TYPE) {
      /* file needs to be closes */
      if (port->direction == SG_OUTPUT_PORT ||
	  port->direction == SG_IN_OUT_PORT) {
	port->flush(port);
      }
      port->close(port);
    }
    break;
  case SG_CUSTOM_PORT_TYPE:
    /* TODO */
    break;
  default:
    break;
  }
  port->closed = TRUE;
  Sg_UnregisterFinalizer(SG_OBJ(port));
}

static void port_finalize(SgObject obj, void *data)
{
  port_cleanup(SG_PORT(obj));
}

static SgPort* make_port(enum SgPortDirection d, enum SgPortType t, enum SgBufferMode m)
{
  SgPort *z = SG_NEW(SgPort);
  SG_SET_HEADER(z, TC_PORT);
  z->direction = d;
  z->type = t;
  z->bufferMode = m;
  /* we only register binary and custom ports to finalizer.
     other has only on memory buffer.
   */
  switch (t) {
  case SG_BINARY_PORT_TYPE:
  case SG_CUSTOM_PORT_TYPE:
    Sg_RegisterFinalizer(SG_OBJ(z), port_finalize, NULL);
    break;
  default:
    break;
  }
  return z;
}

static SgBinaryPort* make_binary_port(enum SgBinaryPortType t)
{
  SgBinaryPort *z = SG_NEW(SgBinaryPort);
  z->type = t;
  z->buffer = NULL;
  z->bufferSize = 0;
  z->bufferIndex = 0;
  z->position = 0;
  z->dirty = FALSE;
  return z;
}

static SgTextualPort* make_textual_port(enum SgTextualPortType t)
{
  SgTextualPort *z = SG_NEW(SgTextualPort);
  z->type = t;
  return z;
}

/* from Gauche */
/* Tracking buffered ports */
#define PORT_VECTOR_SIZE 256
static struct {
  int dummy;
  SgWeakVector *ports;
  /* TODO mutex */
} active_buffered_ports = { 1, NULL };

#define PORT_HASH(port)  \
  ((((SG_WORD(port)>>3) * 2654435761UL)>>16) % PORT_VECTOR_SIZE)


static void register_buffered_port(SgPort *port)
{
  int i, h, c;
  int tried_gc = FALSE;
  int need_gc = FALSE;

 retry:
  h = i = (int)PORT_HASH(port);
  c = 0;
  /* TODO lock */
  while (!SG_FALSEP(Sg_WeakVectorRef(active_buffered_ports.ports,
				     i, SG_FALSE))) {
    i -= ++c; while (i < 0) i += PORT_VECTOR_SIZE;
    if (i == h) {
      /* Vector entry is full. We run global GC to try to collect
	 unused entry. */
      need_gc = TRUE;
      break;
    }
  }
  if (!need_gc) {
    Sg_WeakVectorSet(active_buffered_ports.ports, i, SG_OBJ(port));
  }

  if (need_gc) {
    if (tried_gc) {
      Sg_Panic("active buffered port table overflow.");
    } else {
      Sg_GC();
      tried_gc = TRUE;
      need_gc = FALSE;
      goto retry;
    }
  }
}

static void unregister_buffered_port(SgPort *port)
{
  int i, h, c;
  SgObject p;

  h = i = (int)PORT_HASH(port);
  c = 0;
  /* TODO lock */
  do {
    p = Sg_WeakVectorRef(active_buffered_ports.ports, i, SG_FALSE);
    if (!SG_FALSEP(p) && SG_EQ(SG_OBJ(port), p)) {
      Sg_WeakVectorSet(active_buffered_ports.ports, i, SG_FALSE);
      break;
    }
    i -= ++c; while (i < 0) i += PORT_VECTOR_SIZE;
  } while (i != h);
}

/*
  TODO: set finalizer for all ports.
  for now i don't close any port when gc is run.

  TODO: refactering.
 */

#define SG_PORT_FILE(p) SG_BINARY_PORT(p)->src.file

static int file_open(SgObject self)
{
  return SG_PORT_FILE(self)->isOpen(SG_PORT_FILE(self));
}

static int file_close(SgObject self)
{
  if (!SG_PORT(self)->closed) {
    SG_PORT(self)->closed = TRUE;
    if (SG_PORT(self)->direction == SG_OUTPUT_PORT ||
	SG_PORT(self)->direction == SG_IN_OUT_PORT) {
      /* flush */
      SG_PORT(self)->flush(self);
      unregister_buffered_port(SG_PORT(self));
    }
    SG_PORT_FILE(self)->close(SG_PORT_FILE(self));
  }
  return SG_PORT(self)->closed;
}

static void file_flush_internal(SgObject self)
{
  uint8_t *buf = SG_BINARY_PORT(self)->buffer;
  while (SG_BINARY_PORT(self)->bufferIndex > 0) {
    int64_t written_size = SG_PORT_FILE(self)->write(SG_PORT_FILE(self),
						     buf,
						     SG_BINARY_PORT(self)->bufferIndex);
    buf += written_size;
    SG_BINARY_PORT(self)->bufferIndex -= written_size;
    ASSERT(SG_BINARY_PORT(self)->bufferIndex >= 0);
  }
  ASSERT(SG_BINARY_PORT(self)->bufferIndex == 0);
  SG_BINARY_PORT(self)->bufferIndex = 0;
  SG_BINARY_PORT(self)->bufferSize = 0;
}

static void file_flush(SgObject self)
{
  if (SG_BINARY_PORT(self)->buffer) {
    SG_PORT_FILE(self)->seek(SG_PORT_FILE(self),
			     SG_BINARY_PORT(self)->position - SG_BINARY_PORT(self)->bufferIndex,
			     SG_BEGIN);
    file_flush_internal(self);
  }
}

static void file_fill_buffer(SgObject self)
{
  int64_t read_size = 0;
  if (SG_BINARY_PORT(self)->dirty &&
      SG_PORT(self)->direction == SG_IN_OUT_PORT) {
    file_flush(self);
  }
  while (read_size < PORT_DEFAULT_BUF_SIZE) {
    int64_t result = SG_PORT_FILE(self)->read(SG_PORT_FILE(self),
					      SG_BINARY_PORT(self)->buffer + read_size,
					      PORT_DEFAULT_BUF_SIZE - read_size);
    ASSERT(result >= 0);	/* file raises error */
    if (result == 0) {
      break;			/* EOF */
    } else {
      read_size += result;
    }
  }
  SG_BINARY_PORT(self)->bufferSize = read_size;
  SG_BINARY_PORT(self)->bufferIndex = 0;
}

/* To use this both input and input/output port, this does not change position */
static int64_t file_read_from_buffer(SgObject self, uint8_t *dest, int64_t req_size)
{
  int64_t opos = SG_PORT_FILE(self)->seek(SG_PORT_FILE(self), 0, SG_CURRENT);
  int64_t read_size = 0;
  int need_unwind = FALSE;

  while (read_size < req_size) {
    int64_t buf_diff = SG_BINARY_PORT(self)->bufferSize - SG_BINARY_PORT(self)->bufferIndex;
    int64_t size_diff = req_size - read_size;
    ASSERT(SG_BINARY_PORT(self)->bufferSize >= SG_BINARY_PORT(self)->bufferIndex);
    if (buf_diff >= size_diff) {
      memcpy(dest + read_size,
	     SG_BINARY_PORT(self)->buffer + SG_BINARY_PORT(self)->bufferIndex,
	     size_diff);
      SG_BINARY_PORT(self)->bufferIndex += size_diff;
      read_size += size_diff;
      break;
    } else {
      memcpy(dest + read_size,
	     SG_BINARY_PORT(self)->buffer + SG_BINARY_PORT(self)->bufferIndex,
	     buf_diff);
      read_size += buf_diff;
      file_fill_buffer(self);
      need_unwind = TRUE;
      if (SG_BINARY_PORT(self)->bufferSize == 0) {
	/* EOF */
	break;
      }
    }
  }
  if (need_unwind && SG_PORT(self)->direction == SG_IN_OUT_PORT) {
    SG_PORT_FILE(self)->seek(SG_PORT_FILE(self), opos, SG_BEGIN);
  }
  return read_size;
}

static void file_forward_position(SgObject self, int64_t offset)
{
  SG_BINARY_PORT(self)->position += offset;
}

static int file_get_u8(SgObject self)
{
  uint8_t buf;
  int64_t result;
  if (SG_BINARY_PORT(self)->buffer) {
    result = file_read_from_buffer(self, &buf, 1);
  } else {
    if (SG_PORT_HAS_U8_AHEAD(self)) {
      buf = SG_PORT_U8_AHEAD(self);
      SG_PORT_U8_AHEAD(self) = EOF;
    } else {
      result = SG_PORT_FILE(self)->read(SG_PORT_FILE(self), &buf, 1);
    }
  }
  if (result == 0) {
    return EOF;
  }
  file_forward_position(self, 1);
  return buf;
}

static int file_look_ahead_u8(SgObject self)
{
  uint8_t buf;
  int64_t result;
  if (SG_BINARY_PORT(self)->buffer) {
    result = file_read_from_buffer(self, &buf, 1);
    SG_BINARY_PORT(self)->bufferIndex--;
  } else {
    if (SG_PORT_HAS_U8_AHEAD(self)) {
      return SG_PORT_U8_AHEAD(self);
    } else {
      result = SG_PORT_FILE(self)->read(SG_PORT_FILE(self), &buf, 1);
      SG_PORT_U8_AHEAD(self) = (result == 0) ? EOF : buf;
    }
  }
  if (result == 0) {
    return EOF;
  }
  return buf;
}

static int64_t file_read_u8(SgObject self, uint8_t *buf, int64_t size)
{
  int64_t result;
  if (SG_BINARY_PORT(self)->buffer) {
    result = file_read_from_buffer(self, buf, size);
  } else {
    int offset = 0;
    if (SG_PORT_HAS_U8_AHEAD(self)) {
      buf[0] = SG_PORT_U8_AHEAD(self);
      SG_PORT_U8_AHEAD(self) = EOF;
      offset++;
    }
    result = SG_PORT_FILE(self)->read(SG_PORT_FILE(self), buf + offset, size - offset);
    /* we also need to add offset to forward position. */
    result += offset;
  }
  file_forward_position(self, result);
  return result;
}

static int64_t file_read_u8_all(SgObject self, uint8_t **buf)
{
  int64_t rest_size, result;
  uint8_t *dest;

  rest_size = SG_PORT_FILE(self)->size(SG_PORT_FILE(self)) - SG_BINARY_PORT(self)->position;
  if (rest_size == 0) return 0;

  dest = SG_NEW_ATOMIC2(uint8_t *, rest_size);
  *buf = dest;
  if (SG_BINARY_PORT(self)->buffer) {
    result = file_read_from_buffer(self, dest, rest_size);
  } else {
    int offset = 0;
    if (SG_PORT_HAS_U8_AHEAD(self)) {
      dest[0] = SG_PORT_U8_AHEAD(self);
      SG_PORT_U8_AHEAD(self) = EOF;
      offset++;
    }
    result = SG_PORT_FILE(self)->read(SG_PORT_FILE(self), dest + offset, rest_size - offset);
    result += offset;
  }
  file_forward_position(self, result);
  return 0;
}

SgObject Sg_MakeFileBinaryInputPort(SgFile *file, int bufferMode)
{
  /* TODO is buffer mode correct? */
  SgPort *z = make_port(SG_INPUT_PORT, SG_BINARY_PORT_TYPE, bufferMode);
  SgBinaryPort *b = make_binary_port(SG_FILE_BINARY_PORT_TYPE);
  /* file must be opened before this method is called. */
  ASSERT(file->isOpen(file));

  z->closed = FALSE;
  z->flush = NULL;
  z->close = file_close;
  /* set binary input port */
  z->impl.bport = b;
  /* initialize binary input port */
  b->src.file = file;
  b->open = file_open;
  b->getU8 = file_get_u8;
  b->lookAheadU8 = file_look_ahead_u8;
  b->readU8 = file_read_u8;
  b->readU8All = file_read_u8_all;
  b->putU8 = NULL;
  b->putU8Array = NULL;
  b->bufferWriter = NULL;
  if (bufferMode != SG_BUFMODE_NONE) {
    b->buffer = SG_NEW_ATOMIC2(uint8_t *, PORT_DEFAULT_BUF_SIZE);
  } else {
    /* reset ahead u8 */
    SG_PORT_U8_AHEAD(z) = EOF;
  }
  return SG_OBJ(z);
}

static int64_t file_write_to_block_buffer(SgObject self, uint8_t *v, int64_t req_size)
{
  int64_t write_size = 0;
  int64_t opos = SG_PORT_FILE(self)->seek(SG_PORT_FILE(self), 0, SG_CURRENT);
  int need_unwind = FALSE;

  if (req_size > 0) {
    SG_BINARY_PORT(self)->dirty = TRUE;
  }
  while (write_size < req_size) {
    int64_t buf_diff =  PORT_DEFAULT_BUF_SIZE - SG_BINARY_PORT(self)->bufferIndex;
    int64_t size_diff = req_size - write_size;
    ASSERT(buf_diff >= 0);
    ASSERT(req_size > write_size);
    if (buf_diff >= size_diff) {
      memcpy(SG_BINARY_PORT(self)->buffer + SG_BINARY_PORT(self)->bufferIndex,
	     v + write_size, size_diff);
      SG_BINARY_PORT(self)->bufferIndex += size_diff;
      write_size += size_diff;
    } else {
      memcpy(SG_BINARY_PORT(self)->buffer + SG_BINARY_PORT(self)->bufferIndex,
	     v + write_size, buf_diff);
      SG_BINARY_PORT(self)->bufferIndex += buf_diff;
      write_size += size_diff;
      file_flush_internal(self);
      need_unwind = TRUE;
    }
  }
  if (need_unwind && SG_PORT(self)->direction == SG_IN_OUT_PORT) {
    SG_PORT_FILE(self)->seek(SG_PORT_FILE(self), opos, SG_BEGIN);
  }
  return write_size;
}

static int64_t file_write_to_line_buffer(SgObject self, uint8_t *v, int64_t req_size)
{
  int64_t write_size = 0;
  int64_t opos = SG_PORT_FILE(self)->seek(SG_PORT_FILE(self), 0, SG_CURRENT);
  int need_unwind = FALSE;

  if (req_size > 0) {
    SG_BINARY_PORT(self)->dirty = TRUE;
  }
  while (write_size < req_size) {
    int64_t buf_diff =  PORT_DEFAULT_BUF_SIZE - SG_BINARY_PORT(self)->bufferIndex;
    if (buf_diff == 0) {
      file_flush_internal(self);
      need_unwind = TRUE;
    }
    *(SG_BINARY_PORT(self)->buffer + SG_BINARY_PORT(self)->bufferIndex) = *(v + write_size);
    SG_BINARY_PORT(self)->bufferIndex++;
    write_size++;
    if (SG_BINARY_PORT(self)->buffer[SG_BINARY_PORT(self)->bufferIndex - 1] == '\n') {
      file_flush_internal(self);
      need_unwind = TRUE;
    }
  }
  if (need_unwind && SG_PORT(self)->direction == SG_IN_OUT_PORT) {
    SG_PORT_FILE(self)->seek(SG_PORT_FILE(self), opos, SG_BEGIN);
  }
  return write_size;
}

static int64_t file_put_u8_array(SgObject self, uint8_t *v, int64_t size)
{
  if (SG_BINARY_PORT(self)->buffer) {
    int64_t written_size = SG_BINARY_PORT(self)->bufferWriter(self, v, size);
    file_forward_position(self, written_size);
    return written_size;
  } else {
    return SG_PORT_FILE(self)->write(SG_PORT_FILE(self), v, size);
  }
}


static int64_t file_put_u8(SgObject self, uint8_t v)
{
  return file_put_u8_array(self, &v, 1);
}

SgObject Sg_MakeFileBinaryOutputPort(SgFile *file, int bufferMode)
{
  SgPort *z = make_port(SG_OUTPUT_PORT, SG_BINARY_PORT_TYPE, bufferMode);
  SgBinaryPort *b = make_binary_port(SG_FILE_BINARY_PORT_TYPE);
  /* file must be opened before this method is called. */
  ASSERT(file->isOpen(file));

  z->closed = FALSE;
  z->flush = file_flush;
  z->close = file_close;

  b->src.file = file;
  b->open = file_open;
  b->getU8 = NULL;
  b->lookAheadU8 = NULL;
  b->readU8 = NULL;
  b->readU8All = NULL;
  b->putU8 = file_put_u8;
  b->putU8Array = file_put_u8_array;
  if (bufferMode != SG_BUFMODE_NONE) {
    b->buffer = SG_NEW_ATOMIC2(uint8_t *, PORT_DEFAULT_BUF_SIZE);
    b->bufferWriter = (bufferMode == SG_BUFMODE_BLOCK) ? file_write_to_block_buffer
                                                       : file_write_to_line_buffer;
    register_buffered_port(z);
  } else {
    b->bufferWriter = NULL;
  }

  z->impl.bport = b;
  return SG_OBJ(z);
}

/* input/output port
   this port is just combination of in and out port.
 */
SgObject Sg_MakeFileBinaryInputOutputPort(SgFile *file, int bufferMode)
{
  SgPort *z = make_port(SG_IN_OUT_PORT, SG_BINARY_PORT_TYPE, bufferMode);
  SgBinaryPort *b = make_binary_port(SG_FILE_BINARY_PORT_TYPE);
  /* file must be opened before this method is called. */
  ASSERT(file->isOpen(file));

  z->closed = FALSE;
  z->flush = file_flush;
  z->close = file_close;
  z->impl.bport = b;

  b->src.file = file;
  b->open = file_open;
  b->getU8 = file_get_u8;
  b->lookAheadU8 = file_look_ahead_u8;
  b->readU8 = file_read_u8;
  b->readU8All = file_read_u8_all;
  b->putU8 = file_put_u8;
  b->putU8Array = file_put_u8_array;
  if (bufferMode != SG_BUFMODE_NONE) {
    b->buffer = SG_NEW_ATOMIC2(uint8_t *, PORT_DEFAULT_BUF_SIZE);
    b->bufferWriter = (bufferMode == SG_BUFMODE_BLOCK) ? file_write_to_block_buffer
                                                       : file_write_to_line_buffer;
    register_buffered_port(z);
  } else {
    b->bufferWriter = NULL;
    SG_PORT_U8_AHEAD(z) = EOF;
  }

  return SG_OBJ(z);
}

/*****
   ByteArray port
 */
static int byte_array_close(SgObject self)
{
  SG_PORT(self)->closed = TRUE;
  return TRUE;
}

static int byte_array_open(SgObject self)
{
  return TRUE;
}

static int byte_array_get_u8(SgObject self)
{
  int index = SG_BINARY_PORT(self)->src.buffer.index;
  int size =  SG_BVECTOR_SIZE(SG_BINARY_PORT(self)->src.buffer.bvec);
  if (index >= size) return EOF;
  return Sg_ByteVectorU8Ref(SG_BINARY_PORT(self)->src.buffer.bvec,
			    SG_BINARY_PORT(self)->src.buffer.index++);
}

static int byte_array_look_ahead_u8(SgObject self)
{
  int index = SG_BINARY_PORT(self)->src.buffer.index;
  int size =  SG_BVECTOR_SIZE(SG_BINARY_PORT(self)->src.buffer.bvec);
  if (index >= size) return EOF;
  return Sg_ByteVectorU8Ref(SG_BINARY_PORT(self)->src.buffer.bvec,
			    SG_BINARY_PORT(self)->src.buffer.index);
}

static int64_t byte_array_read_u8(SgObject self, uint8_t *buf, int64_t size)
{
  SgBinaryPort *bp = SG_BINARY_PORT(self);
  SgByteVector *bvec = bp->src.buffer.bvec;
  int bsize = SG_BVECTOR_SIZE(bvec);
  int bindex = bp->src.buffer.index;
  int rest = bsize - bindex;
  int i, read_size = (rest >= size) ? size : rest;

  for (i = 0; i < read_size; i++) {
    buf[i] = Sg_ByteVectorU8Ref(bvec, bindex + i);
  }
  SG_BINARY_PORT(self)->src.buffer.index += size;
  return read_size;
}

static int64_t byte_array_read_u8_all(SgObject self, uint8_t **buf)
{
  SgByteVector *bvec = SG_BINARY_PORT(self)->src.buffer.bvec;
  int bsize = SG_BVECTOR_SIZE(bvec);
  int bindex = SG_BINARY_PORT(self)->src.buffer.index;
  int rest_size = bsize - bindex, i;

  *buf = SG_NEW_ATOMIC2(uint8_t *, rest_size);
  *buf = SG_BVECTOR_ELEMENTS(bvec);
  
  return byte_array_read_u8(self, *buf, rest_size);
}

SgObject Sg_MakeByteVectorInputPort(SgByteVector *bv, int offset)
{
  /* TODO is buffer mode correct? */
  SgPort *z = make_port(SG_INPUT_PORT, SG_BINARY_PORT_TYPE, SG_BUFMODE_NONE);
  SgBinaryPort *b = make_binary_port(SG_BYTE_ARRAY_BINARY_PORT_TYPE);

  z->closed = FALSE;
  z->flush = NULL;
  z->close = byte_array_close;
  /* initialize binary input port */
  b->src.buffer.bvec = bv;
  b->src.buffer.index = offset;
  b->open = byte_array_open;
  b->getU8 = byte_array_get_u8;
  b->lookAheadU8 = byte_array_look_ahead_u8;
  b->readU8 = byte_array_read_u8;
  b->readU8All = byte_array_read_u8_all;
  b->putU8 = NULL;
  b->putU8Array = NULL;
  /* set binary input port */
  z->impl.bport = b;
  return SG_OBJ(z);
}

SgObject Sg_MakeByteArrayInputPort(const uint8_t *src, int64_t size)
{
  /* TODO is buffer mode correct? */
  SgPort *z = make_port(SG_INPUT_PORT, SG_BINARY_PORT_TYPE, SG_BUFMODE_NONE);
  SgBinaryPort *b = make_binary_port(SG_BYTE_ARRAY_BINARY_PORT_TYPE);

  z->closed = FALSE;
  z->flush = NULL;
  z->close = byte_array_close;
  /* initialize binary input port */
  b->src.buffer.bvec = SG_BVECTOR(Sg_MakeByteVectorFromU8Array(src, size));
  b->src.buffer.index = 0;
  b->open = byte_array_open;
  b->getU8 = byte_array_get_u8;
  b->lookAheadU8 = byte_array_look_ahead_u8;
  b->readU8 = byte_array_read_u8;
  b->readU8All = byte_array_read_u8_all;
  b->putU8 = NULL;
  b->putU8Array = NULL;
  /* set binary input port */
  z->impl.bport = b;
  return SG_OBJ(z);
}

#define DEFAULT_BUFFER_SIZE        256
#define INCREASE_BUFFER_SIZE       32

static int obyte_array_close(SgObject self)
{
  SG_PORT(self)->closed = TRUE;
  /* gc friendliness */
  SG_BINARY_PORT(self)->src.buffer.bvec = NULL;
  return TRUE;
}

static int64_t put_byte_array_u8_array(SgObject self, uint8_t *buf, int64_t size)
{
  SgBinaryPort *bp = SG_BINARY_PORT(self);
  SgByteVector *bvec = bp->src.buffer.bvec;
  int current_size = SG_BVECTOR_SIZE(bvec);
  int current_index = bp->src.buffer.index;
  int i;

  if (current_index + size >= current_size) {
    /*
      This may be too much but if it's overflowing now, next time will also
      overflow. Why not allocate bigger to avoid memory allocating?
     */
    int new_size = current_size + size + INCREASE_BUFFER_SIZE;
    SgByteVector *tmp = Sg_MakeByteVector(new_size, 0);
    Sg_ByteVectorCopyX(bvec, 0, tmp, 0, current_index);
    bp->src.buffer.bvec = tmp;
    bvec = tmp;			/* for convenience */
  }
  for (i = 0; i < size; i++) {
    Sg_ByteVectorU8Set(bvec, current_index + i, buf[i]);
  }
  bp->src.buffer.index += size;
  return size;
}

static int64_t put_byte_array_u8(SgObject self, uint8_t b)
{
  return put_byte_array_u8_array(self, &b, 1);
}

SgObject Sg_MakeByteArrayOutputPort(int size)
{
  SgPort *z = make_port(SG_OUTPUT_PORT, SG_BINARY_PORT_TYPE, SG_BUFMODE_NONE);
  SgBinaryPort *b = make_binary_port(SG_BYTE_ARRAY_BINARY_PORT_TYPE);

  uint8_t *buffer;
  int actual_size = (size > 0) ? size : DEFAULT_BUFFER_SIZE;

  z->closed = FALSE;
  z->flush = NULL;
  z->close = obyte_array_close;
  /* initialize binary output port */
  b->src.buffer.bvec = SG_BVECTOR(Sg_MakeByteVector(actual_size, 0));
  b->src.buffer.index = 0;
  b->open = byte_array_open;
  b->getU8 = NULL;
  b->lookAheadU8 = NULL;
  b->readU8 = NULL;
  b->readU8All = NULL;
  b->putU8 = put_byte_array_u8;
  b->putU8Array = put_byte_array_u8_array;
  /* set binary input port */
  z->impl.bport = b;
  return SG_OBJ(z);
}

/*
  This function always return new allocated byte array.
 */
uint8_t* Sg_GetByteArrayFromBinaryPort(SgPort *port)
{
  SgBinaryPort *bp = SG_BINARY_PORT(port);
  uint8_t *r;
  if (bp->type != SG_BYTE_ARRAY_BINARY_PORT_TYPE) {
    Sg_Error(UC("byte array port required"));
  }

  r = SG_NEW2(uint8_t*, sizeof(uint8_t) * bp->src.buffer.index);
  memcpy(r, SG_BVECTOR_ELEMENTS(bp->src.buffer.bvec), bp->src.buffer.index);
  return r;
}


/*****
   Transcoded port
 */

/* look ahead char is common for all textual ports */
static SgChar lookAheadChar(SgObject self)
{
  SgChar c = SG_TEXTUAL_PORT(self)->getChar(self);
  if (c != EOF) {
    SG_TEXTUAL_PORT(self)->unGetChar(self, c);
  }
  return c;
}

/* useful macro for transcoded port */
#define SG_TPORT_TRANSCODER(obj) (SG_TEXTUAL_PORT(obj)->src.transcoded.transcoder)
#define SG_TPORT_PORT(obj)       (SG_TEXTUAL_PORT(obj)->src.transcoded.port)

static int transGetLineNo(SgObject self)
{
  return SG_TPORT_TRANSCODER(self)->lineNo;
}

static SgChar transGetChar(SgObject self)
{
  return SG_TPORT_TRANSCODER(self)->getChar(SG_TPORT_TRANSCODER(self), SG_TPORT_PORT(self));
}

static void transUnGetChar(SgObject self, SgChar c)
{
  return SG_TPORT_TRANSCODER(self)->unGetChar(SG_TPORT_TRANSCODER(self), c);
}

static int transClose(SgObject self)
{
  SG_PORT(self)->closed = TRUE;
  ASSERT(SG_TPORT_PORT(self) != NULL);
  return SG_TPORT_PORT(self)->close(SG_TPORT_PORT(self));
}

SgObject Sg_MakeTranscodedInputPort(SgPort *port, SgTranscoder *transcoder)
{
  SgPort *z = make_port(SG_INPUT_PORT, SG_TEXTUAL_PORT_TYPE, -1);
  SgTextualPort *t = make_textual_port(SG_TRANSCODED_TEXTUAL_PORT_TYPE);

  z->closed = FALSE;
  z->flush = NULL;
  z->close = transClose;

  t->src.transcoded.transcoder = transcoder;
  t->src.transcoded.port = port;
  t->getChar = transGetChar;
  t->unGetChar = transUnGetChar;
  t->getLineNo = transGetLineNo;
  t->lookAheadChar = lookAheadChar;
  t->putChar = NULL;

  z->impl.tport = t;
  return SG_OBJ(z);
}


static void transPutChar(SgObject self, SgChar c)
{
  SG_TPORT_TRANSCODER(self)->putChar(SG_TPORT_TRANSCODER(self),
				     SG_TPORT_PORT(self),
				     c);
}

static void transFlush(SgObject self)
{
  SG_TPORT_PORT(self)->flush(SG_TPORT_PORT(self));
}

SgObject Sg_MakeTranscodedOutputPort(SgPort *port, SgTranscoder *transcoder)
{
  SgPort *z = make_port(SG_OUTPUT_PORT, SG_TEXTUAL_PORT_TYPE, -1);
  SgTextualPort *t = make_textual_port(SG_TRANSCODED_TEXTUAL_PORT_TYPE);

  z->closed = FALSE;
  z->flush = transFlush;
  z->close = transClose;

  t->src.transcoded.transcoder = transcoder;
  t->src.transcoded.port = port;
  t->getChar = NULL;
  t->unGetChar = NULL;
  t->getLineNo = NULL;
  t->lookAheadChar = NULL;
  t->putChar = transPutChar;

  z->impl.tport = t;
  return SG_OBJ(z);
}

SgObject Sg_MakeTranscodedInputOutputPort(SgPort *port, SgTranscoder *transcoder)
{
  SgPort *z = make_port(SG_IN_OUT_PORT, SG_TEXTUAL_PORT_TYPE, -1);
  SgTextualPort *t = make_textual_port(SG_TRANSCODED_TEXTUAL_PORT_TYPE);

  z->closed = FALSE;
  z->flush = transFlush;
  z->close = transClose;

  t->src.transcoded.transcoder = transcoder;
  t->src.transcoded.port = port;
  t->getChar = transGetChar;
  t->unGetChar = transUnGetChar;
  t->getLineNo = transGetLineNo;
  t->lookAheadChar = lookAheadChar;
  t->putChar = transPutChar;

  z->impl.tport = t;
  return SG_OBJ(z);
}

/*****
   Transcoded port
 */

/* String output port */

static void string_port_flush(SgObject self)
{
  /* dummy */
}

static int string_iport_close(SgObject self)
{
  SG_PORT(self)->closed = TRUE;
  return TRUE;
}

static int string_oport_close(SgObject self)
{
  SG_PORT(self)->closed = TRUE;
  SG_TEXTUAL_PORT(self)->src.buffer.str = NULL;
  return TRUE;
}

static void string_oport_putchar(SgObject self, SgChar c)
{
  SgTextualPort *tp = SG_TEXTUAL_PORT(self);
  SgString *str = tp->src.buffer.str;
  int current_size = SG_STRING_SIZE(str);
  int current_index = tp->src.buffer.index;

  if (current_index + 1 >= current_size) {
    int new_size = current_size + INCREASE_BUFFER_SIZE, i;
    SgString *tmp = Sg_ReserveString(new_size, ' ');
    memcpy(SG_STRING_VALUE(tmp), SG_STRING_VALUE(str),
	   current_index * sizeof(SgChar));
    tp->src.buffer.str = tmp;
    str = tmp;
  }
  SG_STRING_VALUE_AT(str, current_index) = c;
  tp->src.buffer.index++;
}

static SgChar string_iport_getchar(SgObject self)
{
  SgChar ch;
  SgString *str = SG_TEXTUAL_PORT(self)->src.buffer.str;
  int size = SG_STRING_SIZE(str);
  int index = SG_TEXTUAL_PORT(self)->src.buffer.index;
  if (size == index) {
    return EOF;
  }
  ch = SG_STRING_VALUE_AT(str, index);
  if (ch == '\n') {
    SG_TEXTUAL_PORT(self)->src.buffer.lineNo++;
  }
  SG_TEXTUAL_PORT(self)->src.buffer.index++;
  return ch;
}

static void string_iport_ungetchar(SgObject self, SgChar c)
{
  if (EOF == c) return;
  SG_TEXTUAL_PORT(self)->src.buffer.index--;
}

static int string_iport_getlineno(SgObject self)
{
  return SG_TEXTUAL_PORT(self)->src.buffer.lineNo;
}

static SgChar string_iport_look_aheadchar(SgObject self)
{
  SgString *str = SG_TEXTUAL_PORT(self)->src.buffer.str;
  int current_index = SG_TEXTUAL_PORT(self)->src.buffer.index;
  return SG_STRING_VALUE_AT(str, current_index + 1);
}


SgObject Sg_MakeStringOutputPort(int bufferSize)
{
  SgPort *z = make_port(SG_OUTPUT_PORT, SG_TEXTUAL_PORT_TYPE, SG_BUFMODE_NONE);
  SgTextualPort *t = make_textual_port(SG_STRING_TEXTUAL_PORT_TYPE);
  int size = (bufferSize > 0) ? bufferSize : DEFAULT_BUFFER_SIZE;

  z->closed = FALSE;
  z->flush = string_port_flush;
  z->close = string_oport_close;

  t->src.buffer.str = Sg_ReserveString(size, ' ');
  t->src.buffer.index = 0;
  t->src.buffer.lineNo = -1;
  t->getChar = NULL;
  t->unGetChar = NULL;
  t->getLineNo = NULL;
  t->lookAheadChar = NULL;
  t->putChar = string_oport_putchar;

  z->impl.tport = t;
  return SG_OBJ(z); 
}

SgObject Sg_MakeStringInputPort(SgString *s, int private)
{
  SgPort *z = make_port(SG_INPUT_PORT, SG_TEXTUAL_PORT_TYPE, SG_BUFMODE_NONE);
  SgTextualPort *t = make_textual_port(SG_STRING_TEXTUAL_PORT_TYPE);

  z->closed = FALSE;
  z->flush = string_port_flush;
  z->close = string_iport_close;

  t->src.buffer.str = s;
  t->src.buffer.index = 0;
  t->src.buffer.lineNo  = 1;

  t->getChar = string_iport_getchar;
  t->unGetChar = string_iport_ungetchar;
  t->getLineNo = string_iport_getlineno;
  t->lookAheadChar = lookAheadChar;
  t->putChar = NULL;

  z->impl.tport = t;
  return SG_OBJ(z); 
}

SgObject Sg_GetByteVectorFromBinaryPort(SgPort *port)
{
  SgBinaryPort *bp;

  if (!SG_BINARY_PORTP(port)) {
    Sg_Error(UC("binary port required, but got %S"), port);
  }

  bp = SG_BINARY_PORT(port);
  if (bp->type == SG_FILE_BINARY_PORT_TYPE) {
    /* TODO file size */
  } else if (bp->type == SG_BYTE_ARRAY_BINARY_PORT_TYPE) {
    if (SG_INPORTP(port)) {
      /* TODO should I re-create it? */
      return SG_OBJ(bp->src.buffer.bvec);
    } else {
      /* recreate */
      SgByteVector *bv = Sg_MakeByteVector(bp->src.buffer.index, 0);
      Sg_ByteVectorCopyX(bp->src.buffer.bvec, 0, bv, 0, bp->src.buffer.index);
      return SG_OBJ(bv);
    }
  }
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_GetStringFromStringPort(SgPort *port)
{
  SgTextualPort *tp = SG_TEXTUAL_PORT(port);
  if (tp->type != SG_STRING_TEXTUAL_PORT_TYPE) {
    Sg_Error(UC("string textual port required"));
  }
  if (SG_INPORTP(port)) {
    /* TODO should this return from current index? */
    return tp->src.buffer.str;
  } else {
    SgString *ret = Sg_CopyString(tp->src.buffer.str);
    SG_STRING_SIZE(ret) = tp->src.buffer.index;
    return ret;
  }
}

void Sg_ClosePort(SgPort *port)
{
  port->close(port);
}

/* this doesn't close port, just pseudo.
   on C level we don't check if a port was closed or not.
   but on Scheme level we need to do it.
 */
void Sg_PseudoClosePort(SgPort *port)
{
  ASSERT(SG_BINARY_PORTP(port));
  SG_BINARY_PORT(port)->closed = SG_BPORT_PSEUDO;
}

void Sg_FlushPort(SgPort *port)
{
  if (SG_PORT(port)->flush) {
    SG_PORT(port)->flush(port);
  }
}

void Sg_FlushAllPort(int exitting)
{
  SgWeakVector *ports;
  SgVector *save;
  SgObject p = SG_FALSE;
  int i, saved = 0;

  save = SG_VECTOR(Sg_MakeVector(PORT_VECTOR_SIZE, SG_FALSE));
  ports = active_buffered_ports.ports;

  for (i = 0; i < PORT_VECTOR_SIZE;) {
    /* TODO lock */
    for (; i < PORT_VECTOR_SIZE; i++) {
      p = Sg_WeakVectorRef(ports, i, SG_FALSE);
      if (SG_PORTP(p)) {
	Sg_VectorSet(save, i, p);
	Sg_WeakVectorSet(ports, i, SG_TRUE);
	saved++;
	break;
      }
    }
    if (SG_PORTP(p)) {
      if (SG_PORT(p)->flush)	/* I don't think I need this, but just in case */
	SG_PORT(p)->flush(p);
    }
  }
  if (!exitting && saved) {
    /* TODO lock */
    for (i = 0; i < PORT_VECTOR_SIZE; i++) {
      p = Sg_VectorRef(save, i, SG_FALSE);
      if (SG_PORTP(p)) Sg_WeakVectorSet(ports, i, p);
    }
  }
}

/* TODO port lock */
int Sg_Getb(SgPort *port)
{
  int b;
  SG_PORT_LOCK(port);
  b = Sg_GetbUnsafe(port);
  SG_PORT_UNLOCK(port);
  return b;
}

int64_t Sg_Readb(SgPort *port, uint8_t *buf, int64_t size)
{
  int64_t ret;
  SG_PORT_LOCK(port);
  ret = Sg_ReadbUnsafe(port, buf, size);
  SG_PORT_UNLOCK(port);
  return ret;
}

int64_t Sg_ReadbAll(SgPort *port, uint8_t **buf)
{
  int64_t ret;
  SG_PORT_LOCK(port);
  ret = Sg_ReadbAllUnsafe(port, buf);
  SG_PORT_UNLOCK(port);
  return ret;
}

SgChar Sg_Getc(SgPort *port)
{
  SgChar ch;
  SG_PORT_LOCK(port);
  ch = Sg_GetcUnsafe(port);
  SG_PORT_UNLOCK(port);
  return ch;
}

void Sg_Putc(SgPort *port, SgChar ch)
{
  SG_PORT_LOCK(port);
  Sg_PutcUnsafe(port, ch);
  SG_PORT_UNLOCK(port);
}

void Sg_Putz(SgPort *port, const char *str)
{
  SG_PORT_LOCK(port);
  Sg_PutzUnsafe(port, str);
  SG_PORT_UNLOCK(port);
}

void Sg_Putuz(SgPort *port, const SgChar *str)
{
  SG_PORT_LOCK(port);
  Sg_PutuzUnsafe(port, str);
  SG_PORT_UNLOCK(port);
}

void Sg_Puts(SgPort *port, SgString *str)
{
  SG_PORT_LOCK(port);
  Sg_PutsUnsafe(port, str);
  SG_PORT_UNLOCK(port);
}

void Sg_PutcUnsafe(SgPort *port, SgChar ch)
{
  ASSERT(SG_TEXTUAL_PORTP(port));
  SG_TEXTUAL_PORT(port)->putChar(port, ch);
}

void Sg_PutzUnsafe(SgPort *port, const char *str)
{
  ASSERT(SG_TEXTUAL_PORTP(port));
  for (;*str;) Sg_PutcUnsafe(port, ((SgChar)*str++));
}

void Sg_PutuzUnsafe(SgPort *port, const SgChar *str)
{
  ASSERT(SG_TEXTUAL_PORTP(port));
  for (;*str;) Sg_PutcUnsafe(port, *str++);
}

void Sg_PutsUnsafe(SgPort *port, SgString *str)
{
  const SgChar *p;
  int i, size;

  ASSERT(SG_TEXTUAL_PORTP(port));
  p = SG_STRING_VALUE(str);
  size = SG_STRING_SIZE(str);
  for (i = 0; i < size; i++) Sg_PutcUnsafe(port, p[i]);
}

int Sg_GetbUnsafe(SgPort *port)
{
  ASSERT(SG_BINARY_PORTP(port));
  return SG_BINARY_PORT(port)->getU8(port);
}

int64_t Sg_ReadbUnsafe(SgPort *port, uint8_t *buf, int64_t size)
{
  ASSERT(SG_BINARY_PORTP(port));
  return SG_BINARY_PORT(port)->readU8(port, buf, size);
}

int64_t Sg_ReadbAllUnsafe(SgPort *port, uint8_t **buf)
{
  ASSERT(SG_BINARY_PORTP(port));
  return SG_BINARY_PORT(port)->readU8All(port, buf);
}

SgChar Sg_GetcUnsafe(SgPort *port)
{
  ASSERT(SG_TEXTUAL_PORTP(port));
  return SG_TEXTUAL_PORT(port)->getChar(port);
}

void Sg_UngetcUnsafe(SgPort *port, SgChar ch)
{
  ASSERT(SG_TEXTUAL_PORTP(port));
  SG_TEXTUAL_PORT(port)->unGetChar(port, ch);
}

SgChar Sg_PeekcUnsafe(SgPort *port)
{
  ASSERT(SG_TEXTUAL_PORTP(port));
  return SG_TEXTUAL_PORT(port)->lookAheadChar(port);
}

int Sg_HasPortPosition(SgPort *port)
{
  if (SG_BINARY_PORTP(port)) {
    return TRUE;
  } else if (SG_TEXTUAL_PORTP(port)) {
    switch (SG_TEXTUAL_PORT(port)->type) {
    case SG_TRANSCODED_TEXTUAL_PORT_TYPE:
      return FALSE;
    case SG_STRING_TEXTUAL_PORT_TYPE:
      return TRUE;
    default:
      Sg_Error(UC("unknown textual port type. may be bug? %S"), port);
    }
  } else {
    /* custom port */
    /* TODO */
    return FALSE;
  }
  Sg_Error(UC("port required, but got %S"), port);
  return FALSE;			/* dummy */
}

int Sg_HasSetPortPosition(SgPort *port)
{
  if (SG_BINARY_PORTP(port)) {
    return TRUE;
  } else if (SG_TEXTUAL_PORTP(port)) {
    switch (SG_TEXTUAL_PORT(port)->type) {
    case SG_TRANSCODED_TEXTUAL_PORT_TYPE:
      return FALSE;
    case SG_STRING_TEXTUAL_PORT_TYPE:
      return TRUE;
    default:
      Sg_Error(UC("unknown textual port type. may be bug? %S"), port);
    }
  } else {
    /* custom port */
    /* TODO */
    return FALSE;
  }
  Sg_Error(UC("port required, but got %S"), port);
  return FALSE;			/* dummy */
}

int64_t Sg_PortPosition(SgPort *port)
{
  if (SG_BINARY_PORTP(port)) {
    off_t pos;
    switch (SG_BINARY_PORT(port)->type) {
    case SG_FILE_BINARY_PORT_TYPE:
      pos = SG_BINARY_PORT(port)->src.file->tell(SG_BINARY_PORT(port)->src.file);
      break;
    case SG_BYTE_ARRAY_BINARY_PORT_TYPE:
      pos = (off_t)SG_BINARY_PORT(port)->src.buffer.index;
      break;
    default:
      Sg_Error(UC("unknown binary port type. may be bug? %S"), port);
    }
    return (int64_t)pos;
  } else if (SG_TEXTUAL_PORTP(port)) {
    off_t pos;
    switch (SG_TEXTUAL_PORT(port)->type) {
    case SG_TRANSCODED_TEXTUAL_PORT_TYPE:
      Sg_Error(UC("transcoded textual port does not support port-position")); 
      break;
    case SG_STRING_TEXTUAL_PORT_TYPE:
      pos = (off_t)SG_TEXTUAL_PORT(port)->src.buffer.index;
      break;
    default:
      Sg_Error(UC("unknown textual port type. may be bug? %S"), port);
    }
    return (int64_t)pos;
  } else {
    /* custom port */
    /* TODO */
    Sg_Error(UC("custom port is not supported yet."));
  }
  Sg_Error(UC("port required, but got %S"), port);
  return (int64_t)-1;		/* dummy */
}

void Sg_SetPortPosition(SgPort *port, int64_t offset)
{
  if (SG_OUTPORTP(port)) port->flush(port);
  if (SG_BINARY_PORTP(port)) {
    switch (SG_BINARY_PORT(port)->type) {
    case SG_FILE_BINARY_PORT_TYPE:
      SG_BINARY_PORT(port)->src.file->seek(SG_BINARY_PORT(port)->src.file,
					   offset, SG_BEGIN);
      break;
    case SG_BYTE_ARRAY_BINARY_PORT_TYPE:
      SG_BINARY_PORT(port)->src.buffer.index = offset;
      break;
    default:
      Sg_Error(UC("unknown binary port type. may be bug? %S"), port);
    }
    return;
  } else if (SG_TEXTUAL_PORTP(port)) {
    switch (SG_TEXTUAL_PORT(port)->type) {
    case SG_TRANSCODED_TEXTUAL_PORT_TYPE:
      Sg_Error(UC("transcoded textual port does not support port-position")); 
      break;
    case SG_STRING_TEXTUAL_PORT_TYPE:
      SG_TEXTUAL_PORT(port)->src.buffer.index = offset;
      break;
    default:
      Sg_Error(UC("unknown textual port type. may be bug? %S"), port);
    }
    return;
  } else {
    /* custom port */
    /* TODO */
    Sg_Error(UC("custom port is not supported yet."));
  }
  Sg_Error(UC("port required, but got %S"), port);
}

int Sg_LineNo(SgPort *port)
{
  ASSERT(SG_TEXTUAL_PORTP(port));
  return SG_TEXTUAL_PORT(port)->getLineNo(port);
}

SgObject Sg_FileName(SgPort *port)
{
  SgFile *file = NULL;
  if (!SG_PORTP(port)) {
    Sg_Error(UC("port required, but got %S"), port);
  }

  if (SG_TEXTUAL_PORTP(port)) {
    if (SG_TEXTUAL_PORT(port)->type == SG_TRANSCODED_TEXTUAL_PORT_TYPE) {
      SgPort *bp = SG_TEXTUAL_PORT(port)->src.transcoded.port;
      file = SG_BINARY_PORT(bp)->src.file;
    }
  } else if (SG_BINARY_PORTP(port)) {
    if (SG_BINARY_PORT(port)->type == SG_FILE_BINARY_PORT_TYPE) {
      file = SG_BINARY_PORT(port)->src.file;
    }
  }
  if (file != NULL) {
    return Sg_MakeString(file->name, SG_LITERAL_STRING);
  }

  return SG_FALSE;
}

SgObject Sg_PortTranscoder(SgObject port)
{
  if (SG_BINARY_PORTP(port)) return SG_FALSE;
  else if (SG_TEXTUAL_PORTP(port)) {
    if (SG_TEXTUAL_PORT(port)->type == SG_TRANSCODED_TEXTUAL_PORT_TYPE) {
      return SG_TEXTUAL_PORT(port)->src.transcoded.transcoder;
    } else {
      /* String port doesn't have transcoder */
      return SG_FALSE;
    }
  } else {
    /* TODO custom port */
    return SG_FALSE;
  }
}

/* standard ports */
static SgObject sg_stdin  = SG_UNBOUND;
static SgObject sg_stdout = SG_UNBOUND;
static SgObject sg_stderr = SG_UNBOUND;

SgObject Sg_StandardOutputPort()
{
  return SG_OBJ(sg_stdout);
}

SgObject Sg_StandardInputPort()
{
  return SG_OBJ(sg_stdin);
}

SgObject Sg_StandardErrorPort()
{
  return SG_OBJ(sg_stderr);
}


void Sg__InitPort()
{
  SgVM *vm = Sg_VM();
  /* TODO lock */
  active_buffered_ports.ports = SG_WEAK_VECTOR(Sg_MakeWeakVector(PORT_VECTOR_SIZE));

  sg_stdin = Sg_MakeFileBinaryInputPort(Sg_StandardIn(), SG_BUFMODE_NONE);
  sg_stdout = Sg_MakeFileBinaryOutputPort(Sg_StandardOut(), SG_BUFMODE_LINE);
  sg_stderr = Sg_MakeFileBinaryOutputPort(Sg_StandardError(), SG_BUFMODE_NONE);

  vm->currentInputPort = Sg_MakeTranscodedInputPort(sg_stdin,
						    Sg_IsUTF16Console(Sg_StandardIn()) ? Sg_MakeNativeConsoleTranscoder()
						                                       : Sg_MakeNativeTranscoder());
  vm->currentOutputPort = Sg_MakeTranscodedOutputPort(sg_stdout,
						     Sg_IsUTF16Console(Sg_StandardOut()) ? Sg_MakeNativeConsoleTranscoder()
						                                         : Sg_MakeNativeTranscoder());
  vm->currentErrorPort = Sg_MakeTranscodedOutputPort(sg_stderr,
						     Sg_IsUTF16Console(Sg_StandardError()) ? Sg_MakeNativeConsoleTranscoder()
						                                           : Sg_MakeNativeTranscoder());
  vm->logPort = vm->currentErrorPort;
}
  
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
