/*  -*- C -*- */
/*
 * unicode.c
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
#define LIBSAGITTARIUS_BODY
#include "sagittarius/unicode.h"
#include "sagittarius/error.h"
#include "sagittarius/port.h"
#include "sagittarius/transcoder.h"
#include "sagittarius/codec.h"
#include "sagittarius/number.h"
#include "sagittarius/string.h"
#include "sagittarius/symbol.h"
#include "sagittarius/writer.h"
#include "sagittarius/bytevector.h"

#include "../unicode/lexeme.inc"

int Sg_Ucs4ConstituentP(SgChar c)
{
  int offset = c / 8;
  int bit = 1 << (c & 7);
  return (constituent[offset] & bit) != 0;
}

int Sg_Ucs4SubsequentP(SgChar c)
{
  int offset = c / 8;
  int bit = 1 << (c & 7);
  return (subsequent[offset] & bit) != 0;
}

int Sg_Ucs4WhiteSpaceP(SgChar c)
{
  if (c == 0x0020) return TRUE;                   /*; White_Space # Zs       SPACE */
  if (c >= 0x0009 && c <= 0x000d) return TRUE;    /*; White_Space # Cc   [5] <control-0009>..<control-000D> */
  if (c <= 0x007F) return FALSE;
  if (c >= 0x2000 && c <= 0x200a) return TRUE;    /*; White_Space # Zs  [11] EN QUAD..HAIR SPACE */
  switch (c) {
  case 0x0085:    /*; White_Space # Cc       <control-0085> */
  case 0x00A0:    /*; White_Space # Zs       NO-BREAK SPACE */
  case 0x1680:    /*; White_Space # Zs       OGHAM SPACE MARK */
  case 0x180E:    /*; White_Space # Zs       MONGOLIAN VOWEL SEPARATOR */
  case 0x2028:    /*; White_Space # Zl       LINE SEPARATOR */
  case 0x2029:    /*; White_Space # Zp       PARAGRAPH SEPARATOR */
  case 0x202F:    /*; White_Space # Zs       NARROW NO-BREAK SPACE */
  case 0x205F:    /*; White_Space # Zs       MEDIUM MATHEMATICAL SPACE */
  case 0x3000:    /*; White_Space # Zs       IDEOGRAPHIC SPACE */
    return TRUE;
  }
  return FALSE;
}

int Sg_Ucs4IntralineWhiteSpaceP(SgChar c)
{
  if (c == 0x0020) return TRUE;                   /*; White_Space # Zs       SPACE */
  if (c == 0x0009) return TRUE;                   /*; White_Space # Cc   [5] <control-0009> */
  if (c <= 0x007F) return FALSE;
  if (c >= 0x2000 && c <= 0x200a) return TRUE;    /*; White_Space # Zs  [11] EN QUAD..HAIR SPACE */
  switch (c) {
  case 0x00A0:    /*; White_Space # Zs       NO-BREAK SPACE */
  case 0x1680:    /*; White_Space # Zs       OGHAM SPACE MARK */
  case 0x180E:    /*; White_Space # Zs       MONGOLIAN VOWEL SEPARATOR */
  case 0x202F:    /*; White_Space # Zs       NARROW NO-BREAK SPACE */
  case 0x205F:    /*; White_Space # Zs       MEDIUM MATHEMATICAL SPACE */
  case 0x3000:    /*; White_Space # Zs       IDEOGRAPHIC SPACE */
    return TRUE;
  }
  return FALSE;
}

int Sg_ConvertUcs4ToUtf8(SgChar ucs4, uint8_t utf8[4], ErrorHandlingMode mode)
{
  if (ucs4 < 0x80) {
    utf8[0] = ucs4;
    return 1;
  } else if (ucs4 < 0x800) {
    utf8[0] = ((ucs4 >>  6) & 0x1f) | 0xc0;
    utf8[1] = ((ucs4      ) & 0x3f) | 0x80;
    return 2;
  } else if (ucs4 < 0x10000) {
    utf8[0] = ((ucs4 >> 12) & 0x0f) | 0xe0;
    utf8[1] = ((ucs4 >>  6) & 0x3f) | 0x80;
    utf8[2] = ((ucs4      ) & 0x3f) | 0x80;
    return 3;
  } else if (ucs4 < 0x200000) {
    utf8[0] = ((ucs4 >> 18) & 0x07) | 0xf0;
    utf8[1] = ((ucs4 >> 12) & 0x3f) | 0x80;
    utf8[2] = ((ucs4 >>  6) & 0x3f) | 0x80;
    utf8[3] = ((ucs4      ) & 0x3f) | 0x80;
    return 4;
  } else {
    if (mode == SG_RAISE_ERROR) {
      Sg_IOError(SG_IO_ENCODE_ERROR, SG_INTERN("convert-ucs4-to-utf8"),
		 Sg_Sprintf(UC("character out of utf8 range %s:%d %x"),
			    UC(__FILE__), __LINE__, ucs4),
		 SG_UNDEF, SG_UNDEF);
      return 0;
    } else if (mode == SG_REPLACE_ERROR) {
      utf8[0] = 0xff;
      utf8[1] = 0xfd;
      return 2;
    } else {
      if (mode == SG_IGNORE_ERROR) {
	return 0;
      } else {
	Sg_Error(UC("invalid error handling mode"));
      }
    }
  }
  return 0; /* dummy */
}

int Sg_ConvertUcs4ToUtf16(SgChar ucs4, uint8_t utf8[4], ErrorHandlingMode mode, int littlep)
{
#define put2byte(buf, in)			\
  if (littlep) {				\
    (buf)[0] = (uint8_t)(in);			\
    (buf)[1] = (uint8_t)((in) >> 8);		\
  } else {					\
    (buf)[0] = (uint8_t)((in) >> 8);		\
    (buf)[1] = (uint8_t)(in);			\
  }

  if (ucs4 > 0x10FFFF) {
    if (mode == SG_RAISE_ERROR) {
      Sg_IOError(SG_IO_ENCODE_ERROR, SG_INTERN("convert-ucs4-to-utf16"),
		 Sg_Sprintf(UC("character out of utf16 range %s:%d %x"),
			    UC(__FILE__), __LINE__, ucs4),
		 SG_UNDEF, SG_UNDEF);
      return 0;
    } else if (mode == SG_REPLACE_ERROR) {
      utf8[0] = 0xff;
      utf8[1] = 0xfd;
      return 2;
    } else {
      if (mode == SG_IGNORE_ERROR) {
	return 0;
      } else {
	Sg_Error(UC("invalid error handling mode"));
      }
    }
  }
  if (ucs4 < 0x10000) {
    put2byte(utf8, ucs4);
    return 2;
  } else {
    /* http://unicode.org/faq/utf_bom.html#utf16-3 */
    const uint16_t HI_SURROGATE_START = 0xD800;
    uint16_t X = (uint16_t) ucs4;
    SgChar   U = (ucs4 >> 16) & ((1 << 5) - 1);
    uint16_t W = (uint16_t) U - 1;
    uint16_t HiSurrogate = HI_SURROGATE_START | (W << 6) | X >> 10;
    uint16_t LoSurrogate;
    const uint16_t LO_SURROGATE_START = 0xDC00;
    X = (uint16_t)ucs4;
    LoSurrogate = (uint16_t) (LO_SURROGATE_START | (X & ((1 << 10) - 1)));
    put2byte(utf8 + 0, HiSurrogate);
    put2byte(utf8 + 2, LoSurrogate);
    return 4;
  }
}

#define decodeError(who)						\
  if (mode == SG_RAISE_ERROR) {						\
    Sg_IOError(SG_IO_DECODE_ERROR, who,					\
	       Sg_Sprintf(UC("Invalid encode %s:%x\n"),			\
			  UC(__FILE__), __LINE__),			\
	       SG_UNDEF, port);						\
  } else if (mode == SG_REPLACE_ERROR) {				\
    return 0xFFFD;							\
  } else {								\
    ASSERT(mode == SG_IGNORE_ERROR);					\
    goto retry;								\
  }

static inline int isUtf8Tail(uint8_t b)
{
    return (0x80 <= b && b <= 0xbf);
}

SgChar Sg_ConvertUtf8ToUcs4(SgPort *port, ErrorHandlingMode mode)
{
  int f;
  uint8_t first;

 retry:
  /* since we have reckless flag we should not check this here... */
  /* ASSERT(SG_BINARY_PORTP(port)); */

  f = Sg_GetbUnsafe(port);
  if (f == EOF) return EOF;
  first = (uint8_t)(f & 0xff);

  /* UTF8-1(ascii) = %x00-7F */
  if (first < 0x80) {
    return first;
    /* UTF8-2 = %xC2-DF UTF8-tail */
  } else if (0xc2 <= first && first <= 0xdf) {
    uint8_t second = Sg_GetbUnsafe(port);
    if (isUtf8Tail(second)) {
      return ((first & 0x1f) << 6) | (second & 0x3f);
    } else {
      decodeError(SG_INTERN("convert-utf8-to-ucs4"));
    }
    /* UTF8-3 = %xE0 %xA0-BF UTF8-tail / %xE1-EC 2( UTF8-tail ) /  */
    /*          %xED %x80-9F UTF8-tail / %xEE-EF 2( UTF8-tail )    */
  } else if (0xe0 <= first && first <= 0xef) {
    uint8_t second = Sg_GetbUnsafe(port);
    uint8_t third =  Sg_GetbUnsafe(port);
    if (!isUtf8Tail(third)) {
      decodeError(SG_INTERN("convert-utf8-to-ucs4"));
    } else if ((0xe0 == first && 0xa0 <= second && second <= 0xbf)    ||
	       (0xed == first && 0x80 <= second && second <= 0x9f)    ||
	       (0xe1 <= first && first <= 0xec && isUtf8Tail(second)) ||
	       ((0xee == first || 0xef == first) && isUtf8Tail(second))) {
      return ((first & 0xf) << 12) | ((second & 0x3f) << 6) | (third & 0x3f);
    } else {
      decodeError(SG_INTERN("convert-utf8-to-ucs4"));
    }
    /* UTF8-4 = %xF0 %x90-BF 2( UTF8-tail ) / %xF1-F3 3( UTF8-tail ) /  */
    /*          %xF4 %x80-8F 2( UTF8-tail )  */
  } else if (0xf0 <= first && first <= 0xf4) {
    uint8_t second = Sg_GetbUnsafe(port);
    uint8_t third =  Sg_GetbUnsafe(port);
    uint8_t fourth = Sg_GetbUnsafe(port);
    if (!isUtf8Tail(third) || !isUtf8Tail(fourth)) {
      decodeError(SG_INTERN("convert-utf8-to-ucs4"));
    } else if ((0xf0 == first && 0x90 <= second && second <= 0xbf)     ||
	       (0xf4 == first && 0x80 <= second && second <= 0x8f)     ||
	       (0xf1 <= first && first <= 0xf3 && isUtf8Tail(second))) {
      return ((first & 0x7) << 18) | ((second & 0x3f) << 12) | ((third & 0x3f) << 6) | fourth;
    } else {
      decodeError(SG_INTERN("convert-utf8-to-ucs4"));
    }
  } else {
    decodeError(SG_INTERN("convert-utf8-to-ucs4"));
  }
  return ' ';
}

SgChar Sg_ConvertUtf16ToUcs4(SgPort *port, ErrorHandlingMode mode, SgCodec *codec, int checkBOMNow)
{
  uint16_t hi;
  uint16_t lo;
  SgChar   X;
  SgChar   W;
  SgChar   U;
  SgChar   C;
  uint16_t val1, val2;
  int a, b, c, d;

#define isLittleEndian(c) (SG_CODEC_ENDIAN(c) == UTF_16LE)
 retry:
  /* TODO assert */
  a = Sg_GetbUnsafe(port);
  b = Sg_GetbUnsafe(port);

  if (a == EOF) return EOF;
  if (b == EOF) {
    decodeError(SG_INTERN("convert-utf16-to-ucs4"));
  }

  if (checkBOMNow && SG_CODEC_ENDIAN(codec) == UTF_16CHECK_BOM) {
    if (a == 0xFE && b == 0xFF) {
      SG_CODEC_ENDIAN(codec) = UTF_16BE;
      return Sg_ConvertUtf16ToUcs4(port, mode, codec, FALSE);
    } else if (a == 0xFF && b == 0xFE) {
      SG_CODEC_ENDIAN(codec) = UTF_16LE;
      return Sg_ConvertUtf16ToUcs4(port, mode, codec, FALSE);
    } else {
      SG_CODEC_ENDIAN(codec) = UTF_16BE;
      /* correct? */
    }
  }

  val1 = isLittleEndian(codec) ? ((b << 8) | a) : ((a << 8) | b);
  if (val1 < 0xD800 || val1 > 0xDFFF) {
    return val1;
  }
  c = Sg_GetbUnsafe(port);
  if (EOF == c) {
    decodeError(SG_INTERN("convert-utf16-to-ucs4"));
  }
  d = Sg_GetbUnsafe(port);
  if (EOF == d) {
    decodeError(SG_INTERN("convert-utf16-to-ucs4"));
  }
  val2 = isLittleEndian(codec) ? ((d << 8) | c) : ((c << 8) | d);
  /* http://unicode.org/faq/utf_bom.html#utf16-3 */
  hi = val1;
  lo = val2;
  X = (hi & ((1 << 6) -1)) << 10 | (lo & ((1 << 10) -1));
  W = (hi >> 6) & ((1 << 5) - 1);
  U = W + 1;
  C = U << 16 | X;
  return C;
}

SgChar Sg_EnsureUcs4(SgChar c)
{
  ASSERT(c >= 0);
  if (c > 0x10ffff) Sg_Error(UC("code point out of range, U+%X"), c);
  if (c >= 0xd800 && c <= 0xdfff) Sg_Error(UC("code point in excluded range, U+%X"), c);
  return c;
}

SgObject Sg_Utf8sToUtf32s(const char *s, int len)
{
  SgPort *p = Sg_MakeByteArrayInputPort((uint8_t *)s, len);
  SgTranscoder *t = Sg_MakeTranscoder(Sg_MakeUtf8Codec(), LF, SG_IGNORE_ERROR);
  SgPort *sp = Sg_MakeStringOutputPort(len * sizeof(SgChar));
  int i;
  for (i = 0; i < len; i++) {
    Sg_Putc(sp, t->getChar(t, p));
  }
  return Sg_GetStringFromStringPort(sp);
}

SgObject Sg_Utf16sToUtf32s(const char *s, int len)
{
  SgPort *p = Sg_MakeByteArrayInputPort((uint8_t *)s, len);
  SgTranscoder *t = Sg_MakeTranscoder(Sg_MakeUtf16Codec(UTF_16CHECK_BOM), LF, SG_IGNORE_ERROR);
  SgPort *sp = Sg_MakeStringOutputPort(len * sizeof(SgChar));
  int i;
  for (i = 0; i < len; i++) {
    Sg_Putc(sp, t->getChar(t, p));
  }
  return Sg_GetStringFromStringPort(sp);
}

char* Sg_Utf32sToUtf8s(const SgString *s)
{
  SgPort *p = Sg_MakeByteArrayOutputPort(s->size + sizeof(SgChar));
  SgTranscoder *t = Sg_MakeTranscoder(Sg_MakeUtf8Codec(), LF, SG_IGNORE_ERROR);
  int i, len = s->size;
  SgChar *value = s->value;
  for (i = 0; i < len; i++) {
    t->putChar(t, p, *(value + i));
  }
  t->putChar(t, p, '\0');
  return (char*)Sg_GetByteArrayFromBinaryPort(p);
}

size_t ustrcspn(const SgChar *s1, const char *s2)
{
  register const SgChar *ss1 = s1;
  register int c;
  register const char *ss2;
  for (; (c = *ss1) != '\0'; ss1++) {
    for (ss2 = s2; *ss2 != '\0'; ss2++) {
      if (c == *ss2) {
        goto quit;
      }
    }
  }
quit:
  return ss1 - s1;
}

int ustrcmp(const SgChar *s1, const char *s2)
{
  register const uint32_t *ss1;
  register const unsigned char *ss2;
  for (ss1 = (const uint32_t *)s1,
	 ss2 = (const unsigned char *)s2;
       *ss1 == *ss2 && *ss1 != 0;
       ss1++, ss2++)
    ;
  return *ss1 - *ss2;
}

int ustrncmp(const SgChar *s1,
	     const char *s2, size_t n)
{
#if 0
  register const uint32_t *ss1;
  register const unsigned char *ss2, *t;
  for (ss1 = (const unsigned int *)s1,
	 ss2 = (const unsigned char *)s2,
	 t = ss1 + n;
       ss1 != t && *ss1 == *ss2 && *ss1 != 0;
       ss1++, ss2++)
    ;
  return *ss1 - *ss2;
#endif
  size_t i;
  for(i = 0; i < n; i++) {
    if(s1[i] ^ s2[i]) return s1[i] - s2[i];
  }
  return 0;
}


size_t ustrlen(const SgChar *value)
{
  /* TODO naive? */
  int count = 0;
  for (; *value ;value++, count++);
  return count;
}

#define CASE_OFFSET 0x20

#include "../unicode/general-category-1.inc"
#include "../unicode/general-category-2.inc"
#include "../unicode/numeric-property.inc"
#include "../unicode/other-alphabetic.inc"
#include "../unicode/other-lowercase.inc"
#include "../unicode/other-uppercase.inc"
#include "../unicode/simple-lowercase.inc"
#include "../unicode/simple-titlecase.inc"
#include "../unicode/simple-uppercase.inc"
#include "../unicode/canonical-class.inc"
#include "../unicode/compose.inc"
#include "../unicode/compatibility.inc"

#include "../unicode/case-folding.inc"
#include "../unicode/special-casing-lower.inc"
#include "../unicode/special-casing-title.inc"
#include "../unicode/special-casing-upper.inc"
#include "../unicode/decompose.inc"

#define DECLARE_SIMPLE_CASE(name)				\
  static SgChar name (SgChar ch)				\
  {								\
    const int size = array_sizeof(SG_CPP_CAT(s_, name));	\
    int i;							\
    for (i = 0; i < size; i++) {				\
      if (SG_CPP_CAT(s_, name)[i].in == ch) {			\
	return SG_CPP_CAT(s_, name)[i].out;			\
      }								\
    }								\
    return ch;							\
  }

DECLARE_SIMPLE_CASE(simple_uppercase);
DECLARE_SIMPLE_CASE(simple_lowercase);
DECLARE_SIMPLE_CASE(simple_titlecase);
DECLARE_SIMPLE_CASE(canonical_class);
DECLARE_SIMPLE_CASE(compose);

#define DECLARE_BOOL_CASE(name)					\
  static int name (SgChar ch)					\
  {								\
    const int size = array_sizeof(SG_CPP_CAT(s_, name));	\
    int i;							\
    for (i = 0; i < size; i++) {				\
      if (SG_CPP_CAT(s_, name)[i].in == ch) {			\
	return TRUE;						\
      }								\
    }								\
    return FALSE;						\
  }

DECLARE_BOOL_CASE(compatibility);

static int other_alphabetic_property_p(SgChar ch)
{
  if (0x345 <= ch && ch <= 0x10A0F) {
    const int size = array_sizeof(s_other_alphabetic);
    int i;
    for (i = 0; i < size; i++) {
      if (s_other_alphabetic[i].in <= ch &&
	  s_other_alphabetic[i].out <= ch) {
	return TRUE;
      }
    }
  }
  return FALSE;
}

SgObject Sg_DigitValue(SgChar ch)
{
  if ('0' <= ch && ch <= '9') return SG_MAKE_INT(ch - '0');
  else {
    const int size = array_sizeof(s_numeric_property);
    int i;
    for (i = 0; i < size; i++) {
      if (s_numeric_property[i].in == (int32_t)ch)
	return Sg_MakeInteger(s_numeric_property[i].out);
    }
    return SG_FALSE;;
  }
}

SgChar Sg_CharUpCase(SgChar ch)
{
  if (ch < 'a') return ch;
  else if (ch > 'z') return simple_uppercase(ch);
  else return ch - CASE_OFFSET;
}

SgChar Sg_CharDownCase(SgChar ch)
{
  if (ch < 'A') return ch;
  else if (ch <= 'Z') return ch + CASE_OFFSET;
  else if (ch > 'z') return simple_lowercase(ch);
  else return ch;
}

SgChar Sg_CharTitleCase(SgChar ch)
{
  return simple_titlecase(ch);
}

SgChar Sg_CharFoldCase(SgChar ch)
{
  if (ch <= 'z') return Sg_CharDownCase(ch);
  else if (ch == 0x130 ||
	   ch == 0x131) return ch;
  else return Sg_CharDownCase(Sg_CharUpCase(ch));
}

int Sg_CharAlphabeticP(SgChar ch)
{
  if ('a' <= ch && ch <= 'z') return TRUE;
  else if ('A' <= ch && ch <= 'Z') return TRUE;
  else if (0x80 <= ch) {
    switch (Sg_CharGeneralCategory(ch)) {
    case Lu: case Ll: case Lt: case Lm: case Lo: case Nl:
      return TRUE;
    case Mn: case Mc: case So:
      return other_alphabetic_property_p(ch);
    default:
      return FALSE;
    }
  }
  else return FALSE;
}

int Sg_CharNumericP(SgChar ch)
{
  if ('0' <= ch && ch <= '9') return TRUE;
  else if (0x80 <= ch) {
    return Sg_CharGeneralCategory(ch) == Nd;
  }
  else return FALSE;
}

int Sg_CharUpperCaseP(SgChar ch)
{
  if ('A' <= ch && ch <= 'Z') return TRUE;
  else if (0x80 <= ch) {
    switch (Sg_CharGeneralCategory(ch)) {
    case Lu:
      return TRUE;
    case Nl: case So:
      return other_alphabetic_property_p(ch);
    default:
      return FALSE;
    }
  }
  else return FALSE;
}

int Sg_CharLowerCaseP(SgChar ch)
{
  if ('a' <= ch && ch <= 'z') return TRUE;
  else if (0x80 <= ch) {
    switch (Sg_CharGeneralCategory(ch)) {
    case Ll:
      return TRUE;
    case Lm: case Mn: case Nl: case So:
      return other_alphabetic_property_p(ch);
    default:
      return FALSE;
    }
  }
  else return FALSE;
}

int Sg_CharTitleCaseP(SgChar ch)
{
  return Sg_CharGeneralCategory(ch) == Lt;
}

GeneralCategory Sg_CharGeneralCategory(SgChar ch)
{
  const int cate1_size = array_sizeof(s_general_category_1);
  const int cate2_size = array_sizeof(s_general_category_2);
  int i;
  for (i = 0; i < cate1_size; i++) {
    if (s_general_category_1[i].in == ch) return s_general_category_1[i].out;
  }
  for (i = 0; i < cate2_size; i++) {
    if (s_general_category_2[i].in == ch) return s_general_category_2[i].out;
  }
  if (0x3400 <= ch && ch <= 0x4DB5) return Lo;
  else if (0x4E00 <= ch && ch <= 0x9FBB) return Lo;
  else if (0xAC00 <= ch && ch <= 0xD7A3) return Lo;
  else if (0xD800 <= ch && ch <= 0xDB7F) return Cs;
  else if (0xDB80 <= ch && ch <= 0xDBFF) return Cs;
  else if (0xDC00 <= ch && ch <= 0xDFFF) return Cs;
  else if (0xE000 <= ch && ch <= 0xF8FF) return Co;
  else if (0x20000 <= ch && ch <= 0x2A6D6) return Lo;
  else if (0xF0000 <= ch && ch <= 0xFFFFD) return Co;
  else if (0x100000 <= ch && ch <= 0x10FFFD) return Co;
  else return Cn;
}

SgObject Sg_CategroyToSymbol(GeneralCategory cate)
{
#define CASE_INTERN(c)			\
  case c : return SG_INTERN(#c)

  switch (cate) {
  CASE_INTERN(Lu);
  CASE_INTERN(Ll);
  CASE_INTERN(Lt);
  CASE_INTERN(Lm);
  CASE_INTERN(Lo);
  CASE_INTERN(Mn);
  CASE_INTERN(Mc);
  CASE_INTERN(Me);
  CASE_INTERN(Nd);
  CASE_INTERN(Nl);
  CASE_INTERN(No);
  CASE_INTERN(Ps);
  CASE_INTERN(Pe);
  CASE_INTERN(Pi);
  CASE_INTERN(Pf);
  CASE_INTERN(Pd);
  CASE_INTERN(Pc);
  CASE_INTERN(Po);
  CASE_INTERN(Sc);
  CASE_INTERN(Sm);
  CASE_INTERN(Sk);
  CASE_INTERN(So);
  CASE_INTERN(Zs);
  CASE_INTERN(Zp);
  CASE_INTERN(Zl);
  CASE_INTERN(Cc);
  CASE_INTERN(Cf);
  CASE_INTERN(Cs);
  CASE_INTERN(Co);
  CASE_INTERN(Cn);
  }
  /* never happen */
  return SG_INTERN("Cn");
}

#define DECLARE_SPECIAL_CASING(name)				\
  static int name (SgChar ch)					\
  {								\
    const int size = array_sizeof(SG_CPP_CAT(s_, name));	\
    int i;							\
    for (i = 0; i < size; i++) {				\
      if (SG_CPP_CAT(s_, name)[i].in == ch) return i;		\
    }								\
    return -1;							\
  }

DECLARE_SPECIAL_CASING(special_casing_upper);
DECLARE_SPECIAL_CASING(special_casing_lower);
DECLARE_SPECIAL_CASING(special_casing_title);
DECLARE_SPECIAL_CASING(case_folding);
DECLARE_SPECIAL_CASING(decompose);

static int final_sigma_p(int index, SgString *in, SgPort *out)
{
  SgChar ch;
  int size = SG_STRING_SIZE(in);
  if (size <= index + 1) {
    return Sg_PortPosition(out) != 0;
  }
  ch = SG_STRING_VALUE_AT(in, index + 1);
  if (Sg_CharAlphabeticP(ch)) return FALSE;
  else if (Sg_Ucs4WhiteSpaceP(ch)) return TRUE;
  else if (Sg_CharGeneralCategory(ch) == Pd) return TRUE;
  else {
    int i = index;
    for (; i < size; i++) {
      ch = SG_STRING_VALUE_AT(in, i);
      if (Sg_CharAlphabeticP(ch)) return FALSE;
      else if (Sg_Ucs4WhiteSpaceP(ch)) return TRUE;
      else if (Sg_CharGeneralCategory(ch) == Pd) return TRUE;
    }
    return Sg_PortPosition(out) != 0;
  }
}

SgObject Sg_StringUpCase(SgString *str)
{
  int i, size = SG_STRING_SIZE(str);
  SgObject out = Sg_MakeStringOutputPort(size);
  SgObject newS;

  for (i = 0; i < size; i++) {
    int r = special_casing_upper(SG_STRING_VALUE_AT(str, i));
    if (r >= 0) {
      const int up_size = array_sizeof(s_special_casing_upper[r].out);
      int j;
      for (j = 0; j < up_size; j++) {
	if (s_special_casing_upper[r].out[j] == 0) break;
	Sg_PutcUnsafe(out, Sg_CharUpCase(s_special_casing_upper[r].out[j]));
      } 
    } else {
      Sg_PutcUnsafe(out, Sg_CharUpCase(SG_STRING_VALUE_AT(str, i)));
    }
  }
  newS = Sg_GetStringFromStringPort(out);
  if (Sg_StringEqual(str, newS)) {
    newS = NULL;
    return str;
  } else {
    return newS;
  }
}

SgObject Sg_StringDownCase(SgString *str)
{
  int i, size = SG_STRING_SIZE(str), r;
  SgObject out = Sg_MakeStringOutputPort(size);
  SgObject newS;
  SgChar ch, lastCh = ' ';
  for (i = 0; i < size; i++, lastCh = ch) {
    ch = SG_STRING_VALUE_AT(str, i);
    
    if (ch == 0x03A3) { 	/* greek capital letter sigma */
      if (Sg_Ucs4WhiteSpaceP(lastCh)) {
	Sg_PutcUnsafe(out, 0x03C3);
      } else {
	if (final_sigma_p(i, str, out)) {
	  Sg_PutcUnsafe(out, 0x03C2); /* greek small letter final sigma */
	} else {
	  Sg_PutcUnsafe(out, 0x03C3); /* greek small letter sigma */
	}
      }
    } else {
      r = special_casing_lower(ch);
      if (r >= 0) {
	const int up_size = array_sizeof(s_special_casing_lower[r].out);
	int j;
	for (j = 0; j < up_size; j++) {
	  if (s_special_casing_lower[r].out[j] == 0) break;
	  Sg_PutcUnsafe(out, Sg_CharDownCase(s_special_casing_lower[r].out[j]));
	} 
      } else {
	Sg_PutcUnsafe(out, Sg_CharDownCase(ch));
      }
    }
  }
  newS = Sg_GetStringFromStringPort(out);
  if (Sg_StringEqual(str, newS)) {
    newS = NULL;
    return str;
  } else {
    return newS;
  }
}

static int titlecase_first_char(int index, SgString *in, SgPort *out);

static int downcase_subsequence(int index, SgString *in, SgPort *out)
{
  int i, size = SG_STRING_SIZE(in);
  SgChar ch;
  for (i = index; i < size; i++) {
    ch = SG_STRING_VALUE_AT(in, i);
    switch (Sg_CharGeneralCategory(ch)) {
      case Ll: case Lu: case Lt:
	Sg_PutcUnsafe(out, Sg_CharDownCase(ch));
	break;
    case Po: case Pf:
      if (ch == 0x0027 ||	/* mid letter # Po apostrophe */
	  ch == 0x003A ||	/* mid letter # Po colon */
	  ch == 0x00B7 ||	/* mid letter # Po middle dot */
	  ch == 0x05F4 ||	/* mid letter # Po hebrew punctuation gershayim */
	  ch == 0x2019 ||	/* mid letter # Po right single quotation mark */
	  ch == 0x2027) {	/* mid letter # Po hyphenation point */
	Sg_PutcUnsafe(out, ch);
      } else {
	Sg_PutcUnsafe(out, ch);
	i += titlecase_first_char(++i, in, out);
      }
      break;
    case Nd:
      Sg_PutcUnsafe(out, ch);
      break;
    default:
      Sg_PutcUnsafe(out, ch);
      i += titlecase_first_char(++i, in, out);
    }
  }
  return i - index;
}

static int titlecase_first_char(int index, SgString *in, SgPort *out)
{
  int i, size = SG_STRING_SIZE(in);
  SgChar ch;
  for (i = index; i < size; i++) {
    ch = SG_STRING_VALUE_AT(in, i);
    switch (Sg_CharGeneralCategory(ch)) {
    case Ll: case Lu: case Lt:
      Sg_PutcUnsafe(out, Sg_CharTitleCase(ch));
      i += downcase_subsequence(++i, in, out);
      break;
    default:
      Sg_PutcUnsafe(out, ch);
      break;
    }
  }
  return i - index;
}

SgObject Sg_StringTitleCase(SgString *str)
{
  int size = SG_STRING_SIZE(str);
  SgObject out = Sg_MakeStringOutputPort(size);
  SgObject newS;

  titlecase_first_char(0, str, SG_PORT(out));

  newS = Sg_GetStringFromStringPort(out);
  if (Sg_StringEqual(str, newS)) {
    newS = NULL;
    return str;
  } else {
    return newS;
  }
}

SgObject Sg_StringFoldCase(SgString *str)
{
  int i, size = SG_STRING_SIZE(str);
  SgObject out = Sg_MakeStringOutputPort(size);
  SgObject newS;

  for (i = 0; i < size; i++) {
    int r = case_folding(SG_STRING_VALUE_AT(str, i));
    if (r >= 0) {
      const int up_size = array_sizeof(s_case_folding[r].out);
      int j;
      for (j = 0; j < up_size; j++) {
	if (s_case_folding[r].out[j] == 0) break;
	Sg_PutcUnsafe(out, s_case_folding[r].out[j]);
      } 
    } else {
      Sg_PutcUnsafe(out, SG_STRING_VALUE_AT(str, i));
    }
  }
  newS = Sg_GetStringFromStringPort(out);
  if (Sg_StringEqual(str, newS)) {
    newS = NULL;
    return str;
  } else {
    return newS;
  }
}

#define SBase 0xAC00
#define LBase 0x1100
#define VBase 0x1161
#define TBase 0x11A7
#define LCount 19
#define VCount 21
#define TCount 28
#define NCount (VCount * TCount)
#define SCount (LCount * NCount)

static void recursive_decomposition(int canonicalP, SgChar sv, SgPort *out)
{
  int dindex = decompose(sv), sindex = sv - SBase, i;

  if (dindex >= 0 &&
      !(canonicalP && compatibility(sv))) {
    const int size = array_sizeof(s_decompose[dindex].out);
    for (i = 0; i < size; i++) {
      SgChar ch = s_decompose[dindex].out[i];
      if (ch == 0) break;
      recursive_decomposition(canonicalP, ch, out);
    }
  } else if (-1 < sindex && sindex < SCount) {
    int L = LBase + (sindex / NCount);
    int V = VBase + ((sindex % NCount) / TCount);
    int T = TBase + (sindex % TCount);
    Sg_PutcUnsafe(out, L);
    Sg_PutcUnsafe(out, V);
    if (T != TBase) {
      Sg_PutcUnsafe(out, T);
    }
  } else {
    Sg_PutcUnsafe(out, sv);
  }
}

static SgByteVector* decompose_rec(SgString *in, int canonicalP)
{
  int i, size = SG_STRING_SIZE(in);
  SgPort *out = SG_PORT(Sg_MakeStringOutputPort(size));
  /* TODO this could be non memmory allocate */
  SgTranscoder *trans = Sg_MakeTranscoder(Sg_MakeUtf32Codec(UTF_32USE_NATIVE_ENDIAN), LF, SG_RAISE_ERROR);
  for (i = 0; i < size; i++) {
    SgChar ch = SG_STRING_VALUE_AT(in, i);
    recursive_decomposition(canonicalP, ch, out);
  }
  return SG_BVECTOR(Sg_StringToByteVector(Sg_GetStringFromStringPort(out), trans, 0, -1));
}

static SgByteVector* sort_combining_marks(SgByteVector *bv)
{
  int last = SG_BVECTOR_SIZE(bv) - 4, i;
  for (i = 0; i < last;) {
    uint32_t this = Sg_ByteVectorU32NativeRef(bv, i);
    uint32_t next = Sg_ByteVectorU32NativeRef(bv, i + 4);
    int32_t this_cc = canonical_class(this);
    int32_t next_cc = canonical_class(next);
    if (this_cc > 0 &&
	next_cc > 0 &&
	this_cc > next_cc) {
      Sg_ByteVectorU32NativeSet(bv, i, next);
      Sg_ByteVectorU32NativeSet(bv, i + 4, this);
      if (i >= 4) {
	i -= 4;
      } else {
	i = 4;
      }
    } else {
      i += 4;
    }
  }
  return bv;
}

SgObject Sg_StringNormalizeNfd(SgString *str)
{
  SgTranscoder *trans = Sg_MakeTranscoder(Sg_MakeUtf32Codec(UTF_32USE_NATIVE_ENDIAN),
					  LF, SG_RAISE_ERROR);
  SgByteVector *bv = decompose_rec(str, TRUE);
  sort_combining_marks(bv);
  return Sg_ByteVectorToString(bv, trans, 0, -1);
}

SgObject Sg_StringNormalizeNfkd(SgString *str)
{
  SgTranscoder *trans = Sg_MakeTranscoder(Sg_MakeUtf32Codec(UTF_32USE_NATIVE_ENDIAN),
					  LF, SG_RAISE_ERROR);
  SgByteVector *bv = decompose_rec(str, FALSE);
  sort_combining_marks(bv);
  return Sg_ByteVectorToString(bv, trans, 0, -1);
}

static uint32_t pair_wise_composition(uint32_t first, uint32_t second)
{
  if (first < 0        ||		/* happen? */
      first > 0x10FFFF ||
      second < 0       ||		/* happen? */
      second > 0x10FFFF) {
    return FALSE;
  } else {
    int32_t lindex = first - LBase;
    int32_t vindex = second - VBase;
    int32_t sindex = first - SBase;
    int32_t tindex = second - TBase;
    if ((-1 < lindex && lindex < LCount) &&
	(-1 < vindex && vindex < VCount)) {
      return SBase + (TCount * (vindex + (lindex + VCount)));
    } else if ((-1 < sindex && sindex < SCount) &&
	       (-1 < tindex && tindex < TCount) &&
	       ((sindex % TCount) == 0)) {
      return first + tindex;
    } else {
      int64_t val = (first * 0x10000) + second;
      return compose(val);
    }
  }
}

static SgObject compose_rec(SgByteVector *bv)
{
  SgTranscoder *trans = Sg_MakeTranscoder(Sg_MakeUtf32Codec(UTF_32USE_NATIVE_ENDIAN),
					  LF, SG_RAISE_ERROR);
  SgByteVector *out;
  int size = SG_BVECTOR_SIZE(bv);
  uint32_t first = Sg_ByteVectorU32NativeRef(bv, 0);
  int32_t first_cc = (canonical_class(first) == first) ? 0 : 256;
  
  uint32_t starter = first;
  int32_t starter_cc = first_cc;
  int starter_pos = 0, comp_pos = 4, i;

  for (i = 4; i < size; i += 4) {
    uint32_t this = Sg_ByteVectorU32NativeRef(bv, i);
    int32_t this_cc = canonical_class(this);
    if ((starter_cc == 0 || starter_cc < this_cc)) {
      uint32_t composit = pair_wise_composition(starter, this);
      if (composit) {
	Sg_ByteVectorU32NativeSet(bv, starter_pos, composit);
	starter = composit;
	starter_cc = canonical_class(composit);
	continue;
      }
    }
    Sg_ByteVectorU32NativeSet(bv, comp_pos, this);
    if (this_cc == 0) {
      starter = this;
      starter_pos = comp_pos;
    }
    starter_cc = this_cc;
    comp_pos += 4;
  }
  out = Sg_MakeByteVector(comp_pos, 0);
  Sg_ByteVectorCopyX(bv, 0, out, 0, comp_pos);
  return Sg_ByteVectorToString(out, trans, 0, -1);  
}

SgObject Sg_StringNormalizeNfc(SgString *str)
{
  SgByteVector *bv = decompose_rec(str, TRUE);
  sort_combining_marks(bv);
  return compose_rec(bv);
}

SgObject Sg_StringNormalizeNfkc(SgString *str)
{
  SgByteVector *bv = decompose_rec(str, FALSE);
  sort_combining_marks(bv);
  return compose_rec(bv);
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
