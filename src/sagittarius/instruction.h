/* -*- C -*- */
/* do not edit! this file was automatically generated by gen-instruction.scm.*/
#ifndef SAGITTARIUS_INSTRUCATIONS_H
#define SAGITTARIUS_INSTRUCATIONS_H

#include "sagittariusdefs.h"
#define INSN_MASK 0xFF
#define INSN_VALUE1_MASK  0xFFF
#define INSN_VALUE2_MASK  ((1 << (sizeof(intptr_t) * 8)) - 1)
#define INSN_VALUE1_SHIFT 8
#define INSN_VALUE2_SHIFT 20
#ifdef _MSC_VER
/* what a stupid macro definition on windows.h*/
#undef CONST
#endif
/**
   @brief set of instructions.

   For Sagittarius' instruction, it must be 1 byte, so that there are
   maximam 255 of instructions.

   @author Takashi Kato
 */
typedef enum {
  NOP = 0x00,
  HALT = 0x01,
  UNDEF = 0x02,
  CONST = 0x03,
  CONSTI = 0x04,
  LREF = 0x05,
  LSET = 0x06,
  FREF = 0x07,
  FSET = 0x08,
  GREF = 0x09,
  GSET = 0x0a,
  PUSH = 0x0b,
  BOX = 0x0c,
  UNBOX = 0x0d,
  ADD = 0x0e,
  ADDI = 0x0f,
  SUB = 0x10,
  SUBI = 0x11,
  MUL = 0x12,
  MULI = 0x13,
  DIV = 0x14,
  DIVI = 0x15,
  NEG = 0x16,
  TEST = 0x17,
  JUMP = 0x18,
  SHIFTJ = 0x19,
  MARK = 0x1a,
  BNNUME = 0x1b,
  BNLT = 0x1c,
  BNLE = 0x1d,
  BNGT = 0x1e,
  BNGE = 0x1f,
  BNEQ = 0x20,
  BNEQV = 0x21,
  BNNULL = 0x22,
  NOT = 0x23,
  NUM_EQ = 0x24,
  NUM_LT = 0x25,
  NUM_LE = 0x26,
  NUM_GT = 0x27,
  NUM_GE = 0x28,
  RECEIVE = 0x29,
  CLOSURE = 0x2a,
  APPLY = 0x2b,
  CALL = 0x2c,
  LOCAL_CALL = 0x2d,
  TAIL_CALL = 0x2e,
  LOCAL_TAIL_CALL = 0x2f,
  RET = 0x30,
  FRAME = 0x31,
  LET_FRAME = 0x32,
  POP_LET_FRAME = 0x33,
  DISPLAY = 0x34,
  ENTER = 0x35,
  LEAVE = 0x36,
  DEFINE = 0x37,
  LIBRARY = 0x38,
  CAR = 0x39,
  CDR = 0x3a,
  CONS = 0x3b,
  LIST = 0x3c,
  VALUES = 0x3d,
  EQ = 0x3e,
  EQV = 0x3f,
  NULLP = 0x40,
  PAIRP = 0x41,
  SYMBOLP = 0x42,
  VECTOR = 0x43,
  VECTORP = 0x44,
  VEC_LEN = 0x45,
  VEC_REF = 0x46,
  VEC_SET = 0x47,
  LREF_PUSH = 0x48,
  FREF_PUSH = 0x49,
  GREF_PUSH = 0x4a,
  CONST_PUSH = 0x4b,
  CONSTI_PUSH = 0x4c,
  GREF_CALL = 0x4d,
  GREF_TAIL_CALL = 0x4e,
  SET_CAR = 0x4f,
  SET_CDR = 0x50,
  CAAR = 0x51,
  CADR = 0x52,
  CDAR = 0x53,
  CDDR = 0x54,
  LREF_CAR = 0x55,
  LREF_CDR = 0x56,
  FREF_CAR = 0x57,
  FREF_CDR = 0x58,
  GREF_CAR = 0x59,
  GREF_CDR = 0x5a,
  INSTRUCTION_COUNT = 91, /** number of instructions */
} Instruction;

typedef struct InsnInfoRec InsnInfo;
struct InsnInfoRec
{
  const char *name;
  int         number;
  int         instValues;
  int         argc;
  int         hasSrc;
  int         label;
};
#define INSN(o)            ((o) & INSN_MASK)
#define INSN_VAL1(v, insn) ((v) = ((int)(insn)) >> INSN_VALUE1_SHIFT)
#define INSN_VAL2(v1, v2, insn)	\
  do {				\
    (v1) = ((((int)(insn)) >> INSN_VALUE1_SHIFT) & INSN_VALUE1_MASK);	\
    (v2) = ((((int)(insn)) >> INSN_VALUE2_SHIFT) & INSN_VALUE2_MASK);	\
  } while (0)
#define MERGE_INSN_VALUE1(insn, value)      \
  ((insn) | ((value) << INSN_VALUE1_SHIFT))
#define MERGE_INSN_VALUE2(insn, val1, val2) \
  ((insn) | ((val1) << INSN_VALUE1_SHIFT) | ((val2) << INSN_VALUE2_SHIFT))
SG_CDECL_BEGIN
SG_EXTERN InsnInfo* Sg_LookupInsnName(Instruction insn);
SG_CDECL_END
#endif
