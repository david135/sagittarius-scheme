/* Generated automatically from ../boot/lib/enums.scm. DO NOT EDIT! */
#define LIBSAGITTARIUS_BODY 
#include <sagittarius.h>
static struct sg__rcRec {
  SgObject d15[153];
  SgWord d16[874];
  SgCodeBuilder d17[23];
} sg__rc = {
  {  /* SgObject d15 */
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
    SG_UNBOUND,
  },
  {  /* SgWord d16 */
    /* #f */0x00000046    /*   0 FREF_PUSH */,
    0x00000045    /*   1 LREF_PUSH */,
    0x00000048    /*   2 CONST_PUSH */,
    SG_WORD(SG_FALSE) /* #f */,
    0x0000034b    /*   4 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable-ref#core.enums> */,
    0x0000002f    /*   6 RET */,
    /* make-enumeration-type */0x00000030    /*   0 FRAME */,
    SG_WORD(3),
    0x0000004a    /*   2 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier make-eq-hashtable#core.enums> */,
    0x0000000b    /*   4 PUSH */,
    0x00000045    /*   5 LREF_PUSH */,
    0x00000049    /*   6 CONSTI_PUSH */,
    0x00000205    /*   7 LREF */,
    0x00000021    /*   8 BNNULL */,
    SG_WORD(3),
    0x00000018    /*  10 JUMP */,
    SG_WORD(15),
    0x00000030    /*  12 FRAME */,
    SG_WORD(6),
    0x00000145    /*  14 LREF_PUSH */,
    0x0000025b    /*  15 LREF_CAR_PUSH */,
    0x00000345    /*  16 LREF_PUSH */,
    0x0000034a    /*  17 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier hashtable-set!#core.enums> */,
    0x0000025c    /*  19 LREF_CDR_PUSH */,
    0x00000305    /*  20 LREF */,
    0x0000010f    /*  21 ADDI */,
    0x0000000b    /*  22 PUSH */,
    0x00200219    /*  23 SHIFTJ */,
    0x00000018    /*  24 JUMP */,
    SG_WORD(-18),
    0x00000232    /*  26 LEAVE */,
    0x00000045    /*  27 LREF_PUSH */,
    0x00000145    /*  28 LREF_PUSH */,
    0x00000029    /*  29 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d17[0])) /* #<code-builder #f (1 0 1)> */,
    0x0000000b    /*  31 PUSH */,
    0x0000024b    /*  32 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier make-enum-type#core.enums> */,
    0x0000002f    /*  34 RET */,
    /* make-enumeration */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier list?#core.enums> */,
    0x00000017    /*   5 TEST */,
    SG_WORD(27),
    0x00000030    /*   7 FRAME */,
    SG_WORD(6),
    0x00000047    /*   9 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier symbol?#core.enums> */,
    0x00000045    /*  11 LREF_PUSH */,
    0x0000024a    /*  12 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier for-all#core.enums> */,
    0x00000017    /*  14 TEST */,
    SG_WORD(11),
    0x00000030    /*  16 FRAME */,
    SG_WORD(4),
    0x00000045    /*  18 LREF_PUSH */,
    0x0000014a    /*  19 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier make-enumeration-type#core.enums> */,
    0x0000000b    /*  21 PUSH */,
    0x00000045    /*  22 LREF_PUSH */,
    0x0000024b    /*  23 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier make-enum-set#core.enums> */,
    0x0000002f    /*  25 RET */,
    0x00000048    /*  26 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* make-enumeration */,
    0x00000048    /*  28 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* argument 1 must be a list of symbols */,
    0x0000024b    /*  30 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.enums> */,
    0x0000002f    /*  32 RET */,
    0x00000018    /*  33 JUMP */,
    SG_WORD(-8),
    0x0000002f    /*  35 RET */,
    /* enum-set-universe */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-type#core.enums> */,
    0x0000000b    /*   5 PUSH */,
    0x00000030    /*   6 FRAME */,
    SG_WORD(9),
    0x00000030    /*   8 FRAME */,
    SG_WORD(4),
    0x00000045    /*  10 LREF_PUSH */,
    0x0000014a    /*  11 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-type#core.enums> */,
    0x0000000b    /*  13 PUSH */,
    0x0000014a    /*  14 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-type-universe#core.enums> */,
    0x0000000b    /*  16 PUSH */,
    0x0000024b    /*  17 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier make-enum-set#core.enums> */,
    0x0000002f    /*  19 RET */,
    /* enum-set-indexer */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-type#core.enums> */,
    0x0000000b    /*   5 PUSH */,
    0x0000014b    /*   6 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-type-indexer#core.enums> */,
    0x0000002f    /*   8 RET */,
    /* #f */0x00000045    /*   0 LREF_PUSH */,
    0x00000046    /*   1 FREF_PUSH */,
    0x0000024b    /*   2 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier memq#core.enums> */,
    0x0000002f    /*   4 RET */,
    /* enum-set-constructor */0x00000030    /*   0 FRAME */,
    SG_WORD(9),
    0x00000030    /*   2 FRAME */,
    SG_WORD(4),
    0x00000046    /*   4 FREF_PUSH */,
    0x0000014a    /*   5 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-type#core.enums> */,
    0x0000000b    /*   7 PUSH */,
    0x0000014a    /*   8 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-type-universe#core.enums> */,
    0x0000000b    /*  10 PUSH */,
    0x00000030    /*  11 FRAME */,
    SG_WORD(8),
    0x00000145    /*  13 LREF_PUSH */,
    0x00000029    /*  14 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d17[5])) /* #<code-builder #f (1 0 1)> */,
    0x0000000b    /*  16 PUSH */,
    0x00000045    /*  17 LREF_PUSH */,
    0x0000024a    /*  18 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier for-all#core.enums> */,
    0x00000017    /*  20 TEST */,
    SG_WORD(11),
    0x00000030    /*  22 FRAME */,
    SG_WORD(4),
    0x00000046    /*  24 FREF_PUSH */,
    0x0000014a    /*  25 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-type#core.enums> */,
    0x0000000b    /*  27 PUSH */,
    0x00000045    /*  28 LREF_PUSH */,
    0x0000024b    /*  29 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier make-enum-set#core.enums> */,
    0x0000002f    /*  31 RET */,
    0x00000048    /*  32 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* enum-set-constructor */,
    0x00000048    /*  34 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* the symbol list must all belong to the universe. */,
    0x00000145    /*  36 LREF_PUSH */,
    0x00000045    /*  37 LREF_PUSH */,
    0x0000044b    /*  38 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.enums> */,
    0x0000002f    /*  40 RET */,
    /* enum-set-constructor */0x00000045    /*   0 LREF_PUSH */,
    0x00000029    /*   1 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d17[6])) /* #<code-builder enum-set-constructor (1 0 1)> */,
    0x0000002f    /*   3 RET */,
    /* loop */0x00000005    /*   0 LREF */,
    0x00000021    /*   1 BNNULL */,
    SG_WORD(3),
    0x00000061    /*   3 CONST_RET */,
    SG_WORD(SG_NIL) /* () */,
    0x00000030    /*   5 FRAME */,
    SG_WORD(5),
    0x0000005b    /*   7 LREF_CAR_PUSH */,
    0x00000146    /*   8 FREF_PUSH */,
    0x0000024a    /*   9 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier memq#core.enums> */,
    0x00000017    /*  11 TEST */,
    SG_WORD(10),
    0x0000005b    /*  13 LREF_CAR_PUSH */,
    0x00000030    /*  14 FRAME */,
    SG_WORD(5),
    0x0000005c    /*  16 LREF_CDR_PUSH */,
    0x00000007    /*  17 FREF */,
    0x0000000d    /*  18 UNBOX */,
    0x0000012c    /*  19 LOCAL_CALL */,
    0x00000037    /*  20 CONS */,
    0x0000002f    /*  21 RET */,
    0x0000005c    /*  22 LREF_CDR_PUSH */,
    0x00000007    /*  23 FREF */,
    0x0000000d    /*  24 UNBOX */,
    0x0000012e    /*  25 LOCAL_TAIL_CALL */,
    0x0000002f    /*  26 RET */,
    /* enum-set->list */0x00000030    /*   0 FRAME */,
    SG_WORD(9),
    0x00000030    /*   2 FRAME */,
    SG_WORD(4),
    0x00000045    /*   4 LREF_PUSH */,
    0x0000014a    /*   5 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-type#core.enums> */,
    0x0000000b    /*   7 PUSH */,
    0x0000014a    /*   8 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-type-universe#core.enums> */,
    0x0000000b    /*  10 PUSH */,
    0x00000030    /*  11 FRAME */,
    SG_WORD(4),
    0x00000045    /*  13 LREF_PUSH */,
    0x0000014a    /*  14 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-members#core.enums> */,
    0x0000000b    /*  16 PUSH */,
    0x00000002    /*  17 UNDEF */,
    0x0000000b    /*  18 PUSH */,
    0x0000000c    /*  19 BOX */,
    0x00000245    /*  20 LREF_PUSH */,
    0x00000345    /*  21 LREF_PUSH */,
    0x00000029    /*  22 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d17[8])) /* #<code-builder loop (1 0 2)> */,
    0x00000306    /*  24 LSET */,
    0x00000145    /*  25 LREF_PUSH */,
    0x00000305    /*  26 LREF */,
    0x0000000d    /*  27 UNBOX */,
    0x0000012e    /*  28 LOCAL_TAIL_CALL */,
    0x0000002f    /*  29 RET */,
    /* enum-set-member? */0x00000030    /*   0 FRAME */,
    SG_WORD(10),
    0x00000045    /*   2 LREF_PUSH */,
    0x00000030    /*   3 FRAME */,
    SG_WORD(4),
    0x00000145    /*   5 LREF_PUSH */,
    0x0000014a    /*   6 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-members#core.enums> */,
    0x0000000b    /*   8 PUSH */,
    0x0000024a    /*   9 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier memq#core.enums> */,
    0x00000017    /*  11 TEST */,
    SG_WORD(3),
    0x00000003    /*  13 CONST */,
    SG_WORD(SG_TRUE) /* #t */,
    0x0000002f    /*  15 RET */,
    /* #f */0x00000045    /*   0 LREF_PUSH */,
    0x00000046    /*   1 FREF_PUSH */,
    0x0000024b    /*   2 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier memq#core.enums> */,
    0x0000002f    /*   4 RET */,
    /* #f */0x00000045    /*   0 LREF_PUSH */,
    0x00000046    /*   1 FREF_PUSH */,
    0x0000024b    /*   2 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-member?#core.enums> */,
    0x0000002f    /*   4 RET */,
    /* enum-set-subset? */0x00000030    /*   0 FRAME */,
    SG_WORD(9),
    0x00000030    /*   2 FRAME */,
    SG_WORD(4),
    0x00000145    /*   4 LREF_PUSH */,
    0x0000014a    /*   5 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-universe#core.enums> */,
    0x0000000b    /*   7 PUSH */,
    0x0000014a    /*   8 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set->list#core.enums> */,
    0x0000000b    /*  10 PUSH */,
    0x00000030    /*  11 FRAME */,
    SG_WORD(18),
    0x00000245    /*  13 LREF_PUSH */,
    0x00000029    /*  14 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d17[11])) /* #<code-builder #f (1 0 1)> */,
    0x0000000b    /*  16 PUSH */,
    0x00000030    /*  17 FRAME */,
    SG_WORD(9),
    0x00000030    /*  19 FRAME */,
    SG_WORD(4),
    0x00000045    /*  21 LREF_PUSH */,
    0x0000014a    /*  22 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-universe#core.enums> */,
    0x0000000b    /*  24 PUSH */,
    0x0000014a    /*  25 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set->list#core.enums> */,
    0x0000000b    /*  27 PUSH */,
    0x0000024a    /*  28 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier for-all#core.enums> */,
    0x00000132    /*  30 LEAVE */,
    0x00000017    /*  31 TEST */,
    SG_WORD(13),
    0x00000145    /*  33 LREF_PUSH */,
    0x00000029    /*  34 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d17[12])) /* #<code-builder #f (1 0 1)> */,
    0x0000000b    /*  36 PUSH */,
    0x00000030    /*  37 FRAME */,
    SG_WORD(4),
    0x00000045    /*  39 LREF_PUSH */,
    0x0000014a    /*  40 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-members#core.enums> */,
    0x0000000b    /*  42 PUSH */,
    0x0000024b    /*  43 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier for-all#core.enums> */,
    0x0000002f    /*  45 RET */,
    /* enum-set=? */0x00000030    /*   0 FRAME */,
    SG_WORD(5),
    0x00000045    /*   2 LREF_PUSH */,
    0x00000145    /*   3 LREF_PUSH */,
    0x0000024a    /*   4 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-subset?#core.enums> */,
    0x00000017    /*   6 TEST */,
    SG_WORD(5),
    0x00000145    /*   8 LREF_PUSH */,
    0x00000045    /*   9 LREF_PUSH */,
    0x0000024b    /*  10 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-subset?#core.enums> */,
    0x0000002f    /*  12 RET */,
    /* enum-set-union */0x00000002    /*   0 UNDEF */,
    0x00000030    /*   1 FRAME */,
    SG_WORD(4),
    0x00000045    /*   3 LREF_PUSH */,
    0x0000014a    /*   4 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-type#core.enums> */,
    0x0000000b    /*   6 PUSH */,
    0x00000030    /*   7 FRAME */,
    SG_WORD(4),
    0x00000145    /*   9 LREF_PUSH */,
    0x0000014a    /*  10 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-type#core.enums> */,
    0x0000001f    /*  12 BNEQ */,
    SG_WORD(55),
    0x00000030    /*  14 FRAME */,
    SG_WORD(4),
    0x00000045    /*  16 LREF_PUSH */,
    0x0000014a    /*  17 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-type#core.enums> */,
    0x0000000b    /*  19 PUSH */,
    0x00000030    /*  20 FRAME */,
    SG_WORD(4),
    0x00000045    /*  22 LREF_PUSH */,
    0x0000014a    /*  23 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-members#core.enums> */,
    0x0000000b    /*  25 PUSH */,
    0x00000030    /*  26 FRAME */,
    SG_WORD(4),
    0x00000145    /*  28 LREF_PUSH */,
    0x0000014a    /*  29 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-members#core.enums> */,
    0x0000000b    /*  31 PUSH */,
    0x00000345    /*  32 LREF_PUSH */,
    0x00000445    /*  33 LREF_PUSH */,
    0x00000605    /*  34 LREF */,
    0x00000021    /*  35 BNNULL */,
    SG_WORD(4),
    0x00000505    /*  37 LREF */,
    0x00000018    /*  38 JUMP */,
    SG_WORD(23),
    0x00000030    /*  40 FRAME */,
    SG_WORD(5),
    0x0000065b    /*  42 LREF_CAR_PUSH */,
    0x00000545    /*  43 LREF_PUSH */,
    0x0000024a    /*  44 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier memq#core.enums> */,
    0x00000017    /*  46 TEST */,
    SG_WORD(8),
    0x00000545    /*  48 LREF_PUSH */,
    0x0000065c    /*  49 LREF_CDR_PUSH */,
    0x00500219    /*  50 SHIFTJ */,
    0x00000018    /*  51 JUMP */,
    SG_WORD(-18),
    0x00000018    /*  53 JUMP */,
    SG_WORD(8),
    0x0000065b    /*  55 LREF_CAR_PUSH */,
    0x00000505    /*  56 LREF */,
    0x00000054    /*  57 CONS_PUSH */,
    0x0000065c    /*  58 LREF_CDR_PUSH */,
    0x00500219    /*  59 SHIFTJ */,
    0x00000018    /*  60 JUMP */,
    SG_WORD(-27),
    0x00000232    /*  62 LEAVE */,
    0x00000232    /*  63 LEAVE */,
    0x0000000b    /*  64 PUSH */,
    0x0000024b    /*  65 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier make-enum-set#core.enums> */,
    0x0000002f    /*  67 RET */,
    0x00000048    /*  68 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* enum-set-union */,
    0x00000048    /*  70 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* enum-set1 and enum-set2 must be enumeration sets that have the same enumeration type. */,
    0x0000024b    /*  72 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.enums> */,
    0x0000002f    /*  74 RET */,
    /* enum-set-intersection */0x00000002    /*   0 UNDEF */,
    0x00000030    /*   1 FRAME */,
    SG_WORD(4),
    0x00000045    /*   3 LREF_PUSH */,
    0x0000014a    /*   4 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-type#core.enums> */,
    0x0000000b    /*   6 PUSH */,
    0x00000030    /*   7 FRAME */,
    SG_WORD(4),
    0x00000145    /*   9 LREF_PUSH */,
    0x0000014a    /*  10 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-type#core.enums> */,
    0x0000001f    /*  12 BNEQ */,
    SG_WORD(56),
    0x00000030    /*  14 FRAME */,
    SG_WORD(4),
    0x00000045    /*  16 LREF_PUSH */,
    0x0000014a    /*  17 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-type#core.enums> */,
    0x0000000b    /*  19 PUSH */,
    0x00000030    /*  20 FRAME */,
    SG_WORD(4),
    0x00000045    /*  22 LREF_PUSH */,
    0x0000014a    /*  23 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-members#core.enums> */,
    0x0000000b    /*  25 PUSH */,
    0x00000030    /*  26 FRAME */,
    SG_WORD(4),
    0x00000145    /*  28 LREF_PUSH */,
    0x0000014a    /*  29 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-members#core.enums> */,
    0x0000000b    /*  31 PUSH */,
    0x00000048    /*  32 CONST_PUSH */,
    SG_WORD(SG_NIL) /* () */,
    0x00000345    /*  34 LREF_PUSH */,
    0x00000605    /*  35 LREF */,
    0x00000021    /*  36 BNNULL */,
    SG_WORD(4),
    0x00000505    /*  38 LREF */,
    0x00000018    /*  39 JUMP */,
    SG_WORD(23),
    0x00000030    /*  41 FRAME */,
    SG_WORD(5),
    0x0000065b    /*  43 LREF_CAR_PUSH */,
    0x00000445    /*  44 LREF_PUSH */,
    0x0000024a    /*  45 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier memq#core.enums> */,
    0x00000017    /*  47 TEST */,
    SG_WORD(10),
    0x0000065b    /*  49 LREF_CAR_PUSH */,
    0x00000505    /*  50 LREF */,
    0x00000054    /*  51 CONS_PUSH */,
    0x0000065c    /*  52 LREF_CDR_PUSH */,
    0x00500219    /*  53 SHIFTJ */,
    0x00000018    /*  54 JUMP */,
    SG_WORD(-20),
    0x00000018    /*  56 JUMP */,
    SG_WORD(6),
    0x00000545    /*  58 LREF_PUSH */,
    0x0000065c    /*  59 LREF_CDR_PUSH */,
    0x00500219    /*  60 SHIFTJ */,
    0x00000018    /*  61 JUMP */,
    SG_WORD(-27),
    0x00000232    /*  63 LEAVE */,
    0x00000232    /*  64 LEAVE */,
    0x0000000b    /*  65 PUSH */,
    0x0000024b    /*  66 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier make-enum-set#core.enums> */,
    0x0000002f    /*  68 RET */,
    0x00000048    /*  69 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* enum-set-intersection */,
    0x00000048    /*  71 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* enum-set1 and enum-set2 must be enumeration sets that have the same enumeration type. */,
    0x0000024b    /*  73 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.enums> */,
    0x0000002f    /*  75 RET */,
    /* enum-set-difference */0x00000002    /*   0 UNDEF */,
    0x00000030    /*   1 FRAME */,
    SG_WORD(4),
    0x00000045    /*   3 LREF_PUSH */,
    0x0000014a    /*   4 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-type#core.enums> */,
    0x0000000b    /*   6 PUSH */,
    0x00000030    /*   7 FRAME */,
    SG_WORD(4),
    0x00000145    /*   9 LREF_PUSH */,
    0x0000014a    /*  10 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-type#core.enums> */,
    0x0000001f    /*  12 BNEQ */,
    SG_WORD(56),
    0x00000030    /*  14 FRAME */,
    SG_WORD(4),
    0x00000045    /*  16 LREF_PUSH */,
    0x0000014a    /*  17 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-type#core.enums> */,
    0x0000000b    /*  19 PUSH */,
    0x00000030    /*  20 FRAME */,
    SG_WORD(4),
    0x00000045    /*  22 LREF_PUSH */,
    0x0000014a    /*  23 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-members#core.enums> */,
    0x0000000b    /*  25 PUSH */,
    0x00000030    /*  26 FRAME */,
    SG_WORD(4),
    0x00000145    /*  28 LREF_PUSH */,
    0x0000014a    /*  29 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-members#core.enums> */,
    0x0000000b    /*  31 PUSH */,
    0x00000048    /*  32 CONST_PUSH */,
    SG_WORD(SG_NIL) /* () */,
    0x00000345    /*  34 LREF_PUSH */,
    0x00000605    /*  35 LREF */,
    0x00000021    /*  36 BNNULL */,
    SG_WORD(4),
    0x00000505    /*  38 LREF */,
    0x00000018    /*  39 JUMP */,
    SG_WORD(23),
    0x00000030    /*  41 FRAME */,
    SG_WORD(5),
    0x0000065b    /*  43 LREF_CAR_PUSH */,
    0x00000445    /*  44 LREF_PUSH */,
    0x0000024a    /*  45 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier memq#core.enums> */,
    0x00000017    /*  47 TEST */,
    SG_WORD(8),
    0x00000545    /*  49 LREF_PUSH */,
    0x0000065c    /*  50 LREF_CDR_PUSH */,
    0x00500219    /*  51 SHIFTJ */,
    0x00000018    /*  52 JUMP */,
    SG_WORD(-18),
    0x00000018    /*  54 JUMP */,
    SG_WORD(8),
    0x0000065b    /*  56 LREF_CAR_PUSH */,
    0x00000505    /*  57 LREF */,
    0x00000054    /*  58 CONS_PUSH */,
    0x0000065c    /*  59 LREF_CDR_PUSH */,
    0x00500219    /*  60 SHIFTJ */,
    0x00000018    /*  61 JUMP */,
    SG_WORD(-27),
    0x00000232    /*  63 LEAVE */,
    0x00000232    /*  64 LEAVE */,
    0x0000000b    /*  65 PUSH */,
    0x0000024b    /*  66 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier make-enum-set#core.enums> */,
    0x0000002f    /*  68 RET */,
    0x00000048    /*  69 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* enum-set-difference */,
    0x00000048    /*  71 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* enum-set1 and enum-set2 must be enumeration sets that have the same enumeration type. */,
    0x0000024b    /*  73 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier assertion-violation#core.enums> */,
    0x0000002f    /*  75 RET */,
    /* #f */0x00000030    /*   0 FRAME */,
    SG_WORD(5),
    0x00000045    /*   2 LREF_PUSH */,
    0x00000046    /*   3 FREF_PUSH */,
    0x0000024a    /*   4 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier memq#core.enums> */,
    0x00000022    /*   6 NOT */,
    0x0000002f    /*   7 RET */,
    /* enum-set-complement */0x00000030    /*   0 FRAME */,
    SG_WORD(4),
    0x00000045    /*   2 LREF_PUSH */,
    0x0000014a    /*   3 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-members#core.enums> */,
    0x0000000b    /*   5 PUSH */,
    0x00000030    /*   6 FRAME */,
    SG_WORD(4),
    0x00000045    /*   8 LREF_PUSH */,
    0x0000014a    /*   9 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-type#core.enums> */,
    0x0000000b    /*  11 PUSH */,
    0x00000030    /*  12 FRAME */,
    SG_WORD(18),
    0x00000145    /*  14 LREF_PUSH */,
    0x00000029    /*  15 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d17[18])) /* #<code-builder #f (1 0 1)> */,
    0x0000000b    /*  17 PUSH */,
    0x00000030    /*  18 FRAME */,
    SG_WORD(9),
    0x00000030    /*  20 FRAME */,
    SG_WORD(4),
    0x00000045    /*  22 LREF_PUSH */,
    0x0000014a    /*  23 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-type#core.enums> */,
    0x0000000b    /*  25 PUSH */,
    0x0000014a    /*  26 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-type-universe#core.enums> */,
    0x0000000b    /*  28 PUSH */,
    0x0000024a    /*  29 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier filter#core.enums> */,
    0x0000000b    /*  31 PUSH */,
    0x0000024b    /*  32 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier make-enum-set#core.enums> */,
    0x0000002f    /*  34 RET */,
    /* #f */0x00000045    /*   0 LREF_PUSH */,
    0x00000046    /*   1 FREF_PUSH */,
    0x0000024b    /*   2 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier memq#core.enums> */,
    0x0000002f    /*   4 RET */,
    /* enum-set-projection */0x00000030    /*   0 FRAME */,
    SG_WORD(5),
    0x00000045    /*   2 LREF_PUSH */,
    0x00000145    /*   3 LREF_PUSH */,
    0x0000024a    /*   4 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-subset?#core.enums> */,
    0x00000017    /*   6 TEST */,
    SG_WORD(3),
    0x00000005    /*   8 LREF */,
    0x0000002f    /*   9 RET */,
    0x00000030    /*  10 FRAME */,
    SG_WORD(9),
    0x00000030    /*  12 FRAME */,
    SG_WORD(4),
    0x00000145    /*  14 LREF_PUSH */,
    0x0000014a    /*  15 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-type#core.enums> */,
    0x0000000b    /*  17 PUSH */,
    0x0000014a    /*  18 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-type-universe#core.enums> */,
    0x0000000b    /*  20 PUSH */,
    0x00000030    /*  21 FRAME */,
    SG_WORD(4),
    0x00000045    /*  23 LREF_PUSH */,
    0x0000014a    /*  24 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-members#core.enums> */,
    0x0000000b    /*  26 PUSH */,
    0x00000030    /*  27 FRAME */,
    SG_WORD(4),
    0x00000145    /*  29 LREF_PUSH */,
    0x0000014a    /*  30 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-type#core.enums> */,
    0x0000000b    /*  32 PUSH */,
    0x00000030    /*  33 FRAME */,
    SG_WORD(8),
    0x00000245    /*  35 LREF_PUSH */,
    0x00000029    /*  36 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d17[20])) /* #<code-builder #f (1 0 1)> */,
    0x0000000b    /*  38 PUSH */,
    0x00000345    /*  39 LREF_PUSH */,
    0x0000024a    /*  40 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier filter#core.enums> */,
    0x0000000b    /*  42 PUSH */,
    0x0000024b    /*  43 GREF_TAIL_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier make-enum-set#core.enums> */,
    0x0000002f    /*  45 RET */,
    /* #f */0x00000034    /*   0 LIBRARY */,
    SG_WORD(SG_UNDEF) /* #<library core.enums> */,
    0x00000030    /*   2 FRAME */,
    SG_WORD(19),
    0x00000048    /*   4 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* <enum-type> */,
    0x00000048    /*   6 CONST_PUSH */,
    SG_WORD(SG_FALSE) /* #f */,
    0x00000048    /*   8 CONST_PUSH */,
    SG_WORD(SG_FALSE) /* #f */,
    0x00000048    /*  10 CONST_PUSH */,
    SG_WORD(SG_FALSE) /* #f */,
    0x00000048    /*  12 CONST_PUSH */,
    SG_WORD(SG_FALSE) /* #f */,
    0x00000048    /*  14 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* (immutable universe) */,
    0x00000003    /*  16 CONST */,
    SG_WORD(SG_UNDEF) /* (immutable indexer) */,
    0x00000240    /*  18 VECTOR */,
    0x0000000b    /*  19 PUSH */,
    0x0000064a    /*  20 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier make-record-type-descriptor#core.enums> */,
    0x0000000b    /*  22 PUSH */,
    0x00000030    /*  23 FRAME */,
    SG_WORD(8),
    0x00000045    /*  25 LREF_PUSH */,
    0x00000048    /*  26 CONST_PUSH */,
    SG_WORD(SG_FALSE) /* #f */,
    0x00000048    /*  28 CONST_PUSH */,
    SG_WORD(SG_FALSE) /* #f */,
    0x0000034a    /*  30 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier make-record-constructor-descriptor#core.enums> */,
    0x0000000b    /*  32 PUSH */,
    0x00000030    /*  33 FRAME */,
    SG_WORD(7),
    0x00000048    /*  35 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* <enum-type> */,
    0x00000045    /*  37 LREF_PUSH */,
    0x00000145    /*  38 LREF_PUSH */,
    0x0000034a    /*  39 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier make-record-type#core.enums> */,
    0x00000132    /*  41 LEAVE */,
    0x00000132    /*  42 LEAVE */,
    0x00000033    /*  43 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier <enum-type>#core.enums> */,
    0x00000030    /*  45 FRAME */,
    SG_WORD(10),
    0x00000030    /*  47 FRAME */,
    SG_WORD(5),
    0x00000047    /*  49 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier <enum-type>#core.enums> */,
    0x0000014a    /*  51 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier record-type-rcd#core.enums> */,
    0x0000000b    /*  53 PUSH */,
    0x0000014a    /*  54 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier record-constructor#core.enums> */,
    0x00000033    /*  56 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier make-enum-type#core.enums> */,
    0x00000030    /*  58 FRAME */,
    SG_WORD(10),
    0x00000030    /*  60 FRAME */,
    SG_WORD(5),
    0x00000047    /*  62 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier <enum-type>#core.enums> */,
    0x0000014a    /*  64 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier record-type-rtd#core.enums> */,
    0x0000000b    /*  66 PUSH */,
    0x0000014a    /*  67 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier record-predicate#core.enums> */,
    0x00000033    /*  69 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-type?#core.enums> */,
    0x00000030    /*  71 FRAME */,
    SG_WORD(11),
    0x00000030    /*  73 FRAME */,
    SG_WORD(5),
    0x00000047    /*  75 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier <enum-type>#core.enums> */,
    0x0000014a    /*  77 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier record-type-rtd#core.enums> */,
    0x0000000b    /*  79 PUSH */,
    0x00000049    /*  80 CONSTI_PUSH */,
    0x0000024a    /*  81 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier record-accessor#core.enums> */,
    0x00000033    /*  83 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-type-universe#core.enums> */,
    0x00000030    /*  85 FRAME */,
    SG_WORD(11),
    0x00000030    /*  87 FRAME */,
    SG_WORD(5),
    0x00000047    /*  89 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier <enum-type>#core.enums> */,
    0x0000014a    /*  91 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier record-type-rtd#core.enums> */,
    0x0000000b    /*  93 PUSH */,
    0x00000149    /*  94 CONSTI_PUSH */,
    0x0000024a    /*  95 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier record-accessor#core.enums> */,
    0x00000033    /*  97 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-type-indexer#core.enums> */,
    0x00000030    /*  99 FRAME */,
    SG_WORD(19),
    0x00000048    /* 101 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* <enum-set> */,
    0x00000048    /* 103 CONST_PUSH */,
    SG_WORD(SG_FALSE) /* #f */,
    0x00000048    /* 105 CONST_PUSH */,
    SG_WORD(SG_FALSE) /* #f */,
    0x00000048    /* 107 CONST_PUSH */,
    SG_WORD(SG_FALSE) /* #f */,
    0x00000048    /* 109 CONST_PUSH */,
    SG_WORD(SG_FALSE) /* #f */,
    0x00000048    /* 111 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* (immutable type) */,
    0x00000003    /* 113 CONST */,
    SG_WORD(SG_UNDEF) /* (immutable members) */,
    0x00000240    /* 115 VECTOR */,
    0x0000000b    /* 116 PUSH */,
    0x0000064a    /* 117 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier make-record-type-descriptor#core.enums> */,
    0x0000000b    /* 119 PUSH */,
    0x00000030    /* 120 FRAME */,
    SG_WORD(8),
    0x00000045    /* 122 LREF_PUSH */,
    0x00000048    /* 123 CONST_PUSH */,
    SG_WORD(SG_FALSE) /* #f */,
    0x00000048    /* 125 CONST_PUSH */,
    SG_WORD(SG_FALSE) /* #f */,
    0x0000034a    /* 127 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier make-record-constructor-descriptor#core.enums> */,
    0x0000000b    /* 129 PUSH */,
    0x00000030    /* 130 FRAME */,
    SG_WORD(7),
    0x00000048    /* 132 CONST_PUSH */,
    SG_WORD(SG_UNDEF) /* <enum-set> */,
    0x00000045    /* 134 LREF_PUSH */,
    0x00000145    /* 135 LREF_PUSH */,
    0x0000034a    /* 136 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier make-record-type#core.enums> */,
    0x00000132    /* 138 LEAVE */,
    0x00000132    /* 139 LEAVE */,
    0x00000033    /* 140 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier <enum-set>#core.enums> */,
    0x00000030    /* 142 FRAME */,
    SG_WORD(10),
    0x00000030    /* 144 FRAME */,
    SG_WORD(5),
    0x00000047    /* 146 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier <enum-set>#core.enums> */,
    0x0000014a    /* 148 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier record-type-rcd#core.enums> */,
    0x0000000b    /* 150 PUSH */,
    0x0000014a    /* 151 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier record-constructor#core.enums> */,
    0x00000033    /* 153 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier make-enum-set#core.enums> */,
    0x00000030    /* 155 FRAME */,
    SG_WORD(10),
    0x00000030    /* 157 FRAME */,
    SG_WORD(5),
    0x00000047    /* 159 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier <enum-set>#core.enums> */,
    0x0000014a    /* 161 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier record-type-rtd#core.enums> */,
    0x0000000b    /* 163 PUSH */,
    0x0000014a    /* 164 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier record-predicate#core.enums> */,
    0x00000033    /* 166 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set?#core.enums> */,
    0x00000030    /* 168 FRAME */,
    SG_WORD(11),
    0x00000030    /* 170 FRAME */,
    SG_WORD(5),
    0x00000047    /* 172 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier <enum-set>#core.enums> */,
    0x0000014a    /* 174 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier record-type-rtd#core.enums> */,
    0x0000000b    /* 176 PUSH */,
    0x00000049    /* 177 CONSTI_PUSH */,
    0x0000024a    /* 178 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier record-accessor#core.enums> */,
    0x00000033    /* 180 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-type#core.enums> */,
    0x00000030    /* 182 FRAME */,
    SG_WORD(11),
    0x00000030    /* 184 FRAME */,
    SG_WORD(5),
    0x00000047    /* 186 GREF_PUSH */,
    SG_WORD(SG_UNDEF) /* #<identifier <enum-set>#core.enums> */,
    0x0000014a    /* 188 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier record-type-rtd#core.enums> */,
    0x0000000b    /* 190 PUSH */,
    0x00000149    /* 191 CONSTI_PUSH */,
    0x0000024a    /* 192 GREF_CALL */,
    SG_WORD(SG_UNDEF) /* #<identifier record-accessor#core.enums> */,
    0x00000033    /* 194 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-members#core.enums> */,
    0x00000029    /* 196 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d17[1])) /* #<code-builder make-enumeration-type (1 0 0)> */,
    0x00000033    /* 198 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier make-enumeration-type#core.enums> */,
    0x00000029    /* 200 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d17[2])) /* #<code-builder make-enumeration (1 0 0)> */,
    0x00000033    /* 202 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier make-enumeration#core.enums> */,
    0x00000029    /* 204 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d17[3])) /* #<code-builder enum-set-universe (1 0 0)> */,
    0x00000033    /* 206 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-universe#core.enums> */,
    0x00000029    /* 208 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d17[4])) /* #<code-builder enum-set-indexer (1 0 0)> */,
    0x00000033    /* 210 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-indexer#core.enums> */,
    0x00000029    /* 212 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d17[7])) /* #<code-builder enum-set-constructor (1 0 0)> */,
    0x00000033    /* 214 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-constructor#core.enums> */,
    0x00000029    /* 216 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d17[9])) /* #<code-builder enum-set->list (1 0 0)> */,
    0x00000033    /* 218 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set->list#core.enums> */,
    0x00000029    /* 220 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d17[10])) /* #<code-builder enum-set-member? (2 0 0)> */,
    0x00000033    /* 222 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-member?#core.enums> */,
    0x00000029    /* 224 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d17[13])) /* #<code-builder enum-set-subset? (2 0 0)> */,
    0x00000033    /* 226 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-subset?#core.enums> */,
    0x00000029    /* 228 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d17[14])) /* #<code-builder enum-set=? (2 0 0)> */,
    0x00000033    /* 230 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set=?#core.enums> */,
    0x00000029    /* 232 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d17[15])) /* #<code-builder enum-set-union (2 0 0)> */,
    0x00000033    /* 234 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-union#core.enums> */,
    0x00000029    /* 236 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d17[16])) /* #<code-builder enum-set-intersection (2 0 0)> */,
    0x00000033    /* 238 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-intersection#core.enums> */,
    0x00000029    /* 240 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d17[17])) /* #<code-builder enum-set-difference (2 0 0)> */,
    0x00000033    /* 242 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-difference#core.enums> */,
    0x00000029    /* 244 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d17[19])) /* #<code-builder enum-set-complement (1 0 0)> */,
    0x00000033    /* 246 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-complement#core.enums> */,
    0x00000029    /* 248 CLOSURE */,
    SG_WORD(SG_OBJ(&sg__rc.d17[21])) /* #<code-builder enum-set-projection (2 0 0)> */,
    0x00000033    /* 250 DEFINE */,
    SG_WORD(SG_UNDEF) /* #<identifier enum-set-projection#core.enums> */,
    0x00000002    /* 252 UNDEF */,
    0x0000002f    /* 253 RET */,
  },
  {  /* SgCodeBuilder d17 */
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d16[0]), SG_FALSE, 1, 0, 1, 10, 7),
    
    SG_STATIC_CODE_BUILDER( /* make-enumeration-type */
      (SgWord *)SG_OBJ(&sg__rc.d16[7]), SG_FALSE, 1, 0, 0, 15, 35),
    
    SG_STATIC_CODE_BUILDER( /* make-enumeration */
      (SgWord *)SG_OBJ(&sg__rc.d16[42]), SG_FALSE, 1, 0, 0, 14, 36),
    
    SG_STATIC_CODE_BUILDER( /* enum-set-universe */
      (SgWord *)SG_OBJ(&sg__rc.d16[78]), SG_FALSE, 1, 0, 0, 11, 20),
    
    SG_STATIC_CODE_BUILDER( /* enum-set-indexer */
      (SgWord *)SG_OBJ(&sg__rc.d16[98]), SG_FALSE, 1, 0, 0, 9, 9),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d16[107]), SG_FALSE, 1, 0, 1, 9, 5),
    
    SG_STATIC_CODE_BUILDER( /* enum-set-constructor */
      (SgWord *)SG_OBJ(&sg__rc.d16[112]), SG_FALSE, 1, 0, 1, 19, 41),
    
    SG_STATIC_CODE_BUILDER( /* enum-set-constructor */
      (SgWord *)SG_OBJ(&sg__rc.d16[153]), SG_FALSE, 1, 0, 0, 7, 4),
    
    SG_STATIC_CODE_BUILDER( /* loop */
      (SgWord *)SG_OBJ(&sg__rc.d16[157]), SG_FALSE, 1, 0, 2, 13, 27),
    
    SG_STATIC_CODE_BUILDER( /* enum-set->list */
      (SgWord *)SG_OBJ(&sg__rc.d16[184]), SG_FALSE, 1, 0, 0, 14, 30),
    
    SG_STATIC_CODE_BUILDER( /* enum-set-member? */
      (SgWord *)SG_OBJ(&sg__rc.d16[214]), SG_FALSE, 2, 0, 0, 11, 16),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d16[230]), SG_FALSE, 1, 0, 1, 9, 5),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d16[235]), SG_FALSE, 1, 0, 1, 9, 5),
    
    SG_STATIC_CODE_BUILDER( /* enum-set-subset? */
      (SgWord *)SG_OBJ(&sg__rc.d16[240]), SG_FALSE, 2, 0, 0, 19, 46),
    
    SG_STATIC_CODE_BUILDER( /* enum-set=? */
      (SgWord *)SG_OBJ(&sg__rc.d16[286]), SG_FALSE, 2, 0, 0, 12, 13),
    
    SG_STATIC_CODE_BUILDER( /* enum-set-union */
      (SgWord *)SG_OBJ(&sg__rc.d16[299]), SG_FALSE, 2, 0, 0, 27, 75),
    
    SG_STATIC_CODE_BUILDER( /* enum-set-intersection */
      (SgWord *)SG_OBJ(&sg__rc.d16[374]), SG_FALSE, 2, 0, 0, 27, 76),
    
    SG_STATIC_CODE_BUILDER( /* enum-set-difference */
      (SgWord *)SG_OBJ(&sg__rc.d16[450]), SG_FALSE, 2, 0, 0, 27, 76),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d16[526]), SG_FALSE, 1, 0, 1, 9, 8),
    
    SG_STATIC_CODE_BUILDER( /* enum-set-complement */
      (SgWord *)SG_OBJ(&sg__rc.d16[534]), SG_FALSE, 1, 0, 0, 16, 35),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d16[569]), SG_FALSE, 1, 0, 1, 9, 5),
    
    SG_STATIC_CODE_BUILDER( /* enum-set-projection */
      (SgWord *)SG_OBJ(&sg__rc.d16[574]), SG_FALSE, 2, 0, 0, 19, 46),
    
    SG_STATIC_CODE_BUILDER( /* #f */
      (SgWord *)SG_OBJ(&sg__rc.d16[620]), SG_FALSE, 0, 0, 0, 0, 254),
  },
};
static SgCodeBuilder *toplevel = 
   SG_CODE_BUILDER(SG_OBJ(&sg__rc.d17[22]));
void Sg__Init_core_enums() {
  SgObject save = Sg_VM()->currentLibrary;
  SgObject h = SG_NIL, t = SG_NIL; /* for exports */ 

  sg__rc.d15[2] = SG_MAKE_STRING("(core enums)");
  sg__rc.d15[1] = Sg_Intern(sg__rc.d15[2]); /* (core enums) */
  sg__rc.d15[0] = Sg_FindLibrary(SG_SYMBOL(sg__rc.d15[1]), TRUE);
  sg__rc.d15[4] = SG_MAKE_STRING("<enum-type>");
  sg__rc.d15[3] = Sg_Intern(sg__rc.d15[4]); /* <enum-type> */
  sg__rc.d15[7] = SG_MAKE_STRING("immutable");
  sg__rc.d15[6] = Sg_Intern(sg__rc.d15[7]); /* immutable */
  sg__rc.d15[9] = SG_MAKE_STRING("universe");
  sg__rc.d15[8] = Sg_Intern(sg__rc.d15[9]); /* universe */
  do {
    SgObject G18 = SG_NIL, G19 = SG_NIL;
    SG_APPEND1(G18, G19, sg__rc.d15[6]); /* immutable */ 
    SG_APPEND1(G18, G19, sg__rc.d15[8]); /* universe */ 
    sg__rc.d15[5] = G18;
  } while (0);
  sg__rc.d15[12] = SG_MAKE_STRING("indexer");
  sg__rc.d15[11] = Sg_Intern(sg__rc.d15[12]); /* indexer */
  do {
    SgObject G20 = SG_NIL, G21 = SG_NIL;
    SG_APPEND1(G20, G21, sg__rc.d15[6]); /* immutable */ 
    SG_APPEND1(G20, G21, sg__rc.d15[11]); /* indexer */ 
    sg__rc.d15[10] = G20;
  } while (0);
  sg__rc.d15[15] = SG_MAKE_STRING("make-record-type-descriptor");
  sg__rc.d15[14] = Sg_Intern(sg__rc.d15[15]); /* make-record-type-descriptor */
  sg__rc.d15[13] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[14]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[18] = SG_MAKE_STRING("make-record-constructor-descriptor");
  sg__rc.d15[17] = Sg_Intern(sg__rc.d15[18]); /* make-record-constructor-descriptor */
  sg__rc.d15[16] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[17]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[21] = SG_MAKE_STRING("make-record-type");
  sg__rc.d15[20] = Sg_Intern(sg__rc.d15[21]); /* make-record-type */
  sg__rc.d15[19] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[20]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[22] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[3]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[25] = SG_MAKE_STRING("record-type-rcd");
  sg__rc.d15[24] = Sg_Intern(sg__rc.d15[25]); /* record-type-rcd */
  sg__rc.d15[23] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[24]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[28] = SG_MAKE_STRING("record-constructor");
  sg__rc.d15[27] = Sg_Intern(sg__rc.d15[28]); /* record-constructor */
  sg__rc.d15[26] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[27]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[31] = SG_MAKE_STRING("make-enum-type");
  sg__rc.d15[30] = Sg_Intern(sg__rc.d15[31]); /* make-enum-type */
  sg__rc.d15[29] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[30]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[34] = SG_MAKE_STRING("record-type-rtd");
  sg__rc.d15[33] = Sg_Intern(sg__rc.d15[34]); /* record-type-rtd */
  sg__rc.d15[32] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[33]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[37] = SG_MAKE_STRING("record-predicate");
  sg__rc.d15[36] = Sg_Intern(sg__rc.d15[37]); /* record-predicate */
  sg__rc.d15[35] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[36]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[40] = SG_MAKE_STRING("enum-type?");
  sg__rc.d15[39] = Sg_Intern(sg__rc.d15[40]); /* enum-type? */
  sg__rc.d15[38] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[39]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[43] = SG_MAKE_STRING("record-accessor");
  sg__rc.d15[42] = Sg_Intern(sg__rc.d15[43]); /* record-accessor */
  sg__rc.d15[41] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[42]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[46] = SG_MAKE_STRING("enum-type-universe");
  sg__rc.d15[45] = Sg_Intern(sg__rc.d15[46]); /* enum-type-universe */
  sg__rc.d15[44] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[45]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[49] = SG_MAKE_STRING("enum-type-indexer");
  sg__rc.d15[48] = Sg_Intern(sg__rc.d15[49]); /* enum-type-indexer */
  sg__rc.d15[47] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[48]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[51] = SG_MAKE_STRING("<enum-set>");
  sg__rc.d15[50] = Sg_Intern(sg__rc.d15[51]); /* <enum-set> */
  sg__rc.d15[54] = SG_MAKE_STRING("type");
  sg__rc.d15[53] = Sg_Intern(sg__rc.d15[54]); /* type */
  do {
    SgObject G22 = SG_NIL, G23 = SG_NIL;
    SG_APPEND1(G22, G23, sg__rc.d15[6]); /* immutable */ 
    SG_APPEND1(G22, G23, sg__rc.d15[53]); /* type */ 
    sg__rc.d15[52] = G22;
  } while (0);
  sg__rc.d15[57] = SG_MAKE_STRING("members");
  sg__rc.d15[56] = Sg_Intern(sg__rc.d15[57]); /* members */
  do {
    SgObject G24 = SG_NIL, G25 = SG_NIL;
    SG_APPEND1(G24, G25, sg__rc.d15[6]); /* immutable */ 
    SG_APPEND1(G24, G25, sg__rc.d15[56]); /* members */ 
    sg__rc.d15[55] = G24;
  } while (0);
  sg__rc.d15[58] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[50]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[61] = SG_MAKE_STRING("make-enum-set");
  sg__rc.d15[60] = Sg_Intern(sg__rc.d15[61]); /* make-enum-set */
  sg__rc.d15[59] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[60]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[64] = SG_MAKE_STRING("enum-set?");
  sg__rc.d15[63] = Sg_Intern(sg__rc.d15[64]); /* enum-set? */
  sg__rc.d15[62] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[63]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[67] = SG_MAKE_STRING("enum-set-type");
  sg__rc.d15[66] = Sg_Intern(sg__rc.d15[67]); /* enum-set-type */
  sg__rc.d15[65] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[66]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[70] = SG_MAKE_STRING("enum-set-members");
  sg__rc.d15[69] = Sg_Intern(sg__rc.d15[70]); /* enum-set-members */
  sg__rc.d15[68] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[69]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[73] = SG_MAKE_STRING("make-eq-hashtable");
  sg__rc.d15[72] = Sg_Intern(sg__rc.d15[73]); /* make-eq-hashtable */
  sg__rc.d15[71] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[72]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[76] = SG_MAKE_STRING("hashtable-set!");
  sg__rc.d15[75] = Sg_Intern(sg__rc.d15[76]); /* hashtable-set! */
  sg__rc.d15[74] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[75]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[79] = SG_MAKE_STRING("hashtable-ref");
  sg__rc.d15[78] = Sg_Intern(sg__rc.d15[79]); /* hashtable-ref */
  sg__rc.d15[77] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[78]), SG_NIL, (sg__rc.d15[0]));
  ((SgWord*)SG_OBJ(&sg__rc.d16[0]))[5] = SG_WORD(sg__rc.d15[77]);
  sg__rc.d15[81] = SG_MAKE_STRING("make-enumeration-type");
  sg__rc.d15[80] = Sg_Intern(sg__rc.d15[81]); /* make-enumeration-type */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d17[1]))->name = sg__rc.d15[80];/* make-enumeration-type */
  ((SgWord*)SG_OBJ(&sg__rc.d16[7]))[3] = SG_WORD(sg__rc.d15[71]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[7]))[18] = SG_WORD(sg__rc.d15[74]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[7]))[33] = SG_WORD(sg__rc.d15[29]);
  sg__rc.d15[82] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[80]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[85] = SG_MAKE_STRING("list?");
  sg__rc.d15[84] = Sg_Intern(sg__rc.d15[85]); /* list? */
  sg__rc.d15[83] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[84]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[88] = SG_MAKE_STRING("symbol?");
  sg__rc.d15[87] = Sg_Intern(sg__rc.d15[88]); /* symbol? */
  sg__rc.d15[86] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[87]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[91] = SG_MAKE_STRING("for-all");
  sg__rc.d15[90] = Sg_Intern(sg__rc.d15[91]); /* for-all */
  sg__rc.d15[89] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[90]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[93] = SG_MAKE_STRING("make-enumeration");
  sg__rc.d15[92] = Sg_Intern(sg__rc.d15[93]); /* make-enumeration */
  sg__rc.d15[94] = SG_MAKE_STRING("argument 1 must be a list of symbols");
  sg__rc.d15[97] = SG_MAKE_STRING("assertion-violation");
  sg__rc.d15[96] = Sg_Intern(sg__rc.d15[97]); /* assertion-violation */
  sg__rc.d15[95] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[96]), SG_NIL, (sg__rc.d15[0]));
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d17[2]))->name = sg__rc.d15[92];/* make-enumeration */
  ((SgWord*)SG_OBJ(&sg__rc.d16[42]))[4] = SG_WORD(sg__rc.d15[83]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[42]))[10] = SG_WORD(sg__rc.d15[86]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[42]))[13] = SG_WORD(sg__rc.d15[89]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[42]))[20] = SG_WORD(sg__rc.d15[82]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[42]))[24] = SG_WORD(sg__rc.d15[59]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[42]))[27] = SG_WORD(sg__rc.d15[92]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[42]))[29] = SG_WORD(sg__rc.d15[94]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[42]))[31] = SG_WORD(sg__rc.d15[95]);
  sg__rc.d15[98] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[92]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[100] = SG_MAKE_STRING("enum-set-universe");
  sg__rc.d15[99] = Sg_Intern(sg__rc.d15[100]); /* enum-set-universe */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d17[3]))->name = sg__rc.d15[99];/* enum-set-universe */
  ((SgWord*)SG_OBJ(&sg__rc.d16[78]))[4] = SG_WORD(sg__rc.d15[65]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[78]))[12] = SG_WORD(sg__rc.d15[65]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[78]))[15] = SG_WORD(sg__rc.d15[44]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[78]))[18] = SG_WORD(sg__rc.d15[59]);
  sg__rc.d15[101] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[99]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[103] = SG_MAKE_STRING("enum-set-indexer");
  sg__rc.d15[102] = Sg_Intern(sg__rc.d15[103]); /* enum-set-indexer */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d17[4]))->name = sg__rc.d15[102];/* enum-set-indexer */
  ((SgWord*)SG_OBJ(&sg__rc.d16[98]))[4] = SG_WORD(sg__rc.d15[65]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[98]))[7] = SG_WORD(sg__rc.d15[47]);
  sg__rc.d15[104] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[102]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[107] = SG_MAKE_STRING("memq");
  sg__rc.d15[106] = Sg_Intern(sg__rc.d15[107]); /* memq */
  sg__rc.d15[105] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[106]), SG_NIL, (sg__rc.d15[0]));
  ((SgWord*)SG_OBJ(&sg__rc.d16[107]))[3] = SG_WORD(sg__rc.d15[105]);
  sg__rc.d15[109] = SG_MAKE_STRING("enum-set-constructor");
  sg__rc.d15[108] = Sg_Intern(sg__rc.d15[109]); /* enum-set-constructor */
  sg__rc.d15[110] = SG_MAKE_STRING("the symbol list must all belong to the universe.");
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d17[6]))->name = sg__rc.d15[108];/* enum-set-constructor */
  ((SgWord*)SG_OBJ(&sg__rc.d16[112]))[6] = SG_WORD(sg__rc.d15[65]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[112]))[9] = SG_WORD(sg__rc.d15[44]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[112]))[19] = SG_WORD(sg__rc.d15[89]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[112]))[26] = SG_WORD(sg__rc.d15[65]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[112]))[30] = SG_WORD(sg__rc.d15[59]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[112]))[33] = SG_WORD(sg__rc.d15[108]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[112]))[35] = SG_WORD(sg__rc.d15[110]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[112]))[39] = SG_WORD(sg__rc.d15[95]);
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d17[7]))->name = sg__rc.d15[108];/* enum-set-constructor */
  sg__rc.d15[111] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[108]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[113] = SG_MAKE_STRING("loop");
  sg__rc.d15[112] = Sg_Intern(sg__rc.d15[113]); /* loop */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d17[8]))->name = sg__rc.d15[112];/* loop */
  ((SgWord*)SG_OBJ(&sg__rc.d16[157]))[10] = SG_WORD(sg__rc.d15[105]);
  sg__rc.d15[115] = SG_MAKE_STRING("enum-set->list");
  sg__rc.d15[114] = Sg_Intern(sg__rc.d15[115]); /* enum-set->list */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d17[9]))->name = sg__rc.d15[114];/* enum-set->list */
  ((SgWord*)SG_OBJ(&sg__rc.d16[184]))[6] = SG_WORD(sg__rc.d15[65]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[184]))[9] = SG_WORD(sg__rc.d15[44]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[184]))[15] = SG_WORD(sg__rc.d15[68]);
  sg__rc.d15[116] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[114]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[118] = SG_MAKE_STRING("enum-set-member?");
  sg__rc.d15[117] = Sg_Intern(sg__rc.d15[118]); /* enum-set-member? */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d17[10]))->name = sg__rc.d15[117];/* enum-set-member? */
  ((SgWord*)SG_OBJ(&sg__rc.d16[214]))[7] = SG_WORD(sg__rc.d15[68]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[214]))[10] = SG_WORD(sg__rc.d15[105]);
  sg__rc.d15[119] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[117]), SG_NIL, (sg__rc.d15[0]));
  ((SgWord*)SG_OBJ(&sg__rc.d16[230]))[3] = SG_WORD(sg__rc.d15[105]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[235]))[3] = SG_WORD(sg__rc.d15[119]);
  sg__rc.d15[121] = SG_MAKE_STRING("enum-set-subset?");
  sg__rc.d15[120] = Sg_Intern(sg__rc.d15[121]); /* enum-set-subset? */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d17[13]))->name = sg__rc.d15[120];/* enum-set-subset? */
  ((SgWord*)SG_OBJ(&sg__rc.d16[240]))[6] = SG_WORD(sg__rc.d15[101]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[240]))[9] = SG_WORD(sg__rc.d15[116]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[240]))[23] = SG_WORD(sg__rc.d15[101]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[240]))[26] = SG_WORD(sg__rc.d15[116]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[240]))[29] = SG_WORD(sg__rc.d15[89]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[240]))[41] = SG_WORD(sg__rc.d15[68]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[240]))[44] = SG_WORD(sg__rc.d15[89]);
  sg__rc.d15[122] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[120]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[124] = SG_MAKE_STRING("enum-set=?");
  sg__rc.d15[123] = Sg_Intern(sg__rc.d15[124]); /* enum-set=? */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d17[14]))->name = sg__rc.d15[123];/* enum-set=? */
  ((SgWord*)SG_OBJ(&sg__rc.d16[286]))[5] = SG_WORD(sg__rc.d15[122]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[286]))[11] = SG_WORD(sg__rc.d15[122]);
  sg__rc.d15[125] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[123]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[127] = SG_MAKE_STRING("enum-set-union");
  sg__rc.d15[126] = Sg_Intern(sg__rc.d15[127]); /* enum-set-union */
  sg__rc.d15[128] = SG_MAKE_STRING("enum-set1 and enum-set2 must be enumeration sets that have the same enumeration type.");
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d17[15]))->name = sg__rc.d15[126];/* enum-set-union */
  ((SgWord*)SG_OBJ(&sg__rc.d16[299]))[5] = SG_WORD(sg__rc.d15[65]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[299]))[11] = SG_WORD(sg__rc.d15[65]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[299]))[18] = SG_WORD(sg__rc.d15[65]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[299]))[24] = SG_WORD(sg__rc.d15[68]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[299]))[30] = SG_WORD(sg__rc.d15[68]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[299]))[45] = SG_WORD(sg__rc.d15[105]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[299]))[66] = SG_WORD(sg__rc.d15[59]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[299]))[69] = SG_WORD(sg__rc.d15[126]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[299]))[71] = SG_WORD(sg__rc.d15[128]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[299]))[73] = SG_WORD(sg__rc.d15[95]);
  sg__rc.d15[129] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[126]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[131] = SG_MAKE_STRING("enum-set-intersection");
  sg__rc.d15[130] = Sg_Intern(sg__rc.d15[131]); /* enum-set-intersection */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d17[16]))->name = sg__rc.d15[130];/* enum-set-intersection */
  ((SgWord*)SG_OBJ(&sg__rc.d16[374]))[5] = SG_WORD(sg__rc.d15[65]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[374]))[11] = SG_WORD(sg__rc.d15[65]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[374]))[18] = SG_WORD(sg__rc.d15[65]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[374]))[24] = SG_WORD(sg__rc.d15[68]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[374]))[30] = SG_WORD(sg__rc.d15[68]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[374]))[46] = SG_WORD(sg__rc.d15[105]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[374]))[67] = SG_WORD(sg__rc.d15[59]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[374]))[70] = SG_WORD(sg__rc.d15[130]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[374]))[72] = SG_WORD(sg__rc.d15[128]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[374]))[74] = SG_WORD(sg__rc.d15[95]);
  sg__rc.d15[132] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[130]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[134] = SG_MAKE_STRING("enum-set-difference");
  sg__rc.d15[133] = Sg_Intern(sg__rc.d15[134]); /* enum-set-difference */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d17[17]))->name = sg__rc.d15[133];/* enum-set-difference */
  ((SgWord*)SG_OBJ(&sg__rc.d16[450]))[5] = SG_WORD(sg__rc.d15[65]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[450]))[11] = SG_WORD(sg__rc.d15[65]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[450]))[18] = SG_WORD(sg__rc.d15[65]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[450]))[24] = SG_WORD(sg__rc.d15[68]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[450]))[30] = SG_WORD(sg__rc.d15[68]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[450]))[46] = SG_WORD(sg__rc.d15[105]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[450]))[67] = SG_WORD(sg__rc.d15[59]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[450]))[70] = SG_WORD(sg__rc.d15[133]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[450]))[72] = SG_WORD(sg__rc.d15[128]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[450]))[74] = SG_WORD(sg__rc.d15[95]);
  sg__rc.d15[135] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[133]), SG_NIL, (sg__rc.d15[0]));
  ((SgWord*)SG_OBJ(&sg__rc.d16[526]))[5] = SG_WORD(sg__rc.d15[105]);
  sg__rc.d15[138] = SG_MAKE_STRING("filter");
  sg__rc.d15[137] = Sg_Intern(sg__rc.d15[138]); /* filter */
  sg__rc.d15[136] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[137]), SG_NIL, (sg__rc.d15[0]));
  sg__rc.d15[140] = SG_MAKE_STRING("enum-set-complement");
  sg__rc.d15[139] = Sg_Intern(sg__rc.d15[140]); /* enum-set-complement */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d17[19]))->name = sg__rc.d15[139];/* enum-set-complement */
  ((SgWord*)SG_OBJ(&sg__rc.d16[534]))[4] = SG_WORD(sg__rc.d15[68]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[534]))[10] = SG_WORD(sg__rc.d15[65]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[534]))[24] = SG_WORD(sg__rc.d15[65]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[534]))[27] = SG_WORD(sg__rc.d15[44]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[534]))[30] = SG_WORD(sg__rc.d15[136]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[534]))[33] = SG_WORD(sg__rc.d15[59]);
  sg__rc.d15[141] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[139]), SG_NIL, (sg__rc.d15[0]));
  ((SgWord*)SG_OBJ(&sg__rc.d16[569]))[3] = SG_WORD(sg__rc.d15[105]);
  sg__rc.d15[143] = SG_MAKE_STRING("enum-set-projection");
  sg__rc.d15[142] = Sg_Intern(sg__rc.d15[143]); /* enum-set-projection */
  SG_CODE_BUILDER(SG_OBJ(&sg__rc.d17[21]))->name = sg__rc.d15[142];/* enum-set-projection */
  ((SgWord*)SG_OBJ(&sg__rc.d16[574]))[5] = SG_WORD(sg__rc.d15[122]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[574]))[16] = SG_WORD(sg__rc.d15[65]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[574]))[19] = SG_WORD(sg__rc.d15[44]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[574]))[25] = SG_WORD(sg__rc.d15[68]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[574]))[31] = SG_WORD(sg__rc.d15[65]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[574]))[41] = SG_WORD(sg__rc.d15[136]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[574]))[44] = SG_WORD(sg__rc.d15[59]);
  sg__rc.d15[144] = Sg_MakeIdentifier(SG_SYMBOL(sg__rc.d15[142]), SG_NIL, (sg__rc.d15[0]));
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[1] = SG_WORD(sg__rc.d15[0]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[5] = SG_WORD(sg__rc.d15[3]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[15] = SG_WORD(sg__rc.d15[5]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[17] = SG_WORD(sg__rc.d15[10]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[21] = SG_WORD(sg__rc.d15[13]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[31] = SG_WORD(sg__rc.d15[16]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[36] = SG_WORD(sg__rc.d15[3]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[40] = SG_WORD(sg__rc.d15[19]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[44] = SG_WORD(sg__rc.d15[22]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[50] = SG_WORD(sg__rc.d15[22]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[52] = SG_WORD(sg__rc.d15[23]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[55] = SG_WORD(sg__rc.d15[26]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[57] = SG_WORD(sg__rc.d15[29]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[63] = SG_WORD(sg__rc.d15[22]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[65] = SG_WORD(sg__rc.d15[32]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[68] = SG_WORD(sg__rc.d15[35]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[70] = SG_WORD(sg__rc.d15[38]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[76] = SG_WORD(sg__rc.d15[22]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[78] = SG_WORD(sg__rc.d15[32]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[82] = SG_WORD(sg__rc.d15[41]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[84] = SG_WORD(sg__rc.d15[44]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[90] = SG_WORD(sg__rc.d15[22]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[92] = SG_WORD(sg__rc.d15[32]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[96] = SG_WORD(sg__rc.d15[41]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[98] = SG_WORD(sg__rc.d15[47]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[102] = SG_WORD(sg__rc.d15[50]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[112] = SG_WORD(sg__rc.d15[52]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[114] = SG_WORD(sg__rc.d15[55]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[118] = SG_WORD(sg__rc.d15[13]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[128] = SG_WORD(sg__rc.d15[16]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[133] = SG_WORD(sg__rc.d15[50]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[137] = SG_WORD(sg__rc.d15[19]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[141] = SG_WORD(sg__rc.d15[58]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[147] = SG_WORD(sg__rc.d15[58]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[149] = SG_WORD(sg__rc.d15[23]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[152] = SG_WORD(sg__rc.d15[26]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[154] = SG_WORD(sg__rc.d15[59]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[160] = SG_WORD(sg__rc.d15[58]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[162] = SG_WORD(sg__rc.d15[32]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[165] = SG_WORD(sg__rc.d15[35]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[167] = SG_WORD(sg__rc.d15[62]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[173] = SG_WORD(sg__rc.d15[58]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[175] = SG_WORD(sg__rc.d15[32]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[179] = SG_WORD(sg__rc.d15[41]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[181] = SG_WORD(sg__rc.d15[65]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[187] = SG_WORD(sg__rc.d15[58]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[189] = SG_WORD(sg__rc.d15[32]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[193] = SG_WORD(sg__rc.d15[41]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[195] = SG_WORD(sg__rc.d15[68]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[199] = SG_WORD(sg__rc.d15[82]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[203] = SG_WORD(sg__rc.d15[98]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[207] = SG_WORD(sg__rc.d15[101]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[211] = SG_WORD(sg__rc.d15[104]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[215] = SG_WORD(sg__rc.d15[111]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[219] = SG_WORD(sg__rc.d15[116]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[223] = SG_WORD(sg__rc.d15[119]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[227] = SG_WORD(sg__rc.d15[122]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[231] = SG_WORD(sg__rc.d15[125]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[235] = SG_WORD(sg__rc.d15[129]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[239] = SG_WORD(sg__rc.d15[132]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[243] = SG_WORD(sg__rc.d15[135]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[247] = SG_WORD(sg__rc.d15[141]);
  ((SgWord*)SG_OBJ(&sg__rc.d16[620]))[251] = SG_WORD(sg__rc.d15[144]);
  sg__rc.d15[146] = SG_MAKE_STRING("null");
  sg__rc.d15[145] = Sg_Intern(sg__rc.d15[146]); /* null */
  Sg_ImportLibrary(sg__rc.d15[0], sg__rc.d15[145]);

  sg__rc.d15[148] = SG_MAKE_STRING("(core base)");
  sg__rc.d15[147] = Sg_Intern(sg__rc.d15[148]); /* (core base) */
  Sg_ImportLibrary(sg__rc.d15[0], sg__rc.d15[147]);

  sg__rc.d15[150] = SG_MAKE_STRING("(sagittarius)");
  sg__rc.d15[149] = Sg_Intern(sg__rc.d15[150]); /* (sagittarius) */
  Sg_ImportLibrary(sg__rc.d15[0], sg__rc.d15[149]);

  SG_APPEND1(h, t, sg__rc.d15[92]); /* make-enumeration */
  SG_APPEND1(h, t, sg__rc.d15[99]); /* enum-set-universe */
  SG_APPEND1(h, t, sg__rc.d15[102]); /* enum-set-indexer */
  SG_APPEND1(h, t, sg__rc.d15[108]); /* enum-set-constructor */
  SG_APPEND1(h, t, sg__rc.d15[114]); /* enum-set->list */
  SG_APPEND1(h, t, sg__rc.d15[117]); /* enum-set-member? */
  SG_APPEND1(h, t, sg__rc.d15[120]); /* enum-set-subset? */
  SG_APPEND1(h, t, sg__rc.d15[123]); /* enum-set=? */
  SG_APPEND1(h, t, sg__rc.d15[126]); /* enum-set-union */
  SG_APPEND1(h, t, sg__rc.d15[130]); /* enum-set-intersection */
  SG_APPEND1(h, t, sg__rc.d15[133]); /* enum-set-difference */
  SG_APPEND1(h, t, sg__rc.d15[139]); /* enum-set-complement */
  SG_APPEND1(h, t, sg__rc.d15[142]); /* enum-set-projection */
  sg__rc.d15[152] = SG_MAKE_STRING("define-enumeration");
  sg__rc.d15[151] = Sg_Intern(sg__rc.d15[152]); /* define-enumeration */
  SG_APPEND1(h, t, sg__rc.d15[151]); /* define-enumeration */
  Sg_LibraryExportedSet(sg__rc.d15[0], Sg_Cons(h, SG_NIL));

  Sg_VM()->currentLibrary = sg__rc.d15[0];
  Sg_VMExecute(SG_OBJ(toplevel));
  Sg_VM()->currentLibrary = save;
}
