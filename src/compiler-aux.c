/* This file is automatically generated. DO NOT EDIT!*/
#define LIBSAGITTARIUS_BODY
#include "sagittarius.h"
#define WORD(x)       SG_WORD(x)
#define SYMBOL(x)     Sg_Intern(Sg_MakeString(UC(x), SG_LITERAL_STRING))
#define SYMBOLW(x)    WORD(SYMBOL(x))
#define UISYMBOL(x)   Sg_MakeSymbol(Sg_MakeString(UC(x), SG_LITERAL_STRING), FALSE)
#define UISYMBOLW(x)  WORD(UISYMBOL(x))
#define STRING(x)     Sg_MakeString(UC(x), SG_LITERAL_STRING)
#define STRINGW(x)    WORD(STRING(x))
#define KEYWORD(x)    Sg_MakeKeyword(STRING(x))
#define KEYWORDW(x)   WORD(KEYWORD(x))
#define IDENT(x, lib) WORD(Sg_MakeIdentifier(SYMBOL(x), SG_NIL, (lib)))
#define UNSIDENT(x, lib) WORD(Sg_MakeIdentifier(UISYMBOL(x), SG_NIL, (lib)))
static struct sg__wcRec {
  SgCodeBuilder cb[3];
  SgWord        w[130];
} sg__wc = {
  { /* code builder */
    SG_STATIC_CODE_BUILDER(&sg__wc.w[71], NULL, 1, FALSE, 0, 13, 30), /* ensure-library-name */
    SG_STATIC_CODE_BUILDER(&sg__wc.w[101], NULL, 1, FALSE, 0, 15, 29), /* parse-args */
  },
  { /* compiled code */
  /*     0 */        0x00000038           /* 0      (LIBRARY) */,
  /*     1 */        WORD(SG_UNDEF)  /* <library (sagittarius compiler util)> */,
  /*     2 */        0x0000002A           /* 2      (CLOSURE) */,
  /*     3 */        WORD(SG_UNDEF)  /* <code-builder> */,
  /*     4 */        0x00000037           /* 4      (DEFINE) */,
  /*     5 */        WORD(SG_UNDEF)  /* identifier#ensure-library-name */,
  /*     6 */        0x0000002A           /* 6      (CLOSURE) */,
  /*     7 */        WORD(SG_UNDEF)  /* <code-builder> */,
  /*     8 */        0x00000037           /* 8      (DEFINE) */,
  /*     9 */        WORD(SG_UNDEF)  /* identifier#parse-args */,
  /*    10 */        0x00000002           /* 10     (UNDEF) */,
  /*    11 */        0x00000003           /* 11     (CONST) */,
  /*    12 */        WORD(SG_UNDEF), /* (($UNDEF . 0) ($DEFINE . 1) ($LREF . 2) ($LSET . 3) ($GREF . 4) ($GSET . 5) ($CONST . 6) ($IF . 7) ($LET . 8) ($LAMBDA . 9) ($RECEIVE . 10) ($LABEL . 11) ($SEQ . 12) ($CALL . 13) ($ASM . 14) ($IT . 15) ($LIST . 16) ($LIBRARY . 17)) */
  /*    13 */        0x00000137           /* 13     (DEFINE) */,
  /*    14 */        WORD(SG_UNDEF)  /* identifier#|.intermediate-tags.| */,
  /*    15 */        0x00000004           /* 15     (CONSTI) */,
  /*    16 */        0x00000137           /* 16     (DEFINE) */,
  /*    17 */        WORD(SG_UNDEF)  /* identifier#$UNDEF */,
  /*    18 */        0x00000104           /* 18     (CONSTI) */,
  /*    19 */        0x00000137           /* 19     (DEFINE) */,
  /*    20 */        WORD(SG_UNDEF)  /* identifier#$DEFINE */,
  /*    21 */        0x00000204           /* 21     (CONSTI) */,
  /*    22 */        0x00000137           /* 22     (DEFINE) */,
  /*    23 */        WORD(SG_UNDEF)  /* identifier#$LREF */,
  /*    24 */        0x00000304           /* 24     (CONSTI) */,
  /*    25 */        0x00000137           /* 25     (DEFINE) */,
  /*    26 */        WORD(SG_UNDEF)  /* identifier#$LSET */,
  /*    27 */        0x00000404           /* 27     (CONSTI) */,
  /*    28 */        0x00000137           /* 28     (DEFINE) */,
  /*    29 */        WORD(SG_UNDEF)  /* identifier#$GREF */,
  /*    30 */        0x00000504           /* 30     (CONSTI) */,
  /*    31 */        0x00000137           /* 31     (DEFINE) */,
  /*    32 */        WORD(SG_UNDEF)  /* identifier#$GSET */,
  /*    33 */        0x00000604           /* 33     (CONSTI) */,
  /*    34 */        0x00000137           /* 34     (DEFINE) */,
  /*    35 */        WORD(SG_UNDEF)  /* identifier#$CONST */,
  /*    36 */        0x00000704           /* 36     (CONSTI) */,
  /*    37 */        0x00000137           /* 37     (DEFINE) */,
  /*    38 */        WORD(SG_UNDEF)  /* identifier#$IF */,
  /*    39 */        0x00000804           /* 39     (CONSTI) */,
  /*    40 */        0x00000137           /* 40     (DEFINE) */,
  /*    41 */        WORD(SG_UNDEF)  /* identifier#$LET */,
  /*    42 */        0x00000904           /* 42     (CONSTI) */,
  /*    43 */        0x00000137           /* 43     (DEFINE) */,
  /*    44 */        WORD(SG_UNDEF)  /* identifier#$LAMBDA */,
  /*    45 */        0x00000A04           /* 45     (CONSTI) */,
  /*    46 */        0x00000137           /* 46     (DEFINE) */,
  /*    47 */        WORD(SG_UNDEF)  /* identifier#$RECEIVE */,
  /*    48 */        0x00000B04           /* 48     (CONSTI) */,
  /*    49 */        0x00000137           /* 49     (DEFINE) */,
  /*    50 */        WORD(SG_UNDEF)  /* identifier#$LABEL */,
  /*    51 */        0x00000C04           /* 51     (CONSTI) */,
  /*    52 */        0x00000137           /* 52     (DEFINE) */,
  /*    53 */        WORD(SG_UNDEF)  /* identifier#$SEQ */,
  /*    54 */        0x00000D04           /* 54     (CONSTI) */,
  /*    55 */        0x00000137           /* 55     (DEFINE) */,
  /*    56 */        WORD(SG_UNDEF)  /* identifier#$CALL */,
  /*    57 */        0x00000E04           /* 57     (CONSTI) */,
  /*    58 */        0x00000137           /* 58     (DEFINE) */,
  /*    59 */        WORD(SG_UNDEF)  /* identifier#$ASM */,
  /*    60 */        0x00000F04           /* 60     (CONSTI) */,
  /*    61 */        0x00000137           /* 61     (DEFINE) */,
  /*    62 */        WORD(SG_UNDEF)  /* identifier#$IT */,
  /*    63 */        0x00001004           /* 63     (CONSTI) */,
  /*    64 */        0x00000137           /* 64     (DEFINE) */,
  /*    65 */        WORD(SG_UNDEF)  /* identifier#$LIST */,
  /*    66 */        0x00001104           /* 66     (CONSTI) */,
  /*    67 */        0x00000137           /* 67     (DEFINE) */,
  /*    68 */        WORD(SG_UNDEF)  /* identifier#$LIBRARY */,
  /*    69 */        0x00000002           /* 69     (UNDEF) */,
  /*    70 */        0x00000030           /* 70     (RET) */,
  /*    71 */        0x0000004C           /* 0      (CONST_PUSH) */,
  /*    72 */        WORD(SG_UNDEF)  /* <keyrowd :null> */,
  /*    73 */        0x00000005           /* 2      (LREF) */,
  /*    74 */        0x00000021           /* 3      (BNEQV) */,
  /*    75 */        WORD(SG_MAKE_INT(4)),
  /*    76 */        0x00000003           /* 5      (CONST) */,
  /*    77 */        WORD(SG_UNDEF), /* null */
  /*    78 */        0x00000030           /* 7      (RET) */,
  /*    79 */        0x0000004C           /* 8      (CONST_PUSH) */,
  /*    80 */        WORD(SG_UNDEF)  /* <keyrowd :sagittarius> */,
  /*    81 */        0x00000005           /* 10     (LREF) */,
  /*    82 */        0x00000021           /* 11     (BNEQV) */,
  /*    83 */        WORD(SG_MAKE_INT(4)),
  /*    84 */        0x00000003           /* 13     (CONST) */,
  /*    85 */        WORD(SG_UNDEF), /* (sagittarius) */
  /*    86 */        0x00000030           /* 15     (RET) */,
  /*    87 */        0x0000004C           /* 16     (CONST_PUSH) */,
  /*    88 */        WORD(SG_UNDEF)  /* <keyrowd :base> */,
  /*    89 */        0x00000005           /* 18     (LREF) */,
  /*    90 */        0x00000021           /* 19     (BNEQV) */,
  /*    91 */        WORD(SG_MAKE_INT(4)),
  /*    92 */        0x00000003           /* 21     (CONST) */,
  /*    93 */        WORD(SG_UNDEF), /* (core base) */
  /*    94 */        0x00000030           /* 23     (RET) */,
  /*    95 */        0x0000004C           /* 24     (CONST_PUSH) */,
  /*    96 */        WORD(SG_UNDEF), /* "invalid library tag:" */
  /*    97 */        0x00000049           /* 26     (LREF_PUSH) */,
  /*    98 */        0x0000024F           /* 27     (GREF_TAIL_CALL) */,
  /*    99 */        WORD(SG_UNDEF)  /* identifier#error */,
  /*   100 */        0x00000030           /* 29     (RET) */,
  /*   101 */        0x00000049           /* 0      (LREF_PUSH) */,
  /*   102 */        0x0000004D           /* 1      (CONSTI_PUSH) */,
  /*   103 */        0x00000335           /* 2      (ENTER) */,
  /*   104 */        0x00000105           /* 3      (LREF) */,
  /*   105 */        0x00000022           /* 4      (BNNULL) */,
  /*   106 */        WORD(SG_MAKE_INT(6)),
  /*   107 */        0x00000249           /* 6      (LREF_PUSH) */,
  /*   108 */        0x00000003           /* 7      (CONST) */,
  /*   109 */        WORD(SG_MAKE_BOOL(FALSE)),
  /*   110 */        0x0000023E           /* 9      (VALUES) */,
  /*   111 */        0x00000030           /* 10     (RET) */,
  /*   112 */        0x00000105           /* 11     (LREF) */,
  /*   113 */        0x00000042           /* 12     (PAIRP) */,
  /*   114 */        0x00000017           /* 13     (TEST) */,
  /*   115 */        WORD(SG_MAKE_INT(9)),
  /*   116 */        0x00000160           /* 15     (LREF_CDR_PUSH) */,
  /*   117 */        0x00000205           /* 16     (LREF) */,
  /*   118 */        0x0000010F           /* 17     (ADDI) */,
  /*   119 */        0x0000000B           /* 18     (PUSH) */,
  /*   120 */        0x00100219           /* 19     (SHIFTJ) */,
  /*   121 */        0x00000018           /* 20     (JUMP) */,
  /*   122 */        WORD(SG_MAKE_INT(-18)),
  /*   123 */        0x00000030           /* 22     (RET) */,
  /*   124 */        0x00000249           /* 23     (LREF_PUSH) */,
  /*   125 */        0x00000003           /* 24     (CONST) */,
  /*   126 */        WORD(SG_MAKE_BOOL(TRUE)),
  /*   127 */        0x0000023E           /* 26     (VALUES) */,
  /*   128 */        0x00000236           /* 27     (LEAVE) */,
  /*   129 */        0x00000030           /* 28     (RET) */,
  }
};

static SgCodeBuilder toplevel_sagittarius_compiler_util = SG_STATIC_CODE_BUILDER(&sg__wc.w[0], SG_FALSE, 0, FALSE, 0, 0, 71);
void Sg__Init_sagittarius_compiler_util()
{
  SgLibrary *lib = Sg_FindLibrary(SYMBOL("(sagittarius compiler util)"), TRUE);
  SgLibrary *save = Sg_VM()->currentLibrary;
  sg__wc.w[1] = SYMBOLW("(sagittarius compiler util)");
  sg__wc.w[85] = WORD(Sg_Cons(SYMBOL("sagittarius"), SG_NIL));
  sg__wc.w[80] = KEYWORDW("sagittarius");
  sg__wc.w[72] = KEYWORDW("null");
  sg__wc.w[96] = STRINGW("invalid library tag:");
  sg__wc.w[88] = KEYWORDW("base");
  sg__wc.w[93] = WORD(Sg_Cons(SYMBOL("core"), Sg_Cons(SYMBOL("base"), SG_NIL)));
  sg__wc.w[7] = WORD(&sg__wc.cb[1]);
  sg__wc.cb[1].name = SYMBOL("parse-args");
  sg__wc.w[3] = WORD(&sg__wc.cb[0]);
  sg__wc.cb[0].name = SYMBOL("ensure-library-name");
  sg__wc.w[12] = WORD(Sg_Cons(Sg_Cons(SYMBOL("$UNDEF"), SG_MAKE_INT(0)), Sg_Cons(Sg_Cons(SYMBOL("$DEFINE"), SG_MAKE_INT(1)), Sg_Cons(Sg_Cons(SYMBOL("$LREF"), SG_MAKE_INT(2)), Sg_Cons(Sg_Cons(SYMBOL("$LSET"), SG_MAKE_INT(3)), Sg_Cons(Sg_Cons(SYMBOL("$GREF"), SG_MAKE_INT(4)), Sg_Cons(Sg_Cons(SYMBOL("$GSET"), SG_MAKE_INT(5)), Sg_Cons(Sg_Cons(SYMBOL("$CONST"), SG_MAKE_INT(6)), Sg_Cons(Sg_Cons(SYMBOL("$IF"), SG_MAKE_INT(7)), Sg_Cons(Sg_Cons(SYMBOL("$LET"), SG_MAKE_INT(8)), Sg_Cons(Sg_Cons(SYMBOL("$LAMBDA"), SG_MAKE_INT(9)), Sg_Cons(Sg_Cons(SYMBOL("$RECEIVE"), SG_MAKE_INT(10)), Sg_Cons(Sg_Cons(SYMBOL("$LABEL"), SG_MAKE_INT(11)), Sg_Cons(Sg_Cons(SYMBOL("$SEQ"), SG_MAKE_INT(12)), Sg_Cons(Sg_Cons(SYMBOL("$CALL"), SG_MAKE_INT(13)), Sg_Cons(Sg_Cons(SYMBOL("$ASM"), SG_MAKE_INT(14)), Sg_Cons(Sg_Cons(SYMBOL("$IT"), SG_MAKE_INT(15)), Sg_Cons(Sg_Cons(SYMBOL("$LIST"), SG_MAKE_INT(16)), Sg_Cons(Sg_Cons(SYMBOL("$LIBRARY"), SG_MAKE_INT(17)), SG_NIL)))))))))))))))))));
  sg__wc.w[68] = IDENT("$LIBRARY", lib);
  sg__wc.w[5] = IDENT("ensure-library-name", lib);
  sg__wc.w[20] = IDENT("$DEFINE", lib);
  sg__wc.w[35] = IDENT("$CONST", lib);
  sg__wc.w[50] = IDENT("$LABEL", lib);
  sg__wc.w[65] = IDENT("$LIST", lib);
  sg__wc.w[14] = IDENT(".intermediate-tags.", lib);
  sg__wc.w[99] = IDENT("error", lib);
  sg__wc.w[17] = IDENT("$UNDEF", lib);
  sg__wc.w[32] = IDENT("$GSET", lib);
  sg__wc.w[47] = IDENT("$RECEIVE", lib);
  sg__wc.w[62] = IDENT("$IT", lib);
  sg__wc.w[9] = IDENT("parse-args", lib);
  sg__wc.w[29] = IDENT("$GREF", lib);
  sg__wc.w[44] = IDENT("$LAMBDA", lib);
  sg__wc.w[59] = IDENT("$ASM", lib);
  sg__wc.w[26] = IDENT("$LSET", lib);
  sg__wc.w[41] = IDENT("$LET", lib);
  sg__wc.w[56] = IDENT("$CALL", lib);
  sg__wc.w[23] = IDENT("$LREF", lib);
  sg__wc.w[38] = IDENT("$IF", lib);
  sg__wc.w[53] = IDENT("$SEQ", lib);
  sg__wc.w[77] = SYMBOLW("null");
  Sg_ImportLibrary(lib, SG_OBJ(SYMBOL("(sagittarius)")));
  Sg_ImportLibrary(lib, SG_OBJ(SYMBOL("(core base)")));
  Sg_ImportLibrary(lib, SG_OBJ(SYMBOL("null")));
  Sg_LibraryExportedSet(lib, Sg_Cons(Sg_Cons(SYMBOL("$UNDEF"), Sg_Cons(SYMBOL("$DEFINE"), Sg_Cons(SYMBOL("$LREF"), Sg_Cons(SYMBOL("$LSET"), Sg_Cons(SYMBOL("$GREF"), Sg_Cons(SYMBOL("$GSET"), Sg_Cons(SYMBOL("$CONST"), Sg_Cons(SYMBOL("$IF"), Sg_Cons(SYMBOL("$LET"), Sg_Cons(SYMBOL("$LAMBDA"), Sg_Cons(SYMBOL("$RECEIVE"), Sg_Cons(SYMBOL("$LABEL"), Sg_Cons(SYMBOL("$SEQ"), Sg_Cons(SYMBOL("$CALL"), Sg_Cons(SYMBOL("$ASM"), Sg_Cons(SYMBOL("$IT"), Sg_Cons(SYMBOL("$LIST"), Sg_Cons(SYMBOL("$LIBRARY"), Sg_Cons(SYMBOL(".intermediate-tags."), Sg_Cons(SYMBOL("define-simple-struct"), Sg_Cons(SYMBOL("define-enum"), Sg_Cons(SYMBOL("case/unquote"), Sg_Cons(SYMBOL("parse-args"), Sg_Cons(SYMBOL("ensure-library-name"), SG_NIL)))))))))))))))))))))))), SG_NIL));
  Sg_VM()->currentLibrary = lib;
  Sg_VMExecute(SG_OBJ(&toplevel_sagittarius_compiler_util));
  Sg_VM()->currentLibrary = save;
}
