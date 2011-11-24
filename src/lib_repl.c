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
  SgCodeBuilder cb[14];
  SgWord        w[371];
} sg__wc = {
  { /* code builder */
    SG_STATIC_CODE_BUILDER(&sg__wc.w[72], NULL, 1, FALSE, 0, 8, 8), /* (read-eval-print-loop #:G10683) */
    SG_STATIC_CODE_BUILDER(&sg__wc.w[80], NULL, 2, TRUE, 0, 9, 4), /* default-exception-printer */
    SG_STATIC_CODE_BUILDER(&sg__wc.w[84], NULL, 1, FALSE, 0, 12, 28), /* #f */
    SG_STATIC_CODE_BUILDER(&sg__wc.w[112], NULL, 2, FALSE, 0, 10, 5), /* default-evaluator */
    SG_STATIC_CODE_BUILDER(&sg__wc.w[117], NULL, 1, FALSE, 0, 12, 28), /* #f */
    SG_STATIC_CODE_BUILDER(&sg__wc.w[145], NULL, 1, TRUE, 0, 9, 6), /* default-printer */
    SG_STATIC_CODE_BUILDER(&sg__wc.w[151], NULL, 1, FALSE, 0, 12, 28), /* #f */
    SG_STATIC_CODE_BUILDER(&sg__wc.w[179], NULL, 0, FALSE, 0, 7, 5), /* default-prompter */
    SG_STATIC_CODE_BUILDER(&sg__wc.w[184], NULL, 1, FALSE, 0, 12, 28), /* #f */
    SG_STATIC_CODE_BUILDER(&sg__wc.w[212], NULL, 0, FALSE, 0, 15, 39), /* read-eval-print-loop */
    SG_STATIC_CODE_BUILDER(&sg__wc.w[251], NULL, 1, FALSE, 2, 9, 12), /* #f */
    SG_STATIC_CODE_BUILDER(&sg__wc.w[263], NULL, 1, FALSE, 1, 8, 27), /* #f */
    SG_STATIC_CODE_BUILDER(&sg__wc.w[290], NULL, 0, FALSE, 2, 19, 81), /* #f */
  },
  { /* compiled code */
  /*     0 */        0x00000029           /* 0      (CLOSURE) */,
  /*     1 */        WORD(SG_UNDEF)  /* <code-builder> */,
  /*     2 */        0x00000033           /* 2      (DEFINE) */,
  /*     3 */        WORD(SG_UNDEF)  /* identifier#G10683 */,
  /*     4 */        0x00000034           /* 4      (LIBRARY) */,
  /*     5 */        WORD(SG_UNDEF)  /* <library (sagittarius interactive)> */,
  /*     6 */        0x00000029           /* 6      (CLOSURE) */,
  /*     7 */        WORD(SG_UNDEF)  /* <code-builder> */,
  /*     8 */        0x00000033           /* 8      (DEFINE) */,
  /*     9 */        WORD(SG_UNDEF)  /* identifier#default-exception-printer */,
  /*    10 */        0x00000030           /* 10     (FRAME) */,
  /*    11 */        WORD(SG_MAKE_INT(8)),
  /*    12 */        0x00000047           /* 12     (GREF_PUSH) */,
  /*    13 */        WORD(SG_UNDEF)  /* identifier#default-exception-printer */,
  /*    14 */        0x00000029           /* 14     (CLOSURE) */,
  /*    15 */        WORD(SG_UNDEF)  /* <code-builder> */,
  /*    16 */        0x0000000B           /* 16     (PUSH) */,
  /*    17 */        0x0000024A           /* 17     (GREF_CALL) */,
  /*    18 */        WORD(SG_UNDEF)  /* identifier#make-parameter */,
  /*    19 */        0x00000033           /* 19     (DEFINE) */,
  /*    20 */        WORD(SG_UNDEF)  /* identifier#current-exception-printer */,
  /*    21 */        0x00000029           /* 21     (CLOSURE) */,
  /*    22 */        WORD(SG_UNDEF)  /* <code-builder> */,
  /*    23 */        0x00000033           /* 23     (DEFINE) */,
  /*    24 */        WORD(SG_UNDEF)  /* identifier#default-evaluator */,
  /*    25 */        0x00000030           /* 25     (FRAME) */,
  /*    26 */        WORD(SG_MAKE_INT(8)),
  /*    27 */        0x00000047           /* 27     (GREF_PUSH) */,
  /*    28 */        WORD(SG_UNDEF)  /* identifier#default-evaluator */,
  /*    29 */        0x00000029           /* 29     (CLOSURE) */,
  /*    30 */        WORD(SG_UNDEF)  /* <code-builder> */,
  /*    31 */        0x0000000B           /* 31     (PUSH) */,
  /*    32 */        0x0000024A           /* 32     (GREF_CALL) */,
  /*    33 */        WORD(SG_UNDEF)  /* identifier#make-parameter */,
  /*    34 */        0x00000033           /* 34     (DEFINE) */,
  /*    35 */        WORD(SG_UNDEF)  /* identifier#current-evaluator */,
  /*    36 */        0x00000029           /* 36     (CLOSURE) */,
  /*    37 */        WORD(SG_UNDEF)  /* <code-builder> */,
  /*    38 */        0x00000033           /* 38     (DEFINE) */,
  /*    39 */        WORD(SG_UNDEF)  /* identifier#default-printer */,
  /*    40 */        0x00000030           /* 40     (FRAME) */,
  /*    41 */        WORD(SG_MAKE_INT(8)),
  /*    42 */        0x00000047           /* 42     (GREF_PUSH) */,
  /*    43 */        WORD(SG_UNDEF)  /* identifier#default-printer */,
  /*    44 */        0x00000029           /* 44     (CLOSURE) */,
  /*    45 */        WORD(SG_UNDEF)  /* <code-builder> */,
  /*    46 */        0x0000000B           /* 46     (PUSH) */,
  /*    47 */        0x0000024A           /* 47     (GREF_CALL) */,
  /*    48 */        WORD(SG_UNDEF)  /* identifier#make-parameter */,
  /*    49 */        0x00000033           /* 49     (DEFINE) */,
  /*    50 */        WORD(SG_UNDEF)  /* identifier#current-printer */,
  /*    51 */        0x00000029           /* 51     (CLOSURE) */,
  /*    52 */        WORD(SG_UNDEF)  /* <code-builder> */,
  /*    53 */        0x00000033           /* 53     (DEFINE) */,
  /*    54 */        WORD(SG_UNDEF)  /* identifier#default-prompter */,
  /*    55 */        0x00000030           /* 55     (FRAME) */,
  /*    56 */        WORD(SG_MAKE_INT(8)),
  /*    57 */        0x00000047           /* 57     (GREF_PUSH) */,
  /*    58 */        WORD(SG_UNDEF)  /* identifier#default-prompter */,
  /*    59 */        0x00000029           /* 59     (CLOSURE) */,
  /*    60 */        WORD(SG_UNDEF)  /* <code-builder> */,
  /*    61 */        0x0000000B           /* 61     (PUSH) */,
  /*    62 */        0x0000024A           /* 62     (GREF_CALL) */,
  /*    63 */        WORD(SG_UNDEF)  /* identifier#make-parameter */,
  /*    64 */        0x00000033           /* 64     (DEFINE) */,
  /*    65 */        WORD(SG_UNDEF)  /* identifier#current-prompter */,
  /*    66 */        0x00000029           /* 66     (CLOSURE) */,
  /*    67 */        WORD(SG_UNDEF)  /* <code-builder> */,
  /*    68 */        0x00000033           /* 68     (DEFINE) */,
  /*    69 */        WORD(SG_UNDEF)  /* identifier#read-eval-print-loop */,
  /*    70 */        0x00000002           /* 70     (UNDEF) */,
  /*    71 */        0x0000002F           /* 71     (RET) */,
  /*    72 */        0x00000030           /* 0      (FRAME) */,
  /*    73 */        WORD(SG_MAKE_INT(4)),
  /*    74 */        0x00000045           /* 2      (LREF_PUSH) */,
  /*    75 */        0x0000014A           /* 3      (GREF_CALL) */,
  /*    76 */        WORD(SG_UNDEF)  /* identifier#write/ss */,
  /*    77 */        0x0000004B           /* 5      (GREF_TAIL_CALL) */,
  /*    78 */        WORD(SG_UNDEF)  /* identifier#newline */,
  /*    79 */        0x0000002F           /* 7      (RET) */,
  /*    80 */        0x00000045           /* 0      (LREF_PUSH) */,
  /*    81 */        0x0000014B           /* 1      (GREF_TAIL_CALL) */,
  /*    82 */        WORD(SG_UNDEF)  /* identifier#report-error */,
  /*    83 */        0x0000002F           /* 3      (RET) */,
  /*    84 */        0x00000005           /* 0      (LREF) */,
  /*    85 */        0x00000017           /* 1      (TEST) */,
  /*    86 */        WORD(SG_MAKE_INT(23)),
  /*    87 */        0x00000030           /* 3      (FRAME) */,
  /*    88 */        WORD(SG_MAKE_INT(4)),
  /*    89 */        0x00000045           /* 5      (LREF_PUSH) */,
  /*    90 */        0x0000014A           /* 6      (GREF_CALL) */,
  /*    91 */        WORD(SG_UNDEF)  /* identifier#procedure? */,
  /*    92 */        0x00000017           /* 8      (TEST) */,
  /*    93 */        WORD(SG_MAKE_INT(3)),
  /*    94 */        0x00000005           /* 10     (LREF) */,
  /*    95 */        0x0000002F           /* 11     (RET) */,
  /*    96 */        0x00000048           /* 12     (CONST_PUSH) */,
  /*    97 */        WORD(SG_UNDEF), /* current-exception-printer */
  /*    98 */        0x00000030           /* 14     (FRAME) */,
  /*    99 */        WORD(SG_MAKE_INT(6)),
  /*   100 */        0x00000048           /* 16     (CONST_PUSH) */,
  /*   101 */        WORD(SG_UNDEF), /* "expected procedure or #f, but got ~s" */
  /*   102 */        0x00000045           /* 18     (LREF_PUSH) */,
  /*   103 */        0x0000024A           /* 19     (GREF_CALL) */,
  /*   104 */        WORD(SG_UNDEF)  /* identifier#format */,
  /*   105 */        0x0000000B           /* 21     (PUSH) */,
  /*   106 */        0x0000024B           /* 22     (GREF_TAIL_CALL) */,
  /*   107 */        WORD(SG_UNDEF)  /* identifier#assertion-violation */,
  /*   108 */        0x0000002F           /* 24     (RET) */,
  /*   109 */        0x00000009           /* 25     (GREF) */,
  /*   110 */        WORD(SG_UNDEF)  /* identifier#values */,
  /*   111 */        0x0000002F           /* 27     (RET) */,
  /*   112 */        0x00000045           /* 0      (LREF_PUSH) */,
  /*   113 */        0x00000145           /* 1      (LREF_PUSH) */,
  /*   114 */        0x0000024B           /* 2      (GREF_TAIL_CALL) */,
  /*   115 */        WORD(SG_UNDEF)  /* identifier#eval */,
  /*   116 */        0x0000002F           /* 4      (RET) */,
  /*   117 */        0x00000005           /* 0      (LREF) */,
  /*   118 */        0x00000017           /* 1      (TEST) */,
  /*   119 */        WORD(SG_MAKE_INT(23)),
  /*   120 */        0x00000030           /* 3      (FRAME) */,
  /*   121 */        WORD(SG_MAKE_INT(4)),
  /*   122 */        0x00000045           /* 5      (LREF_PUSH) */,
  /*   123 */        0x0000014A           /* 6      (GREF_CALL) */,
  /*   124 */        WORD(SG_UNDEF)  /* identifier#procedure? */,
  /*   125 */        0x00000017           /* 8      (TEST) */,
  /*   126 */        WORD(SG_MAKE_INT(3)),
  /*   127 */        0x00000005           /* 10     (LREF) */,
  /*   128 */        0x0000002F           /* 11     (RET) */,
  /*   129 */        0x00000048           /* 12     (CONST_PUSH) */,
  /*   130 */        WORD(SG_UNDEF), /* current-evaluator */
  /*   131 */        0x00000030           /* 14     (FRAME) */,
  /*   132 */        WORD(SG_MAKE_INT(6)),
  /*   133 */        0x00000048           /* 16     (CONST_PUSH) */,
  /*   134 */        WORD(SG_UNDEF), /* "expected procedure or #f, but got ~s" */
  /*   135 */        0x00000045           /* 18     (LREF_PUSH) */,
  /*   136 */        0x0000024A           /* 19     (GREF_CALL) */,
  /*   137 */        WORD(SG_UNDEF)  /* identifier#format */,
  /*   138 */        0x0000000B           /* 21     (PUSH) */,
  /*   139 */        0x0000024B           /* 22     (GREF_TAIL_CALL) */,
  /*   140 */        WORD(SG_UNDEF)  /* identifier#assertion-violation */,
  /*   141 */        0x0000002F           /* 24     (RET) */,
  /*   142 */        0x00000009           /* 25     (GREF) */,
  /*   143 */        WORD(SG_UNDEF)  /* identifier#values */,
  /*   144 */        0x0000002F           /* 27     (RET) */,
  /*   145 */        0x00000047           /* 0      (GREF_PUSH) */,
  /*   146 */        WORD(SG_UNDEF)  /* identifier#G10683 */,
  /*   147 */        0x00000045           /* 2      (LREF_PUSH) */,
  /*   148 */        0x0000024B           /* 3      (GREF_TAIL_CALL) */,
  /*   149 */        WORD(SG_UNDEF)  /* identifier#for-each */,
  /*   150 */        0x0000002F           /* 5      (RET) */,
  /*   151 */        0x00000005           /* 0      (LREF) */,
  /*   152 */        0x00000017           /* 1      (TEST) */,
  /*   153 */        WORD(SG_MAKE_INT(23)),
  /*   154 */        0x00000030           /* 3      (FRAME) */,
  /*   155 */        WORD(SG_MAKE_INT(4)),
  /*   156 */        0x00000045           /* 5      (LREF_PUSH) */,
  /*   157 */        0x0000014A           /* 6      (GREF_CALL) */,
  /*   158 */        WORD(SG_UNDEF)  /* identifier#procedure? */,
  /*   159 */        0x00000017           /* 8      (TEST) */,
  /*   160 */        WORD(SG_MAKE_INT(3)),
  /*   161 */        0x00000005           /* 10     (LREF) */,
  /*   162 */        0x0000002F           /* 11     (RET) */,
  /*   163 */        0x00000048           /* 12     (CONST_PUSH) */,
  /*   164 */        WORD(SG_UNDEF), /* current-printer */
  /*   165 */        0x00000030           /* 14     (FRAME) */,
  /*   166 */        WORD(SG_MAKE_INT(6)),
  /*   167 */        0x00000048           /* 16     (CONST_PUSH) */,
  /*   168 */        WORD(SG_UNDEF), /* "expected procedure or #f, but got ~s" */
  /*   169 */        0x00000045           /* 18     (LREF_PUSH) */,
  /*   170 */        0x0000024A           /* 19     (GREF_CALL) */,
  /*   171 */        WORD(SG_UNDEF)  /* identifier#format */,
  /*   172 */        0x0000000B           /* 21     (PUSH) */,
  /*   173 */        0x0000024B           /* 22     (GREF_TAIL_CALL) */,
  /*   174 */        WORD(SG_UNDEF)  /* identifier#assertion-violation */,
  /*   175 */        0x0000002F           /* 24     (RET) */,
  /*   176 */        0x00000009           /* 25     (GREF) */,
  /*   177 */        WORD(SG_UNDEF)  /* identifier#values */,
  /*   178 */        0x0000002F           /* 27     (RET) */,
  /*   179 */        0x00000048           /* 0      (CONST_PUSH) */,
  /*   180 */        WORD(SG_UNDEF), /* "sash> " */
  /*   181 */        0x0000014B           /* 2      (GREF_TAIL_CALL) */,
  /*   182 */        WORD(SG_UNDEF)  /* identifier#display */,
  /*   183 */        0x0000002F           /* 4      (RET) */,
  /*   184 */        0x00000005           /* 0      (LREF) */,
  /*   185 */        0x00000017           /* 1      (TEST) */,
  /*   186 */        WORD(SG_MAKE_INT(23)),
  /*   187 */        0x00000030           /* 3      (FRAME) */,
  /*   188 */        WORD(SG_MAKE_INT(4)),
  /*   189 */        0x00000045           /* 5      (LREF_PUSH) */,
  /*   190 */        0x0000014A           /* 6      (GREF_CALL) */,
  /*   191 */        WORD(SG_UNDEF)  /* identifier#procedure? */,
  /*   192 */        0x00000017           /* 8      (TEST) */,
  /*   193 */        WORD(SG_MAKE_INT(3)),
  /*   194 */        0x00000005           /* 10     (LREF) */,
  /*   195 */        0x0000002F           /* 11     (RET) */,
  /*   196 */        0x00000048           /* 12     (CONST_PUSH) */,
  /*   197 */        WORD(SG_UNDEF), /* current-prompter */
  /*   198 */        0x00000030           /* 14     (FRAME) */,
  /*   199 */        WORD(SG_MAKE_INT(6)),
  /*   200 */        0x00000048           /* 16     (CONST_PUSH) */,
  /*   201 */        WORD(SG_UNDEF), /* "expected procedure or #f, but got ~s" */
  /*   202 */        0x00000045           /* 18     (LREF_PUSH) */,
  /*   203 */        0x0000024A           /* 19     (GREF_CALL) */,
  /*   204 */        WORD(SG_UNDEF)  /* identifier#format */,
  /*   205 */        0x0000000B           /* 21     (PUSH) */,
  /*   206 */        0x0000024B           /* 22     (GREF_TAIL_CALL) */,
  /*   207 */        WORD(SG_UNDEF)  /* identifier#assertion-violation */,
  /*   208 */        0x0000002F           /* 24     (RET) */,
  /*   209 */        0x00000009           /* 25     (GREF) */,
  /*   210 */        WORD(SG_UNDEF)  /* identifier#values */,
  /*   211 */        0x0000002F           /* 27     (RET) */,
  /*   212 */        0x00000002           /* 0      (UNDEF) */,
  /*   213 */        0x0000000B           /* 1      (PUSH) */,
  /*   214 */        0x0000000C           /* 2      (BOX) */,
  /*   215 */        0x00000131           /* 3      (ENTER) */,
  /*   216 */        0x00000030           /* 4      (FRAME) */,
  /*   217 */        WORD(SG_MAKE_INT(11)),
  /*   218 */        0x00000048           /* 6      (CONST_PUSH) */,
  /*   219 */        WORD(SG_UNDEF), /* null */
  /*   220 */        0x00000048           /* 8      (CONST_PUSH) */,
  /*   221 */        WORD(SG_UNDEF), /* (core base) */
  /*   222 */        0x00000048           /* 10     (CONST_PUSH) */,
  /*   223 */        WORD(SG_UNDEF), /* (sagittarius) */
  /*   224 */        0x00000048           /* 12     (CONST_PUSH) */,
  /*   225 */        WORD(SG_UNDEF), /* (rnrs) */
  /*   226 */        0x0000044A           /* 14     (GREF_CALL) */,
  /*   227 */        WORD(SG_UNDEF)  /* identifier#environment */,
  /*   228 */        0x00000006           /* 16     (LSET) */,
  /*   229 */        0x00000030           /* 17     (FRAME) */,
  /*   230 */        WORD(SG_MAKE_INT(5)),
  /*   231 */        0x00000048           /* 19     (CONST_PUSH) */,
  /*   232 */        WORD(SG_UNDEF), /* "EMACS" */
  /*   233 */        0x0000014A           /* 21     (GREF_CALL) */,
  /*   234 */        WORD(SG_UNDEF)  /* identifier#getenv */,
  /*   235 */        0x0000000B           /* 23     (PUSH) */,
  /*   236 */        0x00000231           /* 24     (ENTER) */,
  /*   237 */        0x00000231           /* 25     (ENTER) */,
  /*   238 */        0x00000030           /* 26     (FRAME) */,
  /*   239 */        WORD(SG_MAKE_INT(8)),
  /*   240 */        0x00000145           /* 28     (LREF_PUSH) */,
  /*   241 */        0x00000045           /* 29     (LREF_PUSH) */,
  /*   242 */        0x00000029           /* 30     (CLOSURE) */,
  /*   243 */        WORD(SG_UNDEF)  /* <code-builder> */,
  /*   244 */        0x0000000B           /* 32     (PUSH) */,
  /*   245 */        0x0000014A           /* 33     (GREF_CALL) */,
  /*   246 */        WORD(SG_UNDEF)  /* identifier#call-with-current-continuation */,
  /*   247 */        0x00200019           /* 35     (SHIFTJ) */,
  /*   248 */        0x00000018           /* 36     (JUMP) */,
  /*   249 */        WORD(SG_MAKE_INT(-11)),
  /*   250 */        0x0000002F           /* 38     (RET) */,
  /*   251 */        0x00000045           /* 0      (LREF_PUSH) */,
  /*   252 */        0x00000029           /* 1      (CLOSURE) */,
  /*   253 */        WORD(SG_UNDEF)  /* <code-builder> */,
  /*   254 */        0x0000000B           /* 3      (PUSH) */,
  /*   255 */        0x00000146           /* 4      (FREF_PUSH) */,
  /*   256 */        0x00000046           /* 5      (FREF_PUSH) */,
  /*   257 */        0x00000029           /* 6      (CLOSURE) */,
  /*   258 */        WORD(SG_UNDEF)  /* <code-builder> */,
  /*   259 */        0x0000000B           /* 8      (PUSH) */,
  /*   260 */        0x0000024B           /* 9      (GREF_TAIL_CALL) */,
  /*   261 */        WORD(SG_UNDEF)  /* identifier#with-error-handler */,
  /*   262 */        0x0000002F           /* 11     (RET) */,
  /*   263 */        0x00000030           /* 0      (FRAME) */,
  /*   264 */        WORD(SG_MAKE_INT(8)),
  /*   265 */        0x00000030           /* 2      (FRAME) */,
  /*   266 */        WORD(SG_MAKE_INT(3)),
  /*   267 */        0x0000004A           /* 4      (GREF_CALL) */,
  /*   268 */        WORD(SG_UNDEF)  /* identifier#current-output-port */,
  /*   269 */        0x0000000B           /* 6      (PUSH) */,
  /*   270 */        0x0000014A           /* 7      (GREF_CALL) */,
  /*   271 */        WORD(SG_UNDEF)  /* identifier#flush-output-port */,
  /*   272 */        0x00000030           /* 9      (FRAME) */,
  /*   273 */        WORD(SG_MAKE_INT(7)),
  /*   274 */        0x00000045           /* 11     (LREF_PUSH) */,
  /*   275 */        0x00000030           /* 12     (FRAME) */,
  /*   276 */        WORD(SG_MAKE_INT(3)),
  /*   277 */        0x0000004A           /* 14     (GREF_CALL) */,
  /*   278 */        WORD(SG_UNDEF)  /* identifier#current-exception-printer */,
  /*   279 */        0x0000012B           /* 16     (CALL) */,
  /*   280 */        0x00000030           /* 17     (FRAME) */,
  /*   281 */        WORD(SG_MAKE_INT(4)),
  /*   282 */        0x00000045           /* 19     (LREF_PUSH) */,
  /*   283 */        0x0000014A           /* 20     (GREF_CALL) */,
  /*   284 */        WORD(SG_UNDEF)  /* identifier#serious-condition? */,
  /*   285 */        0x00000017           /* 22     (TEST) */,
  /*   286 */        WORD(SG_MAKE_INT(3)),
  /*   287 */        0x00000007           /* 24     (FREF) */,
  /*   288 */        0x0000002D           /* 25     (TAIL_CALL) */,
  /*   289 */        0x0000002F           /* 26     (RET) */,
  /*   290 */        0x00000030           /* 0      (FRAME) */,
  /*   291 */        WORD(SG_MAKE_INT(6)),
  /*   292 */        0x00000030           /* 2      (FRAME) */,
  /*   293 */        WORD(SG_MAKE_INT(3)),
  /*   294 */        0x0000004A           /* 4      (GREF_CALL) */,
  /*   295 */        WORD(SG_UNDEF)  /* identifier#current-prompter */,
  /*   296 */        0x0000002B           /* 6      (CALL) */,
  /*   297 */        0x00000030           /* 7      (FRAME) */,
  /*   298 */        WORD(SG_MAKE_INT(8)),
  /*   299 */        0x00000030           /* 9      (FRAME) */,
  /*   300 */        WORD(SG_MAKE_INT(3)),
  /*   301 */        0x0000004A           /* 11     (GREF_CALL) */,
  /*   302 */        WORD(SG_UNDEF)  /* identifier#current-output-port */,
  /*   303 */        0x0000000B           /* 13     (PUSH) */,
  /*   304 */        0x0000014A           /* 14     (GREF_CALL) */,
  /*   305 */        WORD(SG_UNDEF)  /* identifier#flush-output-port */,
  /*   306 */        0x00000030           /* 16     (FRAME) */,
  /*   307 */        WORD(SG_MAKE_INT(8)),
  /*   308 */        0x00000030           /* 18     (FRAME) */,
  /*   309 */        WORD(SG_MAKE_INT(3)),
  /*   310 */        0x0000004A           /* 20     (GREF_CALL) */,
  /*   311 */        WORD(SG_UNDEF)  /* identifier#current-input-port */,
  /*   312 */        0x0000000B           /* 22     (PUSH) */,
  /*   313 */        0x0000014A           /* 23     (GREF_CALL) */,
  /*   314 */        WORD(SG_UNDEF)  /* identifier#read/ss */,
  /*   315 */        0x0000000B           /* 25     (PUSH) */,
  /*   316 */        0x00000131           /* 26     (ENTER) */,
  /*   317 */        0x00000030           /* 27     (FRAME) */,
  /*   318 */        WORD(SG_MAKE_INT(4)),
  /*   319 */        0x00000045           /* 29     (LREF_PUSH) */,
  /*   320 */        0x0000014A           /* 30     (GREF_CALL) */,
  /*   321 */        WORD(SG_UNDEF)  /* identifier#eof-object? */,
  /*   322 */        0x00000017           /* 32     (TEST) */,
  /*   323 */        WORD(SG_MAKE_INT(6)),
  /*   324 */        0x00000030           /* 34     (FRAME) */,
  /*   325 */        WORD(SG_MAKE_INT(4)),
  /*   326 */        0x00000049           /* 36     (CONSTI_PUSH) */,
  /*   327 */        0x0000014A           /* 37     (GREF_CALL) */,
  /*   328 */        WORD(SG_UNDEF)  /* identifier#exit */,
  /*   329 */        0x00000107           /* 39     (FREF) */,
  /*   330 */        0x00000017           /* 40     (TEST) */,
  /*   331 */        WORD(SG_MAKE_INT(10)),
  /*   332 */        0x00000030           /* 42     (FRAME) */,
  /*   333 */        WORD(SG_MAKE_INT(8)),
  /*   334 */        0x00000030           /* 44     (FRAME) */,
  /*   335 */        WORD(SG_MAKE_INT(3)),
  /*   336 */        0x0000004A           /* 46     (GREF_CALL) */,
  /*   337 */        WORD(SG_UNDEF)  /* identifier#current-output-port */,
  /*   338 */        0x0000000B           /* 48     (PUSH) */,
  /*   339 */        0x0000014A           /* 49     (GREF_CALL) */,
  /*   340 */        WORD(SG_UNDEF)  /* identifier#flush-output-port */,
  /*   341 */        0x00000030           /* 51     (FRAME) */,
  /*   342 */        WORD(SG_MAKE_INT(10)),
  /*   343 */        0x00000045           /* 53     (LREF_PUSH) */,
  /*   344 */        0x00000007           /* 54     (FREF) */,
  /*   345 */        0x0000000D           /* 55     (UNBOX) */,
  /*   346 */        0x0000000B           /* 56     (PUSH) */,
  /*   347 */        0x00000030           /* 57     (FRAME) */,
  /*   348 */        WORD(SG_MAKE_INT(3)),
  /*   349 */        0x0000004A           /* 59     (GREF_CALL) */,
  /*   350 */        WORD(SG_UNDEF)  /* identifier#current-evaluator */,
  /*   351 */        0x0000022B           /* 61     (CALL) */,
  /*   352 */        0x00100028           /* 62     (RECEIVE) */,
  /*   353 */        0x00000231           /* 63     (ENTER) */,
  /*   354 */        0x00000030           /* 64     (FRAME) */,
  /*   355 */        WORD(SG_MAKE_INT(8)),
  /*   356 */        0x00000030           /* 66     (FRAME) */,
  /*   357 */        WORD(SG_MAKE_INT(3)),
  /*   358 */        0x0000004A           /* 68     (GREF_CALL) */,
  /*   359 */        WORD(SG_UNDEF)  /* identifier#current-printer */,
  /*   360 */        0x0000000B           /* 70     (PUSH) */,
  /*   361 */        0x00000105           /* 71     (LREF) */,
  /*   362 */        0x0000022A           /* 72     (APPLY) */,
  /*   363 */        0x00000030           /* 73     (FRAME) */,
  /*   364 */        WORD(SG_MAKE_INT(3)),
  /*   365 */        0x0000004A           /* 75     (GREF_CALL) */,
  /*   366 */        WORD(SG_UNDEF)  /* identifier#current-output-port */,
  /*   367 */        0x0000000B           /* 77     (PUSH) */,
  /*   368 */        0x0000014B           /* 78     (GREF_TAIL_CALL) */,
  /*   369 */        WORD(SG_UNDEF)  /* identifier#flush-output-port */,
  /*   370 */        0x0000002F           /* 80     (RET) */,
  }
};

static SgCodeBuilder toplevel_sagittarius_interactive = SG_STATIC_CODE_BUILDER(&sg__wc.w[0], SG_FALSE, 0, FALSE, 0, 0, 72);
void Sg__Init_sagittarius_interactive()
{
  SgLibrary *lib = Sg_FindLibrary(SYMBOL("(sagittarius interactive)"), TRUE);
  SgLibrary *save = Sg_VM()->currentLibrary;
  sg__wc.w[225] = WORD(Sg_Cons(SYMBOL("rnrs"), SG_NIL));
  sg__wc.w[22] = WORD(&sg__wc.cb[3]);
  sg__wc.cb[3].name = SYMBOL("default-evaluator");
  sg__wc.w[1] = WORD(&sg__wc.cb[0]);
  sg__wc.cb[0].name = SG_MAKE_BOOL(FALSE);
  sg__wc.w[223] = WORD(Sg_Cons(SYMBOL("sagittarius"), SG_NIL));
  sg__wc.w[243] = WORD(&sg__wc.cb[10]);
  sg__wc.cb[10].name = SG_MAKE_BOOL(FALSE);
  sg__wc.w[52] = WORD(&sg__wc.cb[7]);
  sg__wc.cb[7].name = SYMBOL("default-prompter");
  sg__wc.w[30] = WORD(&sg__wc.cb[4]);
  sg__wc.cb[4].name = SG_MAKE_BOOL(FALSE);
  sg__wc.w[201] = STRINGW("expected procedure or #f, but got ~s");
  sg__wc.w[101] = STRINGW("expected procedure or #f, but got ~s");
  sg__wc.w[7] = WORD(&sg__wc.cb[1]);
  sg__wc.cb[1].name = SYMBOL("default-exception-printer");
  sg__wc.w[5] = SYMBOLW("(sagittarius interactive)");
  sg__wc.w[253] = WORD(&sg__wc.cb[11]);
  sg__wc.cb[11].name = SG_MAKE_BOOL(FALSE);
  sg__wc.w[134] = STRINGW("expected procedure or #f, but got ~s");
  sg__wc.w[60] = WORD(&sg__wc.cb[8]);
  sg__wc.cb[8].name = SG_MAKE_BOOL(FALSE);
  sg__wc.w[221] = WORD(Sg_Cons(SYMBOL("core"), Sg_Cons(SYMBOL("base"), SG_NIL)));
  sg__wc.w[37] = WORD(&sg__wc.cb[5]);
  sg__wc.cb[5].name = SYMBOL("default-printer");
  sg__wc.w[180] = STRINGW("sash> ");
  sg__wc.w[15] = WORD(&sg__wc.cb[2]);
  sg__wc.cb[2].name = SG_MAKE_BOOL(FALSE);
  sg__wc.w[232] = STRINGW("EMACS");
  sg__wc.w[258] = WORD(&sg__wc.cb[12]);
  sg__wc.cb[12].name = SG_MAKE_BOOL(FALSE);
  sg__wc.w[67] = WORD(&sg__wc.cb[9]);
  sg__wc.cb[9].name = SYMBOL("read-eval-print-loop");
  sg__wc.w[45] = WORD(&sg__wc.cb[6]);
  sg__wc.cb[6].name = SG_MAKE_BOOL(FALSE);
  sg__wc.w[168] = STRINGW("expected procedure or #f, but got ~s");
  sg__wc.w[328] = IDENT("exit", lib);
  sg__wc.w[146] = sg__wc.w[3] = UNSIDENT("G10683", lib);
  sg__wc.w[314] = IDENT("read/ss", lib);
  sg__wc.w[321] = IDENT("eof-object?", lib);
  sg__wc.w[261] = IDENT("with-error-handler", lib);
  sg__wc.w[227] = IDENT("environment", lib);
  sg__wc.w[39] = sg__wc.w[43] = IDENT("default-printer", lib);
  sg__wc.w[82] = IDENT("report-error", lib);
  sg__wc.w[311] = IDENT("current-input-port", lib);
  sg__wc.w[359] = sg__wc.w[50] = IDENT("current-printer", lib);
  sg__wc.w[246] = IDENT("call-with-current-continuation", lib);
  sg__wc.w[13] = sg__wc.w[9] = IDENT("default-exception-printer", lib);
  sg__wc.w[69] = IDENT("read-eval-print-loop", lib);
  sg__wc.w[78] = IDENT("newline", lib);
  sg__wc.w[284] = IDENT("serious-condition?", lib);
  sg__wc.w[54] = sg__wc.w[58] = IDENT("default-prompter", lib);
  sg__wc.w[115] = IDENT("eval", lib);
  sg__wc.w[191] = sg__wc.w[91] = sg__wc.w[158] = sg__wc.w[124] = IDENT("procedure?", lib);
  sg__wc.w[76] = IDENT("write/ss", lib);
  sg__wc.w[35] = sg__wc.w[350] = IDENT("current-evaluator", lib);
  sg__wc.w[18] = sg__wc.w[48] = sg__wc.w[33] = sg__wc.w[63] = IDENT("make-parameter", lib);
  sg__wc.w[210] = sg__wc.w[110] = sg__wc.w[177] = sg__wc.w[143] = IDENT("values", lib);
  sg__wc.w[104] = sg__wc.w[171] = sg__wc.w[137] = sg__wc.w[204] = IDENT("format", lib);
  sg__wc.w[234] = IDENT("getenv", lib);
  sg__wc.w[20] = sg__wc.w[278] = IDENT("current-exception-printer", lib);
  sg__wc.w[295] = sg__wc.w[65] = IDENT("current-prompter", lib);
  sg__wc.w[366] = sg__wc.w[302] = sg__wc.w[337] = sg__wc.w[268] = IDENT("current-output-port", lib);
  sg__wc.w[28] = sg__wc.w[24] = IDENT("default-evaluator", lib);
  sg__wc.w[107] = sg__wc.w[174] = sg__wc.w[140] = sg__wc.w[207] = IDENT("assertion-violation", lib);
  sg__wc.w[369] = sg__wc.w[305] = sg__wc.w[340] = sg__wc.w[271] = IDENT("flush-output-port", lib);
  sg__wc.w[182] = IDENT("display", lib);
  sg__wc.w[149] = IDENT("for-each", lib);
  sg__wc.w[97] = SYMBOLW("current-exception-printer");
  sg__wc.w[197] = SYMBOLW("current-prompter");
  sg__wc.w[164] = SYMBOLW("current-printer");
  sg__wc.w[130] = SYMBOLW("current-evaluator");
  sg__wc.w[219] = SYMBOLW("null");
  Sg_ImportLibrary(lib, SG_OBJ(SYMBOL("(sagittarius)")));
  Sg_ImportLibrary(lib, SG_OBJ(SYMBOL("(core errors)")));
  Sg_ImportLibrary(lib, SG_OBJ(SYMBOL("(core base)")));
  Sg_ImportLibrary(lib, SG_OBJ(SYMBOL("null")));
  Sg_LibraryExportedSet(lib, Sg_Cons(Sg_Cons(SYMBOL("default-prompter"), Sg_Cons(SYMBOL("default-printer"), Sg_Cons(SYMBOL("default-evaluator"), Sg_Cons(SYMBOL("default-exception-printer"), Sg_Cons(SYMBOL("current-prompter"), Sg_Cons(SYMBOL("current-evaluator"), Sg_Cons(SYMBOL("current-exception-printer"), Sg_Cons(SYMBOL("current-printer"), Sg_Cons(SYMBOL("read-eval-print-loop"), SG_NIL))))))))), Sg_Cons(SG_NIL, SG_NIL)));
  Sg_VM()->currentLibrary = lib;
  Sg_VMExecute(SG_OBJ(&toplevel_sagittarius_interactive));
  Sg_VM()->currentLibrary = save;
}
