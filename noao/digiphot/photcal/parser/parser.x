
# line 2 "parser.y"

include	<ctype.h>
include	<lexnum.h>
include "../lib/lexer.h"
include	"../lib/parser.h"
include	"../lib/prdefs.h"

# Parser stack and structure lengths
define	YYMAXDEPTH	128
define	YYOPLEN		LEN_LEX

# Redefine the name of the parser
define	yyparse		parse

define	OBSSECT		257
define	CATSECT		258
define	EXTSECT		259
define	TRNSECT		260
define	FITID		261
define	CONSTID		262
define	DELTAID		263
define	ERRORID		264
define	WEIGHTID		265
define	MINID		266
define	MAXID		267
define	DERIVID		268
define	PLOTID		269
define	SETID		270
define	F_ABS		271
define	F_ACOS		272
define	F_ASIN		273
define	F_ATAN		274
define	F_COS		275
define	F_EXP		276
define	F_LOG		277
define	F_LOG10		278
define	F_SIN		279
define	F_SQRT		280
define	F_TAN		281
define	IDENTIFIER		282
define	INUMBER		283
define	RNUMBER		284
define	PLUS		285
define	MINUS		286
define	STAR		287
define	SLASH		288
define	EXPON		289
define	COLON		290
define	SEMICOLON		291
define	COMMA		292
define	EQUAL		293
define	LPAR		294
define	RPAR		295
define	EOFILE		296
define	UPLUS		297
define	UMINUS		298
define	yyclearin	yychar = -1
define	yyerrok		yyerrflag = 0
define	YYMOVE		call amovi (Memi[$1], Memi[$2], YYOPLEN)
define	YYERRCODE	256

define	YYNPROD		104
define	YYLAST		337
# line	1 "/iraf/iraf/lib/yaccpar.x"
# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# Parser for yacc output, translated to the IRAF SPP language.  The contents
# of this file form the bulk of the source of the parser produced by Yacc.
# Yacc recognizes several macros in the yaccpar input source and replaces
# them as follows:
#	A	user suppled "global" definitions and declarations
# 	B	parser tables
# 	C	user supplied actions (reductions)
# The remainder of the yaccpar code is not changed.

define	yystack_	10		# statement labels for gotos
define	yynewstate_	20
define	yydefault_	30
define	yyerrlab_	40
define	yyabort_	50

define	YYFLAG		(-1000)		# defs used in user actions
define	YYERROR		goto yyerrlab_
define	YYACCEPT	return (OK)
define	YYABORT		return (ERR)


# YYPARSE -- Parse the input stream, returning OK if the source is
# syntactically acceptable (i.e., if compilation is successful),
# otherwise ERR.  The parameters YYMAXDEPTH and YYOPLEN must be
# supplied by the caller in the %{ ... %} section of the Yacc source.
# The token value stack is a dynamically allocated array of operand
# structures, with the length and makeup of the operand structure being
# application dependent.

int procedure yyparse (fd, yydebug, yylex)

int	fd			# stream to be parsed
bool	yydebug			# print debugging information?
int	yylex()			# user-supplied lexical input function
extern	yylex()

short	yys[YYMAXDEPTH]		# parser stack -- stacks tokens
pointer	yyv			# pointer to token value stack
pointer	yyval			# value returned by action
pointer	yylval			# value of token
int	yyps			# token stack pointer
pointer	yypv			# value stack pointer
int	yychar			# current input token number
int	yyerrflag		# error recovery flag
int	yynerrs			# number of errors

short	yyj, yym		# internal variables
pointer	yysp, yypvt
short	yystate, yyn
int	yyxi, i
errchk	salloc, yylex

short	yyexca[6]
data	(yyexca(i),i=  1,  6)	/  -1,   1,   0,  -1,  -2,   0/
short	yyact[337]
data	(yyact(i),i=  1,  8)	/ 131, 132, 133, 134, 135, 136, 137, 138/
data	(yyact(i),i=  9, 16)	/ 139, 140, 141, 130, 142, 143, 125, 126/
data	(yyact(i),i= 17, 24)	/ 152, 153, 154, 155, 156,  40, 161, 128/
data	(yyact(i),i= 25, 32)	/ 183, 112, 180, 152, 153, 154, 155, 156/
data	(yyact(i),i= 33, 40)	/  60,  61,  62,  58,  59, 175, 111,  52/
data	(yyact(i),i= 41, 48)	/  53,  57, 108,  86,  85,  84,  83, 159/
data	(yyact(i),i= 49, 56)	/  73,  72,  70,  69,  39,  51,  38,  34/
data	(yyact(i),i= 57, 64)	/  33,  24,  25,  18,  19, 198,  42, 196/
data	(yyact(i),i= 65, 72)	/  35, 176, 152, 153, 154, 155, 156, 148/
data	(yyact(i),i= 73, 80)	/ 147, 145, 107,  23, 123,  17,  99,  97/
data	(yyact(i),i= 81, 88)	/  95,  91,  98,  96,  21,  29,  15,  68/
data	(yyact(i),i= 89, 96)	/  94, 154, 155, 156,  28,  60,  61,  62/
data	(yyact(i),i= 97,104)	/  58,  59, 156,  32,  52,  53,  57, 116/
data	(yyact(i),i=105,112)	/ 117, 142, 143,  24,  25,  18,  19, 144/
data	(yyact(i),i=113,120)	/  82,  79,  51,  76,  93,  92,  90,  89/
data	(yyact(i),i=121,128)	/  71,  66,  65,  64,  63,  23, 194,  17/
data	(yyact(i),i=129,136)	/ 190,  27,  12,   3, 129,   4,   7, 106/
data	(yyact(i),i=137,144)	/   5, 104,   8, 193,  10, 124,  13, 189/
data	(yyact(i),i=145,152)	/ 184, 114,  74,  80,  41,  20,  14, 115/
data	(yyact(i),i=153,160)	/  77,  31, 127, 105, 109,  81,  78,  75/
data	(yyact(i),i=161,168)	/  56,  55,  54, 166, 164, 162, 181,  30/
data	(yyact(i),i=169,176)	/ 150,  87,  50,  49,  36,  48,  47,  46/
data	(yyact(i),i=177,184)	/  45,  37,  44,  43,  22,   9,  16,  26/
data	(yyact(i),i=185,192)	/  11,   6,   2,   1,   0,   0,   0,   0/
data	(yyact(i),i=193,200)	/  67,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=201,208)	/   0,   0,   0,   0,  88,   0,   0,   0/
data	(yyact(i),i=209,216)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=217,224)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=225,232)	/   0,   0,   0, 110,   0,   0,   0, 118/
data	(yyact(i),i=233,240)	/   0, 118,   0, 118,   0, 100, 101, 102/
data	(yyact(i),i=241,248)	/ 103, 113,   0, 120,   0, 122, 121, 146/
data	(yyact(i),i=249,256)	/ 149, 119,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=257,264)	/   0,   0,   0, 151,   0,   0,   0,   0/
data	(yyact(i),i=265,272)	/   0,   0,   0, 157, 158,   0, 160,   0/
data	(yyact(i),i=273,280)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=281,288)	/   0, 163,   0, 165, 167,   0,   0,   0/
data	(yyact(i),i=289,296)	/ 168,   0,   0,   0,   0,   0, 169, 170/
data	(yyact(i),i=297,304)	/ 171, 172, 173,   0, 177, 174, 178,   0/
data	(yyact(i),i=305,312)	/ 179,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=313,320)	/ 182,   0, 185, 185,   0,   0,   0, 187/
data	(yyact(i),i=321,328)	/ 191, 188, 191,   0, 186, 195,   0,   0/
data	(yyact(i),i=329,336)	/ 195,   0, 192,   0, 197,   0, 199,   0/
data	(yyact(i),i=337,337)	/ 200/
short	yypact[201]
data	(yypact(i),i=  1,  8)	/-125,-1000,-123,-1000,-1000,-1000,-129,-205/
data	(yypact(i),i=  9, 16)	/-1000,-207,-1000,-131,-199,-1000,-206,-1000/
data	(yypact(i),i= 17, 24)	/-155,-184,-238,-239,-227,-1000,-157,-184/
data	(yypact(i),i= 25, 32)	/-240,-242,-275,-229,-1000,-1000,-1000,-1000/
data	(yypact(i),i= 33, 40)	/-1000,-158,-159,-1000,-1000,-1000,-160,-161/
data	(yypact(i),i= 41, 48)	/-1000,-1000,-1000,-168,-1000,-1000,-1000,-1000/
data	(yypact(i),i= 49, 56)	/-1000,-1000,-1000,-203,-243,-244,-1000,-1000/
data	(yypact(i),i= 57, 64)	/-1000,-162,-245,-246,-167,-169,-170,-249/
data	(yypact(i),i= 65, 72)	/-250,-251,-252,-1000,-1000,-163,-164,-212/
data	(yypact(i),i= 73, 80)	/-165,-166,-1000,-204,-213,-1000,-209,-214/
data	(yypact(i),i= 81, 88)	/-1000,-210,-215,-184,-184,-184,-184,-1000/
data	(yypact(i),i= 89, 96)	/-1000,-218,-253,-1000,-257,-270,-167,-182/
data	(yypact(i),i= 97,104)	/-169,-182,-170,-182,-1000,-1000,-1000,-1000/
data	(yypact(i),i=105,112)	/-217,-271,-1000,-171,-220,-1000,-1000,-221/
data	(yypact(i),i=113,120)	/-222,-1000,-1000,-178,-1000,-1000,-1000,-1000/
data	(yypact(i),i=121,128)	/-1000,-1000,-1000,-1000,-219,-271,-271,-247/
data	(yypact(i),i=129,136)	/-271,-1000,-1000,-1000,-1000,-1000,-1000,-1000/
data	(yypact(i),i=137,144)	/-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000/
data	(yypact(i),i=145,152)	/-273,-1000,-1000,-1000,-1000,-1000,-1000,-1000/
data	(yypact(i),i=153,160)	/-271,-271,-271,-271,-271,-1000,-1000,-271/
data	(yypact(i),i=161,168)	/-258,-228,-1000,-1000,-1000,-1000,-1000,-1000/
data	(yypact(i),i=169,176)	/-1000,-198,-198,-191,-191,-1000,-269,-1000/
data	(yypact(i),i=177,184)	/-1000,-268,-1000,-1000,-1000,-1000,-1000,-1000/
data	(yypact(i),i=185,192)	/-138,-1000,-138,-1000,-1000,-141,-230,-1000/
data	(yypact(i),i=193,200)	/-141,-1000,-232,-1000,-1000,-1000,-1000,-1000/
data	(yypact(i),i=201,201)	/-1000/
short	yypgo[48]
data	(yypgo(i),i=  1,  8)	/   0, 187, 186, 185, 184, 183, 150, 135/
data	(yypgo(i),i=  9, 16)	/ 182, 153, 181, 149, 180, 148, 179, 178/
data	(yypgo(i),i= 17, 24)	/ 176, 175, 174, 173, 171, 170, 169, 137/
data	(yypgo(i),i= 25, 32)	/ 168, 166, 165, 164, 144, 143, 139, 163/
data	(yypgo(i),i= 33, 40)	/ 162, 161, 160, 146, 159, 145, 152, 158/
data	(yypgo(i),i= 41, 48)	/ 147, 157, 156, 155, 141, 154, 132, 151/
short	yyr1[104]
data	(yyr1(i),i=  1,  8)	/   0,   1,   1,   3,   3,   3,   3,   3/
data	(yyr1(i),i=  9, 16)	/   6,   6,   8,   8,   8,   2,   2,   2/
data	(yyr1(i),i= 17, 24)	/   2,   2,  10,  11,  11,  12,  12,  12/
data	(yyr1(i),i= 25, 32)	/   9,   4,   4,   4,   5,   5,   5,  13/
data	(yyr1(i),i= 33, 40)	/  13,  14,  14,  14,  14,  14,  14,  14/
data	(yyr1(i),i= 41, 48)	/  15,  22,  24,  16,  25,  17,  26,  20/
data	(yyr1(i),i= 49, 56)	/  27,  21,  31,  28,  29,  29,  30,  30/
data	(yyr1(i),i= 57, 64)	/  18,  18,  18,  32,  35,  35,  36,  33/
data	(yyr1(i),i= 65, 72)	/  38,  38,  39,  34,  40,  40,  41,  19/
data	(yyr1(i),i= 73, 80)	/  42,  23,  43,  44,  44,  44,  44,  44/
data	(yyr1(i),i= 81, 88)	/  44,  44,  44,  44,  44,  44,  45,  45/
data	(yyr1(i),i= 89, 96)	/  45,  45,  45,  45,  45,  45,  45,  45/
data	(yyr1(i),i= 97,104)	/  45,  37,  47,  47,  47,  46,  46,   7/
short	yyr2[104]
data	(yyr2(i),i=  1,  8)	/   0,   5,   1,   2,   3,   2,   1,   1/
data	(yyr2(i),i=  9, 16)	/   1,   2,   2,   5,   5,   3,   4,   3/
data	(yyr2(i),i= 17, 24)	/   2,   1,   1,   1,   2,   2,   5,   5/
data	(yyr2(i),i= 25, 32)	/   1,   2,   1,   1,   2,   2,   1,   1/
data	(yyr2(i),i= 33, 40)	/   2,   1,   1,   1,   1,   1,   1,   1/
data	(yyr2(i),i= 41, 48)	/   7,   1,   1,   9,   1,   9,   1,  10/
data	(yyr2(i),i= 49, 56)	/   1,  10,   1,   1,   3,   1,   3,   1/
data	(yyr2(i),i= 57, 64)	/   1,   1,   1,   2,   1,   3,   3,   2/
data	(yyr2(i),i= 65, 72)	/   1,   3,   3,   2,   1,   3,   3,   5/
data	(yyr2(i),i= 73, 80)	/   1,   2,   1,   3,   3,   3,   3,   3/
data	(yyr2(i),i= 81, 88)	/   2,   2,   4,   3,   1,   1,   1,   1/
data	(yyr2(i),i= 89, 96)	/   1,   1,   1,   1,   1,   1,   1,   1/
data	(yyr2(i),i= 97,104)	/   1,   2,   1,   1,   1,   1,   1,   0/
short	yychk[201]
data	(yychk(i),i=  1,  8)	/-1000,  -1,  -2, 256, 258,  -7,  -3, 257/
data	(yychk(i),i=  9, 16)	/  -7, -10,  -7,  -4, 259,  -7,  -6, 291/
data	(yychk(i),i= 17, 24)	/  -8, 282, 264, 265, -11, 291, -12, 282/
data	(yychk(i),i= 25, 32)	/ 264, 265,  -5, 260, 291, 291,  -6,  -9/
data	(yychk(i),i= 33, 40)	/ 283, 294, 294, 291, -11,  -9, 294, 294/
data	(yychk(i),i= 41, 48)	/ 296, -13, 291, -14, -15, -16, -17, -18/
data	(yychk(i),i= 49, 56)	/ -19, -20, -21, 282, 268, 269, -32, -33/
data	(yychk(i),i= 57, 64)	/ -34, 270, 264, 265, 261, 262, 263, 282/
data	(yychk(i),i= 65, 72)	/ 282, 282, 282, -13, 290, 294, 294, 282/
data	(yychk(i),i= 73, 80)	/ 294, 294, -35, -36, 282, -38, -39, 282/
data	(yychk(i),i= 81, 88)	/ -40, -41, 282, 295, 295, 295, 295, -22/
data	(yychk(i),i= 89, 96)	/  -7, 282, 282, 293, 282, 282, 292, 293/
data	(yychk(i),i= 97,104)	/ 292, 293, 292, 293,  -9,  -9,  -9,  -9/
data	(yychk(i),i=105,112)	/ -23, -43,  -7, 292, 295, -42,  -7, 295/
data	(yychk(i),i=113,120)	/ 295, -35, -37, -47, 285, 286,  -7, -38/
data	(yychk(i),i=121,128)	/ -37, -40, -37, 293, -44, 285, 286, -45/
data	(yychk(i),i=129,136)	/ 294, -46, 282, 271, 272, 273, 274, 275/
data	(yychk(i),i=137,144)	/ 276, 277, 278, 279, 280, 281, 283, 284/
data	(yychk(i),i=145,152)	/ 282, 293, -23, 293, 293, -46, -24,  -7/
data	(yychk(i),i=153,160)	/ 285, 286, 287, 288, 289, -44, -44, 294/
data	(yychk(i),i=161,168)	/ -44, 295, -26,  -7, -27,  -7, -31,  -7/
data	(yychk(i),i=169,176)	/ -23, -44, -44, -44, -44, -44, -44, 295/
data	(yychk(i),i=177,184)	/ 293, -23, -23, -23, 295, -25,  -7, 292/
data	(yychk(i),i=185,192)	/ -28,  -7, -28, -23, -23, -29, 266,  -7/
data	(yychk(i),i=193,200)	/ -29, -30, 267,  -7, 293, -30, 293, -23/
data	(yychk(i),i=201,201)	/ -23/
short	yydef[201]
data	(yydef(i),i=  1,  8)	/ 103,  -2, 103,   2, 103,  17, 103,   6/
data	(yydef(i),i=  9, 16)	/   7,  16,  18,   0,  26,  27,   3,   5/
data	(yydef(i),i= 17, 24)	/   8,   0,   0,   0,  13,  15,  19,   0/
data	(yydef(i),i= 25, 32)	/   0,   0,   0,  30,  25,   4,   9,  10/
data	(yydef(i),i= 33, 40)	/  24,   0,   0,  14,  20,  21,   0,   0/
data	(yydef(i),i= 41, 48)	/   1,  28,  29,  31,  33,  34,  35,  36/
data	(yydef(i),i= 49, 56)	/  37,  38,  39,   0,   0,   0,  56,  57/
data	(yydef(i),i= 57, 64)	/  58,   0,   0,   0,   0,   0,   0,   0/
data	(yydef(i),i= 65, 72)	/   0,   0,   0,  32, 103,   0,   0,   0/
data	(yydef(i),i= 73, 80)	/   0,   0,  59,  60,   0,  63,  64,   0/
data	(yydef(i),i= 81, 88)	/  67,  68,   0,   0,   0,   0,   0, 103/
data	(yydef(i),i= 89, 96)	/  41,   0,   0, 103,   0,   0,   0, 103/
data	(yydef(i),i= 97,104)	/   0, 103,   0, 103,  11,  12,  22,  23/
data	(yydef(i),i=105,112)	/   0,   0,  74,   0,   0, 103,  72,   0/
data	(yydef(i),i=113,120)	/   0,  61,  62,   0,  98,  99, 100,  65/
data	(yydef(i),i=121,128)	/  66,  69,  70, 103,  73,   0,   0,   0/
data	(yydef(i),i=129,136)	/   0,  84,  85,  86,  87,  88,  89,  90/
data	(yydef(i),i=137,144)	/  91,  92,  93,  94,  95,  96, 101, 102/
data	(yydef(i),i=145,152)	/   0, 103,  71, 103, 103,  97, 103,  42/
data	(yydef(i),i=153,160)	/   0,   0,   0,   0,   0,  80,  81,   0/
data	(yydef(i),i=161,168)	/   0,   0, 103,  46, 103,  48, 103,  50/
data	(yydef(i),i=169,176)	/  40,  75,  76,  77,  78,  79,   0,  83/
data	(yydef(i),i=177,184)	/ 103,   0, 103, 103,  82, 103,  44, 103/
data	(yydef(i),i=185,192)	/ 103,  51, 103,  43,  45, 103,   0,  53/
data	(yydef(i),i=193,200)	/ 103,  47,   0,  55, 103,  49, 103,  52/
data	(yydef(i),i=201,201)	/  54/

begin
	call smark (yysp)
	call salloc (yyv, (YYMAXDEPTH+2) * YYOPLEN, TY_STRUCT)

	# Initialization.  The first element of the dynamically allocated
	# token value stack (yyv) is used for yyval, the second for yylval,
	# and the actual stack starts with the third element.

	yystate = 0
	yychar = -1
	yynerrs = 0
	yyerrflag = 0
	yyps = 0
	yyval = yyv
	yylval = yyv + YYOPLEN
	yypv = yylval

yystack_
	# SHIFT -- Put a state and value onto the stack.  The token and
	# value stacks are logically the same stack, implemented as two
	# separate arrays.

	if (yydebug) {
	    call printf ("state %d, char 0%o\n")
		call pargs (yystate)
		call pargi (yychar)
	}
	yyps = yyps + 1
	yypv = yypv + YYOPLEN
	if (yyps > YYMAXDEPTH) {
	    call sfree (yysp)
	    call eprintf ("yacc stack overflow\n")
	    return (ERR)
	}
	yys[yyps] = yystate
	YYMOVE (yyval, yypv)

yynewstate_
	# Process the new state.
	yyn = yypact[yystate+1]

	if (yyn <= YYFLAG)
	    goto yydefault_			# simple state

	# The variable "yychar" is the lookahead token.
	if (yychar < 0) {
	    yychar = yylex (fd, yylval)
	    if (yychar < 0)
		yychar = 0
	}
	yyn = yyn + yychar
	if (yyn < 0 || yyn >= YYLAST)
	    goto yydefault_

	yyn = yyact[yyn+1]
	if (yychk[yyn+1] == yychar) {		# valid shift
	    yychar = -1
	    YYMOVE (yylval, yyval)
	    yystate = yyn
	    if (yyerrflag > 0)
		yyerrflag = yyerrflag - 1
	    goto yystack_
	}

yydefault_
	# Default state action.

	yyn = yydef[yystate+1]
	if (yyn == -2) {
	    if (yychar < 0) {
		yychar = yylex (fd, yylval)
		if (yychar < 0)
		    yychar = 0
	    }

	    # Look through exception table.
	    yyxi = 1
	    while ((yyexca[yyxi] != (-1)) || (yyexca[yyxi+1] != yystate))
		yyxi = yyxi + 2
	    for (yyxi=yyxi+2;  yyexca[yyxi] >= 0;  yyxi=yyxi+2) {
		if (yyexca[yyxi] == yychar)
		    break
	    }

	    yyn = yyexca[yyxi+1]
	    if (yyn < 0) {
		call sfree (yysp)
		return (OK)			# ACCEPT -- all done
	    }
	}


	# SYNTAX ERROR -- resume parsing if possible.

	if (yyn == 0) {
	    switch (yyerrflag) {
	    case 0, 1, 2:
		if (yyerrflag == 0) {		# brand new error
		    call eprintf ("syntax error\n")
yyerrlab_
		    yynerrs = yynerrs + 1
		    # fall through...
		}

	    # case 1:
	    # case 2: incompletely recovered error ... try again
		yyerrflag = 3

		# Find a state where "error" is a legal shift action.
		while (yyps >= 1) {
		    yyn = yypact[yys[yyps]+1] + YYERRCODE
		    if ((yyn >= 0) && (yyn < YYLAST) &&
			(yychk[yyact[yyn+1]+1] == YYERRCODE)) {
			    # Simulate a shift of "error".
			    yystate = yyact[yyn+1]
			    goto yystack_
		    }
		    yyn = yypact[yys[yyps]+1]

		    # The current yyps has no shift on "error", pop stack.
		    if (yydebug) {
			call printf ("error recovery pops state %d, ")
			    call pargs (yys[yyps])
			call printf ("uncovers %d\n")
			    call pargs (yys[yyps-1])
		    }
		    yyps = yyps - 1
		    yypv = yypv - YYOPLEN
		}

		# ABORT -- There is no state on the stack with an error shift.
yyabort_
		call sfree (yysp)
		return (ERR)


	    case 3: # No shift yet; clobber input char.

		if (yydebug) {
		    call printf ("error recovery discards char %d\n")
			call pargi (yychar)
		}

		if (yychar == 0)
		    goto yyabort_		# don't discard EOF, quit
		yychar = -1
		goto yynewstate_		# try again in the same state
	    }
	}


	# REDUCE -- Reduction by production yyn.

	if (yydebug) {
	    call printf ("reduce %d\n")
		call pargs (yyn)
	}
	yyps  = yyps - yyr2[yyn+1]
	yypvt = yypv
	yypv  = yypv - yyr2[yyn+1] * YYOPLEN
	YYMOVE (yypv + YYOPLEN, yyval)
	yym   = yyn

	# Consult goto table to find next state.
	yyn = yyr1[yyn+1]
	yyj = yypgo[yyn+1] + yys[yyps] + 1
	if (yyj >= YYLAST)
	    yystate = yyact[yypgo[yyn+1]+1]
	else {
	    yystate = yyact[yyj+1]
	    if (yychk[yystate+1] != -yyn)
		yystate = yyact[yypgo[yyn+1]+1]
	}

	# Perform action associated with the grammar rule, if any.
	switch (yym) {
	    
case 1:
# line 41 "parser.y"
{
		    return (OK)
		}
case 2:
# line 44 "parser.y"
{
		    return (ERR)
		}
case 5:
# line 55 "parser.y"
{
		    call pr_error ("The observation section is empty",
				   PERR_WARNING)
		}
case 6:
# line 59 "parser.y"
{
		    call pr_error ("The observation section is empty",
				   PERR_WARNING)
		}
case 7:
# line 63 "parser.y"
{
		    call pr_error ("The observation section is undefined",
				   PERR_WARNING)
		}
case 10:
# line 71 "parser.y"
{
		    call pr_obscol (LEX_ID (yypvt-YYOPLEN), LEX_ID (yypvt))
		}
case 11:
# line 74 "parser.y"
{
		    call pr_errcol (LEX_ID (yypvt-2*YYOPLEN), LEX_ID (yypvt))
		}
case 12:
# line 77 "parser.y"
{
		    call pr_wtscol (LEX_ID (yypvt-2*YYOPLEN), LEX_ID (yypvt))
		}
case 15:
# line 88 "parser.y"
{
		    call pr_error ("The catalog section is empty",
				   PERR_WARNING)
		}
case 16:
# line 92 "parser.y"
{
		    call pr_error ("The catalog section is empty",
				   PERR_WARNING)
		}
case 18:
# line 99 "parser.y"
{
			call pr_puti (MINCOL, 2)
		}
case 21:
# line 105 "parser.y"
{
		    call pr_catcol (LEX_ID (yypvt-YYOPLEN), LEX_ID (yypvt))
		}
case 22:
# line 108 "parser.y"
{
		    call pr_errcol (LEX_ID (yypvt-2*YYOPLEN), LEX_ID (yypvt))
		}
case 23:
# line 111 "parser.y"
{
		    call pr_wtscol (LEX_ID (yypvt-2*YYOPLEN), LEX_ID (yypvt))
		}
case 24:
# line 117 "parser.y"
{
		    YYMOVE (yypvt, yyval)
		}
case 29:
# line 134 "parser.y"
{
		    call pr_error ("The transformation section is empty",
				   PERR_WARNING)
		}
case 30:
# line 138 "parser.y"
{
		    call pr_error ("The transformation section is empty",
				   PERR_WARNING)
		}
case 40:
# line 154 "parser.y"
{
		    call pr_treq (LEX_ID (yypvt-6*YYOPLEN),
			LEX_ID (yypvt-3*YYOPLEN), LEX_ID (yypvt),
			LEX_CODE (yypvt-3*YYOPLEN), LEX_CLEN (yypvt-3*YYOPLEN),
			LEX_CODE (yypvt), LEX_CLEN (yypvt))
		}
case 41:
# line 162 "parser.y"
{
		    call pr_section (PRS_TRNREF)
		}
case 42:
# line 167 "parser.y"
{
		    call pr_section (PRS_TRNFIT)
		}
case 43:
# line 175 "parser.y"
{
		    call pr_trder (LEX_ID (yypvt-6*YYOPLEN), LEX_ID (yypvt-4*YYOPLEN),
			LEX_ID (yypvt), LEX_CODE (yypvt), LEX_CLEN (yypvt))
		}
case 44:
# line 181 "parser.y"
{
		    call pr_section (PRS_TRNDER)
		}
case 45:
# line 189 "parser.y"
{
		    call pr_trplot (LEX_ID (yypvt-6*YYOPLEN),
			LEX_ID (yypvt-2*YYOPLEN), LEX_ID (yypvt),
			LEX_CODE (yypvt-2*YYOPLEN), LEX_CLEN (yypvt-2*YYOPLEN),
			LEX_CODE (yypvt), LEX_CLEN (yypvt))
		}
case 46:
# line 197 "parser.y"
{
		    call pr_section (PRS_TRNPLOT)
		}
case 47:
# line 207 "parser.y"
{
		    call pr_erreq (LEX_ID (yypvt-7*YYOPLEN), LEX_ID (yypvt-3*YYOPLEN),
			LEX_ID (yypvt-YYOPLEN), LEX_ID (yypvt),
			LEX_CODE (yypvt-3*YYOPLEN), LEX_CLEN (yypvt-3*YYOPLEN),
			LEX_CODE (yypvt-YYOPLEN), LEX_CLEN (yypvt-YYOPLEN),
			LEX_CODE (yypvt), LEX_CLEN (yypvt))
		}
case 48:
# line 216 "parser.y"
{
		    call pr_section (PRS_ERREQ)
		}
case 49:
# line 226 "parser.y"
{
		    call pr_wtseq (LEX_ID (yypvt-7*YYOPLEN), LEX_ID (yypvt-3*YYOPLEN),
			LEX_ID (yypvt-YYOPLEN), LEX_ID (yypvt),
			LEX_CODE (yypvt-3*YYOPLEN), LEX_CLEN (yypvt-3*YYOPLEN),
			LEX_CODE (yypvt-YYOPLEN), LEX_CLEN (yypvt-YYOPLEN),
			LEX_CODE (yypvt), LEX_CLEN (yypvt))
		}
case 50:
# line 235 "parser.y"
{
		    call pr_section (PRS_WTSEQ)
		}
case 51:
# line 243 "parser.y"
{
		    call pr_section (PRS_LMTEQ)
		}
case 52:
# line 248 "parser.y"
{
		    YYMOVE (yypvt, yyval)
		}
case 53:
# line 251 "parser.y"
{
		    call strcpy ("", LEX_ID (yyval), LEN_ID)
		    LEX_CLEN (yyval) = 0
		}
case 54:
# line 257 "parser.y"
{
		    YYMOVE (yypvt, yyval)
		}
case 55:
# line 260 "parser.y"
{
		    call strcpy ("", LEX_ID (yyval), LEN_ID)
		    LEX_CLEN (yyval) = 0
		}
case 62:
# line 279 "parser.y"
{
		    call pr_fitpar (LEX_ID (yypvt-2*YYOPLEN), LEX_ID (yypvt))
		}
case 66:
# line 288 "parser.y"
{
		    call pr_const (LEX_ID (yypvt-2*YYOPLEN), LEX_ID (yypvt))
		}
case 70:
# line 297 "parser.y"
{
		    call pr_delta (LEX_ID (yypvt-2*YYOPLEN), LEX_ID (yypvt))
		}
case 71:
# line 305 "parser.y"
{
		    call pr_seteq (LEX_ID (yypvt-3*YYOPLEN), LEX_ID (yypvt),
				   LEX_CODE (yypvt), LEX_CLEN (yypvt))
		}
case 72:
# line 311 "parser.y"
{
		    call pr_section (PRS_SETEQ)
		}
case 73:
# line 331 "parser.y"
{
		    YYMOVE (yypvt, yyval)
		    call pr_cend (yyval)
		}
case 74:
# line 337 "parser.y"
{
		    call pr_cinit ()
		}
case 75:
# line 342 "parser.y"
{
		    call pr_cat3 (LEX_ID (yypvt-2*YYOPLEN), LEX_ID (yypvt-YYOPLEN), LEX_ID (yypvt),
			LEX_ID (yyval), LEN_ID)
		    call pr_cgen (PLUS, "", INDEFR)
		}
case 76:
# line 347 "parser.y"
{
		    call pr_cat3 (LEX_ID (yypvt-2*YYOPLEN), LEX_ID (yypvt-YYOPLEN), LEX_ID (yypvt),
			LEX_ID (yyval), LEN_ID)
		    call pr_cgen (MINUS, "", INDEFR)
		}
case 77:
# line 352 "parser.y"
{
		    call pr_cat3 (LEX_ID (yypvt-2*YYOPLEN), LEX_ID (yypvt-YYOPLEN), LEX_ID (yypvt),
			LEX_ID (yyval), LEN_ID)
		    call pr_cgen (STAR, "", INDEFR)
		}
case 78:
# line 357 "parser.y"
{
		    call pr_cat3 (LEX_ID (yypvt-2*YYOPLEN), LEX_ID (yypvt-YYOPLEN), LEX_ID (yypvt),
		  	LEX_ID (yyval), LEN_ID)
		    call pr_cgen (SLASH, "", INDEFR)
		}
case 79:
# line 362 "parser.y"
{
		    call pr_cat3 (LEX_ID (yypvt-2*YYOPLEN), LEX_ID (yypvt-YYOPLEN), LEX_ID (yypvt),
			LEX_ID (yyval), LEN_ID)
		    call pr_cgen (EXPON, "", INDEFR)
		}
case 80:
# line 367 "parser.y"
{
		    call pr_cat2 (LEX_ID (yypvt-YYOPLEN), LEX_ID (yypvt), LEX_ID (yyval), LEN_ID)
		    call pr_cgen (UPLUS, "", INDEFR)
		}
case 81:
# line 371 "parser.y"
{
		    call pr_cat2 (LEX_ID (yypvt-YYOPLEN), LEX_ID (yypvt), LEX_ID (yyval), LEN_ID)
		    call pr_cgen (UMINUS, "", INDEFR)
		}
case 82:
# line 375 "parser.y"
{
		    call pr_cat4 (LEX_ID (yypvt-3*YYOPLEN), LEX_ID (yypvt-2*YYOPLEN), LEX_ID (yypvt-YYOPLEN),
			LEX_ID (yypvt), LEX_ID (yyval), LEN_ID)
		    call pr_cgen (LEX_TOK (yypvt-3*YYOPLEN), "", INDEFR)
		}
case 83:
# line 380 "parser.y"
{
		    call pr_cat3 (LEX_ID (yypvt-2*YYOPLEN), LEX_ID (yypvt-YYOPLEN), LEX_ID (yypvt),
			LEX_ID (yyval), LEN_ID)
		    }
case 84:
# line 384 "parser.y"
{
		    YYMOVE (yypvt, yyval)
		    call pr_cgen (RNUMBER, "", LEX_VAL (yypvt))
		}
case 85:
# line 388 "parser.y"
{
		    call pr_chkid (LEX_ID (yypvt))
		    YYMOVE (yypvt, yyval)
		    call pr_cgen (IDENTIFIER, LEX_ID (yypvt), INDEFR)
		}
case 86:
# line 395 "parser.y"
{
	    	    YYMOVE (yypvt, yyval)
		}
case 87:
# line 398 "parser.y"
{
	    	    YYMOVE (yypvt, yyval)
		}
case 88:
# line 401 "parser.y"
{
		    YYMOVE (yypvt, yyval)
		}
case 89:
# line 404 "parser.y"
{
		    YYMOVE (yypvt, yyval)
		}
case 90:
# line 407 "parser.y"
{
		    YYMOVE (yypvt, yyval)
		}
case 91:
# line 410 "parser.y"
{
		    YYMOVE (yypvt, yyval)
		}
case 92:
# line 413 "parser.y"
{
		    YYMOVE (yypvt, yyval)
		}
case 93:
# line 416 "parser.y"
{
		    YYMOVE (yypvt, yyval)
		}
case 94:
# line 419 "parser.y"
{
		    YYMOVE (yypvt, yyval)
		}
case 95:
# line 422 "parser.y"
{
		    YYMOVE (yypvt, yyval)
		}
case 96:
# line 425 "parser.y"
{
		    YYMOVE (yypvt, yyval)
		}
case 97:
# line 431 "parser.y"
{
		    call pr_cat2 (LEX_ID (yypvt-YYOPLEN), LEX_ID (yypvt),
			LEX_ID (yyval), LEN_ID)
		    LEX_VAL (yyval) = LEX_VAL (yypvt)
		}
case 98:
# line 438 "parser.y"
{
		    YYMOVE (yypvt, yyval)
		}
case 99:
# line 441 "parser.y"
{
		    YYMOVE (yypvt, yyval)
		}
case 100:
# line 444 "parser.y"
{
		    call strcpy ("", LEX_ID (yyval), LEN_ID)
		}
case 101:
# line 449 "parser.y"
{
		    YYMOVE (yypvt, yyval)
		}
case 102:
# line 452 "parser.y"
{
		    YYMOVE (yypvt, yyval)
		}	}

	goto yystack_				# stack new state and value
end
