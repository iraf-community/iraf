# VEX.H -- Structures and constants used by vex

# Strings

define	FN1STR  "abs acos asin atan cos cosh cube double exp int log log10 \
nint real sin sinh sqr sqrt tan tanh"

define	FN2STR  "atan2 dim max min mod sign"

# Characters

define	BLANK		' '
define	CMTCHAR		'#'
define	DOLLAR		'$'
define	DOT		'.'

# Function codes

define	FN1_ABS 	1
define	FN1_ACOS 	2
define	FN1_ASIN 	3
define	FN1_ATAN 	4
define	FN1_COS 	5
define	FN1_COSH	6
define	FN1_CUBE	7
define	FN1_DOUBLE	8
define	FN1_EXP 	9
define	FN1_INT 	10
define	FN1_LOG 	11
define	FN1_LOG10	12
define	FN1_NINT	13
define	FN1_REAL	14
define	FN1_SIN 	15
define	FN1_SINH	16
define	FN1_SQR 	17
define	FN1_SQRT	18
define	FN1_TAN 	19
define	FN1_TANH	20

define	FN2_ATAN2	1
define	FN2_DIM 	2
define	FN2_MAX 	3
define	FN2_MIN 	4
define	FN2_MOD 	5
define	FN2_SIGN	6

# These constants are taken from the output of xyacc run on vexcompile.y

define	Y_WRONG		257
define	Y_LPAR		258
define	Y_RPAR		259
define	Y_COMMA		260
define	Y_VAR		261
define	Y_INT		262
define	Y_REAL		263
define	Y_DOUBLE	264
define	Y_FN1		265
define	Y_FN2		266
define	Y_IF		267
define	Y_THEN		268
define	Y_ELSE		269
define	Y_DONE		270
define	Y_OR		271
define	Y_AND		272
define	Y_NOT		273
define	Y_EQ		274
define	Y_NE		275
define	Y_LT		276
define	Y_GT		277
define	Y_LE		278
define	Y_GE		279
define	Y_ADD		280
define	Y_SUB		281
define	Y_MUL		282
define	Y_DIV		283
define	Y_NEG		284
define	Y_POW		285

# Array lengths

define	MAX_TOKEN	31
define	MAX_STACK	64

# Pseudocode structure

define	SZ_VEXSTRUCT	2

define	VEX_CODE	Memi[$1]	# pointer to code array
define	VEX_STACK	Memi[$1+1]	# pointer to stack structure

# Stack structure

define	SZ_STKSTRUCT	6

define	STK_TOP		Memi[$1]	# top of stack
define	STK_HIGH	Memi[$1+1]	# high water mark in stack
define	STK_LENVAL	Memi[$1+2]	# length of each value array
define	STK_NULLARY	Memi[$1+3]	# pointer to array of null values
define	STK_VALARY	Memi[$1+4]	# pointer to value stack
define	STK_TYPARY	Memi[$1+5]	# pointer to type stack

define	STK_NULL	Memb[STK_NULLARY($1)+$2]
define	STK_VALUE	Memi[STK_VALARY($1)+$2]
define	STK_TYPE	Memi[STK_TYPARY($1)+$2]

define	TOP		-1		# Symbolic constant for top of stack

