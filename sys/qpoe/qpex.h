# QPEX.H -- QPOE expression evaluator definitions.

# Size limiting definitions.
define	DEF_PROGBUFLEN		65536	# default program buffer length, ints
define	DEF_DATABUFLEN		65536	# default data buffer length, chars
define	DEF_SZEXPRBUF		2048	# default size expression buffer
define	INC_SZEXPRBUF		2048	# increment if overflow
define	DEF_XLEN		256	# default (initial) range buffer size
define	MAX_INSTRUCTIONS	ARB	# arbitrary do-loop index
define	MAX_LEVELS		32	# max levels of program nesting
define	DEF_MAXRRLUTLEN		1024	# max RRLUT (reduced resolution) length
define	DEF_MAXFRLUTLEN		8192	# max FRLUT (full resolution) length
define	DEF_LUTMINRANGES	5	# use RRLUT if more ranges than this
define	DEF_LUTSCALE		15	# multiplied by nranges to get rrlutlen

# Magic values used to represent open ranges :N and N:.
define	LEFTI		-MAX_INT
define	RIGHTI		 MAX_INT
define	LEFTR		-MAX_REAL
define	RIGHTR		 MAX_REAL
define	LEFTD		-MAX_DOUBLE
define	RIGHTD		 MAX_DOUBLE

define	IS_LEFTI	(($1) == -MAX_INT)
define	IS_RIGHTI	(($1) ==  MAX_INT)
define	IS_LEFTR	(($1) <= -MAX_REAL)
define	IS_RIGHTR	(($1) >=  MAX_REAL)
define	IS_LEFTD	(($1) <= -MAX_DOUBLE)
define	IS_RIGHTD	(($1) >=  MAX_DOUBLE)

# The compiled expression descriptor.  The program buffer holds the compiled
# expression to be interpreted to test each data event structure.  The data
# buffer is used to store program data, e.g., lookup table descriptors,
# TY_DOUBLE constants (these are too large to be stored directly in the
# compiled program), and the textual expressions compiled to generate the
# program; the latter are used by QPEX_GETFILTER to regenerate the current
# expression.  The expression terms (ET) and lookup table (LT) descriptors
# are maintained on linked lists.  New ET descriptors are linked onto the
# tail of the ET list; LT descriptors are linked onto the head of the LT list.
# The program and data buffers are *nonrelocatable* (hence fixed in size)
# to allow use of absolute pointers to reference structures within the buffers.

define	LEN_EXDES	16
define	EX_QP		Memi[$1]	# back pointer to QPOE descriptor
define	EX_DEBUG	Memi[$1+1]	# debug level
define	EX_START	Memi[$1+2]	# pointer to first instruction
define	EX_PB		Memi[$1+3]	# pointer to program buffer (int)
define	EX_PBTOP	Memi[$1+4]	# pointer to top+1 of pb
define	EX_PBOP		Memi[$1+5]	# pointer to next avail. cell in pb
define	EX_DB		Memi[$1+6]	# data buffer pointer (char)
define	EX_DBTOP	Memi[$1+7]	# pointer to top+1 of db
define	EX_DBOP		Memi[$1+8]	# pointer to next avail. cell in db
define	EX_MAXFRLUTLEN	Memi[$1+9]	# max full-res lut length
define	EX_MAXRRLUTLEN	Memi[$1+10]	# max reduced-res lut length
define	EX_LUTMINRANGES	Memi[$1+11]	# min ranges required for a LUT
define	EX_LUTSCALE	Memi[$1+12]	# scale nranges to frlutlen
define	EX_ETHEAD	Memi[$1+13]	# offset of first expr term descriptor
define	EX_ETTAIL	Memi[$1+14]	# offset of last expr term descriptor
define	EX_LTHEAD	Memi[$1+15]	# offset of first LUT descriptor

# Expression terms descriptor.  Stored in the data buffer and maintained
# as a linked list.

define	LEN_ETDES	9
define	ET_ATTTYPE	Memi[$1]	# datatype of attribute
define	ET_ATTOFF	Memi[$1+1]	# *typed* offset of attribute
define	ET_PROGPTR	Memi[$1+2]	# pointer to program segment 
define	ET_NINSTR	Memi[$1+3]	# program segment size, instructions
define	ET_DELETED	Memi[$1+4]	# set if term is deleted
define	ET_ATNAME	Memi[$1+5]	# attribute name used in expr
define	ET_ASSIGNOP	Memi[$1+6]	# type of assignment ("=", "+=")
define	ET_EXPRTEXT	Memi[$1+7]	# saved expr text
define	ET_NEXT		Memi[$1+8]	# databuf offset of next ET struct

# Lookup table descriptor.  Stored in the data buffer and maintained as a
# linked list.  The table itself is separately allocated.

define	LEN_LTDES	10
define	LT_NEXT		Memi[$1]	# pointer to next stored LUT
define	LT_TYPE		Memi[$1+1]	# TY_SHORT pointer to stored LUT
define	LT_LUTP		Memi[$1+2]	# TY_SHORT pointer to stored LUT
define	LT_NBINS	Memi[$1+3]	# number of lookup table entries
define	LT_LEFT		Memi[$1+4]	# lut value if index off left end
define	LT_RIGHT	Memi[$1+5]	# lut value if index off right end
define	LT_I0		Memr[P2R($1+6)]	# zero point for integer LUT
define	LT_IS		Memr[P2R($1+8)]	# scale factor for integer LUT
define	LT_R0		Memr[P2R($1+6)]	# zero point for real LUT
define	LT_RS		Memr[P2R($1+8)]	# scale factor for real LUT
define	LT_D0		Memd[P2D($1+6)]	# zero point for double LUT
define	LT_DS		Memd[P2D($1+8)]	# scale factor for double LUT

define	LT_LUT		Mems[LT_LUTP($1)+$2-1]		# LT_LUT(lt,i)

# Macros for referencing the fields of an instruction.  TY_DOUBLE arguments
# are stored in the data buffer, storing an offset in the instruction field.

define	LEN_INSTRUCTION		4	# instruction length, ints

define	OPCODE		Memi[$1]	# instruction opcode.
define	IARG1		Memi[$1+1]	# first integer argument
define	IARG2		Memi[$1+2]	# second integer argument
define	IARG3		Memi[$1+3]	# third integer argument
define	RARG1		Memr[P2R($1+1)]	# first real argument
define	RARG2		Memr[P2R($1+2)]	# second real argument
define	RARG3		Memr[P2R($1+3)]	# third real argument
define	DARG1		Memd[IARG1($1)]	# first double argument
define	DARG2		Memd[IARG2($1)]	# second double argument
define	DARG3		Memd[IARG3($1)]	# third double argument

# Instruction opcodes.

define	PASS		00		# set pass=true and return
define	RET		01		# return from subprogram
define	NOP		02		# no-operation
define	GOTO		03		# goto offset
define	XIFT		04		# exit if expr-value true
define	XIFF		05		# exit if expr-value false
define	LDSI		06		# load short to int
define	LDII		07		# load int
define	LDRR		08		# load real
define	LDRD		09		# load real to double
define	LDDD		10		# load double

define	BTTI		11		# bit test, int
define	EQLI		12		# test if equal
define	EQLR		13
define	EQLD		14
define	LEQI		15		# test if less than or equal
define	LEQR		16
define	LEQD		17
define	GEQI		18		# test if greater than or equal
define	GEQR		19
define	GEQD		20
define	RNGI		21		# range test
define	RNGR		22
define	RNGD		23

define	BTTXS		24		# bit test direct and exit if false
define	BTTXI		25
define	NEQXS		26		# not equals test and exit
define	NEQXI		27
define	NEQXR		28
define	NEQXD		29
define	EQLXS		30		# equality test direct and exit if false
define	EQLXI		31
define	EQLXR		32
define	EQLXD		33
define	LEQXS		34		# LEQ test direct and exit if false
define	LEQXI		35
define	LEQXR		36
define	LEQXD		37
define	GEQXS		38		# GEQ test direct and exit if false
define	GEQXI		39
define	GEQXR		40
define	GEQXD		41
define	RNGXS		42		# range test direct and exit if false
define	RNGXI		43
define	RNGXR		44
define	RNGXD		45

define	LUTXS		46		# lookup table test
define	LUTXI		47
define	LUTXR		48
define	LUTXD		49
