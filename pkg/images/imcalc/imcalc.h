# IMCALC definitions.

define	YYMAXDEPTH	150			# parser stack length
define	MAX_REGISTERS	100			# max operand registers
define	MAX_IMAGES	50			# max images in an expr
define	MAX_ARGS	32			# max arguments to a function
define	MAX_CALLS	32			# max function calls per stmt
define	SZ_SBUF		1024			# size of the string buffer
define	LEN_INSTRUCTION	5			# length of metacode instr.

define	S2C		((($1)-1)*SZ_STRUCT+1)	# struct ptr to char ptr
define	S2D		((($1)-1)*SZ_STRUCT/SZ_DOUBLE+1)	# to double
define	S2X		((($1)-1)*SZ_STRUCT/SZ_COMPLEX+1)	# to complex


# Parser stack structure.  The operand value is stored in a VAL field if the
# operand is a constant, else in the associated register.  Be sure to keep
# the VAL field at an offset aligned to SZ_DOUBLE.

define	LEN_OPERAND	4			# size of operand structure
define	YYOPLEN		LEN_OPERAND		# for the parser

define	O_CONSTANT	Memi[($1)]		# operand is a constant
define	O_REGISTER	Memi[($1)+1]		# register pointer, if any
define	O_LBUF		Memi[($1)+2]		# line buffer pointer, if any
define	O_VALB		Memi[($1)+2]		# bool value (stored as int)
define	O_VALC		Memc[Memi[($1)+2]]	# string val (in string buffer)
define	O_VALS		Memi[($1)+2]		# short value (stored as int)
define	O_VALI		Memi[($1)+2]		# int value
define	O_VALL		Meml[($1)+2]		# long value
define	O_VALR		Memr[($1)+2]		# real value
define	O_VALD		Memd[S2D(($1)+2)]	# double value
define	O_VALX		Memx[S2X(($1)+2)]	# complex value


# Register structure.

define	LEN_REGISTER	4			# size of a register
define	R_REGPTR	((($2)-1)*LEN_REGISTER+($1))

define	R_DATATYPE	Memi[($1)]		# datatype of register
define	R_LENGTH	Memi[($1)+1]		# length of vector value
define	R_LBUF		Memi[($1)+2]		# line buffer pointer, if any
define	R_VALB		Memi[($1)+2]		# bool value (stored as int)
define	R_VALS		Memi[($1)+2]		# short value (stored as int)
define	R_VALI		Memi[($1)+2]		# int value
define	R_VALL		Meml[($1)+2]		# long value
define	R_VALR		Memr[($1)+2]		# real value
define	R_VALD		Memd[S2D(($1)+2)]	# double value
define	R_VALX		Memx[S2X(($1)+2)]	# complex value


# Image descriptor structure.

define	LEN_IMAGE	(5+7)
define	I_IMPTR		((($2)-1)*LEN_IMAGE+($1))

define	I_IM		Memi[$1]		# IMIO image descriptor
define	I_LBUF		Memi[$1+1]		# last line buffer
define	I_PIXTYPE	Memi[$1+2]		# pixel datatype
define	I_LINELEN	Memi[$1+3]		# line length
define	I_ATEOF		Memi[$1+4]		# positioned to EOF
define	I_V		Meml[$1+5]		# next line vector


# Instruction opcodes.

define	OP_LOAD		1
define	OP_STORE	2
define	OP_BNEOF	3
define	OP_SELECT	4
define	OP_CALL		5
define	OP_CHT		6
define	OP_NEG		7
define	OP_ADD		8
define	OP_SUB		9
define	OP_MUL		10
define	OP_DIV		11
define	OP_POW		12
define	OP_NOT		13
define	OP_AND		14
define	OP_OR		15
define	OP_LT		16
define	OP_GT		17
define	OP_LE		18
define	OP_GE		19
define	OP_EQ		20
define	OP_NE		21
