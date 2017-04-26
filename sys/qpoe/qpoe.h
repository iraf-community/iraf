# QPOE.H -- QPOE data definitions (private to the package).

# Size limiting definitions.
define	DEF_BLOCKFACTOR	1.0		# default block factor for image matrix
define	DEF_BUCKETLEN	1024		# def nevents per bucket
define	DEF_FMMAXLFILES 128		# def maxlfile per datafile
define	DEF_FMMAXPTPAGES 256		# def maxptpages per datafile
define	DEF_FMPAGESIZE	512		# def datafile page size
define	DEF_FMCACHESIZE 8		# def buffer cache size
define	DEF_STINDEXLEN	100		# def symtab hash index len
define	DEF_STSTABLEN	2048		# initial symbol table len
define	DEF_STSBUFSIZE	2048		# initial string buf size
define	DEF_MAXPUSHBACK	8192		# max pushed back chars (macros)
define	DEF_OPTBUFSIZE	(512*512*2)	# default buffer size for IMIO/QPF/FIO
define	MAX_INDIR	20		# max macro indirection
define	MAX_REDEF	20		# max entries for a symbol
define	MAX_FIELDS	50		# max fields in a user structure
define	INC_STRLEN	32		# unit of storage for strings
define	LEN_PVAL	64		# max TY_USER struct size (in doubles)
define	SZ_QPDFNAME	255		# max size QPOE filename
define	SZ_TEXTBUF	2048		# handy text buffer for macro expansion
define	SZ_TOKBUF	256		# token buffer size

# Magic numbers.
define	LF_QPOE 	1		# QPOE file header and symbol table
define	LF_STATICPARS	2		# static (fixed size) params
define	QPOE_MAGIC	121120B		# QPOE magic code (descriptor type)
define	QPOE_VERSION	101		# QPOE interface version number
define	QPOE_TITLE	"QPOE-V1.2"	# title string, for symbol table
define	QPOE_EXTN	".qp"		# QPOE file extension
define	QPOE_MACROEXTN	".qpm"		# QPOE macro definitions file extension
define	QPWCS		"qpwcs"		# header parameter for default WCS
define	IMMEDIATE	0		# for qp_sizeof
define	INSTANCEOF	1		# for qp_sizeof

# The main QPOE descriptor.
define	LEN_QPDES	160
define	QP_MAGIC	Memi[$1]	# descriptor type code
define	QP_VERSION	Memi[$1+1]	# QPOE version number
define	QP_ACTIVE	Memi[$1+2]	# descriptor fully activated
define	QP_FM		Memi[$1+3]	# datafile handle
define	QP_ST		Memi[$1+4]	# datafile symbol table handle
define	QP_QM		Memi[$1+5]	# global QPOE symbol table handle
define	QP_MODE		Memi[$1+6]	# datafile access mode
define	QP_OQP		Memi[$1+7]	# o_qp, if new copy file
define	QP_EXPBLEN	Memi[$1+8]	# QPEX program buffer length
define	QP_EXDBLEN	Memi[$1+9]	# QPEX data buffer length
define	QP_EXMAXFRLLEN	Memi[$1+10]	# QPEX max FRLUT length
define	QP_EXMAXRRLLEN	Memi[$1+11]	# QPEX max RRLUT length
define	QP_EXLMINRANGES	Memi[$1+12]	# QPEX min ranges before using LUT
define	QP_EXLSCALE	Memi[$1+13]	# QPEX scale nranges to LUT bins
define	QP_SZPBBUF	Memi[$1+14]	# size of pushback buffer for macros
define	QP_BUCKETLEN	Memi[$1+15]	# QPIO event file bucket size
define	QP_FMMAXLFILES	Memi[$1+16]	# FMIO maxlfiles
define	QP_FMMAXPTPAGES	Memi[$1+17]	# FMIO maxptpages
define	QP_FMPAGESIZE	Memi[$1+18]	# FMIO pagesize
define	QP_FMCACHESIZE	Memi[$1+19]	# FMIO buffer cache size
define	QP_STINDEXLEN	Memi[$1+20]	# SYMTAB hash index length
define	QP_STSTABLEN	Memi[$1+21]	# SYMTAB stab len (start)
define	QP_STSBUFSIZE	Memi[$1+22]	# SYMTAB sbuf size (start)
define	QP_STOFFSET	Memi[$1+23]	# lfile offset of stored symbol table
define	QP_MODIFIED	Memi[$1+24]	# QPOE descriptor has been modified
define	QP_DEBUG	Memi[$1+25]	# global debug level (debug messages)
define	QP_XBLOCK	Memr[P2R($1+26)]# default X blocking factor for QPIO
define	QP_YBLOCK	Memr[P2R($1+27)]# default Y blocking factor for QPIO
define	QP_OPTBUFSIZE	Memi[$1+28]	# optimum buffer size for IMIO/QPF/FIO
define	QP_NODEFFILT	Memi[$1+29]	# disable use of default filter
define	QP_NODEFMASK	Memi[$1+30]	# disable use of default mask
define	QP_DFNAME	Memc[P2C($1+31)] # QPOE filename (for messages)

# Symbol descriptor.
define	LEN_SYMBOL	9
define	S_FLAGS 	Memi[$1]	# integer flag word
define	S_DTYPE 	Memi[$1+1]	# datatype code
define	S_DSYM		Memi[$1+2]	# offset of domain symbol if TY_USER
define	S_NELEM 	Memi[$1+3]	# number of elements of dtype
define	S_MAXELEM	Memi[$1+4]	# allocated length
define	S_SZELEM	Memi[$1+5]	# elsize, chars (primary domains only)
define	S_COMMENT	Memi[$1+6]	# pointer to comment string in sbuf
define	S_LFILE 	Memi[$1+7]	# lfile where value is stored
define	S_OFFSET	Memi[$1+8]	# char offset of value in lfile

# Symbol flags.
define	SF_DELETED	0001B		# symbol has been deleted
define	SF_INHERIT	0002B		# inherit in NEW_COPY mode
define	SF_MACARGS	0004B		# macro symbol has symbolic arguments

# QPOE special datatypes.
define	SPPTYPES	"bcsilrdx"	# index is SPP TY_xxx type code
define	TY_MACRO	15		# datafile local macro define
define	TY_OPAQUE	16		# opaque (typeless) binary type
define	TY_USER		17		# some user defined type

# Lexical tokens.
define	TOK_IDENTIFIER	(-99)
define	TOK_NUMBER	(-98)
define	TOK_STRING	(-97)
define	TOK_COMMAND	(-96)
define	TOK_PLUSEQUALS	(-95)
define	TOK_COLONEQUALS	(-94)

# QPOE header as stored in datafile.
define	LEN_QPH		32
define	QPH_MAGIC	Memi[$1]
define	QPH_VERSION	Memi[$1+1]
define	QPH_STOFFSET	Memi[$1+2]

# Domain descriptor structure.
define	LEN_DDDES	110
define	DD_STRUCTLEN	Memi[$1]	# structure length, su
define	DD_NFIELDS	Memi[$1+1]	# number of fields in user structure
define	DD_XFIELD	Memi[$1+2]	# field assigned to coordinate "x"
define	DD_YFIELD	Memi[$1+3]	# field assigned to coordinate "y"
define	DD_FOFFSET	Memi[$1+10+$2-1]# array of field offsets
define	DD_FTYPE	Memi[$1+60+$2-1]# array of field datatypes
