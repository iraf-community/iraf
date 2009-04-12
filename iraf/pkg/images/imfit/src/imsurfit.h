# Header file for IMSURFIT

define	LEN_IMSFSTRUCT	20

# surface parameters
define	SURFACE_TYPE	Memi[P2I($1)]
define	XORDER		Memi[P2I($1+1)]
define	YORDER		Memi[P2I($1+2)]
define	CROSS_TERMS	Memi[P2I($1+3)]
define	TYPE_OUTPUT	Memi[P2I($1+4)]

# median processing parameters
define	MEDIAN		Memi[P2I($1+5)]
define	XMEDIAN		Meml[P2L($1+6)]
define	YMEDIAN		Meml[P2L($1+7)]
define	MEDIAN_PERCENT	Memr[P2R($1+8)]

# pixel rejection parameters
define	REJECT		Memi[P2I($1+9)]
define	NGROW		Memi[P2I($1+10)]
define	NITER		Memi[P2I($1+11)]
define	LOWER		Memr[P2R($1+12)]
define	UPPER		Memr[P2R($1+13)]

define	DIV_MIN		Memr[P2R($1+14)]

# definitions for type_output
define	FIT		1
define	CLEAN		2
define	RESID		3
define	RESP		4

# definitions for good regions
define	ALL		1
define	COLUMNS		2
define	ROWS		3
define	BORDER		4
define	SECTIONS	5
define	CIRCLE		6
define	INVCIRCLE	7
