# Header file for IMSURFIT

define	LEN_IMSFSTRUCT	20

# surface parameters
define	SURFACE_TYPE	Memi[$1]
define	XORDER		Memi[$1+1]
define	YORDER		Memi[$1+2]
define	CROSS_TERMS	Memi[$1+3]
define	TYPE_OUTPUT	Memi[$1+4]

# median processing parameters
define	MEDIAN		Memi[$1+5]
define	XMEDIAN		Memi[$1+6]
define	YMEDIAN		Memi[$1+7]
define	MEDIAN_PERCENT	Memr[P2R($1+8)]

# pixel rejection parameters
define	REJECT		Memi[$1+9]
define	NGROW		Memi[$1+10]
define	NITER		Memi[$1+11]
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
