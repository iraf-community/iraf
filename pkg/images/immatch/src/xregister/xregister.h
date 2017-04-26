# Header file for XREGISTER

# Define the cross correlation structure

define	LEN_XCSTRUCT	(50 + 12 * SZ_FNAME + 12)

define	XC_RC1		Memi[$1]	 # pointers to 1st column of ref regions
define	XC_RC2		Memi[$1+1]	 # pointers to 2nd column of ref regions
define	XC_RL1		Memi[$1+2]	 # pointers to 1st line of ref regions
define	XC_RL2		Memi[$1+3]	 # pointers to 2nd line of ref regions
define	XC_RZERO	Memi[$1+4]	 # pointers to zero pts of ref regions
define	XC_RXSLOPE	Memi[$1+5]	 # pointers to x slopes of ref regions
define	XC_RYSLOPE	Memi[$1+6]	 # pointers to y slopes of ref regions
define	XC_XSHIFTS	Memi[$1+7]	 # pointers to x shifts of ref regions
define	XC_YSHIFTS	Memi[$1+8]	 # pointers to y shifts of ref regions
define	XC_NREGIONS	Memi[$1+9]	 # total number of regions
define	XC_CREGION	Memi[$1+10]	 # the current region

define	XC_NREFPTS	Memi[$1+11]	 # number of reference points
define	XC_XREF		Memi[$1+12]	 # pointer to x reference points
define	XC_YREF		Memi[$1+13]	 # pointer to y reference points
define	XC_TRANSFORM	Memi[$1+14]	 # pointer to the transform
define	XC_IXLAG	Memi[$1+15]	 # initial shift in x
define	XC_IYLAG	Memi[$1+16]	 # initial shift in y
define	XC_XLAG		Memi[$1+17]	 # current shift in x
define	XC_YLAG		Memi[$1+18]	 # current shift in y
define	XC_DXLAG	Memi[$1+19]	 # incremental shift in x
define	XC_DYLAG	Memi[$1+20]	 # incremental shift in y

define	XC_BACKGRD	Memi[$1+21]	 # type of background subtraction
define	XC_BORDER	Memi[$1+22]	 # width of background border
define	XC_BVALUER	Memr[P2R($1+23)] # reference background value
define	XC_BVALUE	Memr[P2R($1+24)] # image bacground value
define	XC_LOREJECT	Memr[P2R($1+25)] # low side rejection
define	XC_HIREJECT	Memr[P2R($1+26)] # high side rejection
define	XC_APODIZE	Memr[P2R($1+27)] # fraction of apodized region
define	XC_FILTER	Memi[$1+28]	 # filter type

define	XC_CFUNC	Memi[$1+30]	 # crosscor function
define	XC_XWINDOW	Memi[$1+31]	 # width of correlation window in x
define	XC_YWINDOW	Memi[$1+32]	 # width of correlation window in y
define	XC_XCOR		Memi[$1+33]	 # pointer to cross-correlation function

define	XC_PFUNC	Memi[$1+34]	 # correlation peak fitting function
define	XC_XCBOX	Memi[$1+35]	 # x width of cor fitting box
define	XC_YCBOX	Memi[$1+36]	 # y width of cor fitting box

define	XC_TXSHIFT	Memr[P2R($1+37)] # total x shift
define	XC_TYSHIFT	Memr[P2R($1+38)] # total y shift

define	XC_BSTRING	Memc[P2C($1+50)]   	        # background type
define	XC_FSTRING	Memc[P2C($1+50+SZ_FNAME+1)]     # filter string
define	XC_CSTRING	Memc[P2C($1+50+2*SZ_FNAME+2)]   # cross-correlation type
define	XC_PSTRING	Memc[P2C($1+50+3*SZ_FNAME+3)]   # peak centering

define	XC_IMAGE	Memc[P2C($1+50+4*SZ_FNAME+4)]   # input image
define	XC_REFIMAGE	Memc[P2C($1+50+5*SZ_FNAME+5)]   # reference image
define	XC_REGIONS	Memc[P2C($1+50+6*SZ_FNAME+6)]   # regions list
define	XC_DATABASE	Memc[P2C($1+50+7*SZ_FNAME+7)]   # shifts database
define	XC_OUTIMAGE	Memc[P2C($1+50+8*SZ_FNAME+8)]   # output image
define	XC_REFFILE	Memc[P2C($1+50+9*SZ_FNAME+9)]   # coordinates file
define	XC_RECORD	Memc[P2C($1+50+10*SZ_FNAME+10)] # record

# Define the id strings

define	RC1		1
define	RC2		2
define	RL1		3
define	RL2		4
define	RZERO		5
define	RXSLOPE		6
define	RYSLOPE		7
define	XSHIFTS		8
define	YSHIFTS		9
define	NREGIONS	10
define	CREGION		11

define	NREFPTS		12
define	XREF		13
define	YREF		14
define	TRANSFORM	15
define	IXLAG		16
define	IYLAG		17
define	XLAG		18
define	YLAG		19
define	DXLAG		20
define	DYLAG		21

define	BACKGRD		22
define	BVALUER		23
define	BVALUE		24
define	BORDER		25
define	LOREJECT	26
define	HIREJECT	27
define	APODIZE		28
define	FILTER		29

define	CFUNC		30
define	XWINDOW		31
define	YWINDOW		32
define	XCOR		33

define	PFUNC		34
define	XCBOX		35
define	YCBOX		36

define	TXSHIFT		37
define	TYSHIFT		38

define	CSTRING		39
define	BSTRING		40
define	PSTRING		41
define	FSTRING		42

define	IMAGE		43
define	REFIMAGE	44
define	REGIONS		45
define	OUTIMAGE	46
define	REFFILE		47
define	DATABASE	48
define	RECORD		49

# Define the default parameter values

define	DEF_IXLAG	0
define	DEF_IYLAG	0
define	DEF_DXLAG	0
define	DEF_DYLAG	0
define	DEF_XWINDOW	5
define	DEF_YWINDOW	5

define	DEF_BACKGRD	XC_BNONE
define	DEF_BORDER	INDEFI
define  DEF_LOREJECT	INDEFR
define	DEF_HIREJECT	INDEFR

define	DEF_XCBOX	5
define  DEF_YCBOX	5
define	DEF_PFUNC	XC_CENTROID

# Define the background fitting techniques

define	XC_BNONE	1
define	XC_MEAN		2
define	XC_MEDIAN	3
define	XC_SLOPE	4

define  XC_BTYPES	"|none|mean|median|plane|"

# Define the filtering options

define	XC_FNONE	1
define	XC_LAPLACE	2

define  XC_FTYPES	"|none|laplace|"

# Define the cross correlation techniques

define	XC_DISCRETE	1
define	XC_FOURIER	2
define	XC_DIFFERENCE	3
define	XC_FILE		4

define	XC_CTYPES	"|discrete|fourier|difference|file|"

# Define the peak fitting functions

define	XC_PNONE	1
define	XC_CENTROID	2
define	XC_SAWTOOTH	3
define	XC_PARABOLA	4
define	XC_MARK		5

define	XC_PTYPES	"|none|centroid|sawtooth|parabola|mark|"

# Miscellaneous

define	MAX_NREGIONS	100
define	MAX_NREF	3
define	MAX_NTRANSFORM	6

# Commands

define  XCMDS "|reference|input|regions|shifts|output|records|transform|\
cregion|xlag|ylag|dxlag|dylag|background|border|loreject|hireject|apodize|\
filter|correlation|xwindow|ywindow|function|xcbox|ycbox|show|mark|"

define  XSHOW	"|data|background|correlation|center|"

define	XSHOW_DATA			1
define	XSHOW_BACKGROUND		2
define	XSHOW_CORRELATION		3
define	XSHOW_PEAKCENTER		4

define	XCMD_REFIMAGE		1
define	XCMD_IMAGE		2
define	XCMD_REGIONS		3
define	XCMD_DATABASE		4
define	XCMD_OUTIMAGE		5
define	XCMD_RECORD		6
define	XCMD_REFFILE		7
define	XCMD_CREGION		8
define	XCMD_XLAG		9
define	XCMD_YLAG		10
define	XCMD_DXLAG		11
define	XCMD_DYLAG		12
define	XCMD_BACKGROUND		13
define	XCMD_BORDER		14
define	XCMD_LOREJECT		15
define	XCMD_HIREJECT		16
define	XCMD_APODIZE		17
define	XCMD_FILTER		18
define	XCMD_CORRELATION	19
define	XCMD_XWINDOW		20
define	XCMD_YWINDOW		21
define	XCMD_PEAKCENTER		22
define	XCMD_XCBOX		23
define	XCMD_YCBOX		24
define	XCMD_SHOW		25
define	XCMD_MARK		26

# Keywords

define	KY_REFIMAGE		"reference"
define	KY_IMAGE		"input"
define	KY_REGIONS		"regions"
define	KY_DATABASE		"shifts"
define	KY_OUTIMAGE		"output"
define	KY_RECORD		"record"
define	KY_REFFILE		"coords"
define	KY_NREGIONS		"nregions"
define	KY_CREGION		"region"
define	KY_XLAG			"xlag"
define	KY_YLAG			"ylag"
define	KY_DXLAG		"dxlag"
define	KY_DYLAG		"dylag"
define	KY_BACKGROUND		"background"
define	KY_BORDER		"border"
define	KY_LOREJECT		"loreject"
define	KY_HIREJECT		"hireject"
define	KY_APODIZE		"apodize"
define	KY_FILTER		"filter"
define	KY_CORRELATION		"correlation"
define	KY_XWINDOW		"xwindow"
define	KY_YWINDOW		"ywindow"
define	KY_PEAKCENTER		"function"
define	KY_XCBOX		"xcbox"
define	KY_YCBOX		"ycbox"
define	KY_TXSHIFT		"xshift"
define	KY_TYSHIFT		"yshift"
