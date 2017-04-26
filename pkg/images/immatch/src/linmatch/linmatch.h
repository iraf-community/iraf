# Header file for LINSCALE

define	LEN_LSSTRUCT (70 + 12 * SZ_FNAME + 12)

# Quantities that define the current region and the number of regions

define	LS_CNREGION	Memi[$1]      # the current region
define	LS_NREGIONS	Memi[$1+1]    # the number of regions
define	LS_MAXNREGIONS	Memi[$1+2]    # the maximum number of regions

# Quantities that are dependent on the number of regions

define	LS_RC1		Memi[$1+3]       # pointers to first columns of regions
define	LS_RC2		Memi[$1+4]       # pointers to last columns of regions
define	LS_RL1		Memi[$1+5]       # pointer to first lines of regions
define	LS_RL2		Memi[$1+6]       # pointers to last lines of regions
define	LS_RXSTEP	Memi[$1+7]       # pointers to the x step sizes
define	LS_RYSTEP	Memi[$1+8]       # pointers to the y step sizes
define	LS_XSHIFT	Memr[P2R($1+9)]  # the x shift from image to reference
define	LS_YSHIFT	Memr[P2R($1+10)] # the y shift from image to reference
define	LS_SXSHIFT	Memr[P2R($1+11)] # the x shift from image to reference
define	LS_SYSHIFT	Memr[P2R($1+12)] # the y shift from image to reference

define	LS_RBUF		Memi[$1+14]      # pointer to the reference image data
define	LS_RGAIN	Memr[P2R($1+15)] # the reference image gain
define	LS_RREADNOISE	Memr[P2R($1+16)] # the reference image readout noise
define	LS_RMEAN	Memi[$1+17]      # pointers to means of ref regions
define	LS_RMEDIAN	Memi[$1+18]      # pointers to medians of ref regions
define	LS_RMODE	Memi[$1+19]      # pointers to modes of ref regions
define	LS_RSIGMA	Memi[$1+20]      # pointers to stdevs of ref regions
define	LS_RSKY		Memi[$1+21]      # pointers to sky values of ref regions
define	LS_RSKYERR	Memi[$1+22]      # pointers to sky errors of ref regions
define	LS_RMAG		Memi[$1+23]      # pointers to magnitudes of ref regions
define	LS_RMAGERR	Memi[$1+24]      # pointers to mag errors of ref regions
define	LS_RNPTS	Memi[$1+25]      # pointers to npts of ref regions

define	LS_IBUF		Memi[$1+27]      # pointer to the input image data
define	LS_IGAIN	Memr[P2R($1+28)] # the input image gain
define	LS_IREADNOISE	Memr[P2R($1+29)] # the input image readout noise
define	LS_IMEAN	Memi[$1+30]   # pointers to means of image regions
define	LS_IMEDIAN	Memi[$1+31]   # pointers to medians of image regions
define	LS_IMODE	Memi[$1+32]   # pointers to modes of image regions
define	LS_ISIGMA	Memi[$1+33]   # pointers to stdevs of image regions
define	LS_ISKY		Memi[$1+34]   # pointers to sky values of image regions
define	LS_ISKYERR	Memi[$1+35]   # pointers to sky errors of image regions
define	LS_IMAG		Memi[$1+36]   # pointers to magnitudes of image regions
define	LS_IMAGERR	Memi[$1+37]   # pointers to mag errors of image regions
define	LS_INPTS	Memi[$1+38]   # pointers to npts of image regions

define	LS_RBSCALE	Memi[$1+39]   # pointers to bscales of regions
define	LS_RBSCALEERR	Memi[$1+40]   # pointers to bscale errors of regions
define	LS_RBZERO	Memi[$1+41]   # pointers to bzero errors of regions
define	LS_RBZEROERR	Memi[$1+42]   # pointers to bzero errors of regions
define	LS_RDELETE	Memi[$1+43]   # pointer to the delete array
define	LS_RCHI		Memi[$1+44]   # pointer to the resid array

# Quantities that affect the fitting algorithms

define	LS_BSALGORITHM	Memi[$1+45]	 # bscale fitting algorithm
define	LS_BZALGORITHM	Memi[$1+46]	 # bzero fitting algorithm
define	LS_CBZERO	Memr[P2R($1+47)] # constant bzero
define	LS_CBSCALE	Memr[P2R($1+48)] # constant bscale
define	LS_DNX		Memi[$1+49]	 # x width of data region to extract
define	LS_DNY		Memi[$1+50]	 # y width of data region to extract
#define	LS_PNX		Memi[$1+51]	 # x width of photometry region
#define	LS_PNY		Memi[$1+52]	 # y widht of photometry region
define	LS_DATAMIN	Memr[P2R($1+51)] # the minimum good data value
define	LS_DATAMAX	Memr[P2R($1+52)] # the maximum good data value
define	LS_MAXITER	Memi[$1+53]	 # maximum number of iterations
define	LS_NREJECT	Memi[$1+54]	 # maximum number of rejections cycles
define	LS_LOREJECT	Memr[P2R($1+55)] # low-side sigma rejection criterion
define	LS_HIREJECT	Memr[P2R($1+56)] # high-side sigma rejection criterion
define	LS_GAIN		Memr[P2R($1+57)] # the constant gain value in e-/adu
define	LS_READNOISE	Memr[P2R($1+58)] # the constant readout noise value in e-

# Quantities that define the answers

define	LS_TBSCALE	Memr[P2R($1+59)] 	# bzero value
define	LS_TBSCALEERR	Memr[P2R($1+60)] 	# bscale error estimate
define	LS_TBZERO	Memr[P2R($1+61)] 	# bzero value
define	LS_TBZEROERR	Memr[P2R($1+62)] 	# bzero error estimate

# String quantities

define	LS_BSSTRING	Memc[P2C($1+65)]                # bscale string
define	LS_BZSTRING	Memc[P2C($1+65+SZ_FNAME+1)]     # bzero string
define	LS_CCDGAIN	Memc[P2C($1+65+2*SZ_FNAME+2)]   # gain keyword
define	LS_CCDREAD	Memc[P2C($1+65+3*SZ_FNAME+3)]   # readout noise keyword
define	LS_IMAGE	Memc[P2C($1+65+4*SZ_FNAME+4)]   # input image
define	LS_REFIMAGE	Memc[P2C($1+65+5*SZ_FNAME+5)]   # reference image
define	LS_REGIONS	Memc[P2C($1+65+6*SZ_FNAME+6)]   # regions list
define	LS_DATABASE	Memc[P2C($1+65+7*SZ_FNAME+7)]   # database file
define	LS_OUTIMAGE	Memc[P2C($1+65+8*SZ_FNAME+8)]   # output image
define	LS_SHIFTSFILE	Memc[P2C($1+65+9*SZ_FNAME+9)]   # shifts file
define	LS_PHOTFILE	Memc[P2C($1+65+10*SZ_FNAME+10)]   # shifts file
define	LS_RECORD	Memc[P2C($1+65+11*SZ_FNAME+11)] # the record name


# Define the bzero and bscale fitting algorithms

define	LS_MEAN		1
define	LS_MEDIAN	2
define	LS_MODE		3
define	LS_FIT		4
define	LS_PHOTOMETRY	5
define	LS_FILE		6
define	LS_NUMBER	7

define  LS_SCALING	"|mean|median|mode|fit|photometry|file|"

# Define the parameters

define	CNREGION	1
define	NREGIONS	2
define	MAXNREGIONS	3

define	RC1		4
define	RC2		5
define	RL1		6
define	RL2		7
define	RXSTEP		8
define	RYSTEP		9
define	XSHIFT		10
define	YSHIFT		11
define	SXSHIFT		12
define	SYSHIFT		13

define	RBUF		14
define	RGAIN		15
define	RREADNOISE	16
define	RMEAN		17
define	RMEDIAN		18
define	RMODE		19
define	RSIGMA		20
define	RSKY		21
define	RSKYERR		22
define	RMAG		23
define	RMAGERR		24
define	RNPTS		25

define	IBUF		26
define	IGAIN		27
define	IREADNOISE	28
define	IMEAN		29
define	IMEDIAN		30
define	IMODE		31
define	ISIGMA		32
define	ISKY		33
define	ISKYERR		34
define	IMAG		35
define	IMAGERR		36
define	INPTS		37

define	RBSCALE		38
define	RBSCALEERR	39
define	RBZERO		40
define	RBZEROERR	41
define	RDELETE		42
define	RCHI		43

define	BZALGORITHM     44	
define	BSALGORITHM	45
define	CBZERO		46
define	CBSCALE		47
define	DNX		48
define	DNY		49
#define	PNX		50
#define	PNY		51
define	DATAMIN		50
define	DATAMAX		51
define	MAXITER		52

define	NREJECT		53
define	LOREJECT	54
define	HIREJECT	55
define	GAIN		56
define	READNOISE	57

define	TBZERO		58
define	TBZEROERR	59
define	TBSCALE		60
define	TBSCALEERR	61

define	BSSTRING	62
define	BZSTRING	63
define	CCDGAIN		64
define	CCDREAD		65

define	IMAGE		66
define	REFIMAGE	67
define	REGIONS		68
define	DATABASE	69
define	OUTIMAGE	70
define	RECORD		71
define	SHIFTSFILE	72
define	PHOTFILE	73

# Set some default values

define	DEF_MAXNREGIONS	100
define	DEF_BZALGORITHM	LS_FIT
define	DEF_BSALGORITHM	LS_FIT
define	DEF_CBZERO	0.0
define	DEF_CBSCALE	1.0
define	DEF_DNX		31
define	DEF_DNY		31
define	DEF_MAXITER	10
define	DEF_DATAMIN	INDEFR
define	DEF_DATAMAX	INDEFR
define	DEF_NREJECT	0
define	DEF_LOREJECT	INDEFR
define	DEF_HIREJECT	INDEFR
define	DEF_GAIN	INDEFR
define	DEF_READNOISE	INDEFR

# The mode computation parameters.

define	LMODE_NMIN	10
define	LMODE_ZRANGE	1.0
define	LMODE_ZBIN	0.1
define	LMODE_ZSTEP	0.01
define	LMODE_HWIDTH	3.0

# The default plot types.

define	LS_MMHIST	1
define	LS_MMFIT	2
define	LS_MMRESID	3
define	LS_RIFIT	4
define	LS_RIRESID	5
define	LS_BSZFIT	6
define	LS_BSZRESID	7
define	LS_MAGSKYFIT	8
define	LS_MAGSKYRESID	9

# The bad point deletions code.

define	LS_NO		0
define	LS_BADREGION	1
define	LS_BADSIGMA	2
define	LS_DELETED	3

# Commands

define	LSCMDS	"|input|reference|regions|lintransform|output|photfile|\
shifts|records|xshift|yshift|dnx|dny|maxnregions|datamin|datamax|\
maxiter|nreject|loreject|hireject|gain|readnoise|show|markcoords|marksections|"

define	LSCMD_IMAGE		1
define	LSCMD_REFIMAGE		2
define	LSCMD_REGIONS		3
define	LSCMD_DATABASE		4
define	LSCMD_OUTIMAGE		5
define	LSCMD_PHOTFILE		6
define	LSCMD_SHIFTSFILE	7
define	LSCMD_RECORD		8
define	LSCMD_XSHIFT		9
define	LSCMD_YSHIFT		10
define	LSCMD_DNX		11
define	LSCMD_DNY		12
define	LSCMD_MAXNREGIONS	13
define	LSCMD_DATAMIN		14
define	LSCMD_DATAMAX		15
define	LSCMD_MAXITER		16
define	LSCMD_NREJECT		17
define	LSCMD_LOREJECT		18
define	LSCMD_HIREJECT		19
define	LSCMD_GAIN		20
define	LSCMD_READNOISE		21
define	LSCMD_SHOW		22
define	LSCMD_MARKCOORDS        23
define	LSCMD_MARKSECTIONS      24

# Keywords

define	KY_REFIMAGE		"reference"
define	KY_IMAGE		"input"
define	KY_REGIONS		"regions"
define	KY_DATABASE		"lintransform"
define	KY_OUTIMAGE		"output"
define	KY_PHOTFILE		"photfile"
define	KY_SHIFTSFILE		"shifts"
define	KY_RECORD		"records"
define	KY_XSHIFT		"xshift"
define	KY_YSHIFT		"yshift"
define	KY_DNX			"dnx"
define	KY_DNY			"dny"
define	KY_MAXNREGIONS		"maxnregions"
define	KY_DATAMIN		"datamin"
define	KY_DATAMAX		"datamax"
define	KY_MAXITER		"maxiter"
define	KY_NREJECT		"nreject"
define	KY_LOREJECT		"loreject"
define	KY_HIREJECT		"hireject"
define	KY_GAIN			"gain"
define	KY_READNOISE		"readnoise"
define KY_NREGIONS		"nregions"

