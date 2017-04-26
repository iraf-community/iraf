# Header file for PSFMATCH

define	LEN_PSFSTRUCT (45 + 12 * SZ_FNAME + 12)

# Define the psf fitting structure

define	PM_RC1		Memi[$1]	 # pointer to first column of region
define	PM_RC2		Memi[$1+1]	 # pointer to last column of region
define	PM_RL1		Memi[$1+2]	 # pointer to first line of region
define	PM_RL2		Memi[$1+3]	 # pointer to last line of region
define	PM_RZERO	Memi[$1+4]	 # pointer to zero point of ref regions
define	PM_RXSLOPE	Memi[$1+5]	 # pointer to x slopes of ref regions
define	PM_RYSLOPE	Memi[$1+6]	 # pointer to y slopes of ref regions
define	PM_NREGIONS	Memi[$1+7]	 # total number of regions
define	PM_CNREGION	Memi[$1+8]	 # the current region

define	PM_CENTER	Memi[$1+9]	 # the the psf objects
define	PM_BACKGRD	Memi[$1+10]	 # type of background subtraction
define	PM_BVALUER	Memr[P2R($1+11)] # reference background value
define	PM_BVALUE	Memr[P2R($1+12)] # image background value
define	PM_LOREJECT	Memr[P2R($1+13)] # low side rejection
define	PM_HIREJECT	Memr[P2R($1+14)] # high side rejection
define	PM_APODIZE	Memr[P2R($1+15)] # fraction of region to be apodized

define	PM_CONVOLUTION	Memi[$1+16]	 # the convolution type
define	PM_DNX		Memi[$1+17]	 # x dimension of kernel
define	PM_DNY		Memi[$1+18]	 # y dimension of kernel
define	PM_PNX		Memi[$1+19]	 # x dimension of user kernel
define	PM_PNY		Memi[$1+20]	 # y dimension of user kernel
define	PM_KNX		Memi[$1+21]	 # x size of kernel
define	PM_KNY		Memi[$1+22]	 # x size of kernel

define	PM_POWER	Memi[$1+23]	 # save power spectrum of kernel ?

define	PM_UFLUXRATIO	Memr[P2R($1+24)] # the user ref / input flux ratio
define	PM_FLUXRATIO	Memr[P2R($1+25)] # ref / input flux ratio
define	PM_FILTER	Memi[$1+26]	 # background filtering
define	PM_SXINNER	Memr[P2R($1+27)] # inner radius for cosine bell
define	PM_SXOUTER	Memr[P2R($1+28)] # outer radius for cosine bell
define	PM_SYINNER	Memr[P2R($1+29)] # inner radius for cosine bell
define	PM_SYOUTER	Memr[P2R($1+30)] # outer radius for cosine bell
define	PM_RADSYM	Memi[$1+31]	 # radial symmetry in convolution
define	PM_THRESHOLD	Memr[P2R($1+32)] # threshold in divisor for model

define	PM_NORMFACTOR	Memr[P2R($1+34)] # the normalization factor

#define	PM_PRATIO	Memr[P2R($1+24)] # power ration threshold
#define	PM_XSHIFTS	Memi[$1+26]	 # pointer to x shifts
#define	PM_YSHIFTS	Memi[$1+27]	 # pointer to y shifts

define	PM_REFFFT	Memi[$1+35]	 # pointer to reference fft
define  PM_IMFFT	Memi[$1+36]	 # pointer to image fft
define  PM_FFT		Memi[$1+37]	 # pointer to unfiltered fft
define  PM_CONV		Memi[$1+38]	 # pointer to kernel
define	PM_ASFFT	Memi[$1+39]	 # pointer to power spectrum
define  PM_NXFFT	Memi[$1+40]	 # x dimension of FFT
define  PM_NYFFT	Memi[$1+41]	 # y dimension of FFT

define	PM_BSTRING	Memc[P2C($1+42)]              # background string
define	PM_CSTRING	Memc[P2C($1+42+SZ_FNAME+1)]   # convolution string
define	PM_FSTRING	Memc[P2C($1+42+2*SZ_FNAME+2)]   # convolution string

define	PM_IMAGE	Memc[P2C($1+42+4*SZ_FNAME+4)] # input image
define	PM_REFIMAGE	Memc[P2C($1+42+5*SZ_FNAME+5)] # reference image
define	PM_PSFDATA	Memc[P2C($1+42+6*SZ_FNAME+6)] # psf data
define	PM_PSFIMAGE	Memc[P2C($1+42+7*SZ_FNAME+7)] # psf image if any
define	PM_OBJLIST	Memc[P2C($1+42+8*SZ_FNAME+8)] # object list if any
define	PM_KERNEL	Memc[P2C($1+42+9*SZ_FNAME+9)] # kernel image
define	PM_OUTIMAGE	Memc[P2C($1+42+10*SZ_FNAME+10)] # output convolved image

# Define the paramerter ids

define	RC1		1
define	RC2		2
define	RL1		3
define	RL2		4
define	RZERO		5
define	RXSLOPE		6
define	RYSLOPE		7
define	NREGIONS	8
define	CNREGION	9

define	CENTER		10
define	BACKGRD		11
define	BVALUER		12
define	BVALUE		13
define	LOREJECT	15
define	HIREJECT	16
define	APODIZE		17

define	CONVOLUTION	18
define	DNX		19
define	DNY		20
define	PNX		21
define	PNY		22
define  KNX		23
define  KNY		24
define	POWER		25

#define	XSHIFTS		20
#define	YSHIFTS		21

define	REFFFT		26
define	IMFFT		27
define	FFT		28
define	CONV		29
define	ASFFT		30
define	NXFFT		31
define	NYFFT		32

define	UFLUXRATIO	33
define	FLUXRATIO	34
define	FILTER		35
define	SXINNER		36
define	SXOUTER		37
define	SYINNER		38
define	SYOUTER		39
define	RADSYM		40
define	THRESHOLD	41

define	NORMFACTOR	43

#define	PRATIO		34

define	BSTRING		44
define	CSTRING		45
define	FSTRING		46

define	REFIMAGE	48
define	IMAGE		49
define	PSFDATA		50
define	PSFIMAGE	51
define	OBJLIST		52
define	KERNEL		53
define	OUTIMAGE	54

# Define the default parameter values

define	DEF_CENTER	YES
define	DEF_BACKGRD	PM_BMEDIAN
define	DEF_LOREJECT	INDEFR
define	DEF_HIREJECT	INDEFR

define	DEF_CONVOLUTION	PM_CONIMAGE
define	DEF_DNX		63
define	DEF_DNY		63
define	DEF_PNX		31
define	DEF_PNY		31
define	DEF_POWER	NO

define	DEF_FILTER	PM_FREPLACE
define	DEF_SXINNER	INDEFR
define	DEF_SXOUTER	INDEFR
define	DEF_SYINNER	INDEFR
define	DEF_SYOUTER	INDEFR
define	DEF_RADSYM	NO
define	DEF_THRESHOLD	0.0

#define	DEF_PRATIO	0.0

define	DEF_NORMFACTOR	1.0
define	DEF_UFLUXRATIO	INDEFR

# Define the background fitting techniques

define	PM_BNONE	1
define	PM_BMEAN	2
define	PM_BMEDIAN	3
define	PM_BSLOPE	4
define	PM_BNUMBER	5

define  PM_BTYPES	"|none|mean|median|plane|"

# Define the convolution computation options

define	PM_CONIMAGE	1
define	PM_CONPSF	2
define	PM_CONKERNEL	3

define  PM_CTYPES	"|image|psf|kernel|"

# Define the filtering options

define	PM_FNONE	1
define	PM_FCOSBELL	2
define	PM_FREPLACE	3
define	PM_FMODEL	4

define	PM_FTYPES	"|none|cosbell|replace|model|"

# Define the normalization options

define	PM_UNIT		1
define	PM_RATIO	2
define	PM_NUMBER	3

define	PM_NTYPES	"|unit|ratio|"

# Miscellaneous

define	MAX_NREGIONS	100

# Commands

define	PMCMDS	"|input|reference|psfdata|psfimage|kernel|output|dnx|dny|\
pnx|pny|center|background|loreject|hireject|apodize|convolution|fluxratio|\
filter|sx1|sx2|sy1|sy2|radsym|threshold|normfactor|show|mark|"

define	PMCMD_IMAGE		1
define	PMCMD_REFIMAGE		2
define	PMCMD_PSFDATA		3
define	PMCMD_PSFIMAGE		4
define	PMCMD_KERNEL		5
define	PMCMD_OUTIMAGE		6

define	PMCMD_DNX		7
define	PMCMD_DNY		8
define	PMCMD_PNX		9
define	PMCMD_PNY		10

define	PMCMD_CENTER		11
define	PMCMD_BACKGRD		12
define	PMCMD_LOREJECT		13
define	PMCMD_HIREJECT		14
define	PMCMD_APODIZE		15

define	PMCMD_CONVOLUTION	16
define	PMCMD_UFLUXRATIO	17
define	PMCMD_FILTER		18
define	PMCMD_SXINNER		19
define	PMCMD_SXOUTER		20
define	PMCMD_SYINNER		21
define	PMCMD_SYOUTER		22
define	PMCMD_RADSYM		23
define	PMCMD_THRESHOLD		24

define	PMCMD_NORMFACTOR	25

define	PMCMD_SHOW		26
define	PMCMD_MARK		27

# Keywords

define	KY_IMAGE		"input"
define	KY_REFIMAGE		"reference"
define	KY_PSFDATA		"psfdata"
define	KY_PSFIMAGE		"psfimage"
define	KY_KERNEL		"kernel"
define	KY_OUTIMAGE		"output"

define	KY_DNX			"dnx"
define	KY_DNY			"dny"
define	KY_PNX			"pnx"
define	KY_PNY			"pny"

define	KY_CENTER		"center"
define	KY_BACKGRD		"background"
define	KY_LOREJECT		"loreject"
define	KY_HIREJECT		"hireject"
define	KY_APODIZE		"apodize"

define	KY_CONVOLUTION		"convolution"

define	KY_UFLUXRATIO		"fluxratio"
define	KY_FILTER		"filter"
define	KY_SXINNER		"sx1"
define	KY_SXOUTER		"sx2"
define	KY_SYINNER		"sy1"
define	KY_SYOUTER		"sy2"
define	KY_RADSYM		"radsym"
define	KY_THRESHOLD		"threshold"

define	KY_NORMFACTOR		"normfactor"

