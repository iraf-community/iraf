# SCOMBINE Definitions

# Grouping options
define	GROUP	"|all|images|apertures|"
define	GRP_ALL		1
define	GRP_IMAGES	2
define	GRP_APERTURES	3

# Sorting options
define	SORT		"|none|increasing|decreasing|"
define	SORT_NONE	1
define	SORT_INC	2
define	SORT_DEC	3

# Combining modes in interactive mode
define	CMB_AGAIN	0
define	CMB_ALL		1
define	CMB_FIRST	2
define	CMB_NEXT	3
define	CMB_SKIP	4

# Rejection options:
define	REJECT	"|none|ccdclip|crreject|minmax|pclip|sigclip|avsigclip|"
define	NONE		1	# No rejection algorithm
define	CCDCLIP		2	# CCD noise function clipping
define	CRREJECT	3	# CCD noise function clipping
define	MINMAX		4	# Minmax rejection
define	PCLIP		5	# Percentile clip
define	SIGCLIP		6	# Sigma clip
define	AVSIGCLIP	7	# Sigma clip with average poisson sigma

# Combine options:
define	COMBINE	"|average|median|sum|"
define	AVERAGE		1
define	MEDIAN		2
define	SUM		3

# Scaling options:
define	STYPES		"|none|mode|median|mean|exposure|"
define	ZTYPES		"|none|mode|median|mean|"
define	WTYPES		"|none|mode|median|mean|exposure|"
define	S_NONE		1
define	S_MODE		2
define	S_MEDIAN	3
define	S_MEAN		4
define	S_EXPOSURE	5
define	S_FILE		6
define	S_KEYWORD	7
define	S_SECTION	"|input|output|overlap|"
define	S_INPUT		1
define	S_OUTPUT	2
define	S_OVERLAP	3

# Data flag
define	D_ALL		0	# All pixels are good
define	D_NONE		1	# All pixels are bad or rejected
define	D_MIX		2	# Mixture of good and bad pixels

define	TOL		0.001	# Tolerance for equal residuals

# Spectrum data structure
define	NS	Memi[$1+$2-1]			# Number of spec of given ap
define	SH	Memi[Memi[$1+$2-1]+$3-1]	# Spectrum header structure

# Combining options
#define	COMBINE		"|average|sum|"
#define	CMB_AVERAGE	1
#define	CMB_SUM		2

# Weighting options
#define	WT_TYPE		"|none|expo|user|"
#define	WT_NONE		1
#define	WT_EXPO		2
#define	WT_USER		3
