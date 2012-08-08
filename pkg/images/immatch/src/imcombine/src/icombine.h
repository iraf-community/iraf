# ICOMBINE Definitions

# Memory management parameters;
define	MAXMEMORY		500000000	# maximum memory
define	FUDGE			0.8		# fudge factor

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
define	COMBINE	"|average|median|lmedian|sum|quadrature|nmodel|"
define	AVERAGE		1
define	MEDIAN		2
define	LMEDIAN		3
define	SUM		4
define	QUAD		5
define	NMODEL		6

# Median types:
define	MEDAVG		1	# Central average for even N
define	MEDLOW		2	# Lower value for even N

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

# Mask options
define	MASKTYPES	"|none|goodvalue|badvalue|goodbits|badbits|novalue|"
define	M_NONE		1	# Don't use mask images
define	M_GOODVAL	2	# Value selecting good pixels
define	M_BADVAL	3	# Value selecting bad pixels
define	M_GOODBITS	4	# Bits selecting good pixels
define	M_BADBITS	5	# Bits selecting bad pixels
define	M_NOVAL		6	# Value selecting no value (good = 0)
define	M_LTVAL		7	# Values less than specified are good
define	M_GTVAL		8	# Values greater than specified are good
define	M_BOOLEAN	-1	# Ignore mask values

# Data flag
define	D_ALL		0	# All pixels are good
define	D_NONE		1	# All pixels are bad or rejected
define	D_MIX		2	# Mixture of good and bad pixels

define	TOL		0.001	# Tolerance for equal residuals
