# Definitions for EXTRACT task.

define	WEIGHTS	"|profile|variance|"
define	PROFILE		1
define	VARIANCE	2

define	BACKGROUND	"|none|average|fit|"
define	NONE		1
define	AVERAGE		2
define	FIT		3

# Parameter Structure

define	EX_LENSTRUCT	11		# Length of structure

define	EX_ONED		Memi[$1]	# One dimensional extraction?
define	EX_NAVG		Memi[$1+1]	# Number of profiles to average
define	EX_ASI		Memi[$1+2]	# Image interpolator pointer
define	EX_CLEAN	Memi[$1+3]	# Clean bad pixels?
define	EX_NCLEAN	Memi[$1+4]	# Num pix to clean per aperture per line
define	EX_LSIGMA	Memr[$1+5]	# Lower sigma factor for cleaning
define	EX_USIGMA	Memr[$1+6]	# Lower sigma factor for cleaning
define	EX_WTTYPE	Memi[$1+7]	# Type of extraction weighting
define	EX_V0		Memr[$1+8]	# Variance intercept
define	EX_V1		Memr[$1+9]	# Variance slope
define	EX_BKGD		Memi[$1+10]	# Subtract background?
