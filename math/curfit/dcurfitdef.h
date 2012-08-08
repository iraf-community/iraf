# Header file for the curve fitting package

# set up the curve descriptor structure

define	LEN_CVSTRUCT	30

define	CV_XMAX		Memd[P2D($1)]	# Maximum x value
define	CV_XMIN		Memd[P2D($1+2)]	# Minimum x value
define	CV_RANGE	Memd[P2D($1+4)]	# 2. / (xmax - xmin), polynomials
define	CV_MAXMIN	Memd[P2D($1+6)]	# - (xmax + xmin) / 2., polynomials
define	CV_SPACING	Memd[P2D($1+8)]	# order / (xmax - xmin), splines
define	CV_USERFNCD	Memd[P2D($1+10)]# Real version of above for cvrestore.
define	CV_TYPE		Memi[$1+12]	# Type of curve to be fitted
define	CV_ORDER	Memi[$1+13]	# Order of the fit
define	CV_NPIECES	Memi[$1+14]	# Number of polynomial pieces - 1
define	CV_NCOEFF	Memi[$1+15]	# Number of coefficients
define	CV_NPTS		Memi[$1+16]	# Number of data points

define	CV_XBASIS	Memi[$1+17]	# Pointer to non zero basis for single x
define	CV_MATRIX	Memi[$1+18]	# Pointer to original matrix
define	CV_CHOFAC	Memi[$1+19]	# Pointer to Cholesky factorization
define	CV_VECTOR	Memi[$1+20]	# Pointer to  vector
define	CV_COEFF	Memi[$1+21]	# Pointer to coefficient vector
define	CV_BASIS	Memi[$1+22]	# Pointer to basis functions (all x)
define	CV_LEFT		Memi[$1+23]	# Pointer to first non-zero basis
define	CV_WY		Memi[$1+24]	# Pointer to y * w (cvrefit)
define	CV_USERFNC	Memi[$1+25]	# Pointer to external user subroutine
					# one free slot left

# matrix and vector element definitions

define	XBASIS		Memd[$1]	# Non zero basis for single x
define	MATRIX		Memd[$1]	# Element of MATRIX
define	CHOFAC		Memd[$1]	# Element of CHOFAC
define	VECTOR		Memd[$1]	# Element of VECTOR
define	COEFF		Memd[$1]	# Element of COEFF
define	BASIS		Memd[$1]	# Element of BASIS
define	LEFT		Memi[$1]	# Element of LEFT

# structure definitions for restore

define	CV_SAVETYPE	$1[1]
define	CV_SAVEORDER	$1[2]
define	CV_SAVEXMIN	$1[3]
define	CV_SAVEXMAX	$1[4]
define	CV_SAVEFNC	$1[5]

define	CV_SAVECOEFF	5

# miscellaneous

define	SPLINE3_ORDER	4
define	SPLINE1_ORDER	2
define	DELTA		EPSILON
