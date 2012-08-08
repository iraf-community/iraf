# Header file for the curve fitting package

# set up the curve descriptor structure

define	LEN_CVSTRUCT	20

define	CV_TYPE		Memi[$1]	# Type of curve to be fitted
define	CV_ORDER	Memi[$1+1]	# Order of the fit
define	CV_NPIECES	Memi[$1+2]	# Number of polynomial pieces - 1
define	CV_NCOEFF	Memi[$1+3]	# Number of coefficients
define	CV_XMAX		Memr[P2R($1+4)]	# Maximum x value
define	CV_XMIN		Memr[P2R($1+5)]	# Minimum x value
define	CV_RANGE	Memr[P2R($1+6)]	# 2. / (xmax - xmin), polynomials
define	CV_MAXMIN	Memr[P2R($1+7)]	# - (xmax + xmin) / 2., polynomials
define	CV_SPACING	Memr[P2R($1+8)]	# order / (xmax - xmin), splines
define	CV_NPTS		Memi[$1+9]	# Number of data points

define	CV_XBASIS	Memi[$1+10]	# Pointer to non zero basis for single x
define	CV_MATRIX	Memi[$1+11]	# Pointer to original matrix
define	CV_CHOFAC	Memi[$1+12]	# Pointer to Cholesky factorization
define	CV_VECTOR	Memi[$1+13]	# Pointer to  vector
define	CV_COEFF	Memi[$1+14]	# Pointer to coefficient vector
define	CV_BASIS	Memi[$1+15]	# Pointer to basis functions (all x)
define	CV_LEFT		Memi[$1+16]	# Pointer to first non-zero basis
define	CV_WY		Memi[$1+17]	# Pointer to y * w (cvrefit)
define	CV_USERFNC	Memi[$1+18]	# Pointer to external user subroutine
define	CV_USERFNCR	Memr[P2R($1+18)]# Real version of above for cvrestore.
					# one free slot left

# matrix and vector element definitions

define	XBASIS		Memr[P2P($1)]	# Non zero basis for single x
define	MATRIX		Memr[P2P($1)]	# Element of MATRIX
define	CHOFAC		Memr[P2P($1)]	# Element of CHOFAC
define	VECTOR		Memr[P2P($1)]	# Element of VECTOR
define	COEFF		Memr[P2P($1)]	# Element of COEFF
define	BASIS		Memr[P2P($1)]	# Element of BASIS
define	LEFT		Memi[P2P($1)]	# Element of LEFT

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
