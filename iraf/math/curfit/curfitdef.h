# Header file for the curve fitting package

# set up the curve descriptor structure

define	LEN_CVSTRUCT	20

define	CV_TYPE		Memi[P2I($1)]	# Type of curve to be fitted
define	CV_ORDER	Memi[P2I($1+1)]	# Order of the fit
define	CV_NPIECES	Memi[P2I($1+2)]	# Number of polynomial pieces - 1
define	CV_NCOEFF	Memi[P2I($1+3)]	# Number of coefficients
define	CV_XMAX		Memr[P2R($1+4)]	# Maximum x value
define	CV_XMIN		Memr[P2R($1+5)]	# Minimum x value
define	CV_RANGE	Memr[P2R($1+6)]	# 2. / (xmax - xmin), polynomials
define	CV_MAXMIN	Memr[P2R($1+7)]	# - (xmax + xmin) / 2., polynomials
define	CV_SPACING	Memr[P2R($1+8)]	# order / (xmax - xmin), splines
define	CV_NPTS		Memz[P2Z($1+9)]	# Number of data points

define	CV_XBASIS	Memp[$1+10]	# Pointer to non zero basis for single x
define	CV_MATRIX	Memp[$1+11]	# Pointer to original matrix
define	CV_CHOFAC	Memp[$1+12]	# Pointer to Cholesky factorization
define	CV_VECTOR	Memp[$1+13]	# Pointer to  vector
define	CV_COEFF	Memp[$1+14]	# Pointer to coefficient vector
define	CV_BASIS	Memp[$1+15]	# Pointer to basis functions (all x)
define	CV_LEFT		Memp[$1+16]	# Pointer to first non-zero basis
define	CV_WY		Memp[$1+17]	# Pointer to y * w (cvrefit)
define	CV_USERFNC	Memp[$1+18]	# Pointer to external user subroutine
define	CV_USERFNCR	Memr[P2R($1+18)]	# Real version of above for cvrestore.
					# one free slot left

# matrix and vector element definitions

define	XBASIS		Memr[$1]	# Non zero basis for single x
define	MATRIX		Memr[$1]	# Element of MATRIX
define	CHOFAC		Memr[$1]	# Element of CHOFAC
define	VECTOR		Memr[$1]	# Element of VECTOR
define	COEFF		Memr[$1]	# Element of COEFF
define	BASIS		Memr[$1]	# Element of BASIS
define	LEFT		Memz[$1]	# Element of LEFT

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
