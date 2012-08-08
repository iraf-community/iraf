# Header file for the surface fitting package

# set up the curve descriptor structure

define	LEN_GSSTRUCT	64

define	GS_TYPE		Memi[$1]	# Type of curve to be fitted
define	GS_XORDER	Memi[$1+1]	# Order of the fit in x
define	GS_YORDER	Memi[$1+2]	# Order of the fit in y
define	GS_XTERMS	Memi[$1+3]	# Cross terms for polynomials
define	GS_NXCOEFF	Memi[$1+4]	# Number of x coefficients
define	GS_NYCOEFF	Memi[$1+5]	# Number of y coefficients
define	GS_NCOEFF	Memi[$1+6]	# Total number of coefficients
define	GS_XREF		Memr[P2R($1+7)]	# x reference value
define	GS_YREF		Memr[P2R($1+8)]	# y reference value
define	GS_ZREF		Memr[P2R($1+9)]	# z reference value
define	GS_XMAX		Memr[P2R($1+10)]# Maximum x value
define	GS_XMIN		Memr[P2R($1+11)]# Minimum x value
define	GS_YMAX		Memr[P2R($1+12)]# Maximum y value
define	GS_YMIN		Memr[P2R($1+13)]# Minimum y value		
define	GS_XRANGE	Memr[P2R($1+14)]# 2. / (xmax - xmin), polynomials
define	GS_XMAXMIN	Memr[P2R($1+15)]# - (xmax + xmin) / 2., polynomials
define	GS_YRANGE	Memr[P2R($1+16)]# 2. / (ymax - ymin), polynomials
define	GS_YMAXMIN	Memr[P2R($1+17)]# - (ymax + ymin) / 2., polynomials
define	GS_NPTS		Memi[$1+18]	# Number of data points

define	GS_MATRIX	Memi[$1+19]	# Pointer to original matrix
define	GS_CHOFAC	Memi[$1+20]	# Pointer to Cholesky factorization
define	GS_VECTOR	Memi[$1+21]	# Pointer to  vector
define	GS_COEFF	Memi[$1+22]	# Pointer to coefficient vector
define	GS_XBASIS	Memi[$1+23]	# Pointer to basis functions (all x)
define	GS_YBASIS	Memi[$1+24]	# Pointer to basis functions (all y)
define	GS_WZ		Memi[$1+25]	# Pointer to w * z (gsrefit)

# matrix and vector element definitions

define	XBASIS		Memr[P2P($1)]	# Non zero basis for all x
define	YBASIS		Memr[P2P($1)]	# Non zero basis for all y
define	XBS		Memr[P2P($1)]	# Non zero basis for single x
define	YBS		Memr[P2P($1)]	# Non zero basis for single y
define	MATRIX		Memr[P2P($1)]	# Element of MATRIX
define	CHOFAC		Memr[P2P($1)]	# Element of CHOFAC
define	VECTOR		Memr[P2P($1)]	# Element of VECTOR
define	COEFF		Memr[P2P($1)]	# Element of COEFF

# structure definitions for restore

define	GS_SAVETYPE	$1[1]
define	GS_SAVEXORDER	$1[2]
define	GS_SAVEYORDER	$1[3]
define	GS_SAVEXTERMS	$1[4]
define	GS_SAVEXMIN	$1[5]
define	GS_SAVEXMAX	$1[6]
define	GS_SAVEYMIN	$1[7]
define	GS_SAVEYMAX	$1[8]

# data type

define	DELTA		EPSILON

# miscellaneous
