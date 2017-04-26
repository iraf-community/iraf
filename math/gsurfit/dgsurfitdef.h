# Header file for the surface fitting package

# set up the curve descriptor structure

define	LEN_GSSTRUCT	64

define	GS_XREF		Memd[P2D($1)]	# x reference value
define	GS_YREF		Memd[P2D($1+2)]	# y reference value
define	GS_ZREF		Memd[P2D($1+4)]	# z reference value
define	GS_XMAX		Memd[P2D($1+6)]	# Maximum x value
define	GS_XMIN		Memd[P2D($1+8)]	# Minimum x value
define	GS_YMAX		Memd[P2D($1+10)]# Maximum y value
define	GS_YMIN		Memd[P2D($1+12)]# Minimum y value		
define	GS_XRANGE	Memd[P2D($1+14)]# 2. / (xmax - xmin), polynomials
define	GS_XMAXMIN	Memd[P2D($1+16)]# - (xmax + xmin) / 2., polynomials
define	GS_YRANGE	Memd[P2D($1+18)]# 2. / (ymax - ymin), polynomials
define	GS_YMAXMIN	Memd[P2D($1+20)]# - (ymax + ymin) / 2., polynomials
define	GS_TYPE		Memi[$1+22]	# Type of curve to be fitted
define	GS_XORDER	Memi[$1+23]	# Order of the fit in x
define	GS_YORDER	Memi[$1+24]	# Order of the fit in y
define	GS_XTERMS	Memi[$1+25]	# Cross terms for polynomials
define	GS_NXCOEFF	Memi[$1+26]	# Number of x coefficients
define	GS_NYCOEFF	Memi[$1+27]	# Number of y coefficients
define	GS_NCOEFF	Memi[$1+28]	# Total number of coefficients
define	GS_NPTS		Memi[$1+29]	# Number of data points

define	GS_MATRIX	Memi[$1+30]	# Pointer to original matrix
define	GS_CHOFAC	Memi[$1+31]	# Pointer to Cholesky factorization
define	GS_VECTOR	Memi[$1+32]	# Pointer to  vector
define	GS_COEFF	Memi[$1+33]	# Pointer to coefficient vector
define	GS_XBASIS	Memi[$1+34]	# Pointer to basis functions (all x)
define	GS_YBASIS	Memi[$1+35]	# Pointer to basis functions (all y)
define	GS_WZ		Memi[$1+36]	# Pointer to w * z (gsrefit)

# matrix and vector element definitions

define	XBASIS		Memd[$1]	# Non zero basis for all x
define	YBASIS		Memd[$1]	# Non zero basis for all y
define	XBS		Memd[$1]	# Non zero basis for single x
define	YBS		Memd[$1]	# Non zero basis for single y
define	MATRIX		Memd[$1]	# Element of MATRIX
define	CHOFAC		Memd[$1]	# Element of CHOFAC
define	VECTOR		Memd[$1]	# Element of VECTOR
define	COEFF		Memd[$1]	# Element of COEFF

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
