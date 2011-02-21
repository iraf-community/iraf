# set up the curve fitting structure

define	LEN_SFSTRUCT	 35

define	SF_TYPE		Memi[$1]	# Type of curve to be fitted
define	SF_NXCOEFF	Memi[$1+1]	# Number of coefficients
define	SF_XORDER	Memi[$1+2]	# Order of the fit in x
define	SF_NXPIECES	Memi[$1+3]	# Number of x polynomial pieces - 1
define	SF_NCOLS	Memi[$1+4]	# Maximum x value
define	SF_NXPTS	Memi[$1+5]	# Number of points in x
define	SF_NYCOEFF	Memi[$1+6]	# Number of y coefficients
define	SF_YORDER	Memi[$1+7]	# Order of the fit in y
define	SF_NYPIECES	Memi[$1+8]	# Number of y polynomial pieces - 1
define	SF_NLINES	Memi[$1+9]	# Minimum x value
define	SF_NYPTS	Memi[$1+10]	# Number of y points
define	SF_XTERMS	Memi[$1+11]	# cross terms?

define	SF_XBASIS	Memi[$1+12]	# Pointer to the x basis functions
define	SF_XLEFT	Memi[$1+13]	# Indices to x basis functions, spline
define	SF_YBASIS	Memi[$1+14]	# Pointer to the y basis functions
define	SF_YLEFT	Memi[$1+15]	# Indices to y basis functions, spline
define	SF_XMATRIX	Memi[$1+16]	# Pointer to x data matrix
define	SF_YMATRIX	Memi[$1+17]	# Pointer to y data matrix
define	SF_XCOEFF	Memi[$1+18]	# X coefficient matrix
define	SF_COEFF	Memi[$1+19]	# Pointer to coefficient vector

define	SF_XMIN		Memr[P2R($1+20)] # Min x value
define	SF_XMAX		Memr[P2R($1+21)] # Max x value
define	SF_XRANGE	Memr[P2R($1+22)] # 2. / (xmax - xmin), polynomials
define	SF_XMAXMIN	Memr[P2R($1+23)] # - (xmax + xmin) / 2., polynomials
define	SF_XSPACING	Memr[P2R($1+24)] # order / (xmax - xmin), splines
define	SF_YMIN		Memr[P2R($1+25)] # Min y value
define	SF_YMAX		Memr[P2R($1+26)] # Max y value
define	SF_YRANGE	Memr[P2R($1+27)] # 2. / (ymax - ymin), polynomials
define	SF_YMAXMIN	Memr[P2R($1+28)] # - (ymax + ymin) / 2., polynomials
define	SF_YSPACING	Memr[P2R($1+29)] # order / (ymax - ymin), splines

define	SF_WZ		Memi[$1+30]
define	SF_TLEFT	Memi[$1+31]

# matrix and vector element definitions

define	XBASIS		Memr[P2P($1)]	#
define	XBS		Memr[P2P($1)]	#
define	YBASIS		Memr[P2P($1)]	#
define	YBS		Memr[P2P($1)]	#
define	XMATRIX		Memr[P2P($1)]	#
define	XCHOFAC		Memr[P2P($1)]	# 
define	YMATRIX		Memr[P2P($1)]	#
define	XCOEFF		Memr[P2P($1)]	#
define	COEFF		Memr[P2P($1)]	#
define	XLEFT		Memi[$1]	#
define	YLEFT		Memi[$1]	#

# structure definitions for the save restore functions

define	SF_SAVETYPE	$1[1]
define	SF_SAVEXORDER	$1[2]
define	SF_SAVEYORDER	$1[3]
define	SF_SAVEXTERMS	$1[4]
define	SF_SAVENCOLS	$1[5]
define	SF_SAVENLINES	$1[6]
define	SF_SAVECOEFF	6

# miscellaneous

define	SPLINE3_ORDER	4
define	SPLINE1_ORDER	2

# data type

define	MEM_TYPE	TY_REAL
define	VAR_TYPE	real
define	DELTA		EPSILON
