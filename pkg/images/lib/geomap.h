# Header file for GEOMAP

define	LEN_GEOMAP	(54 + SZ_FNAME + SZ_LINE + 2)

define	GM_XO		Memd[P2D($1)]		# X origin
define	GM_YO		Memd[P2D($1+2)]		# Y origin
define	GM_ZO		Memd[P2D($1+4)]		# Z origin
define	GM_XOREF	Memd[P2D($1+6)]		# X reference origin
define	GM_YOREF	Memd[P2D($1+8)]		# Y reference origin
define	GM_XMIN		Memd[P2D($1+10)]	# Minimum x value
define	GM_XMAX		Memd[P2D($1+12)]	# Maximum x value
define	GM_YMIN		Memd[P2D($1+14)]	# Minimum y value
define	GM_YMAX		Memd[P2D($1+16)]	# Maximum y value
define	GM_XOREF	Memd[P2D($1+18)]	# Mean of xref coords
define	GM_YOREF	Memd[P2D($1+20)]	# Mean of yref coords
define	GM_XOIN		Memd[P2D($1+22)]	# Mean of x coords
define	GM_YOIN		Memd[P2D($1+24)]	# Mean of y coords
define	GM_XREFPT	Memd[P2D($1+26)]	# Computed X reference point
define	GM_YREFPT	Memd[P2D($1+28)]	# Computed Y reference point
define	GM_XRMS		Memd[P2D($1+30)]	# Rms of x fit
define	GM_YRMS		Memd[P2D($1+32)]	# Rms of y fit
define	GM_REJECT	Memd[P2D($1+34)]	# Sigma limit for rejection
define	GM_PROJECTION	Memi[$1+36]		# Coordinate projection type
define	GM_FIT		Memi[$1+37]		# Fit geometry type
define	GM_FUNCTION	Memi[$1+38]		# Function type
define	GM_XXORDER	Memi[$1+39]		# X fit X order
define	GM_XYORDER	Memi[$1+40]		# X fit Y order
define	GM_XXTERMS	Memi[$1+41]		# X fit cross-terms
define	GM_YXORDER	Memi[$1+42]		# Y fit X order
define	GM_YYORDER	Memi[$1+43]		# Y fit Y order
define	GM_YXTERMS	Memi[$1+44]		# Y fit cross-terms
define	GM_MAXITER	Memi[$1+45]		# maximum number of iterations
define	GM_NPTS		Memi[$1+46]		# Number of data points
define	GM_NREJECT	Memi[$1+47]		# Number of rejected pixels
define	GM_NWTS0	Memi[$1+48]		# Number of pts with wts <= 0
define	GM_REJ		Memi[$1+49]		# Pointer to rejected pixels
define	GM_RECORD	Memc[P2C($1+50)]	# Record name
define	GM_PROJSTR	Memc[P2C($1+50+SZ_FNAME+1)]	# Projection parameters

# geoset parameters
define	GMXO		1
define	GMYO		2
define	GMXOREF		3
define	GMYOREF		4
define	GMPROJECTION	5
define  GMFIT	  	6
define  GMFUNCTION  	7
define  GMXXORDER   	8
define  GMXYORDER   	9
define  GMYXORDER   	10
define  GMYYORDER   	11
define  GMXXTERMS   	12
define  GMYXTERMS   	13
define  GMREJECT    	14
define  GMMAXITER    	15

# define the permitted coordinate projections

define  GM_PROJLIST      "|lin|azp|tan|sin|stg|arc|zpn|zea|air|cyp|car|\
mer|cea|cop|cod|coe|coo|bon|pco|gls|par|ait|mol|csc|qsc|tsc|tnx|zpx|"

define	GM_NONE	     0
define  GM_LIN	     1
define  GM_AZP       2
define  GM_TAN       3
define  GM_SIN       4
define  GM_STG       5
define  GM_ARC       6
define  GM_ZPN       7
define  GM_ZEA       8
define  GM_AIR       9
define  GM_CYP       10
define  GM_CAR       11
define  GM_MER       12
define  GM_CEA       13
define  GM_COP       14
define  GM_COD       15
define  GM_COE       16
define  GM_COO       17
define  GM_BON       18
define  GM_PCO       19
define  GM_GLS       20
define  GM_PAR       21
define  GM_AIT       22
define  GM_MOL       23
define  GM_CSC       24
define  GM_QSC       25
define  GM_TSC       26
define  GM_TNX       27
define  GM_ZPX       28

# define the permitted fitting geometries 

define	GM_GEOMETRIES	"|shift|xyscale|rotate|rscale|rxyscale|general|"

define	GM_SHIFT		1
define	GM_XYSCALE		2
define	GM_ROTATE		3
define	GM_RSCALE		4
define	GM_RXYSCALE		5
define	GM_GENERAL		6

# define the permitted fitting functions

define	GM_FUNCS	"|chebyshev|legendre|polynomial|"

# define the permitted x-terms functions

define	GM_XFUNCS	"|none|full|half|"
