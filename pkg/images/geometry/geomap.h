# Header file for GEOMAP

define	LEN_GEOMAP	(25 + SZ_FNAME + 1)

define	GM_FUNCTION		Memi[$1]	# Function type
define	GM_XXORDER		Memi[$1+1]	# X fit X order
define	GM_XYORDER		Memi[$1+2]	# X fit Y order
define	GM_XXTERMS		Memi[$1+3]	# X fit cross-terms
define	GM_YXORDER		Memi[$1+4]	# Y fit X order
define	GM_YYORDER		Memi[$1+5]	# Y fit Y order
define	GM_YXTERMS		Memi[$1+6]	# Y fit cross-terms
define	GM_XMIN			Memr[$1+7]	# Minimum x value
define	GM_XMAX			Memr[$1+8]	# Maximum x value
define	GM_YMIN			Memr[$1+9]	# Minimum y value
define	GM_YMAX			Memr[$1+11]	# Maximum y value
define	GM_REJECT		Memr[$1+12]	# Sigma limit for rejection
define	GM_NREJECT		Memi[$1+13]	# Number of rejected pixels
define	GM_REJ			Memi[$1+14]	# Pointer to rejected pixel list
define	GM_NPTS			Memi[$1+15]	# Number of data points
define	GM_XRMS			Memr[$1+16]	# Rms of x fit
define	GM_YRMS			Memr[$1+17]	# Rms of y fit
define	GM_XOREF		Memr[$1+18]	# Mean of xref coords
define	GM_YOREF		Memr[$1+19]	# Mean of yref coords
define	GM_XOIN			Memr[$1+20]	# Mean of x coords
define	GM_YOIN			Memr[$1+21]	# Mean of y coords
define	GM_NWTS0		Memi[$1+23]	# Number of pts with wts <= 0
define	GM_NAME			Memc[P2C($1+24)]# Coordinate file	
