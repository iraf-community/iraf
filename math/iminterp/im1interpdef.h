# Header file for asi package

# set up the asi descriptor

define	LEN_ASISTRUCT	10

define	ASI_TYPE	Memi[$1]	# interpolator type
define	ASI_NSINC	Memi[$1+1]	# sinc interpolator half-width
define	ASI_NINCR	Memi[$1+2]	# number of sinc interpolator luts
define	ASI_SHIFT	Memr[P2R($1+3)]	# sinc interpolator shift
define	ASI_PIXFRAC	Memr[P2R($1+4)]	# pixel fraction for drizzle
define	ASI_NCOEFF	Memi[$1+5]	# number of coefficients
define	ASI_OFFSET	Memi[$1+6]	# offset of first data point
define	ASI_COEFF	Memi[$1+7]	# pointer to coefficient array
define	ASI_LTABLE	Memi[$1+8]	# pointer to sinc look-up table array
define	ASI_BADVAL	Memr[P2R($1+9)]	# bad value for drizzle

# define element of the coefficient array

define	COEFF		Memr[P2P($1)]	# element of the coefficient matrix
define	LTABLE		Memr[P2P($1)]	# element of the look-up table

# define structure for ASISAVE and ASIRESTORE

define	ASI_SAVETYPE	$1[1]
define	ASI_SAVENSINC	$1[2]
define	ASI_SAVENINCR	$1[3]
define	ASI_SAVESHIFT	$1[4]
define	ASI_SAVEPIXFRAC	$1[5]
define	ASI_SAVENCOEFF	$1[6]
define	ASI_SAVEOFFSET	$1[7]
define	ASI_SAVEBADVAL	$1[8]
define	ASI_SAVECOEFF	8

# define the sinc function truncation length, taper and precision parameters
# These should be identical to the definitions in im2interpdef.h.

define	NSINC		15		# the sinc truncation length
define	NINCR		20		# the number of sinc look-up tables
define	DX		0.001		# sinc interpolation minimum
define	PIXFRAC		1.0		# drizzle pixel fraction
define	MIN_PIXFRAC	0.001		# the minimum drizzle pixel fraction
define	BADVAL		0.0

# define number of points used in spline interpolation for ARIEVAL, ARIDER
# and ARBPIX

define	SPLPTS		16

# miscellaneous

define	SPLINE3_ORDER	4
define	POLY3_ORDER	4
define	POLY5_ORDER	6
define	MAX_NDERIVS	6
