# Header file for asi package

# set up the asi descriptor

define	LEN_ASISTRUCT	4

define	ASI_TYPE	Memi[$1]	# intepolator type
define	ASI_NCOEFF	Memi[$1+1]	# number of data points
define	ASI_OFFSET	Memi[$1+2]	# offset into coefficient array of
					# first data point
define	ASI_COEFF	Memi[$1+3]	# pointer to coefficient array

# define element of the coefficient array

define	COEFF		Memr[$1]	# element of the coefficient matrix

# define structure for ASISAVE and ASIRESTORE

define	ASI_SAVETYPE	$1[1]
define	ASI_SAVENCOEFF	$1[2]
define	ASI_SAVEOFFSET	$1[3]
define	ASI_SAVECOEFF	3

# define the sinc function truncation length, taper and precision parameters

define	NSINC		30 		# the sinc truncation length
define	NTAPER		25 		# the start of the sinc taper
define	STAPER		0.2		# the slope of the taper
define	DX		0.001		# interpolation minimum

# define number of points used in spline interpolation for ARIEVAL, ARIDER
# and ARBPIX

define	SPLPTS		16


# miscellaneous

define	SPLINE3_ORDER	4
define	POLY3_ORDER	4
define	POLY5_ORDER	6
define	MAX_NDERIVS	6
