# 2D-FRUTTI Definitions

# The 2D-FRUTTI standard readable by the 2D-FRUTTI reader:
# 
# 1.  8 bits / byte
# 2.  ASCII character code
# 3.  16 bit, twos complement with least significant bytes first
#
# The following deviations from the 2D-FRUTTI standard are allowed:
# 
# A user specified flag allows selecting most significant byte format

define	CAM_BYTE		8	# Number of bits in 2D-FRUTTI byte
define	BITPIX			16	# Bits per 2D-FRUTTI data values
#define	LSBF			YES	# Least Significant Byte First
define	LEN_CAM_PARAMETERS	256	# Number of 2D-FRUTTI header parameters
define	LEN_CAM_TEXT		40	# Length of 2D-FRUTTI text
define	LEN_HEADER		4096	# Number of 16 bit words in the header
define	FST_HDRBYTE		101	# The first word of the ID string

# Mapping of 2D-FRUTTI Parameters to IRAF image header

define	NAXIS	 IM_NDIM($1)		# Number of image dimensions
define	PARAM5	 IM_LEN($1,1)		# Number of pixels in first dimension
define	PARAM6	 IM_LEN($1,2)		# Number of pixels in second dimension
define	TITLE	 IM_TITLE($1)

define	LEN_TITLE	40

# Additional IRAF header parameters

define	PIXTYPE		IM_PIXTYPE($1)
define	LIMTIME		IM_LIMTIME($1)
define	IRAFMAX		IM_MAX($1)
define	IRAFMIN		IM_MIN($1)

# define the user area parameters

define	LEN_USER_AREA	2880
define	UNKNOWN		Memc[($1+IMU-1)*SZ_STRUCT + 1]
define	LEN_KEYWORD	8
define	LEN_OBJECT	63

# Define rcamera structure. Definitions which are commented out may be needed
# later

define	CCD_PICNO	$1[1]		# CCD picture number
#define	DATA_TYPE	$1[0]		# Data type, object, bias etc.
#define	NRECS		(($1[11]*$1[12]-1)/4096+1)	# Number of DATA records
define	NAXIS1		$1[12]		# Number of columns
define	NAXIS2		$1[11]		# Number of rows
define	ITIME		$1[9]		# Integration time in seconds
define	TTIME		$1[8]		# Total time in seconds
define	OTIME		$1[10]		# Open time in seconds
#define	UT_HR		$1[0]		# Universal time
#define	UT_MIN		$1[0]		#
#define	UT_SEC		$1[0]		#
#define	ZD_DEG		$1[0]		# Zenith distance
#define	ZD_MIN		$1[0]		#
#define	ZD_SEC		$1[0]		#
#define	OBS_MON		$1[0]		# Date of observation
#define	OBS_DAY		$1[0]		#
#define	OBS_YR		$1[0]		#
#define	ST_HR		$1[0]		# Sidereal time
#define	ST_MIN		$1[0]		#
#define	ST_SEC		$1[0]		#
#define	EPOCH		$1[0]		# Epoch of RA and DEC
define	REC_LEN		($1[1] * 0 + 4096)	# Length of a data record
#define	BIAS_PIX	$1[0]		#
#define	RA_HR		$1[0]		# RA
#define	RA_MIN		$1[0]		#
#define	RA_SEC		$1[0]		#
#define	DEC_DEG		$1[0]		# Declination
#define	DEC_MIN		$1[0]		#
#define	DEC_SEC		$1[0]		#
#define	CAM_TEMP	$1[0]		# Camera temperature
#define	DEW_TEMP	$1[0]		# Dewar temperature
#define	CAM_HEAD	$1[0]		# Camera head ID
#define	F1POS		$1[0]		# Position of filter bolt 1
#define	F2POS		$1[0]		# Position of filter bolt 2
#define	TV_FILTER	$1[0]		# TV filter
#define	COMP_LAMP	$1[0]		# Comparison lamp
#define	TILT_POS	$1[0]		# Tilt position
#define	PED_POS		$1[0]		# Pedestal positions
#define	AIR_MASS	$1[0]		# Airmass * 100
#define	BT_FLAG		$1[0]		# Bias trim flag
#define	BP_FLAG		$1[0]		# Bad pixel cleaning flag
#define	CR_FLAG		$1[0]		# Cosmic ray cleaning flag
#define	DK_FLAG		$1[0]		# Dark subtraction flag
#define	FF_FLAG		$1[0]		# Flat field flag
#define	FR_FLAG		$1[0]		# Fringe correction flag
#define	FR_SC100	$1[0]		# Fringe scaling parameter X 100
#define	FR_SC1		$1[0]		# Fringe scaling parameter X 1
#define	BI_FLAG		$1[0]		# Bias subtract flag

# Define image data types.

define	OBJECT		0
define	DARK		1
define	PFLAT		2
define	SFLAT		3
define	COMP		4
define	BIAS		5
define	DFLAT		6
define	MASK		7
define	MULT		8
define	SCAN		9
