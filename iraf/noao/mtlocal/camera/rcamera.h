# CAMERA Definitions

# The CAMERA standard readable by the CAMERA reader:
# 
# 1.  8 bits / byte
# 2.  ASCII character code
# 3.  16 bit, twos complement with least significant bytes first
#
# The following deviations from the CAMERA standard are allowed:
# 
# A user specified flag allows selecting most significant byte format

define	CAM_BYTE		8	# Number of bits in CAMERA byte
define	BITPIX			16	# Bits per CAMERA data values
#define	LSBF			YES	# Least Significant Byte First
define	LEN_CAM_PARAMETERS	256	# Number of CAMERA header parameters
define	LEN_CAM_TEXT		 64	# Length of CAMERA text
define	LEN_HEADER		513	# Number of 16 bit words in the header

# Mapping of CAMERA Parameters to IRAF image header

define	NAXIS	 IM_NDIM($1)		# Number of image dimensions
define	PARAM5	 IM_LEN($1,1)		# Number of pixels in first dimension
define	PARAM6	 IM_LEN($1,2)		# Number of pixels in second dimension
define	TITLE	 IM_TITLE($1)

define	LEN_TITLE	64

# Define IRAF coordinate transformation keywords
# No longer in use, may wish to change in future

define	CRVAL	CT_CRVAL(IM_CTRAN($1), $2)
define	CRPIX	CT_CRPIX(IM_CTRAN($1), $2)
define	CDELT	CT_CDELT(IM_CTRAN($1), $2)
define	CROTA	CT_CROTA(IM_CTRAN($1), $2)
define	CTYPE	CT_CTYPE(IM_CTRAN($1))

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

# define rcamera structure

define	CCD_PICNO	$1[2]		# CCD picture number
define	IMAGE_TYPE	$1[3]		# Data type, object, bias etc.
define	NRECS		$1[4]		# Number of DATA records
define	NAXIS1		$1[5]		# Number of columns
define	NAXIS2		$1[6]		# Number of rows
define	ITIME		$1[7]		# Integration time in seconds
define	TTIME		$1[8]		# Total time in seconds
define	OTIME		$1[9]		# Open time in seconds
define	UT_HR		$1[10]		# Universal time
define	UT_MIN		$1[11]		#
define	UT_SEC		$1[12]		#
define	ZD_DEG		$1[13]		# Zenith distance
define	ZD_MIN		$1[14]		#
define	ZD_SEC		$1[15]		#
define	OBS_MON		$1[16]		# Date of observation
define	OBS_DAY		$1[17]		#
define	OBS_YR		$1[18]		#
define	ST_HR		$1[19]		# Sidereal time
define	ST_MIN		$1[20]		#
define	ST_SEC		$1[21]		#
define	EPOCH		$1[22]		# Epoch of RA and DEC
define	REC_LEN		$1[25]		# Length of a data record
define	BIAS_PIX	$1[26]		#
define	RA_HR		$1[27]		# RA
define	RA_MIN		$1[28]		#
define	RA_SEC		$1[29]		#
define	DEC_DEG		$1[30]		# Declination
define	DEC_MIN		$1[31]		#
define	DEC_SEC		$1[32]		#
define	CAM_TEMP	$1[33]		# Camera temperature
define	DEW_TEMP	$1[34]		# Dewar temperature
define	CAM_HEAD	$1[35]		# Camera head ID
define	F1POS		$1[36]		# Position of filter bolt 1
define	F2POS		$1[37]		# Position of filter bolt 2
define	TV_FILTER	$1[38]		# TV filter
define	COMP_LAMP	$1[39]		# Comparison lamp
define	TILT_POS	$1[40]		# Tilt position
define	PED_POS		$1[41]		# Pedestal positions
define	AIR_MASS	$1[42]		# Airmass * 100
define	GAIN		$1[47]		# Gain
define	PREFLASH	$1[48]		# Preflash
define	PFLEVEL		$1[49]		# Preflash level
define	RDNOISE		$1[50]		# Readout noise
define	BT_FLAG		$1[51]		# Bias trim flag
define	BP_FLAG		$1[52]		# Bad pixel cleaning flag
define	CR_FLAG		$1[53]		# Cosmic ray cleaning flag
define	DK_FLAG		$1[54]		# Dark subtraction flag
define	FF_FLAG		$1[55]		# Flat field flag
define	FR_FLAG		$1[56]		# Fringe correction flag
define	FR_SC100	$1[57]		# Fringe scaling parameter X 100
define	FR_SC1		$1[58]		# Fringe scaling parameter X 1
define	BI_FLAG		$1[59]		# Bias subtract flag
define	PIC_NX		$1[61]		# Number of x pixels in frame
define	PIC_NY		$1[62]		# Number of y pixels in frame
define	PIC_X0		$1[63]		# Overscan pixels in x
define	PIC_Y0		$1[64]		# Overscan pixels in y
define	PIC_XSUM	$1[65]		# Summed pixels in x
define	PIC_YSUM	$1[66]		# Summed pixels in y
define	PIC_XPRE	$1[67]		# Prescan pixels in x
define	PIC_YPRE	$1[68]		# Prescan pixels in y
define	PIC_NXRAW	$1[69]		# Full format pixels in x
define	PIC_NYRAW	$1[70]		# Full format pixels in y
define	PIC_NXOFF	$1[71]		# X offset from data in full format
define	PIC_NYOFF	$1[72]		# Y offset from data in full format
define	PIC_XT1		$1[73]		# X pixels to skip at data start
define	PIC_XT2		$1[74]		# X pixels to skip at data end
define	PIC_YT1		$1[75]		# Y pixels to skip at data start
define	PIC_YT2		$1[76]		# Y pixels to skip at data end
define	PIC_S1		$1[77]		# Pixels to ignore start/end prescan
define	PIC_S2		$1[78]		# Pixels to ignore start/end prescan
define	PIC_IRBSCALE	$1[146]		# Picture scaling factor

# define image data types

define	BEG_IRDATA	32
define	END_IRDATA	44

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
define	OCCULTATION	10

define	IROBJECT	32
define	IRDARK		33
define	IRPFLAT		34
define	IRSFLAT		35
define	IRCOMP		36
define	IRBIAS		37
define	IRDFLAT		38
define	IRMASK		39
define	IRMULT		40
define	IRSCAN		41
define	IRGRID		42
define	IRSPECTRA	43
define	IRSPECKLE	44

# define the instrument types

define	TEK1	1
define	RCA3	2
define	TI1	3
define	RCA0	4
define	RCA2	5
define	RCA1	6
define	TI2	7
define	TI3	8
define	TI4	9
define	TI5	10
