# WFITS header file

# The basic FITS data structure

define	LEN_FITS	(44 + SZ_FNAME + 1)

define	BSCALE		Memd[P2D($1)]	  # FITS bscale value
define	BZERO		Memd[P2D($1+2)]	  # FITS bzero value
define	TAPEMAX		Memd[P2D($1+4)]	  # IRAF tape max
define	TAPEMIN		Memd[P2D($1+6)]	  # IRAF tape min
define	IRAFMAX		Memr[P2R($1+8)]	  # IRAF image maximum
define	IRAFMIN		Memr[P2R($1+9)]	  # IRAF image minimum
define	BLANK		Meml[P2L($1+10)]  # FITS blank value
define	FITS_BITPIX	Memi[$1+11]	  # FITS bits per pixel
define	DATA_BITPIX	Memi[$1+12]	  # Data bits per pixel
define	SCALE		Memi[$1+13]	  # Scale data?
define	BLANK_STRING	Memc[P2C($1+19)]  # String containing FITS blank value
define	TYPE_STRING	Memc[P2C($1+31)]  # String containing IRAF type
define	IRAFNAME	Memc[P2C($1+41)]  # IRAF file name


# Define the FITS record size

define	FITS_RECORD	2880		# Size of standard FITS record (bytes)

# Define the FITS data types

define	FITS_BYTE	8		# Number of bits in a FITS byte
define	FITS_SHORT	16		# Number of bits in a FITS short
define	FITS_LONG	32		# Number of bits in a FITS long
define	FITS_REAL	-32		# Number of bits in a FITS real * -1
define	FITS_DOUBLE	-64		# Number of bits in a FITS double * -1

# Define the FITS precision in decimal digits

define	BYTE_PREC	3		# Precision of FITS byte
define	SHORT_PREC	5		# Precision of FITS short
define	LONG_PREC	10		# Precision of FITS long

# Define the FITS blank data values

define	BYTE_BLANK	0.0d0			# Blank value for a FITS byte
define	SHORT_BLANK	-3.2768d4		# Blank value for a FITS short
define	LONG_BLANK	-2.147483648d9		# Blank value for a FITS long

# Define the FITS integer max and min values

define	BYTE_MAX		  2.55d2	# Max value for a FITS byte
define	BYTE_MIN		   1.0d0	# Min value for a FITS byte
define	SHORT_MAX	        3.2767d4	# Max value for a FITS short
define	SHORT_MIN              -3.2767d4	# Min value for a FITS short
define	LONG_MAX	   2.147483647d9	# Max value for a FITS long
define	LONG_MIN	  -2.147483647d9	# Min value for a FITS long

# Define the FITS card image parameters

define	LEN_CARD		80	# Length of FITS header card
define	LEN_KEYWORD		8	# Length of FITS keyword
define	LEN_NAXIS_KYWRD		5       # Length of the NAXIS keyword string
define	COL_VALUE		11	# First column of value field

# Mapping of FITS task keywords to IRAF image header keywords

define	NAXIS		IM_NDIM($1)		# Number of dimensions
define	NAXISN		IM_LEN($1, $2)		# Length of each dimension
define	OBJECT		IM_TITLE($1)		# Image title
define	HISTORY		IM_HISTORY($1)		# History
define	UNKNOWN	Memc[($1+IMU-1)*SZ_STRUCT+1]	# IRAF user area

define	PIXTYPE		IM_PIXTYPE($1)		# Image pixel type
define	NBPIX		IM_NBPIX($1)		# Number of bad pixels
define	LIMTIME		IM_LIMTIME($1)		# Last modify limits time
define	MTIME		IM_MTIME($1)		# Last modify time
define	CTIME		IM_CTIME($1)		# Create time

define	LEN_USERAREA	28800			# Default user area size

# Set up a structure for the WFITS parameters

# Define the required keywords

define	FIRST_CARD	1		# FITS simple parameter
define	SECOND_CARD	2		# FITS bitpix parameter
define	THIRD_CARD	3		# FITS naxis parameter

# Define the optional FITS KEYWORD parameters

define	NOPTIONS	12		# Number of optional keywords

define	KEY_BSCALE	1		# FITS bscale parameter
define	KEY_BZERO	2		# FITS bzero parameter
define	KEY_BUNIT	3		# FITS physical units
define	KEY_BLANK	4		# FITS value of blank pixel
define	KEY_OBJECT	5		# FITS title string
define	KEY_ORIGIN	6		# Origin of FITS tape
define	KEY_DATE	7		# Date the tape was written
define	KEY_IRAFNAME	8		# Root name of IRAF image
define	KEY_IRAFMAX	9		# Maximum value of IRAF image
define	KEY_IRAFMIN	10		# Minimum value of IRAF image
define	KEY_IRAFBP	11		# Bits per pixel in IRAF image
define	KEY_IRAFTYPE	12		# IRAF image data type

define	LEN_STRING	8		# Minimum length of a string parameter
define	LEN_OBJECT	63		# Maximum length of string parameter
define	LEN_ALIGN	18		# Maximum length for aligning parameter
define	LEN_ORIGIN	9		# Length of origin string
define	LEN_BLANK	11		# Length of the blank string
define	NDEC_REAL	7		# Precision of real data
define	NDEC_DOUBLE	11		# Precision of double precision data

# Miscellaneous

define	CENTURY		1900
