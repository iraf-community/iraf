# FITS Definitions

# The FITS standard readable by the FITS reader using these definitions:
# 
# 1.  8 bits / byte
# 2.  ASCII character code
# 3.  MII integer data format (i.e. 8 bit unsigned integers and 16 and 32
#     bit signed twos complement integers with most significant bytes first.)
# 4.  IEEE 32 and 64 bit floating point format
#
#
# The following deviations from the FITS standard are allowed:
# 
# 1. The number of FITS bytes per record is normally 2880 or up to 10 times
#    2880 bytes but may be arbitrarily specified  by the user.

# Define the bits per pixel, precision and byte order of the basic FITS types

define	FITS_RECORD	2880	# number of bytes in a standard FITS record

define	FITS_BYTE	8	# Bits in a FITS byte
define	FITS_SHORT	16	# Bits in a FITS short
define	FITS_LONG	32	# Bits in a FITS long
define	FITS_REAL	-32	# Bits in a FITS real * -1
define	FITS_DOUBLE	-64	# Bits in a FITS double * -1

define	FITSB_PREC	3	# Decimal digits of precision in a FITS byte
define	FITSS_PREC	5	# Decimal digits of precision in a FITS short
define	FITSL_PREC	10	# Decimal digits of precision in a FITS long

define	LSBF		NO	# Least significant byte first

# Define the basic format of a FITS cardimage

define	LEN_CARD	80	# Length of FITS card in characters
define	COL_VALUE	11	# Starting column for parameter values


# FITS standards not recognized currently by IRAF.
#
# 1. SIMPLE	SIMPLE = 'F' not implemented, file skipped
# 2. GROUPS	Group data not currently implemented, file skippped


# Values for the following quantities are stored in the structure below.

define	LEN_FITS	(15 + SZ_FNAME + 1)

define	FITS_BSCALE	Memd[P2D($1)]	  # FITS scaling parameter
define	FITS_BZERO	Memd[P2D($1+2)]   # FITS zero parameter
define	BLANK_VALUE	Meml[P2L($1+4)]   # Blank value
define	BLANKS		Memi[$1+5]	  # YES if blank keyword in header
define	BITPIX		Memi[$1+6]	  # Bits per pixel (Must be an MII type)
define	SCALE		Memi[$1+7]	  # Scale the data?
define	SIMPLE		Memi[$1+8]	  # Standard FITS format 
define	NRECORDS	Memi[$1+9]	  # Number of FITS logical records
define	IRAFNAME	Memc[P2C($1+12)]  # Old IRAF name

# Mapping of additional IRAF header parameters

define	PIXTYPE		IM_PIXTYPE($1)
define	NBPIX		IM_NBPIX($1)
define	IRAFMAX		IM_MAX($1)
define	IRAFMIN		IM_MIN($1)
define	LIMTIME		IM_LIMTIME($1)
define	LEN_USERAREA	28800

# Mapping of FITS Keywords to IRAF image header

define	NAXIS	 IM_NDIM($1)
define	NAXISN	 IM_LEN($1,$2)
define	OBJECT	 IM_TITLE($1)
define	HISTORY	 IM_HISTORY($1)
define	UNKNOWN	 Memc[($1+IMU-1)*SZ_STRUCT+1]	# All unrecognized keywords
						# are stored here
# Miscellaneous definitions.

define	SZ_OBJECT	SZ_IMTITLE
define	SZ_HISTORY	SZ_IMHIST
define	SZ_FCTYPE	SZ_CTYPE
