# FITS Definitions

# The FITS standard readable by the FITS reader using these definitions:
# 
# 1.  8 bits / byte
# 2.  ASCII character code
# 3.  MII data format (i.e. 8 bit unsigned integers and 16 and 32 bit signed
#     twos complement integers with most significant bytes first.)
#     See mii.h.
#
# The following deviations from the FITS standard are allowed:
# 
# The number of FITS bytes per record is normally 2880 but may be specified
# by the user.

# Define the bits per pixel and precision of the 3 basic FITS types

define	FITS_BYTE	8	# Bits in a FITS byte
define	FITS_SHORT	16	# Bits in a FITS short
define	FITS_LONG	32	# Bits in a FITS long

define	FITSB_PREC	3	# Decimal digits of precision in a FITS byte
define	FITSS_PREC	5	# Decimal digits of precision in a FITS short
define	FITSL_PREC	10	# Decimal digits of precision in a FITS long

define	LSBF		NO	# Least significant byte first
define	LEN_CARD	80	# Length of FITS card in characters
define	COL_VALUE	11	# Starting column for parameter values


# FITS Keywords not mapped into IRAF header but recognized in the FITS reader.
#
#	Keyword		Action
#	________	______
#	SIMPLE		SIMPLE = 'F' not implemented
#	GROUPS		Group data is not currently implemented and the
#			file will be ignored
#	END		End of header interpretation




# Values for the following keywords are stored in the structure below.

define	LEN_FITS	(15 + SZ_FNAME + 1)

define	FITS_BSCALE	Memd[P2D($1)]	# FITS scaling parameter
define	FITS_BZERO	Memd[P2D($1+2)] # FITS zero parameter
define	BLANK_VALUE	Meml[P2L($1+4)]	# Blank value
define	BLANKS		Memi[$1+5]	# YES if blank keyword in header
define	BITPIX		Memi[$1+6]	# Bits per pixel (Must be an MII type)
define	SCALE		Memi[$1+7]	# Scale the data?
define	SIMPLE		Memi[$1+8]	# Standard FITS format 
define	NRECORDS	Memi[$1+9]	# Number of FITS logical records
define	IRAFNAME	Memc[P2C($1+12)]# Old IRAF name

# Additional IRAF header parameters

define	PIXTYPE		IM_PIXTYPE($1)
define	NBPIX		IM_NBPIX($1)
define	IRAFMAX		IM_MAX($1)
define	IRAFMIN		IM_MIN($1)
define	LIMTIME		IM_LIMTIME($1)

# Mapping of FITS Keywords to IRAF image header

define	NAXIS	 IM_NDIM($1)
define	NAXISN	 IM_LEN($1,$2)
define	OBJECT	 IM_TITLE($1)
define	HISTORY	 IM_HISTORY($1)
define	UNKNOWN	 Memc[($1+IMU-1)*SZ_STRUCT+1]	# All unrecognized keywords
						# are stored here

define	SZ_OBJECT	SZ_IMTITLE
define	SZ_HISTORY	SZ_IMHIST
define	SZ_FCTYPE	SZ_CTYPE

# FITS cards not recognized by this header are stored in the USER AREA of
# the image header (UNKNOWN) up to a maximum of:
