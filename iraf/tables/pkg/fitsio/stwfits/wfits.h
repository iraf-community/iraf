# Wfits header file  wfits.h
# 
# Revision history 1991
#
# January 21	Change constant name. KEY_OBJECT to KEY_OPSIZE,
#		KEY_IRAFNAME to KEY_FILENAME KEY_IRAFMAX to KEY_ALLGMAX,
#		KEY_IRAFMIN to KEY_ALLGMIN, drop KEY_IRAFBP with value 11;
#		change value KEY_IRAFTYPE from 12 to 11, KEY_SDASMGNU from
#		13 to 12.

# Mapping of FITS keywords to IRAF image header

define	NAXIS	IM_NDIM($1)			# Number of dimensions
define	NAXISN	IM_LEN($1, $2)			# Length of each dimension
define	OBJECT	IM_TITLE($1)			# Image title
define	HISTORY	IM_HISTORY($1)			# History
define	UNKNOWN	Memc[($1+IMU-1)*SZ_STRUCT+1]	# IRAF user area

define	PIXTYPE		IM_PIXTYPE($1)
define	NBPIX		IM_NBPIX($1)
define	LIMTIME		IM_LIMTIME($1)
define	MTIME		IM_MTIME($1)
define	CTIME		IM_CTIME($1)

define	LEN_BLANK	11	# Length of the blank string
define	LEN_STRING	8	# Minimum length of a FITS string
define	LEN_DATE	10	# length of a date string

# Set up a structure for FITS parameters

define	LEN_FITS	(44 + SZ_FNAME + 1)

define  BSCALE          Memd[P2D($1)]    # FITS bscale value
define  BZERO           Memd[P2D($1+2)]  # FITS bzero value
define  TAPEMAX         Memd[P2D($1+4)]  # IRAF tape max
define  TAPEMIN         Memd[P2D($1+6)]  # IRAF tape min
define  IRAFMAX         Memr[P2R($1+8)]  # IRAF image maximum
define  IRAFMIN         Memr[P2R($1+9)]  # IRAF image minimum
define  BLANK           Meml[P2L($1+10)] # FITS blank value
define  FITS_BITPIX     Memi[P2I($1+11)] # FITS bits per pixel
define  DATA_BITPIX     Memi[P2I($1+12)] # Data bits per pixel
define  SCALE           Memi[P2I($1+13)] # Scale data?
define  FITS_NAXIS      Memi[P2I($1+14)] # Iraf number of axis
define  EXT_POINTER     Memi[P2I($1+15)] # FITS extension pointer
define  BLANK_STRING    Memc[P2C($1+19)] # String containing FITS blank value
define  TYPE_STRING     Memc[P2C($1+31)] # String containing IRAF type
define  IRAFNAME        Memc[P2C($1+41)] # IRAF file name

# Set up structure for the FITS Extension; could be TABLE, BINTABLE or
# IMAGE

define	LEN_EXTENSION	(20+SZ_FNAME+1)

define  EXT_BITPIX      Memi[P2I($1)]        # BITPIX value for extension
define  EXT_NAXIS       Memi[P2I($1+1)]      # Dimensionality of extension
define  EXT_LENAXIS     Memi[P2I($1+2)+$2-1] # Length of the axes
define  EXT_TYPE        Memi[P2I($1+9)]      # Extension type
define  EXT_DEFFMT      Memi[P2I($1+10)]     # For TABLES, use default fmt?
                                             # values are YES, NO
define  EXT_LINE_MAXLEN Memi[P2I($1+11)]     # Maximun line length on FITS TABLE
define  EXT_PCUNDEF     Memi[P2I($1+12)]     # Pointer to array of (true,false) for
                                             # columns with undefined values
define  EXT_PCOL        Memi[P2I($1+13)]     # Pointer the array of columns
                                             # pointers for TABLES
define  EXTVER          Memi[P2I($1+14)]     # External version of extension
define  EXT_NCOLS       Memi[P2I($1+15)]     # Number of columns in table extension
define  EXT_NROWS       Memi[P2I($1+16)]     # Number of rows in table extension
define  EXTNAME         Memc[P2C($1+17)]     # Name of file with Extension
#	         Next location is 17+32

define	COL_UNDEF	Memb[EXT_PCUNDEF($1)]	# (YES,NO) for undefined in
						# columns
define  COLPTR		Memi[EXT_PCOL($1)] # Pointer to table columns

# Define extension types (ext_type)

define  TABLE		1
define  BINTABLE	2
define  IMAGE		3

define  TRL_FILE    	4
define  TXT_FILE       	5

# define FITS data types

define	FITS_BYTE	8		# Number of bits in a FITS byte
define	FITS_SHORT	16		# Number of bits in a FITS short
define	FITS_LONG	32		# NUmber of bits in a FITS long

# define FITS precision in decimal digits

define	BYTE_PREC	3		# Precision of FITS byte
define	SHORT_PREC	5		# Precision of FITS short
define	LONG_PREC	10		# Precision of FITS long

# define FITS blank values

define	BYTE_BLANK	0.0d0			# Blank value for a FITS byte
define	SHORT_BLANK	-3.2768d4		# Blank value for a FITS short
define	LONG_BLANK	-2.147483648d9		# Blank value for a FITS long

# define FITS max and min values

define	BYTE_MAX		  2.55d2	# Max value for a FITS byte
define	BYTE_MIN		    1.0d0	# Min value for a FITS byte
define	SHORT_MAX	        3.2767d4	# Max value for a FITS short
define	SHORT_MIN              -3.2767d4	# Min value for a FITS short
define	LONG_MAX	   2.147483647d9	# Max value for a FITS long
define	LONG_MIN	  -2.147483647d9	# Min value for a FITS long

# define the FITS card image parameters

define	LEN_CARD		80	# Length of FITS header card
define	LEN_KEYWORD		8	# Length of FITS keyword
define	COL_VALUE		11	# First column of field
define	NDEC_REAL		7	# Precision of real
# NZ Aug 18 '89. Change value 11 to 14
define	NDEC_DOUBLE		14	# Precision of double

# define the KEYWORD parameters

define	NOPTIONS	13			# Number of optional keywords

define	FIRST_CARD	1
define	SECOND_CARD	2
define	THIRD_CARD	3
define  FOURTH_CARD	4
define  FIFTH_CARD	5
define	SIXTH_CARD	6
define	SEVENTH_CARD	7
define	EIGHTH_CARD	8
define	NINTH_CARD	9

# define optional header keywords

define	KEY_BSCALE	1		# FITS bscale parameter
define	KEY_BZERO	2		# FITS bzero parameter
define	KEY_BUNIT	3		# FITS physical units
define	KEY_BLANK	4		# FITS value of blank pixel
define	KEY_OPSIZE	5		# Original PSIZE value
define	KEY_ORIGIN	6		# origin of FITS tape
define	KEY_DATE	7		# date the tape was written
define	KEY_FILENAME	8		# Iraf original disk filename
define	KEY_ALLGMAX	9		# maximum value in all groups
define	KEY_ALLGMIN	10		# minimum value in all groups
define	KEY_IRAFTYPE	11		# IRAF image data type
define	KEY_SDASMGNU	12		# Number of groups in input image
define	KEY_OBJECT	13		# Object keyword

# miscellaneous

define	CENTURY		1900
define  SZ_EXTN	        31
define  SZ_MAXCOL	256		# Maximum number of columns for tables
					# for table information.
