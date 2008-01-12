
# PDS Definitions

# The PDS standard readable by the PDS reader:
# 
# 1.  8 bits / byte
# 2.  6 bits of data / byte
# 3.  high order byte is first
# 4.  first 80 bytes are an id string in 059 code containing 40 characters
#     12 bits per character
# 5.  remaining 40 bytes contain short and long parameter values
#
# A user specified flag allows selecting most significant byte format

define	BITPIX			16	# Number of bits per PDS data value
define	PDS_BYTE		 8	# Number of bits per PDS byte
define	LEN_PDS_TEXT		40	# Length of PDS text in characters
define  LEN_PDS_HEADER          120     # Length of PDS header in bytes
define  LEN_TABLE               96	# Length of 059 to ASCII table
define  LEN_PAR_ARRAY		14	# Length of paramter array
define	SZB_MIISHORT		2	# Number of bytes in an MII short
define	SZB_MIILONG		4	# Number of bytes in an MII long

# Mapping of PDS Parameters to IRAF image header

define	NAXIS	 IM_NDIM($1)		# Number of image dimensions
define	NCOLS	 IM_LEN($1, 1)		# Number of pixels in first dimension
define	NLINES	 IM_LEN($1, 2)		# Number of pixels in second dimension
define	TITLE	 IM_TITLE($1)
define	SZ_TITLE SZ_IMTITLE

# define the IRAF coordinate tranformation parameters

define	CRVAL	CT_CRVAL(IM_CTRAN($1), $2)
define	CRPIX	CT_CRPIX(IM_CTRAN($1), $2)
define	CDELT	CT_CDELT(IM_CTRAN($1), $2)
define	CROTA	CT_CROTA(IM_CTRAN($1), $2)
define	CTYPE	CT_CTYPE(IM_CTRAN($1))

# define remaining IRAF header parameters

define	IRAFMAX		IM_MAX($1)
define	IRAFMIN		IM_MIN($1)
define	LIMTIME		IM_LIMTIME($1)
define	PIXTYPE		IM_PIXTYPE($1)

# define scan types
define	RASTER   210
define	EDGE     197
define	FLIPPED  198

# define the octal constants for conversion of signed parameters
define	TWO_TO_23	40000000b
define	TWO_TO_24	100000000b

# define the byte offsets for the header parameters
define  DX               pds_unpacks ($1,81)	# Delta x in microns of scan
define  DY               pds_unpackl ($1,83)	# Delta y in microns of scan
define  NPTS_PER_SCAN    pds_unpackl ($1,87)	# Number of scans
define  NSCANS           pds_unpacks ($1,91)	# Number of scans
define  SCANTYPE         pds_unpacks ($1,93)	# Edge, flipped or raster
define  SCANSPEED        pds_unpacks ($1,95)	# Scanning speed (1-255)
define  SCANORIGIN       pds_unpacks ($1,97)	# Origin of scan 1,2,3,4 or 0
define  CORNER           pds_unpacks ($1,99)	# Starting corner of scan
define  NRECS_PER_SCAN   pds_unpacks ($1,101)	# Number of records per scan
define	XTRAVEL          pds_unpackl ($1,103)	# Xtravel in microns per scan
define  YTRAVEL          pds_unpackl ($1,107)	# Ytravel in microns per scan
define  NPTS_PER_REC     pds_unpacks ($1,111)	# Number of points per record
define  XCOORD           pds_unpackl ($1,113)	# X coordinate of origin
define  YCOORD           pds_unpackl ($1,117)	# Y coordinate of origin

# define the parameter array
define  P_DX		 $1[1]
define  P_DY		 $1[2]
define  P_NPTS_PER_SCAN  $1[3]
define  P_NSCANS	 $1[4]
define  P_SCANTYPE	 $1[5]
define  P_SCANSPEED	 $1[6]
define  P_SCANORIGIN	 $1[7]
define  P_CORNER	 $1[8]
define  P_NRECS_PER_SCAN $1[9]
define  P_XTRAVEL	 $1[10]
define  P_YTRAVEL	 $1[11]
define  P_NPTS_PER_REC   $1[12]
define  P_XCOORD	 $1[13]
define  P_YCOORD	 $1[14]

define	SCANSTART	10	# Number of bytes in the scanstart indicator
