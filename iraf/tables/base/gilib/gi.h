# copied stf.h to gflib/gf.h directory on APRIL 21 1993 N.Z.
# Renamed gflib to gilib, along with all external names (BPS Sept 1 1998)
# Added fxf.h (BPS 24 Nov 1999)
# Source from IRAF V2.11 

# STF.H -- IKI/STF internal definitions.

define	HDR_TEMPLATE	"dev$pix.hhh"	# used by fmkcopy to create new header
define	MAX_LENEXTN	3		# max length imagefile extension
define	STF_HDRPATTERN	"^??h"		# class of legal header extensions
define	STF_DEFHDREXTN	"hhh"		# default header file extension
define	STF_DEFPIXEXTN	"hhd"		# default pixel file extension
define	ENV_DEFIMTYPE	"imtype"	# name of environment variable
define	STF_MAXDIM	7		# max NAXIS
define	MAX_CACHE	5		# max cached header files
define	DEF_CACHE	3		# default size of header file cache
define	ENV_STFCACHE	"stfcache"	# environment variable for cache size
define	MAX_PCOUNT	50		# max param descriptors
define	SZ_DATATYPE	16		# e.g., `REAL*4'
define	SZ_KEYWORD	8		# size of a FITS keyword
define	SZ_PTYPE	8		# e.g., `CRPIX1'
define	SZ_PDTYPE	16		# e.g., `CHAR*8'
define	SZ_COMMENT	FITS_SZCOMMENT	# comment string for GPB card
define	SZ_EXTRASPACE	(81*32)		# extra space for new cards in header

define	FITS_RECLEN	80		# length of a FITS record (card)
define	FITS_STARTVALUE	10		# first column of value field
define	FITS_ENDVALUE	30		# last  column of value field
define	FITS_SZVALSTR	21		# nchars in value string
define	FITS_SZCOMMENT	50		# max chars in comment, incl. /

# STF image descriptor, used internally by the STF interface.  The required
# header parameters are maintained in this descriptor, everything else is
# simply copied into the user area of the IMIO descriptor.

define	LEN_STFDES	(LEN_STFBASE+MAX_PCOUNT*LEN_PDES)
define	STF_CACHE	STF_BITPIX	# cache descriptor starting here
define	STF_CACHELEN	(33+STF_PCOUNT($1)*LEN_PDES)
define	LEN_STFBASE	43

define	STF_ACMODE	Memi[P2I($1)]		# image access mode
define	STF_NEWIMAGE	Memi[P2I($1+1)]		# creating entire new STF format image?
define	STF_GROUP	Memi[P2I($1+2)]		# group to be accessed
define	STF_SZGROUP	Memi[P2I($1+3)]		# size of image+hdr in pixfile, chars
define	STF_PFD		Memi[P2I($1+4)]		# pixfile file descriptor
define	STF_GRARG	Memi[P2I($1+5)]		# group index given in image name
			# (extra space)
define	STF_BITPIX	Memi[P2I($1+10)]	# bits per pixel
define	STF_NAXIS	Memi[P2I($1+11)]	# number of axes in image
define	STF_GROUPS	Memi[P2I($1+12)]	# group format?
define	STF_GCOUNT	Memi[P2I($1+13)]	# number of groups in STF image
define	STF_PSIZE	Memi[P2I($1+14)]	# size of GPB, bits
define	STF_PCOUNT	Memi[P2I($1+15)]	# number of parameters in GPB
define	STF_DATATYPE	Memc[P2C($1+16)]	# datatype string
define	STF_LENAXIS	Memi[P2I($1+35)+$2-1]	# 35:41 = [7] max
define	STF_PDES	(($1)+43+((($2)-1)*LEN_PDES))

# GPB Parameter descriptor.
define	LEN_PDES	81
define	P_OFFSET	Memi[P2I($1)]		# struct offset of parameter
define	P_SPPTYPE	Memi[P2I($1+1)]		# SPP datatype of parameter
define	P_LEN		Memi[P2I($1+2)]		# number of elements
define	P_PSIZE		Memi[P2I($1+3)]		# field size, bits
define	P_PTYPEP	(P2C($1+4))		# pointer to parameter name
define	P_PTYPE		Memc[P2C($1+4)]		# parameter name
define	P_PDTYPE	Memc[P2C($1+13)]	# datatype string
define	P_COMMENT	Memc[P2C($1+30)]	# comment string

# Reserved FITS keywords known to this code.
define	KW_BITPIX	1
define	KW_DATATYPE	2
define	KW_END		3
define	KW_GCOUNT	4
define	KW_GROUPS	5
define	KW_NAXIS	6
define	KW_NAXISN	7
define	KW_PCOUNT	8
define	KW_PDTYPE	9
define	KW_PSIZE	10
define	KW_PSIZEN	11
define	KW_PTYPE	12
define	KW_SIMPLE	13

# FITS.H -- IKI/FITS internal definitions.

define	FITS_ORIGIN	"NOAO-IRAF FITS Image Kernel July 1999"

define	FITS_LENEXTN	4		# max length imagefile extension
define	SZ_DATATYPE	16		# size of datatype string (eg "REAL*4")
define	SZ_EXTTYPE	20		# size of exttype string (eg BINTABLE)
define	SZ_KEYWORD	8		# size of a FITS keyword
define	SZ_EXTRASPACE	(81*32)		# extra space for new cards in header
define	DEF_PHULINES	0		# initial allocation for PHU
define	DEF_EHULINES	0		# initial allocation for EHU
define	DEF_PADLINES	0		# initial value for extra lines in HU

define	FITS_BLOCK_BYTES 2880		# FITS logical block length (bytes)
define	FITS_BLOCK_CHARS 1440		# FITS logical block length (spp chars)
define	FITS_STARTVALUE 10		# first column of value field
define	FITS_ENDVALUE	30		# last	column of value field
define	FITS_SZVALSTR	21		# nchars in value string
define	LEN_CARD	80		# Length of FITS card.
define	LEN_UACARD	81		# Size of a Userarea line.
define	NO_KEYW		-1		# Indicates no keyword is present.

define	MAX_OFFSETS	100		# Max number of offsets per cache entry.
define	MAX_CACHE	60		# Max number of cache entries.
define	DEF_CACHE	10		# default number of cache entries.

define	DEF_HDREXTN	"fits"		# default header file extension
define	ENV_FKINIT	"fkinit"	# FITS kernel initialization

define	DEF_ISOCUTOVER	0		# date when ISO format dates kick in
define	ENV_ISOCUTOVER	"isodates"	# environment override for default

# FITS image descriptor, used internally by the FITS kernel.  The required
# header parameters are maintained in this descriptor, everything else is
# simply copied into the user area of the IMIO descriptor.

define	LEN_FITDES	348
define	LEN_FITBASE	297

define	FIT_ACMODE	Memi[P2I($1)]		# image access mode
define	FIT_PFD		Memi[P2I($1+1)]		# pixel file descriptor
define	FIT_PIXOFF	Memi[P2I($1+2)]		# pixel offset
define	FIT_TOTPIX	Memi[P2I($1+3)]		# size of image in pixfile, chars
define	FIT_IO		Memi[P2I($1+4)]		# FITS I/O channel
define	FIT_ZCNV	Memi[P2I($1+5)]		# Set if on-the-fly conversion needed
define	FIT_IOSTAT	Memi[P2I($1+6)]		# I/O status for zfio routines
			# extra space
define	FIT_BSCALE	Memd[P2D($1+16)]
define	FIT_BZERO	Memd[P2D($1+18)]
define	FIT_BITPIX	Memi[P2I($1+20)]	# bits per pixel
define	FIT_NAXIS	Memi[P2I($1+21)]	# number of axes in image
define	FIT_LENAXIS	Memi[P2I($1+22)+$2-1]	# 35:41 = [7] max
define	FIT_ZBYTES	Memi[P2I($1+30)]	# Status value for FIT_ZCNV mode
define	FIT_HFD		Memi[P2I($1+31)]	# Header file descriptor
define	FIT_PIXTYPE	Memi[P2I($1+32)]
define	FIT_CACHEHDR	Memi[P2I($1+33)]	# Cached main header unit's address.
define	FIT_CACHEHLEN	Memi[P2I($1+34)]	# Lenght of the above.
define	FIT_IM		Memi[P2I($1+35)]	# Has the 'im' descriptor value 
define	FIT_GROUP	Memi[P2I($1+36)]
define	FIT_NEWIMAGE	Memi[P2I($1+37)]	# Newimage flag
define	FIT_HDRPTR	Memi[P2I($1+38)]	# Header data Xtension pointer
define	FIT_PIXPTR	Memi[P2I($1+39)]	# Pixel data Xtension pointer
define	FIT_NUMOFFS	Memi[P2I($1+40)]	# Number of offsets in cache header.
define	FIT_EOFSIZE	Memi[P2I($1+41)]	# Size in char of file before append.
define	FIT_XTENSION	Memi[P2I($1+42)]	# Yes, if an Xtension has been read.
define	FIT_INHERIT	Memi[P2I($1+43)]	# INHERIT header keyword value.
define	FIT_EXTVER	Memi[P2I($1+44)]	# EXTVER value (integer only)
define	FIT_EXPAND	Memi[P2I($1+45)]	# Expand the header?
define	FIT_MIN		Memr[P2R($1+46)]	# Minimum pixel value
define	FIT_MAX		Memr[P2R($1+47)]	# Maximum pixel value
define	FIT_MTIME	Meml[P2L($1+48)]	# Time of last mod. for FITS unit 
define	FIT_SVNANR	Memr[P2R($1+49)]
define	FIT_SVNAND	Memd[P2D($1+50)]
define	FIT_SVMAPRIN	Memi[P2I($1+52)]
define	FIT_SVMAPROUT	Memi[P2I($1+53)]
define	FIT_SVMAPDIN	Memi[P2I($1+54)]
define	FIT_SVMAPDOUT	Memi[P2I($1+55)]
define	FIT_EXTEND	Memi[P2I($1+56)]	# FITS extend keyword
define	FIT_EXTTYPE	Memc[P2C($1+57)]	# extension type
define	FIT_FILENAME	Memc[P2C($1+97)]	# FILENAME value 
define	FIT_EXTNAME	Memc[P2C($1+137)]	# EXTNAME value 
define	FIT_DATATYPE	Memc[P2C($1+177)]	# datatype string
define	FIT_TITLE	Memc[P2C($1+217)]	# datatype string
define	FIT_OBJECT	Memc[P2C($1+257)]	# datatype string

# The FKS terms carry the fkinit or kernel section arguments.
define	FKS_APPEND	Memi[P2I($1+297)]	# YES, NO append an extension
define	FKS_INHERIT	Memi[P2I($1+298)]	# YES, NO inherit the main header
define	FKS_OVERWRITE	Memi[P2I($1+299)]	# YES, NO overwrite an extension
define	FKS_DUPNAME	Memi[P2I($1+300)]	# YES, NO allow duplicated EXTNAME
define	FKS_EXTVER	Memi[P2I($1+301)]	# YES, NO allow duplicated EXTNAME
define	FKS_EXPAND	Memi[P2I($1+302)]	# YES, NO expand the header
define	FKS_PHULINES	Memi[P2I($1+303)]	# Allocated lines in PHU
define	FKS_EHULINES	Memi[P2I($1+304)]	# Allocated lines in EHU
define	FKS_PADLINES	Memi[P2I($1+305)]	# Additional lines for HU
define	FKS_NEWFILE	Memi[P2I($1+306)]	# YES, NO force newfile
define	FKS_CACHESIZE	Memi[P2I($1+307)]	# size of header cache
define	FKS_EXTNAME	Memc[P2C($1+308)]	# EXTNAME value

define	FITS_BYTE	8	# Bits in a FITS byte
define	FITS_SHORT	16	# Bits in a FITS short
define	FITS_LONG	32	# Bits in a FITS long
define	FITS_REAL	-32	# 32 Bits FITS IEEE float representation
define	FITS_DOUBLE	-64	# 64 Bits FITS IEEE double representation

define	COL_VALUE	11	# Starting column for parameter values
define	NDEC_REAL	7	# Precision of real
define	NDEC_DOUBLE	14	# Precision of double
define	LEN_OBJECT	63

define	FITS_LEN_BYTE	(((($1) + 2879)/2880)* 2880)
define	FITS_LEN_CHAR	(((($1) + 1439)/1440)* 1440)


# Mapping of FITS Keywords to IRAF image header.  All unrecognized keywords
# are stored here.

define	UNKNOWN	 Memc[($1+IMU-1)*SZ_STRUCT+1]
