# FITS.H -- IKI/FITS internal definitions.

define	FITS_ORIGIN	"NOAO-IRAF FITS Image Kernel Aug 1 1997"

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

# FITS image descriptor, used internally by the FITS kernel.  The required
# header parameters are maintained in this descriptor, everything else is
# simply copied into the user area of the IMIO descriptor.

define	LEN_FITDES	338
define	LEN_FITBASE	287

define	FIT_ACMODE	Memi[$1]	# image access mode
define	FIT_PFD		Memi[$1+1]	# pixel file descriptor
define	FIT_PIXOFF	Memi[$1+2]	# pixel offset
define	FIT_TOTPIX	Memi[$1+3]	# size of image in pixfile, chars
define	FIT_IO		Memi[$1+4]	# FITS I/O channel
define	FIRST_TIMER	Memi[$1+5]	# First time FITZRD is called?
define	FIT_BSCALE	Memd[P2D($1+6)]
define	FIT_BZERO	Memd[P2D($1+8)]
define	FIT_BITPIX	Memi[$1+10]	# bits per pixel
define	FIT_NAXIS	Memi[$1+11]	# number of axes in image
define	FIT_LENAXIS	Memi[$1+12+$2-1]# 35:41 = [7] max
define	FIRST_TIMEW	Memi[$1+20]	# First time FITZWR is called?
define	FIT_HFD		Memi[$1+21]	# Header file descriptor
define	FIT_PIXTYPE	Memi[$1+22]
define	FIT_CACHEHDR	Memi[$1+23]	# Cached main header unit's address.
define	FIT_CACHEHLEN	Memi[$1+24]	# Lenght of the above.
define	FIT_IM		Memi[$1+25]	# Has the 'im' descriptor value 
define	FIT_GROUP	Memi[$1+26]
define	FIT_NEWIMAGE	Memi[$1+27]	# Newimage flag
define	FIT_HDRPTR	Memi[$1+28]	# Header data Xtension pointer
define	FIT_PIXPTR	Memi[$1+29]	# Pixel data Xtension pointer
define	FIT_NUMOFFS	Memi[$1+30]	# Number of offsets in cache header.
define	FIT_EOFSIZE	Memi[$1+31]	# Size in char of file before append.
define	FIT_XTENSION	Memi[$1+32]	# Yes, if an Xtension has been read.
define	FIT_INHERIT	Memi[$1+33]	# INHERIT header keyword value.
define	FIT_EXTVER	Memi[$1+34]	# EXTVER value (integer only)
define	FIT_EXPAND	Memi[$1+35]	# Expand the header?
define	FIT_MIN		Memr[$1+36]	# Minimum pixel value
define	FIT_MAX		Memr[$1+37]	# Maximum pixel value
define	FIT_MTIME	Meml[$1+38]	# Time of last mod. for FITS unit 
define	FIT_SVNANR	Memr[$1+39]
define	FIT_SVNAND	Memd[P2D($1+40)]
define	FIT_SVMAPRIN	Memi[$1+42]
define	FIT_SVMAPROUT	Memi[$1+43]
define	FIT_SVMAPDIN	Memi[$1+44]
define	FIT_SVMAPDOUT	Memi[$1+45]
define	FIT_EXTEND	Memi[$1+46]	  # FITS extend keyword
define	FIT_EXTTYPE	Memc[P2C($1+47)]  # extension type
define	FIT_FILENAME	Memc[P2C($1+87)]  # FILENAME value 
define	FIT_EXTNAME	Memc[P2C($1+127)] # EXTNAME value 
define	FIT_DATATYPE	Memc[P2C($1+167)] # datatype string
define	FIT_TITLE	Memc[P2C($1+207)] # datatype string
define	FIT_OBJECT	Memc[P2C($1+247)] # datatype string

# The FKS terms carry the fkinit or kernel section arguments.
define	FKS_APPEND	Memi[$1+287]	 # YES, NO append an extension
define	FKS_INHERIT	Memi[$1+288]	 # YES, NO inherit the main header
define	FKS_OVERWRITE	Memi[$1+289]	 # YES, NO overwrite an extension
define	FKS_DUPNAME	Memi[$1+290]	 # YES, NO allow duplicated EXTNAME
define	FKS_EXTVER	Memi[$1+291]	 # YES, NO allow duplicated EXTNAME
define	FKS_EXPAND	Memi[$1+292]	 # YES, NO expand the header
define	FKS_PHULINES	Memi[$1+293]	 # Allocated lines in PHU
define	FKS_EHULINES	Memi[$1+294]	 # Allocated lines in EHU
define	FKS_PADLINES	Memi[$1+295]	 # Additional lines for HU
define	FKS_NEWFILE	Memi[$1+296]	 # YES, NO force newfile
define	FKS_CACHESIZE	Memi[$1+297]	 # size of header cache
define	FKS_EXTNAME	Memc[P2C($1+298)]     # EXTNAME value


# Reserved FITS keywords known to this code.

define	KW_BITPIX	1
define	KW_DATATYPE	2
define	KW_END		3
define	KW_NAXIS	4
define	KW_NAXISN	5
define	KW_SIMPLE	6
define	KW_BSCALE	7
define	KW_BZERO	8
define	KW_ORIGIN	9
define	KW_IRAFTLM	10
define	KW_FILENAME	11
define	KW_EXTEND	12
define	KW_IRAFNAME	13
define	KW_IRAFMAX	14
define	KW_IRAFMIN	15
define	KW_DATAMAX	16
define	KW_DATAMIN	17
define	KW_XTENSION	18
define	KW_OBJECT	19
define	KW_PCOUNT	20
define	KW_EXTNAME	21
define	KW_EXTVER	22
define	KW_NEXTEND	23
define	KW_INHERIT	24

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
