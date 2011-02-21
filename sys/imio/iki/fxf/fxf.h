# FITS.H -- IKI/FITS internal definitions.

define	FITS_ORIGIN	"NOAO-IRAF FITS Image Kernel July 2003"

define	FITS_LENEXTN	4		# max length imagefile extension
define	SZ_DATATYPE	16		# size of datatype string (eg "REAL*4")
define	SZ_EXTTYPE	20		# size of exttype string (eg BINTABLE)
define	SZ_KEYWORD	8		# size of a FITS keyword
define	SZ_EXTRASPACE	(81*32)		# extra space for new cards in header
define	DEF_PHULINES	0		# initial allocation for PHU
define	DEF_EHULINES	0		# initial allocation for EHU
define	DEF_PADLINES	0		# initial value for extra lines in HU
define	DEF_PLMAXLEN	32768		# default max PLIO encoded line length
define	DEF_PLDEPTH	0		# default PLIO mask depth

define	FITS_BLOCK_BYTES 2880		# FITS logical block length (bytes)
define	FITS_BLOCK_CHARS 1440		# FITS logical block length (spp chars)
define	FITS_STARTVALUE 10		# first column of value field
define	FITS_ENDVALUE	30		# last	column of value field
define	FITS_SZVALSTR	21		# nchars in value string
define	LEN_CARD	80		# length of FITS card.
define	LEN_UACARD	81		# size of a Userarea line.
define	LEN_OBJECT	63		# maximum length of a FITS string value
define	LEN_FORMAT	40		# maximum length of a TFORM value
define	NO_KEYW		-1		# indicates no keyword is present.

define	MAX_OFFSETS	100		# max number of offsets per cache entry.
define	MAX_CACHE	60		# max number of cache entries.
define	DEF_CACHE	10		# default number of cache entries.

define	DEF_HDREXTN	"fits"		# default header file extension
define	ENV_FKINIT	"fkinit"	# FITS kernel initialization

define	DEF_ISOCUTOVER	0		# date when ISO format dates kick in
define	ENV_ISOCUTOVER	"isodates"	# environment override for default

define	FITS_BYTE	8	# Bits in a FITS byte
define	FITS_SHORT	16	# Bits in a FITS short
define	FITS_LONG	32	# Bits in a FITS long
define	FITS_REAL	-32	# 32 Bits FITS IEEE float representation
define	FITS_DOUBLE	-64	# 64 Bits FITS IEEE double representation

define	COL_VALUE	11	# Starting column for parameter values
define	NDEC_REAL	7	# Precision of real
define	NDEC_DOUBLE	14	# Precision of double

define	FITS_LEN_CHAR	(((($1) + 1439)/1440)* 1440)

# Extension subtypes.
define	FK_PLIO		1

# Mapping of FITS Keywords to IRAF image header.  All unrecognized keywords
# are stored here.

#define	UNKNOWN	 Memc[($1+IMU-1)*SZ_MII_INT+1]
define	UNKNOWN	 Memc[($1+IMU-1)*SZ_STRUCT+1]


# FITS image descriptor, used internally by the FITS kernel.  The required
# header parameters are maintained in this descriptor, everything else is
# simply copied into the user area of the IMIO descriptor.

define	LEN_FITDES	500
define	LEN_FITBASE	400

define	FIT_ACMODE	Memi[$1]	# image access mode
define	FIT_PFD		Memi[$1+1]	# pixel file descriptor
define	FIT_PIXOFF	Memi[$1+2]	# pixel offset
define	FIT_TOTPIX	Memi[$1+3]	# size of image in pixfile, chars
define	FIT_IO		Memi[$1+4]	# FITS I/O channel
define	FIT_ZCNV	Memi[$1+5]	# set if on-the-fly conversion needed
define	FIT_IOSTAT	Memi[$1+6]	# i/o status for zfio routines
define  FIT_TFORMP      Memi[$1+7]      # TFORM keyword value pointer
define  FIT_TTYPEP      Memi[$1+8]      # TTYPE keyword value pointer
define  FIT_TFIELDS     Memi[$1+9]      # number of fields in binary table
define  FIT_PCOUNT      Memi[$1+10]     # PCOUNT keyword value
			# extra space
define	FIT_BSCALE	Memd[P2D($1+16)]
define	FIT_BZERO	Memd[P2D($1+18)]
define	FIT_BITPIX	Memi[$1+20]	# bits per pixel
define	FIT_NAXIS	Memi[$1+21]	# number of axes in image
define	FIT_LENAXIS	Memi[$1+22+$2-1]# 35:41 = [7] max
define	FIT_ZBYTES	Memi[$1+30]	# Status value for FIT_ZCNV mode
define	FIT_HFD		Memi[$1+31]	# Header file descriptor
define	FIT_PIXTYPE	Memi[$1+32]
define	FIT_CACHEHDR	Memi[$1+33]	# Cached main header unit's address.
define	FIT_CACHEHLEN	Memi[$1+34]	# Lenght of the above.
define	FIT_IM		Memi[$1+35]	# Has the 'im' descriptor value 
define	FIT_GROUP	Memi[$1+36]
define	FIT_NEWIMAGE	Memi[$1+37]	# Newimage flag
define	FIT_HDRPTR	Memi[$1+38]	# Header data Xtension pointer
define	FIT_PIXPTR	Memi[$1+39]	# Pixel data Xtension pointer
define	FIT_NUMOFFS	Memi[$1+40]	# Number of offsets in cache header.
define	FIT_EOFSIZE	Memi[$1+41]	# Size in char of file before append.
define	FIT_XTENSION	Memi[$1+42]	# Yes, if an Xtension has been read.
define	FIT_INHERIT	Memi[$1+43]	# INHERIT header keyword value.
define	FIT_EXTVER	Memi[$1+44]	# EXTVER value (integer only)
define	FIT_EXPAND	Memi[$1+45]	# Expand the header?
define	FIT_MIN		Memr[P2R($1+46)]# Minimum pixel value
define	FIT_MAX		Memr[P2R($1+47)]# Maximum pixel value
define	FIT_MTIME	Meml[$1+48]	# Time of last mod. for FITS unit 
define	FIT_SVNANR	Memr[P2R($1+49)]
define	FIT_SVNAND	Memd[P2D($1+50)]
define	FIT_SVMAPRIN	Memi[$1+52]
define	FIT_SVMAPROUT	Memi[$1+53]
define	FIT_SVMAPDIN	Memi[$1+54]
define	FIT_SVMAPDOUT	Memi[$1+55]
define	FIT_EXTEND	Memi[$1+56]	# FITS extend keyword
define	FIT_PLMAXLEN	Memi[$1+57]	# PLIO maximum linelen
			# extra space
define	FIT_EXTTYPE	Memc[P2C($1+70)]  # extension type
define	FIT_FILENAME	Memc[P2C($1+110)] # FILENAME value 
define	FIT_EXTNAME	Memc[P2C($1+150)] # EXTNAME value 
define	FIT_DATATYPE	Memc[P2C($1+190)] # datatype string
define	FIT_TITLE	Memc[P2C($1+230)] # title string
define	FIT_OBJECT	Memc[P2C($1+270)] # object string
define  FIT_EXTSTYPE    Memc[P2C($1+310)] # FITS extension subtype
			# extra space

# The FKS terms carry the fkinit or kernel section arguments.
define	FKS_APPEND	Memi[$1+400]	# YES, NO append an extension
define	FKS_INHERIT	Memi[$1+401]	# YES, NO inherit the main header
define	FKS_OVERWRITE	Memi[$1+402]	# YES, NO overwrite an extension
define	FKS_DUPNAME	Memi[$1+403]	# YES, NO allow duplicated EXTNAME
define	FKS_EXTVER	Memi[$1+404]	# YES, NO allow duplicated EXTNAME
define	FKS_EXPAND	Memi[$1+405]	# YES, NO expand the header
define	FKS_PHULINES	Memi[$1+406]	# Allocated lines in PHU
define	FKS_EHULINES	Memi[$1+407]	# Allocated lines in EHU
define	FKS_PADLINES	Memi[$1+408]	# Additional lines for HU
define	FKS_NEWFILE	Memi[$1+409]	# YES, NO force newfile
define	FKS_CACHESIZE	Memi[$1+410]	# size of header cache
define  FKS_SUBTYPE     Memi[$1+411]	# BINTABLE subtype
define	FKS_EXTNAME	Memc[P2C($1+412)] # EXTNAME value
			# extra space


# Reserved FITS keywords known to this code.

define	FK_KEYWORDS "|bitpix|datatype|end|naxis|naxisn|simple|bscale|bzero\
|origin|iraf-tlm|filename|extend|irafname|irafmax|irafmin|datamax\
|datamin|xtension|object|pcount|extname|extver|nextend|inherit\
|zcmptype|tform|ttype|tfields|date|"

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
define  KW_ZCMPTYPE     25
define  KW_TFORM        26
define  KW_TTYPE        27
define  KW_TFIELDS      28
define  KW_DATE         29
