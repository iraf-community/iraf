# IMHV2.H -- Version 2 of the OIF binary file header (March 1997).

define	V2_MAGIC	"imhv2"			# file identification tag
define	V2_PMAGIC	"impv2"			# file identification tag
define	V2_VERSION	2			# header version

define	SZ_V2IMPIXFILE	255			# name of pixel storage file
define	SZ_V2IMHDRFILE	255			# name of header storage file
define	SZ_V2IMTITLE	383			# image title string
define	SZ_V2IMHIST	1023			# image history record

# The IMIO image header structure.

# Parameters.
define	LEN_V2IMHDR	1024			# length of std header
define	LEN_V2PIXHDR	293			# length of pixel file header
define	V2U		LEN_V2IMHDR		# offset to user fields
define	IM_V2USERAREA	(P2C($1+V2U))		# user area (database)

# Disk resident header.
define	IM_V2MAGIC	Memi[$1]		# contains the string "imhdr"
define	IM_V2HDRLEN	Memi[$1+3]		# length of image header
define	IM_V2PIXTYPE	Memi[$1+4]		# datatype of the pixels
define	IM_V2SWAPPED	Memi[$1+5]		# pixels are byte swapped
define	IM_V2NDIM	Memi[$1+6]		# number of dimensions
define	IM_V2LEN	Meml[$1+$2+7-1]		# length of the dimensions
define	IM_V2PHYSLEN	Meml[$1+$2+14-1]	# physical length (as stored)
define	IM_V2SSMTYPE	Meml[$1+21]		# type of subscript mapping
define	IM_V2LUTOFF	Meml[$1+22]		# offset to subscript map luts
define	IM_V2PIXOFF	Meml[$1+23]		# offset of the pixels
define  IM_V2HGMOFF	Meml[$1+24]		# offset of hgm pixels
define  IM_V2BLIST	Meml[$1+25]		# offset of bad pixel list
define  IM_V2SZBLIST	Meml[$1+26]		# size of bad pixel list
define  IM_V2NBPIX	Meml[$1+27]		# number of bad pixels
define	IM_V2CTIME	Meml[$1+28]		# time of image creation
define	IM_V2MTIME	Meml[$1+29]		# time of last modify
define	IM_V2LIMTIME	Meml[$1+30]		# time min,max computed
define	IM_V2MAX	Memr[P2R($1+31)]	# max pixel value
define	IM_V2MIN	Memr[P2R($1+32)]	# min pixel value
define	IM_V2PIXFILE	Memc[P2C($1+37)]	# name of pixel storage file
define	IM_V2HDRFILE	Memc[P2C($1+165)]	# name of header storage file
define	IM_V2TITLE	Memc[P2C($1+293)]	# image name string
define	IM_V2HISTORY	Memc[P2C($1+485)]	# history comment string
