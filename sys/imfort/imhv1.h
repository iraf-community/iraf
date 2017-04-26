# IMHV1.H -- Version 1 of the OIF binary file header (April 1988).

define	V1_MAGIC	"imhdr"			# file identification tag
define	V1_PMAGIC	"impix"			# file identification tag
define	V1_VERSION	1			# header version number

define	SZ_V1IMPIXFILE	79			# name of pixel storage file
define	SZ_V1IMHDRFILE	79			# name of header storage file
define	SZ_V1IMTITLE	79			# image title string
define	SZ_V1IMHIST	511			# image history record
define	SZ_V1BUNIT	9			# brightness units string
define	SZ_V1CTYPE	9			# coord axes units string

# The IMIO image header structure.

# Parameters.
define	LEN_V1IMHDR	513			# length of std header
define	LEN_V1PIXHDR	183			# length of pixel file header
define	V1U		LEN_V1IMHDR		# offset to user fields
define	IM_V1USERAREA	(P2C($1+V1U))		# user area (database)

# Disk resident header.
define	IM_V1MAGIC	Memi[$1]		# contains the string "imhdr"
define	IM_V1HDRLEN	Memi[$1+3]		# length of image header
define	IM_V1PIXTYPE	Memi[$1+4]		# datatype of the pixels
define	IM_V1NDIM	Memi[$1+5]		# number of dimensions
define	IM_V1LEN	Meml[$1+$2+6-1]		# length of the dimensions
define	IM_V1PHYSLEN	Meml[$1+$2+13-1]	# physical length (as stored)
define	IM_V1SSMTYPE	Meml[$1+20]		# type of subscript mapping
define	IM_V1LUTOFF	Meml[$1+21]		# offset to subscript map luts
define	IM_V1PIXOFF	Meml[$1+22]		# offset of the pixels
define	IM_V1HGMOFF	Meml[$1+23]		# offset of hgm pixels
define	IM_V1BLIST	Meml[$1+24]		# offset of bad pixel list
define	IM_V1SZBLIST	Meml[$1+25]		# size of bad pixel list
define	IM_V1NBPIX	Meml[$1+26]		# number of bad pixels
define	IM_V1CTIME	Meml[$1+27]		# time of image creation
define	IM_V1MTIME	Meml[$1+28]		# time of last modify
define	IM_V1LIMTIME	Meml[$1+29]		# time min,max computed
define	IM_V1MAX	Memr[P2R($1+30)]	# max pixel value
define	IM_V1MIN	Memr[P2R($1+31)]	# min pixel value
define	IM_V1HGM	($1+33)			# histogram descriptor
define	IM_V1CTRAN	($1+52)			# coordinate transformations
define	IM_V1PIXFILE	Memc[P2C($1+103)]	# name of pixel storage file
define	IM_V1HDRFILE	Memc[P2C($1+143)]	# name of header storage file
define	IM_V1TITLE	Memc[P2C($1+183)]	# image name string
define	IM_V1HISTORY	Memc[P2C($1+223)]	# history comment string

# The Histogram structure (field IM_HGM)
define	LEN_HGMSTRUCT	20
define	HGM_TIME	Meml[$1]		# time when hgm was computed
define	HGM_LEN		Meml[$1+1]		# number of bins in hgm
define	HGM_NPIX	Meml[$1+2]		# npix used to compute hgm
define	HGM_MIN		Memr[P2R($1+3)]		# min hgm value
define	HGM_MAX		Memr[P2R($1+4)]		# max hgm value
define	HGM_INTEGRAL	Memr[P2R($1+5)]		# integral of hgm
define	HGM_MEAN	Memr[P2R($1+6)]		# mean value
define	HGM_VARIANCE	Memr[P2R($1+7)]		# variance about mean
define	HGM_SKEWNESS	Memr[P2R($1+8)]		# skewness of hgm
define	HGM_MODE	Memr[P2R($1+9)]		# modal value of hgm
define	HGM_LCUT	Memr[P2R($1+10)]	# low cutoff value
define	HGM_HCUT	Memr[P2R($1+11)]	# high cutoff value
# next available field: ($1+12)

# The Coordinate Transformation Structure (IM_CTRAN)
define	LEN_CTSTRUCT	50
define	CT_VALID	Memi[$1]		# (y/n) is structure valid?
define	CT_BSCALE	Memr[P2R($1+1)]		# pixval scale factor
define	CT_BZERO	Memr[P2R($1+2)]		# pixval offset
define	CT_CRVAL	Memr[P2R($1+$2+3-1)]	# value at pixel
define	CT_CRPIX	Memr[P2R($1+$2+10-1)]	# index of pixel
define	CT_CDELT	Memr[P2R($1+$2+17-1)]	# increment along axis
define	CT_CROTA	Memr[P2R($1+$2+24-1)]	# rotation angle
define	CT_BUNIT	Memc[P2C($1+31)]	# pixval ("brightness") units
define	CT_CTYPE	Memc[P2C($1+36)]	# coord units string
# next available field: ($1+41)
