# Task parameters

define	LEN_IDSTRUCT	47		# Length ID structure

define	ID_IMAGE	Memi[$1]	# Image (pointer)
define	ID_SECTION	Memi[$1+1]	# Section for 2D images (pointer)
define	ID_TITLE	Memi[$1+2]	# Title (pointer)
define	ID_NSUM		Memi[$1+3]	# Number of lines to sum
define	ID_MAXFEATURES	Memi[$1+4]	# Maximum number of features
define	ID_FTYPE	Memi[$1+5]	# Feature type
define	ID_MINSEP	Memr[$1+6]	# Minimum pixel separation
define	ID_MATCH	Memr[$1+7]	# Maximum matching separation
define	ID_FWIDTH	Memr[$1+8]	# Feature width in pixels
define	ID_CRADIUS	Memr[$1+9]	# Centering radius in pixels
define	ID_THRESHOLD	Memr[$1+10]	# Centering threshold
define	ID_ZWIDTH	Memr[$1+11]	# Zoom window width in fit units
define	ID_DATABASE	Memi[$1+12]	# Name of database (pointer)
define	ID_COORDLIST	Memi[$1+13]	# Name of coordinate list (pointer)
define	ID_LABELS	Memi[$1+14]	# Type of feature labels
define	ID_LOGFILES	Memi[$1+15]	# List of logfiles

# Common image data

define	ID_CRPIX	Memd[P2D($1+16)]	# Coordinate reference pixel
define	ID_CRVAL	Memd[P2D($1+18)]	# Coordinate reference value
define	ID_CDELT	Memd[P2D($1+20)]	# Coordinate reference interval
define	ID_SHIFT	Memd[P2D($1+22)]	# Wavelength shift
define	ID_IMDATA	Memi[$1+24]	# Image data (pointer)
define	ID_PIXDATA	Memi[$1+25]	# Pixel coordinates (pointer)
define	ID_FITDATA	Memi[$1+26]	# Fit coordinates (pointer)
define	ID_NPTS		Memi[$1+27]	# Number of points

# Features

define	ID_NFEATURES	Memi[$1+28]	# Number of features
define	ID_NALLOC	Memi[$1+29]	# Length of allocated feature arrays
define	ID_PIX		Memi[$1+30]	# Feature pixel coordinates (pointer)
define	ID_FIT		Memi[$1+31]	# Feature fit coordinates (pointer)
define	ID_USER		Memi[$1+32]	# Feature user coordinates (pointer)
define	ID_WTS		Memi[$1+33]	# Feature weights (pointer)
define	ID_FWIDTHS	Memi[$1+34]	# Feature width (pointer)
define	ID_FTYPES	Memi[$1+35]	# Feature type (pointer)
define	ID_CURRENT	Memi[$1+36]	# Current feature

# Pointers for other packages

define	ID_IC		Memi[$1+37]	# ICFIT pointer
define	ID_CV		Memi[$1+38]	# Curfit pointer
define	ID_GP		Memi[$1+39]	# GIO pointer
define	ID_GT		Memi[$1+40]	# Gtools pointer

# Flags

define	ID_NEWFEATURES	Memi[$1+41]	# Has feature list changed?
define	ID_NEWCV	Memi[$1+42]	# Has fitting function changed?
define	ID_NEWGRAPH	Memi[$1+43]	# Has graph changed?
define	ID_NEWDBENTRY	Memi[$1+44]	# Has database entry changed?
define	ID_REFIT	Memi[$1+45]	# Refit feature data?
define	ID_GTYPE	Memi[$1+46]	# Graph type

# End of structure ----------------------------------------------------------

define	LABELS	"|none|index|pixel|user|"
define	FTYPES	"|emission|absorption|"

define	IMDATA		Memd[ID_IMDATA($1)+$2-1]
define	PIXDATA		Memd[ID_PIXDATA($1)+$2-1]
define	FITDATA		Memd[ID_FITDATA($1)+$2-1]

define	PIX		Memd[ID_PIX($1)+$2-1]
define	FIT		Memd[ID_FIT($1)+$2-1]
define	USER		Memd[ID_USER($1)+$2-1]
define	WTS		Memd[ID_WTS($1)+$2-1]
define	FWIDTH		Memr[ID_FWIDTHS($1)+$2-1]
define	FTYPE		Memi[ID_FTYPES($1)+$2-1]
