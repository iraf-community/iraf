# Task parameters

define	LEN_EC		53		# Length ID structure

define	EC_IMAGE	Memi[$1]	# Image (pointer)
define	EC_TITLE	Memi[$1+1]	# Title (pointer)
define	EC_MAXFEATURES	Memi[$1+2]	# Maximum number of features
define	EC_FTYPE	Memi[$1+3]	# Feature type
define	EC_MINSEP	Memr[$1+4]	# Minimum pixel separation
define	EC_MATCH	Memr[$1+5]	# Maximum matching separation
define	EC_FWIDTH	Memr[$1+6]	# Feature width in pixels
define	EC_CRADIUS	Memr[$1+7]	# Centering radius in pixels
define	EC_THRESHOLD	Memr[$1+8]	# Centering threshold
define	EC_ZWIDTH	Memr[$1+9]	# Zoom window width in fit units
define	EC_DATABASE	Memi[$1+10]	# Name of database (pointer)
define	EC_COORDLIST	Memi[$1+11]	# Name of coordinate list (pointer)
define	EC_LABELS	Memi[$1+12]	# Type of feature labels
define	EC_LOGFILES	Memi[$1+13]	# List of logfiles

# Common image data

define	EC_SHIFT	Memd[P2D($1+14)]# Wavelength shift
define	EC_IMDATA	Memi[$1+16]	# Image data (pointer)
define	EC_PIXDATA	Memi[$1+17]	# Pixel coordinates (pointer)
define	EC_FITDATA	Memi[$1+18]	# Fit coordinates (pointer)
define	EC_NPTS		Memi[$1+19]	# Number of points
define	EC_IMLINE	Memi[$1+20]	# Image data (pointer)
define	EC_PIXLINE	Memi[$1+21]	# Pixel coordinates (pointer)
define	EC_FITLINE	Memi[$1+22]	# Fit coordinates (pointer)
define	EC_NPTSLINE	Memi[$1+23]	# Number of points
define	EC_NLINES	Memi[$1+24]	# Number of lines/apertures/orders
define	EC_APS		Memi[$1+25]	# Pointer to aperture numbers
define	EC_ORDERS	Memi[$1+52]	# Pointer to orders
define	EC_CRVAL	Memi[$1+50]	# Pointer to crvals
define	EC_CDELT	Memi[$1+51]	# Pointer to cdelts

# Features

define	EC_NFEATURES	Memi[$1+26]	# Number of features
define	EC_NALLOC	Memi[$1+27]	# Length of allocated feature arrays
define	EC_APNUM	Memi[$1+28]	# Aperture number (pointer)
define	EC_LINENUM	Memi[$1+29]	# Image line number (pointer)
define	EC_ORD		Memi[$1+30]	# Feature order number (pointer)
define	EC_PIX		Memi[$1+31]	# Feature pixel coordinates (pointer)
define	EC_FIT		Memi[$1+32]	# Feature fit coordinates (pointer)
define	EC_USER		Memi[$1+33]	# Feature user coordinates (pointer)
define	EC_FWIDTHS	Memi[$1+34]	# Feature width (pointer)
define	EC_FTYPES	Memi[$1+35]	# Feature type (pointer)

# Current status

define	EC_CURRENT	Memi[$1+36]	# Current feature
define	EC_AP		Memi[$1+37]	# Current aperture
define	EC_LINE		Memi[$1+38]	# Current line
define	EC_ORDER	Memi[$1+39]	# Current order

# Pointers for other packages

define	EC_LL		Memi[$1+40]	# Linelist pointer
define	EC_ECF		Memi[$1+41]	# Curfit pointer
define	EC_GP		Memi[$1+42]	# GIO pointer
define	EC_GT		Memi[$1+43]	# Gtools pointer

# Flags

define	EC_NEWFEATURES	Memi[$1+44]	# Has feature list changed?
define	EC_NEWECF	Memi[$1+45]	# Has fitting function changed?
define	EC_NEWGRAPH	Memi[$1+46]	# Has graph changed?
define	EC_NEWDBENTRY	Memi[$1+47]	# Has database entry changed?
define	EC_REFIT	Memi[$1+48]	# Refit feature data?
define	EC_GTYPE	Memi[$1+49]	# Graph type

# End of structure ----------------------------------------------------------

define	LABELS	"|none|index|pixel|user|"
define	FTYPES	"|emission|absorption|"

define	IMDATA		Memd[EC_IMLINE($1)+$2-1]
define	PIXDATA		Memd[EC_PIXLINE($1)+$2-1]
define	FITDATA		Memd[EC_FITLINE($1)+$2-1]

define	APS		Memi[EC_APS($1)+$2-1]
define	ORDERS		Memi[EC_ORDERS($1)+$2-1]
define	CRVAL		Memd[EC_CRVAL($1)+$2-1]
define	CDELT		Memd[EC_CDELT($1)+$2-1]

define	AP		Memi[EC_APNUM($1)+$2-1]
define	LINE		Memi[EC_LINENUM($1)+$2-1]
define	ORDER		Memi[EC_ORD($1)+$2-1]
define	PIX		Memd[EC_PIX($1)+$2-1]
define	FIT		Memd[EC_FIT($1)+$2-1]
define	USER		Memd[EC_USER($1)+$2-1]
define	FWIDTH		Memr[EC_FWIDTHS($1)+$2-1]
define	FTYPE		Memi[EC_FTYPES($1)+$2-1]
