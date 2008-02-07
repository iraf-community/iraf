# Task parameters

define	LEN_EC		52		# Length ID structure

define	EC_IMAGE	Memp[$1]	# Image name (pointer)
define	EC_MAXFEATURES	Memi[P2I($1+1)]	# Maximum number of features
define	EC_FTYPE	Memi[P2I($1+2)]	# Feature type
define	EC_MINSEP	Memr[P2R($1+3)]	# Minimum pixel separation
define	EC_MATCH	Memr[P2R($1+4)]	# Maximum matching separation
define	EC_FWIDTH	Memr[P2R($1+5)]	# Feature width in pixels
define	EC_CRADIUS	Memr[P2R($1+6)]	# Centering radius in pixels
define	EC_THRESHOLD	Memr[P2R($1+7)]	# Centering threshold
define	EC_ZWIDTH	Memr[P2R($1+8)]	# Zoom window width in fit units
define	EC_DATABASE	Memp[$1+9]	# Name of database (pointer)
define	EC_COORDLIST	Memp[$1+10]	# Name of coordinate list (pointer)
define	EC_LABELS	Memi[P2I($1+11)]	# Type of feature labels
define	EC_LOGFILES	Memp[$1+12]	# List of logfiles

# Common image data

define	EC_NCOLS	Memi[P2I($1+13)]	# Number of columns
define	EC_NLINES	Memi[P2I($1+14)]	# Number of lines/apertures/orders
define	EC_SHS		Memp[$1+15]	# Pointer to SHDR pointers
define	EC_PIXDATA	Memp[$1+16]	# Pixel coordinates (pointer)
define	EC_FITDATA	Memp[$1+17]	# Fit coordinates (pointer)

define	EC_IMLINE	Memp[$1+18]	# Image data (pointer)
define	EC_PIXLINE	Memp[$1+19]	# Pixel coordinates (pointer)
define	EC_FITLINE	Memp[$1+20]	# Fit coordinates (pointer)
define	EC_NPTS		Memi[P2I($1+21)]	# Number of points

define	EC_SHIFT	Memd[P2D($1+22)]# Wavelength shift

# Features

define	EC_NFEATURES	Memi[P2I($1+24)]	# Number of features
define	EC_NALLOC	Memi[P2I($1+25)]	# Length of allocated feature arrays
define	EC_APNUM	Memp[$1+26]	# Aperture number (pointer)
define	EC_LINENUM	Memp[$1+27]	# Image line number (pointer)
define	EC_ORD		Memp[$1+28]	# Feature order number (pointer)
define	EC_PIX		Memp[$1+29]	# Feature pixel coordinates (pointer)
define	EC_FIT		Memp[$1+30]	# Feature fit coordinates (pointer)
define	EC_USER		Memp[$1+31]	# Feature user coordinates (pointer)
define	EC_FWIDTHS	Memp[$1+32]	# Feature width (pointer)
define	EC_FTYPES	Memp[$1+33]	# Feature type (pointer)

# Current status

define	EC_CURRENT	Memi[P2I($1+34)]	# Current feature
define	EC_SH		Memp[$1+35]	# Current SHDR pointer
define	EC_AP		Memi[P2I($1+36)]	# Current aperture
define	EC_LINE		Memi[P2I($1+37)]	# Current line
define	EC_ORDER	Memi[P2I($1+38)]	# Current order

# Pointers for other packages

define	EC_LP		Memp[$1+39]	# Logical to physical transformation
define	EC_PL		Memp[$1+40]	# Physical to logical transformation
define	EC_LL		Memp[$1+41]	# Linelist pointer
define	EC_ECF		Memp[$1+42]	# Curfit pointer
define	EC_GP		Memp[$1+43]	# GIO pointer
define	EC_GT		Memp[$1+44]	# Gtools pointer
define	EC_UN		Memp[$1+45]	# Units pointer

# Flags

define	EC_NEWFEATURES	Memi[P2I($1+46)]	# Has feature list changed?
define	EC_NEWECF	Memi[P2I($1+47)]	# Has fitting function changed?
define	EC_NEWGRAPH	Memi[P2I($1+48)]	# Has graph changed?
define	EC_NEWDBENTRY	Memi[P2I($1+49)]	# Has database entry changed?
define	EC_REFIT	Memi[P2I($1+50)]	# Refit feature data?
define	EC_GTYPE	Memi[P2I($1+51)]	# Graph type

# End of structure ----------------------------------------------------------

define	LABELS	"|none|index|pixel|user|"
define	FTYPES	"|emission|absorption|"

define	IMDATA		Memr[EC_IMLINE($1)+$2-1]
define	PIXDATA		Memd[EC_PIXLINE($1)+$2-1]
define	FITDATA		Memd[EC_FITLINE($1)+$2-1]

define	SH		Memp[EC_SHS($1)+$2-1]
define	APS		AP(SH($1,$2))
define	ORDERS		BEAM(SH($1,$2))

define	APN		Memi[EC_APNUM($1)+$2-1]
define	LINE		Memi[EC_LINENUM($1)+$2-1]
define	ORDER		Memi[EC_ORD($1)+$2-1]
define	PIX		Memd[EC_PIX($1)+$2-1]
define	FIT		Memd[EC_FIT($1)+$2-1]
define	USER		Memd[EC_USER($1)+$2-1]
define	FWIDTH		Memr[EC_FWIDTHS($1)+$2-1]
define	FTYPE		Memi[EC_FTYPES($1)+$2-1]
