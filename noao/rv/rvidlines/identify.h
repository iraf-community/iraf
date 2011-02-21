# Task parameters

define	LEN_IDSTRUCT	64		# Length ID structure

define	ID_TASK		Memi[$1]	# Task ID
define	ID_IMAGE	Memi[$1+1]	# Image (pointer)
define	ID_SECTION	Memi[$1+2]	# Section for 2D and 3D images (pointer)
define	ID_LINE		Memi[$1+$2+2]	# Image line or column [2]
define	ID_MAXLINE	Memi[$1+$2+4]	# Maximum line or column [2]
define	ID_AP		Memi[$1+$2+6]	# Aperture if appropriate [2]
define	ID_APS		Memi[$1+9]	# Array of apertures (pointer)
define	ID_NSUM		Memi[$1+$2+10]	# Number of lines to sum [2]
define	ID_MAXFEATURES	Memi[$1+13]	# Maximum number of features
define	ID_FTYPE	Memi[$1+14]	# Feature type
define	ID_MINSEP	Memr[P2R($1+15)] # Minimum pixel separation
define	ID_MATCH	Memr[P2R($1+16)] # Maximum matching separation
define	ID_FWIDTH	Memr[P2R($1+17)] # Feature width in pixels
define	ID_CRADIUS	Memr[P2R($1+18)] # Centering radius in pixels
define	ID_THRESHOLD	Memr[P2R($1+19)] # Centering threshold
define	ID_ZWIDTH	Memr[P2R($1+20)] # Zoom window width in fit units
define	ID_DATABASE	Memi[$1+21]	# Name of database (pointer)
define	ID_COORDLIST	Memi[$1+22]	# Name of coordinate list (pointer)
define	ID_LL		Memi[$1+23]	# Pointer to lines in coordinate list
define	ID_LABELS	Memi[$1+24]	# Type of feature labels
define	ID_LOGFILES	Memi[$1+25]	# List of logfiles

# Common image data

define	ID_SHIFT	Memd[P2D($1+26)]# Wavelength shift
define	ID_REDSHIFT	Memd[P2D($1+28)]# Redshift of spectrum
define	ID_RMSRED	Memd[P2D($1+30)]# Redshift of spectrum
define	ID_ZHELIO	Memd[P2D($1+32)]# Heliocentric correction in redshift
define	ID_IMDATA	Memi[$1+34]	# Image data (pointer)
define	ID_PIXDATA	Memi[$1+35]	# Pixel coordinates (pointer)
define	ID_FITDATA	Memi[$1+36]	# Fit coordinates (pointer)
define	ID_NPTS		Memi[$1+37]	# Number of points

# Features

define	ID_NFEATURES	Memi[$1+38]	# Number of features
define	ID_NALLOC	Memi[$1+39]	# Length of allocated feature arrays
define	ID_PIX		Memi[$1+40]	# Feature pixel coordinates (pointer)
define	ID_FIT		Memi[$1+41]	# Feature fit coordinates (pointer)
define	ID_USER		Memi[$1+42]	# Feature user coordinates (pointer)
define	ID_WTS		Memi[$1+43]	# Feature weights (pointer)
define	ID_FWIDTHS	Memi[$1+44]	# Feature width (pointer)
define	ID_FTYPES	Memi[$1+45]	# Feature type (pointer)
define	ID_LABEL	Memi[$1+46]	# Feature label (pointer)
define	ID_CURRENT	Memi[$1+47]	# Current feature

# Pointers for other packages and to save data

define	ID_SH		Memi[$1+48]	# SHDR pointer
define	ID_LP		Memi[$1+49]	# Logical to physical transformation
define	ID_PL		Memi[$1+50]	# Physical to logical transformation
define	ID_IC		Memi[$1+51]	# ICFIT pointer
define	ID_CV		Memi[$1+52]	# Curfit pointer
define	ID_GP		Memi[$1+53]	# GIO pointer
define	ID_GT		Memi[$1+54]	# Gtools pointer
define	ID_ID		Memi[$1+55]	# Array of structure pointers (pointer)
define	ID_NID		Memi[$1+56]	# Number of saved structure
define	ID_DT		Memi[$1+57]	# Database pointer

# Flags

define	ID_NEWFEATURES	Memi[$1+58]	# Has feature list changed?
define	ID_NEWCV	Memi[$1+59]	# Has fitting function changed?
define	ID_NEWGRAPH	Memi[$1+60]	# Has graph changed?
define	ID_NEWDBENTRY	Memi[$1+61]	# Has database entry changed?
define	ID_REFIT	Memi[$1+62]	# Refit feature data?
define	ID_GTYPE	Memi[$1+63]	# Graph type

# End of structure ----------------------------------------------------------

# Task ID
define	IDENTIFY	1		# Standard identify
define	RVIDLINES	2		# Line radial velocities

define	LABELS	"|none|index|pixel|coord|user|both|"
define	FTYPES	"|emission|absorption|gemission|gabsorption|"
define	EMISSION	1		# Emission feature (center1d)
define	ABSORPTION	2		# Absorption feature (center1d)
define	GEMISSION	3		# Emission feature (center1d)
define	GABSORPTION	4		# Absorption feature (center1d)

define	IMDATA		Memr[ID_IMDATA($1)+$2-1]
define	PIXDATA		Memd[ID_PIXDATA($1)+$2-1]
define	FITDATA		Memd[ID_FITDATA($1)+$2-1]

define	PIX		Memd[ID_PIX($1)+$2-1]
define	FIT		Memd[ID_FIT($1)+$2-1]
define	USER		Memd[ID_USER($1)+$2-1]
define	WTS		Memd[ID_WTS($1)+$2-1]
define	FWIDTH		Memr[ID_FWIDTHS($1)+$2-1]
define	FTYPE		Memi[ID_FTYPES($1)+$2-1]

define	VLIGHT	2.997925e5	# Speed of light, Km/sec
