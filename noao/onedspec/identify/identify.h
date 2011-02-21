# Task parameters

define	ID_LENSTRING	99		# Length of strings in ID structure
define	ID_LENSTRUCT	354		# Length ID structure

define	ID_IMAGE	Memc[P2C($1)]	  # Image
define	ID_SECTION	Memc[P2C($1+50)]  # Section for 2D and 3D images
define	ID_DATABASE	Memc[P2C($1+100)] # Name of database
define	ID_COORDLIST	Memc[P2C($1+150)] # Name of coordinate list
define	ID_COORDSPEC	Memc[P2C($1+200)] # Name of coordinate spectrum
define	ID_SAVEID	Memc[P2C($1+250)] # ID for save structure
define	ID_LINE		Memi[$1+$2+299]	  # Image line or column [2]
define	ID_MAXLINE	Memi[$1+$2+301]	  # Maximum line or column [2]
define	ID_AP		Memi[$1+$2+303]	  # Aperture if appropriate [2]
define	ID_APS		Memi[$1+306]	  # Array of apertures (pointer)
define	ID_NSUM		Memi[$1+$2+306]	  # Number of lines to sum [2]
define	ID_MAXFEATURES	Memi[$1+309]	  # Maximum number of features
define	ID_FTYPE	Memi[$1+310]	  # Feature type
define	ID_MINSEP	Memr[P2R($1+311)] # Minimum pixel separation
define	ID_MATCH	Memr[P2R($1+312)] # Maximum matching separation
define	ID_FWIDTH	Memr[P2R($1+313)] # Feature width in pixels
define	ID_CRADIUS	Memr[P2R($1+314)] # Centering radius in pixels
define	ID_THRESHOLD	Memr[P2R($1+315)] # Centering threshold
define	ID_ZWIDTH	Memr[P2R($1+316)] # Zoom window width in fit units
define	ID_LL		Memi[$1+317]	  # Pointer to coordinate list lines
define	ID_LLL		Memi[$1+318]	  # Pointer to coordinate list labels
define	ID_NLL		Memi[$1+319]	  # Number of coordinate list lines
define	ID_LABELS	Memi[$1+320]	  # Type of feature labels
define	ID_LOGFILES	Memi[$1+321]	  # List of logfiles

# Common image data

define	ID_SHIFT	Memd[P2D($1+322)]# Wavelength shift
define	ID_IMDATA	Memi[$1+324]	# Image data (pointer)
define	ID_PIXDATA	Memi[$1+325]	# Pixel coordinates (pointer)
define	ID_FITDATA	Memi[$1+326]	# Fit coordinates (pointer)
define	ID_NPTS		Memi[$1+327]	# Number of points

# Features

define	ID_NFEATURES	Memi[$1+328]	# Number of features
define	ID_NALLOC	Memi[$1+329]	# Length of allocated feature arrays
define	ID_PIX		Memi[$1+330]	# Feature pixel coordinates (pointer)
define	ID_FIT		Memi[$1+331]	# Feature fit coordinates (pointer)
define	ID_USER		Memi[$1+332]	# Feature user coordinates (pointer)
define	ID_WTS		Memi[$1+333]	# Feature weights (pointer)
define	ID_FWIDTHS	Memi[$1+334]	# Feature width (pointer)
define	ID_FTYPES	Memi[$1+335]	# Feature type (pointer)
define	ID_LABEL	Memi[$1+336]	# Feature label (pointer)
define	ID_CURRENT	Memi[$1+337]	# Current feature

# Pointers for other packages and to save data

define	ID_SH		Memi[$1+338]	# SHDR pointer
define	ID_LP		Memi[$1+339]	# Logical to physical transformation
define	ID_PL		Memi[$1+340]	# Physical to logical transformation
define	ID_IC		Memi[$1+341]	# ICFIT pointer
define	ID_CV		Memi[$1+342]	# Curfit pointer
define	ID_GP		Memi[$1+343]	# GIO pointer
define	ID_GT		Memi[$1+344]	# Gtools pointer
define	ID_STP		Memi[$1+345]	# Symbol table of saved data
define	ID_DT		Memi[$1+346]	# Database pointer
define	ID_UN		Memi[$1+347]	# Units pointer

# Flags

define	ID_NEWFEATURES	Memi[$1+348]	# Has feature list changed?
define	ID_NEWCV	Memi[$1+349]	# Has fitting function changed?
define	ID_NEWGRAPH	Memi[$1+350]	# Has graph changed?
define	ID_NEWDBENTRY	Memi[$1+351]	# Has database entry changed?
define	ID_REFIT	Memi[$1+352]	# Refit feature data?
define	ID_GTYPE	Memi[$1+353]	# Graph type

# End of structure ----------------------------------------------------------

define	LABELS	"|none|index|pixel|coord|user|both|"
define	FTYPES	"|emission|absorption|"
define	EMISSION	1		# Emission feature
define	ABSORPTION	2		# Absorption feature

define	IMDATA		Memr[ID_IMDATA($1)+$2-1]
define	PIXDATA		Memd[ID_PIXDATA($1)+$2-1]
define	FITDATA		Memd[ID_FITDATA($1)+$2-1]

define	PIX		Memd[ID_PIX($1)+$2-1]
define	FIT		Memd[ID_FIT($1)+$2-1]
define	USER		Memd[ID_USER($1)+$2-1]
define	WTS		Memd[ID_WTS($1)+$2-1]
define	FWIDTH		Memr[ID_FWIDTHS($1)+$2-1]
define	FTYPE		Memi[ID_FTYPES($1)+$2-1]
