# Parameter data structure
 
define	EP_SZFNAME	99			# Length of file name
define	EP_SZLINE	199			# Length of line
define	EP_LEN		379			# Length of EP structure
 
define	EP_INPUT	Memc[P2C($1)]		# Input image name
define	EP_OUTPUT	Memc[P2C($1+50)]	# Output image name
define	EP_WORK		Memc[P2C($1+100)]	# Working image name
define	EP_SECTION	Memc[P2C($1+150)]	# Image section
define	EP_GRAPHICS	Memc[P2C($1+200)]	# Graphics device
define	EP_COMMAND	Memc[P2C($1+250)]	# Display command
 
define	EP_ANGH		Memr[P2R($1+350)]	# Horizontal viewing angle
define	EP_ANGV		Memr[P2R($1+351)]	# Vertical viewing angle
define	EP_APERTURE	Memi[$1+352]		# Aperture type
define	EP_AUTODISPLAY	Memi[$1+353]		# Automatic image display?
define	EP_AUTOSURFACE	Memi[$1+354]		# Automatic surface plots?
define	EP_BUFFER	Memr[P2R($1+355)]	# Background buffer width
define	EP_DEFAULT	Memi[$1+356]		# Default edit option
define	EP_DISPLAY	Memi[$1+357]		# Display images?
define	EP_FIXPIX	Memi[$1+358]		# Fixpix input?
define	EP_RADIUS	Memr[P2R($1+359)]	# Aperture radius
define	EP_SEARCH	Memr[P2R($1+360)]	# Search radius
define	EP_SIGMA	Memr[P2R($1+361)]	# Added noise sigma
define	EP_VALUE	Memr[P2R($1+362)]	# Substitution value
define	EP_MINVALUE	Memr[P2R($1+363)]	# Minimum value for edit
define	EP_MAXVALUE	Memr[P2R($1+364)]	# Maximum value for edit
define	EP_WIDTH	Memr[P2R($1+365)]	# Background width
define	EP_XORDER	Memi[$1+366]		# Background xorder
define	EP_YORDER	Memi[$1+367]		# Background xorder
 
define	EP_LOGFD	Memi[$1+368]		# Log file descriptor
define	EP_IM		Memi[$1+369]		# IMIO pointer
define	EP_INDATA	Memi[$1+370]		# Input data pointer
define	EP_OUTDATA	Memi[$1+371]		# Output data pointer
define	EP_NX		Memi[$1+372]		# Number of columns in subraster
define	EP_NY		Memi[$1+373]		# Number of lines in subraster
define	EP_NPTS		Memi[$1+374]		# Number of pixels in subraster
define	EP_X1		Memi[$1+375]		# Starting column of subraster
define	EP_Y1		Memi[$1+376]		# Starting line of subraster
define	EP_X2		Memi[$1+377]		# Ending column of subraster
define	EP_Y2		Memi[$1+378]		# Ending line of subraster
 
define	APTYPES		"|circular|square|"	# Aperture types
define	APRECTANGLE	0			# Rectangular aperture
define	APCIRCULAR	1			# Circular aperture
define	APSQUARE	2			# Square aperture
define	APCDIAG		3			# Diagonal with column interp
define	APLDIAG		4			# Diagonal with column interp
