# Parameter data structure
 
define	EP_SZFNAME	99			# Length of file name
define	EP_SZLINE	199			# Length of line
define	EP_LEN		377			# Length of EP structure
 
define	EP_INPUT	Memc[P2C($1)]		# Input image name
define	EP_OUTPUT	Memc[P2C($1+50)]	# Output image name
define	EP_WORK		Memc[P2C($1+100)]	# Working image name
define	EP_SECTION	Memc[P2C($1+150)]	# Image section
define	EP_GRAPHICS	Memc[P2C($1+200)]	# Graphics device
define	EP_COMMAND	Memc[P2C($1+250)]	# Display command
 
define	EP_ANGH		Memr[$1+350]		# Horizontal viewing angle
define	EP_ANGV		Memr[$1+351]		# Vertical viewing angle
define	EP_APERTURE	Memi[$1+352]		# Aperture type
define	EP_AUTODISPLAY	Memi[$1+353]		# Automatic image display?
define	EP_AUTOSURFACE	Memi[$1+354]		# Automatic surface plots?
define	EP_BUFFER	Memr[$1+355]		# Background buffer width
define	EP_DEFAULT	Memi[$1+356]		# Default edit option
define	EP_DISPLAY	Memi[$1+357]		# Display images?
define	EP_FIXPIX	Memi[$1+358]		# Fixpix input?
define	EP_RADIUS	Memr[$1+359]		# Aperture radius
define	EP_SEARCH	Memr[$1+360]		# Search radius
define	EP_SIGMA	Memr[$1+361]		# Added noise sigma
define	EP_VALUE	Memr[$1+362]		# Substitution value
define	EP_WIDTH	Memr[$1+363]		# Background width
define	EP_XORDER	Memi[$1+364]		# Background xorder
define	EP_YORDER	Memi[$1+365]		# Background xorder
 
define	EP_LOGFD	Memi[$1+366]		# Log file descriptor
define	EP_IM		Memi[$1+367]		# IMIO pointer
define	EP_INDATA	Memi[$1+368]		# Input data pointer
define	EP_OUTDATA	Memi[$1+369]		# Output data pointer
define	EP_NX		Memi[$1+370]		# Number of columns in subraster
define	EP_NY		Memi[$1+371]		# Number of lines in subraster
define	EP_NPTS		Memi[$1+372]		# Number of pixels in subraster
define	EP_X1		Memi[$1+373]		# Starting column of subraster
define	EP_Y1		Memi[$1+374]		# Starting line of subraster
define	EP_X2		Memi[$1+375]		# Ending column of subraster
define	EP_Y2		Memi[$1+376]		# Ending line of subraster
 
define	APTYPES		"|circular|square|"	# Aperture types
define	APRECTANGLE	0			# Rectangular aperture
define	APCIRCULAR	1			# Circular aperture
define	APSQUARE	2			# Square aperture
define	APCDIAG		3			# Diagonal with column interp
define	APLDIAG		4			# Diagonal with column interp
