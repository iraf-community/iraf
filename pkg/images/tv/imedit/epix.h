# Parameter data structure
 
define	EP_SZFNAME	99			# Length of file name
define	EP_SZLINE	199			# Length of line
define	EP_LEN		327			# Length of EP structure
 
define	EP_INPUT	Memc[P2C($1)]		# Input image name
define	EP_OUTPUT	Memc[P2C($1+50)]	# Output image name
define	EP_SECTION	Memc[P2C($1+100)]	# Image section
define	EP_GRAPHICS	Memc[P2C($1+150)]	# Graphics device
define	EP_COMMAND	Memc[P2C($1+200)]	# Display command
 
define	EP_ANGH		Memr[$1+300]		# Horizontal viewing angle
define	EP_ANGV		Memr[$1+301]		# Vertical viewing angle
define	EP_APERTURE	Memi[$1+302]		# Aperture type
define	EP_AUTODISPLAY	Memi[$1+303]		# Automatic image display?
define	EP_AUTOSURFACE	Memi[$1+304]		# Automatic surface plots?
define	EP_BUFFER	Memr[$1+305]		# Background buffer width
define	EP_DEFAULT	Memi[$1+306]		# Default edit option
define	EP_DISPLAY	Memi[$1+307]		# Display images?
define	EP_FIXPIX	Memi[$1+308]		# Fixpix input?
define	EP_RADIUS	Memr[$1+309]		# Aperture radius
define	EP_SEARCH	Memr[$1+310]		# Search radius
define	EP_SIGMA	Memr[$1+311]		# Added noise sigma
define	EP_VALUE	Memr[$1+312]		# Substitution value
define	EP_WIDTH	Memr[$1+313]		# Background width
define	EP_XORDER	Memi[$1+314]		# Background xorder
define	EP_YORDER	Memi[$1+315]		# Background xorder
 
define	EP_LOGFD	Memi[$1+316]		# Log file descriptor
define	EP_IM		Memi[$1+317]		# IMIO pointer
define	EP_INDATA	Memi[$1+318]		# Input data pointer
define	EP_OUTDATA	Memi[$1+319]		# Output data pointer
define	EP_NX		Memi[$1+320]		# Number of columns in subraster
define	EP_NY		Memi[$1+321]		# Number of lines in subraster
define	EP_NPTS		Memi[$1+322]		# Number of pixels in subraster
define	EP_X1		Memi[$1+323]		# Starting column of subraster
define	EP_Y1		Memi[$1+324]		# Starting line of subraster
define	EP_X2		Memi[$1+325]		# Ending column of subraster
define	EP_Y2		Memi[$1+326]		# Ending line of subraster
 
define	EPIXBUF		"epixbuf"		# EPIX buffer image
 
define	APTYPES		"|circular|square|"	# Aperture types
define	APRECTANGLE	0			# Rectangular aperture
define	APCIRCULAR	1			# Circular aperture
define	APSQUARE	2			# Square aperture
define	APCDIAG		3			# Diagonal with column interp
define	APLDIAG		4			# Diagonal with column interp
