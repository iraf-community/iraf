# GIM.H -- Public definitions for GIM graphics-imaging package.

# Default static color assignments.  Although explicit color names are used
# here, the actual colors may be reassigned in the server.  NOTE - do not
# use LAST_COLOR to determine the index of the first dynamically allocatable
# color cell; this value is device dependent and is set in graphcap.
# FIRST_COLOR, LAST_COLOR are intended for use to programmatically cycle
# through the static colors.

define	BACKGROUND		0
define	FOREGROUND		1
define	BLACK			0
define	WHITE			1
define	RED			2
define	GREEN			3
define	BLUE			4
define	CYAN			5
define	YELLOW			6
define	MAGENTA			7
define	AUX_COLOR_1		8
define	AUX_COLOR_2		9
define	FIRST_COLOR		2
define	LAST_COLOR		9

# Max amount of pixel data passed in a single WritePixels or ReadPixels call.
define	GIM_MAXCHARS		32736

# Raster type options (set to zero if don't care).
define	RT_NORMAL		1		# normal (client) raster
define	RT_CACHED		2		# cached in server memory

# Coordinate type options.
define	CT_PIXEL		0		# raster pixel coordinates
define	CT_NDC			1		# normalized device coordinates

# Rasterop fields.
define R_OPCODEMASK		00017B		# opcode bits
define R_TRANSIENT		00020B		# map only to screen
define R_REFRESH_ALL		00040B		# force refresh in setmapping
define R_REFRESH_NONE		00100B		# disable refresh in setmapping
