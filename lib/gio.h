# GIO.H -- GIO package definitions.

define	MAX_WCS		16			# max user defined WCS
define	LEN_PLBUF	2048			# max points in a polyline
define	MAX_SZMARKER	4			# max standard marker sizes
define	POLYLINE	1			# G_PLTYPE values
define	POLYMARKER	2
define	FILLAREA	3
define	POINTMODE	4			# pointmode polymarker
define	LINEAR		0			# axis scaling functions
define	LOG		1
define	ELOG		2
define	DEF_CHARHEIGHT	0.0286			# default char height
define	DEF_CHARWIDTH	0.0125			# default char width

# PSIO/CURSOR definitions.

define	KSHIFT		100
define	DATA		0
define	XMIT		1
define	XFER		2
define	PSIO		3
define	OSCMD		4

# WCS and attribute packet states.  A modified packet must be "fixed" to
# the device (by output of a GKI_SET instruction) before the associated
# output instruction is issued.

define	UNSET		0		# initial state
define	MODIFIED	1		# modified internally
define	FIXED		2		# output in GKI stream

# GP_FLAGS bit flag definitions.  Lots of spare flag bits available.

define	GF_CLOSEFD	1		# close output file at gclose time
define	GF_WSOPEN	2		# workstation has been opened
define	GF_WSACTIVE	4		# workstation is activated

# Graphics descriptor.  The polyline, polymarker, text, and fill area
# attributes and the GLABAX parameters for the X and Y axes are stored
# in the descriptor as substructures.

define	LEN_GDES	337
define	LEN_WCS		11
define	LEN_WCSARRAY	(LEN_WCS*MAX_WCS)
define	SZ_DEVNAME	29
define	SZ_TICKFORMAT	9

define	GP_FD		Memi[$1]		# graphics stream
define	GP_TTY		Memi[$1+1]		# graphcap descriptor
define	GP_GFLAGS	Memi[$1+2]		# GIO flag bits
define	GP_ACMODE	Memi[$1+3]		# gopen access mode
define	GP_WCS		Memi[$1+4]		# current WCS
define	GP_WCSSTATE	Memi[$1+5]		# unset, modifed, or fixed
define	GP_CURSOR	Memi[$1+6]		# current cursor number
define	GP_DEVASPECT	Memr[$1+7]		# device aspect ratio
define	GP_SZMARKER	Memr[$1+8+$2-1]		# standard marker sizes
			# (extra space)
define	GP_PLAP		($1+20)			# polyline attributes
define	GP_PMAP		($1+24)			# polymarker attributes
define	GP_FAAP		($1+28)			# fill area attributes
define	GP_TXAP		($1+31)			# default text attributes
define	GP_TXAPCUR	($1+41)			# text drawing attributes
define	GP_DRAWTITLE	Memi[$1+55]		# draw title on graph
define	GP_TITLESIZE	Memr[$1+56]		# character size of title
define	GP_TITLEJUST	Memi[$1+57]		# title justification
define	GP_NTITLELINES	Memi[$1+58]		# number of lines in title
define	GP_ASPECT	Memr[$1+59]		# aspect ratio of viewport
define	GP_XAP		($1+60)			# glabax parameters for X axis
define	GP_YAP		($1+90)			# glabax parameters for Y axis
define	GP_DEVNAME	Memc[P2C($1+120)]	# gopen device name
define	GP_WCSPTR	(($2)*LEN_WCS+$1+150)	# pointer to WCS substructure
			# (150:336 wcs storage, 17*11=187 units)
			# (next=337)

# Substructure definitions.

define	LEN_PL		4
define	PL_STATE	Memi[$1]		# polyline attributes
define	PL_LTYPE	Memi[$1+1]
define	PL_WIDTH	Memr[$1+2]
define	PL_COLOR	Memi[$1+3]

define	LEN_PM		4
define	PM_STATE	Memi[$1]		# polymarker attributes
define	PM_LTYPE	Memi[$1+1]
define	PM_WIDTH	Memr[$1+2]
define	PM_COLOR	Memi[$1+3]

define	LEN_FA		3
define	FA_STATE	Memi[$1]		# fill area attributes
define	FA_STYLE	Memi[$1+1]
define	FA_COLOR	Memi[$1+2]

define	LEN_TX		10
define	TX_STATE	Memi[$1]		# text attributes
define	TX_UP		Memi[$1+1]
define	TX_SIZE		Memr[$1+2]
define	TX_PATH		Memi[$1+3]
define	TX_SPACING	Memr[$1+4]
define	TX_HJUSTIFY	Memi[$1+5]
define	TX_VJUSTIFY	Memi[$1+6]
define	TX_FONT		Memi[$1+7]
define	TX_QUALITY	Memi[$1+8]
define	TX_COLOR	Memi[$1+9]

# GLABAX parameters for either axis.

define	LEN_GL		28
define	GL_DRAWAXES	Memi[$1]		# 0=none,1=first,2=second,3=both
define	GL_SETAXISPOS	Memi[$1+1]		# X axes to be drawn
define	GL_AXISPOS1	Memr[$1+2]		# WCS coord of axis 1
define	GL_AXISPOS2	Memr[$1+3]		# WCS coord of axis 2
define	GL_DRAWGRID	Memi[$1+4]		# draw grid between ticks
define	GL_ROUND	Memi[$1+5]		# extend WCS to next tick
define	GL_LABELAXIS	Memi[$1+6]		# draw the axis label
define	GL_AXISLABELSIZE Memr[$1+7]		# char size of axis labels
define	GL_DRAWTICKS	Memi[$1+8]		# draw ticks
define	GL_LABELTICKS	Memi[$1+9]		# draw tick labels
define	GL_NMAJOR	Memi[$1+10]		# number of major ticks
define	GL_NMINOR	Memi[$1+11]		# number of minor ticks (if!log)
define	GL_MAJORLENGTH	Memr[$1+12]		# NDC length of major ticks
define	GL_MINORLENGTH	Memr[$1+13]		# NDC length of minor ticks
define	GL_MAJORWIDTH	Memr[$1+14]		# linewidth of major ticks
define	GL_MINORWIDTH	Memr[$1+15]		# linewidth of minor ticks
define	GL_AXISWIDTH	Memr[$1+16]		# linewidth of axis
define	GL_TICKLABELSIZE Memr[$1+17]		# char size of tick labels
define	GL_TICKFORMAT	Memc[P2C($1+18)]	# printf format of ticks

# WCS substructure.
define	WCS_WX1		Memr[$1]		# window coordinates
define	WCS_WX2		Memr[$1+1]
define	WCS_WY1		Memr[$1+2]
define	WCS_WY2		Memr[$1+3]
define	WCS_SX1		Memr[$1+4]		# viewport coordinates
define	WCS_SX2		Memr[$1+5]
define	WCS_SY1		Memr[$1+6]
define	WCS_SY2		Memr[$1+7]
define	WCS_XTRAN	Memi[$1+8]		# type of scaling (linear,log)
define	WCS_YTRAN	Memi[$1+9]
define	WCS_CLIP	Memi[$1+10]		# clip at viewport boundary?
