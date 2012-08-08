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

define	KSHIFT		10000
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

#define	LEN_GDES	552
define	LEN_GDES	600
define	LEN_WCS		12
define	LEN_WCSARRAY	(LEN_WCS*MAX_WCS)
define	SZ_DEVNAME	29
define	SZ_UIFNAME	199
define	SZ_TICKFORMAT	9

define	GP_FD		Memi[$1]		# graphics stream
define	GP_TTY		Memi[$1+1]		# graphcap descriptor
define	GP_GFLAGS	Memi[$1+2]		# GIO flag bits
define	GP_ACMODE	Memi[$1+3]		# gopen access mode
define	GP_WCS		Memi[$1+4]		# current WCS
define	GP_WCSSTATE	Memi[$1+5]		# unset, modifed, or fixed
define	GP_WCSORD	Memi[$1+6]		# unique WCS ordinal
define	GP_CURSOR	Memi[$1+7]		# current cursor number
define	GP_DEVASPECT	Memr[P2R($1+8)]		# device aspect ratio
define	GP_SZMARKER	Memr[P2R($1+9+$2-1)]	# standard marker sizes
			# (extra space)
define	GP_PLAP		($1+20)			# polyline attributes
define	GP_PMAP		($1+24)			# polymarker attributes
define	GP_FAAP		($1+28)			# fill area attributes
define	GP_TXAP		($1+31)			# default text attributes
define	GP_TXAPCUR	($1+41)			# text drawing attributes
define	GP_DRAWTITLE	Memi[$1+55]		# draw title on graph
define	GP_TITLESIZE	Memr[P2R($1+56)]	# character size of title
define	GP_TITLECOLOR	Memi[$1+57]		# color of title
define	GP_TITLEJUST	Memi[$1+58]		# title justification
define	GP_NTITLELINES	Memi[$1+59]		# number of lines in title
define	GP_FRAMECOLOR	Memi[$1+60]		# color of viewport frame
define	GP_FRAMEDRAWN	Memi[$1+61]		# set when frame first drawn
define	GP_ASPECT	Memr[P2R($1+62)]	# aspect ratio of viewport
define	GP_XAP		($1+65)			# glabax parameters for X axis
define	GP_YAP		($1+100)		# glabax parameters for Y axis
define	GP_DEVNAME	Memc[P2C($1+135)]	# gopen device name
define	GP_UIFNAME	Memc[P2C($1+165)]	# UI file name
define	GP_WCSPTR	(($2)*LEN_WCS+($1)+367)	# pointer to WCS substructure
 			# (367:571 aligned wcs storage, 17*12=204 units)
 			# (next=572)

# Substructure definitions.

define	LEN_PL		4
define	PL_STATE	Memi[$1]		# polyline attributes
define	PL_LTYPE	Memi[$1+1]
define	PL_WIDTH	Memr[P2R($1+2)]
define	PL_COLOR	Memi[$1+3]

define	LEN_PM		4
define	PM_STATE	Memi[$1]		# polymarker attributes
define	PM_LTYPE	Memi[$1+1]
define	PM_WIDTH	Memr[P2R($1+2)]
define	PM_COLOR	Memi[$1+3]

define	LEN_FA		3
define	FA_STATE	Memi[$1]		# fill area attributes
define	FA_STYLE	Memi[$1+1]
define	FA_COLOR	Memi[$1+2]

define	LEN_TX		10
define	TX_STATE	Memi[$1]		# text attributes
define	TX_UP		Memi[$1+1]
define	TX_SIZE		Memr[P2R($1+2)]
define	TX_PATH		Memi[$1+3]
define	TX_SPACING	Memr[P2R($1+4)]
define	TX_HJUSTIFY	Memi[$1+5]
define	TX_VJUSTIFY	Memi[$1+6]
define	TX_FONT		Memi[$1+7]
define	TX_QUALITY	Memi[$1+8]
define	TX_COLOR	Memi[$1+9]

# GLABAX parameters for either axis.

define	LEN_GL		33
define	GL_DRAWAXES	Memi[$1]		# 0=none,1=first,2=second,3=both
define	GL_SETAXISPOS	Memi[$1+1]		# X axes to be drawn
define	GL_AXISPOS1	Memr[P2R($1+2)]		# WCS coord of axis 1
define	GL_AXISPOS2	Memr[P2R($1+3)]		# WCS coord of axis 2
define	GL_DRAWGRID	Memi[$1+4]		# draw grid between ticks
define	GL_GRIDCOLOR	Memi[$1+5]		# grid color
define	GL_ROUND	Memi[$1+6]		# extend WCS to next tick
define	GL_LABELAXIS	Memi[$1+7]		# draw the axis label
define	GL_AXISLABELSIZE Memr[P2R($1+8)]	# char size of axis labels
define	GL_AXISLABELCOLOR Memi[$1+9]		# char size of axis labels
define	GL_DRAWTICKS	Memi[$1+10]		# draw ticks
define	GL_LABELTICKS	Memi[$1+11]		# draw tick labels
define	GL_NMAJOR	Memi[$1+12]		# number of major ticks
define	GL_NMINOR	Memi[$1+13]		# number of minor ticks (if!log)
define	GL_MAJORLENGTH	Memr[P2R($1+14)]	# NDC length of major ticks
define	GL_MINORLENGTH	Memr[P2R($1+15)]	# NDC length of minor ticks
define	GL_MAJORWIDTH	Memr[P2R($1+16)]	# linewidth of major ticks
define	GL_MINORWIDTH	Memr[P2R($1+17)]	# linewidth of minor ticks
define	GL_AXISWIDTH	Memr[P2R($1+18)]	# linewidth of axis
define	GL_AXISCOLOR	Memi[$1+19]		# axis color
define	GL_TICKLABELSIZE Memr[P2R($1+20)]	# char size of tick labels
define	GL_TICKLABELCOLOR Memi[$1+21]		# char size of tick labels
define	GL_TICKCOLOR	Memi[$1+22]		# axis color
define	GL_TICKFORMAT	Memc[P2C($1+23)]	# printf format of ticks

# WCS substructure.
define	WCS_WX1		Memr[P2R($1)]		# window coordinates
define	WCS_WX2		Memr[P2R($1+1)]
define	WCS_WY1		Memr[P2R($1+2)]
define	WCS_WY2		Memr[P2R($1+3)]
define	WCS_SX1		Memr[P2R($1+4)]		# viewport coordinates
define	WCS_SX2		Memr[P2R($1+5)]
define	WCS_SY1		Memr[P2R($1+6)]
define	WCS_SY2		Memr[P2R($1+7)]
define	WCS_XTRAN	Memi[$1+8]		# type of scaling (linear,log)
define	WCS_YTRAN	Memi[$1+9]
define	WCS_FLAGS	Memi[$1+10]		# assorted flags
define	WCS_CLIP	WCS_FLAGS		# for backwards compatibility

# WCS_FLAGS bitfields.
define	WF_DEFINED	00001B			# WCS has been defined
define	WF_CLIP		00002B			# clip at viewport boundary
define	WF_NEWFORMAT	00004B			# new format WCS
	# (reserved)				# remaining bits reserved
define	WF_RASTER	(and(($1)/512,0777B))	# get raster number
define	WF_SETRASTER	(or(($1),($2)*512))	# set raster number
