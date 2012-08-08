# GLABAX.H -- Axis drawing and labelling.

define	SZ_FORMAT		19
define	SZ_LABEL		19
define	MAX_LINEARITY		1.0			# no log scaling if gt
define	LEFT_BORDER		9			# nchars at l|r edge
define	BOTTOM_BORDER		5			# nlines at bottom edge
define	Y_LABELOFFSET		7			# Y label dist from axis
define	MAX_SZTITLEBLOCK	0.5			# max sztitleblock, NDC
define	MIN_NTITLELINES		2			# min lines in titleblk
define	TOL			(EPSILONR*10.0)

define	LEN_AX			85
define	AX_POS			Memd[P2D($1)+$2-1]	# tick coords
define	AX_DRAWME		Memi[$1+4]		# draw this axis
define	AX_HORIZONTAL		Memi[$1+5]		# axis is horizontal
define	AX_SCALING		Memi[$1+6]		# type of scaling
define	AX_DRAWTICKS		Memi[$1+7]		# draw the ticks
define	AX_START		Memr[P2R($1+8+$2-1)]	# axis starts here
define	AX_END			Memr[P2R($1+10+$2-1)]	# axis ends here
define	AX_TICK1		Memr[P2R($1+12+$2-1)]	# first tick is here
define	AX_STEP			Memr[P2R($1+14+$2-1)]	# offset between ticks
define	AX_ISTEP		Memr[P2R($1+16+$2-1)]	# intial offset
define	AX_KSTEP		Memr[P2R($1+18)]	# step scalar at majors
define	AX_IKSTEP		Memr[P2R($1+19)]	# initial kstep
define	AX_NMINOR		Memi[$1+20]		# nminor ticks
define	AX_NLEFT		Memi[$1+21]		# nminor to next major
define	AX_INLEFT		Memi[$1+22]		# initial nleft
define	AX_NDIGITS		Memi[$1+23]		# ndigits of precision
define	AX_MINORTICK		Memr[P2R($1+24+$2-1)]	# offset to draw minor
define	AX_MAJORTICK		Memr[P2R($1+26+$2-1)]	# offset to draw major
define	AX_MINORWIDTH		Memr[P2R($1+28)]	# minor tick linewidth
define	AX_MAJORWIDTH		Memr[P2R($1+29)]	# major tick linewidth
define	AX_LABELTICKS		Memi[$1+30]		# draw tick labels
define	AX_TICKLABELOFFSET	Memr[P2R($1+31+$2-1)]	# offset to ticklabel
define	AX_TICKLABELSIZE	Memr[P2R($1+33)]	# char size of ticklabel
define	AX_TICKLABELCOLOR	Memi[$1+34]		# char size of ticklabel
define	AX_TICKCOLOR		Memi[$1+35]		# grid between ticks
define	AX_AXISLABELSIZE	Memr[P2R($1+36)]	# char size axislabel
define	AX_AXISLABELCOLOR	Memi[$1+37]		# char size axislabel
define	AX_AXISWIDTH		Memr[P2R($1+38)]	# axis linewidth
define	AX_AXISCOLOR		Memi[$1+39]		# axis linewidth
define	AX_GRIDCOLOR		Memi[$1+40]		# grid between ticks

define	AX_TICKLABELPOS		Memc[P2C($1+45)]	# gtext format
define	AX_TICKFORMAT		Memc[P2C($1+65)]	# numeric format
