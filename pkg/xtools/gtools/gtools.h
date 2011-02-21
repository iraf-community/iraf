# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# Public Definitions

define	GTVXMIN			0	# Viewport X minimum
define	GTVXMAX			1	# Viewport X maximum
define	GTVYMIN			2	# Viewport Y minimum
define	GTVYMAX			3	# Viewport Y maximum
define	GTXMIN			4	# WCS X minimum
define	GTXMAX			5	# WCS X maximum
define	GTYMIN			6	# WCS Y minimum
define	GTYMAX			7	# WCS Y maximum

define	GTSYSID			8	# Add SYSID?
define	GTPARAMS		9	# Graph parameters
define	GTTITLE			10	# Graph title
define	GTSUBTITLE		11	# Graph subtitle
define	GTCOMMENTS		12	# Comments
define	GTXLABEL		13	# X label
define	GTYLABEL		14	# Y label
define	GTXUNITS		15	# X units
define	GTYUNITS		16	# Y units

define	GTDRAWTITLE		17	# Draw title block?
define	GTDRAWXLABELS		18	# Draw x axis label block?
define	GTDRAWYLABELS		19	# Draw y axis label block?

define	GTTYPE			20	# Graph type
define	GTMARK			21	# Mark type
define	GTLINE			22	# Line type
define	GTXSIZE			23	# X Mark size
define	GTYSIZE			24	# Y Mark size
define	GTCOLOR			25	# Color

define	GTXTRAN			26	# WCS X transform
define	GTYTRAN			27	# WCS Y transform
define	GTXFLIP			28	# Flip X axis
define	GTYFLIP			29	# Flip Y axis
define	GTTRANSPOSE		30	# Transpose X and Y axes?

define	GTXFORMAT		31	# X format
define	GTYFORMAT		32	# Y format

define	GTXBUF			33	# Autoscaling buffer factor
define	GTYBUF			34	# Autoscaling buffer factor
define	GTLCLIP			35	# Low clipping factor
define	GTHCLIP			36	# High clipping factor

# Private Definitions

define	GTRESET			37	# Initialized from GIO structure?

define	GT_TXUP			51	# Text parameters
define	GT_TXSIZE		52
define	GT_TXPATH		53
define	GT_TXSPACING		54
define	GT_TXHJUSTIFY		55
define	GT_TXVJUSTIFY		56
define	GT_TXFONT		57
define	GT_TXQUALITY		58
define	GT_TXCOLOR		59

define	GT_DRAWTITLE		60	# GLABAX, general parameters
define	GT_TITLESIZE		61
define	GT_TITLEJUST		62
define	GT_NTITLELINES		63
define	GT_ASPECT		64
define	GT_CHARSIZE		65
define	GT_TITLECOLOR		66
define	GT_FRAMECOLOR		67
define	GT_DRIDCOLOR		68

define	GT_XDRAWAXES		71	# GLABAX, x axis parameters
define	GT_XSETAXISPOS		72
define	GT_XAXISPOS1		73
define	GT_XAXISPOS2		74
define	GT_XDRAWGRID		75
define	GT_XROUND		76
define	GT_XLABELAXIS		77
define	GT_XAXISLABELSIZE	78
define	GT_XDRAWTICKS		79
define	GT_XLABELTICKS		80
define	GT_XNMAJOR		81
define	GT_XNMINOR		82
define	GT_XMAJORLENGTH		83
define	GT_XMINORLENGTH		84
define	GT_XMAJORWIDTH		85
define	GT_XMINORWIDTH		86
define	GT_XAXISWIDTH		87
define	GT_XTICKLABELSIZE	88
define	GT_XTICKFORMAT		89
define	GT_XGRIDCOLOR		90
define	GT_XAXISLABELCOLOR	91
define	GT_XAXISCOLOR		92
define	GT_XTICKLABELCOLOR	93
define	GT_XTICKCOLOR		94

define	GT_YDRAWAXES		101	# GLABAX, y axis parameters
define	GT_YSETAXISPOS		102
define	GT_YAXISPOS1		103
define	GT_YAXISPOS2		104
define	GT_YDRAWGRID		105
define	GT_YROUND		106
define	GT_YLABELAXIS		107
define	GT_YAXISLABELSIZE	108
define	GT_YDRAWTICKS		109
define	GT_YLABELTICKS		110
define	GT_YNMAJOR		111
define	GT_YNMINOR		112
define	GT_YMAJORLENGTH		113
define	GT_YMINORLENGTH		114
define	GT_YMAJORWIDTH		115
define	GT_YMINORWIDTH		116
define	GT_YAXISWIDTH		117
define	GT_YTICKLABELSIZE	118
define	GT_YTICKFORMAT		119
define	GT_YGRIDCOLOR		120
define	GT_YAXISLABELCOLOR	121
define	GT_YAXISCOLOR		122
define	GT_YTICKLABELCOLOR	123
define	GT_YTICKCOLOR		124

define	LEN_GT		125	# Length of graphics tools extension

define	GT_VXMIN	Memr[P2R($1+GTVXMIN)]
define	GT_VXMAX	Memr[P2R($1+GTVXMAX)]	
define	GT_VYMIN	Memr[P2R($1+GTVYMIN)]
define	GT_VYMAX	Memr[P2R($1+GTVYMAX)]
define	GT_XMIN		Memr[P2R($1+GTXMIN)]
define	GT_XMAX		Memr[P2R($1+GTXMAX)]	
define	GT_YMIN		Memr[P2R($1+GTYMIN)]
define	GT_YMAX		Memr[P2R($1+GTYMAX)]
define	GT_SYSID	Memi[$1+GTSYSID]
define	GT_PARAMS	Memi[$1+GTPARAMS]
define	GT_TITLE	Memi[$1+GTTITLE]
define	GT_SUBTITLE	Memi[$1+GTSUBTITLE]
define	GT_COMMENTS	Memi[$1+GTCOMMENTS]
define	GT_XLABEL	Memi[$1+GTXLABEL]
define	GT_YLABEL	Memi[$1+GTYLABEL]
define	GT_XUNITS	Memi[$1+GTXUNITS]
define	GT_YUNITS	Memi[$1+GTYUNITS]
define	GT_DRWTITLE	Memi[$1+GTDRAWTITLE]
define	GT_DRWXLABELS	Memi[$1+GTDRAWXLABELS]
define	GT_DRWYLABELS	Memi[$1+GTDRAWYLABELS]
define	GT_TYPE		Memi[$1+GTTYPE]
define	GT_MARK		Memi[$1+GTMARK]
define	GT_LINE		Memi[$1+GTLINE]
define	GT_XSIZE	Memr[P2R($1+GTXSIZE)]
define	GT_YSIZE	Memr[P2R($1+GTYSIZE)]
define	GT_COLOR	Memi[$1+GTCOLOR]
define	GT_XTRAN	Memi[$1+GTXTRAN]
define	GT_YTRAN	Memi[$1+GTYTRAN]
define	GT_XFLIP	Memi[$1+GTXFLIP]
define	GT_YFLIP	Memi[$1+GTYFLIP]
define	GT_TRANSPOSE	Memi[$1+GTTRANSPOSE]
define	GT_XFORMAT	Memi[$1+GTXFORMAT]
define	GT_YFORMAT	Memi[$1+GTYFORMAT]
define	GT_XBUF		Memr[P2R($1+GTXBUF)]
define	GT_YBUF		Memr[P2R($1+GTYBUF)]
define	GT_LCLIP	Memr[P2R($1+GTLCLIP)]
define	GT_HCLIP	Memr[P2R($1+GTHCLIP)]
define	GT_RESET	Memi[$1+GTRESET]

define	GTTYPES		"|mark|line|histogram|"
define	GTMARKS	"|point|box|plus|cross|diamond|hline|vline|hebar|vebar|circle|"

define	GT_XAXES	"|none|bottom|top|both|"
define	GT_YAXES	"|none|left|right|both|"
