# CCP definitions.

define	MAX_CHARSIZES	10			# max discreet device char sizes
define	SZ_SBUF		1024			# initial string buffer size
define	SZ_GDEVICE	31			# maxsize forced device name
define	CCP_LDEV	5			# device for "plots(0,0,ldev")
define	CCP_UP		3			# "pen-up" code
define	CCP_DOWN	2			# "pen-down" code
define	PL_SINGLE	1			# rel width of single-width line
define	MAXTRACES	15			# maximum adjacent bold traces
define	SEGSIZE		256			# segment buffer size
define	XSEG		Memr[xseg + $1 - 1]	# segment buffer for ccp_calcseg
define	YSEG		Memr[yseg + $1 - 1]	# "
define	DIS		sqrt ((($3)-($1))**2+(($4)-($2))**2) #dis (x1,y1, x2,y2)
define	XTRAN		($1) * g_xndcto_p	# convert NDC to plotter coords
define	YTRAN		($1) * g_yndcto_p	# "
define	FRAME_OFFSET	1.0			# pltr units between [new]frames
define	MAX_PL_XWIDTH	0.3307			# max pltr x (m) if no graphcap
define	MAX_PL_YHEIGHT	0.2540			# max pltr y (m) if no graphcap
define	DEF_MPER_PUNIT	0.0254			# default meters / plotter unit
define	DEF_DASHLEN	0.1000			# default dash length, pltr unit
define	DEF_GAPLEN	0.0500			# default gap length, pltr units
define	DEF_PLWSEP	0.0050			# default ntracing sep. in pu

# CCP state device descriptor:

define	LEN_CCP		81

define	CCP_SBUF	Memi[$1]		# string buffer
define	CCP_SZSBUF	Memi[$1+1]		# size of string buffer
define	CCP_NEXTCH	Memi[$1+2]		# next char pos in string buf
define	CCP_NCHARSIZES	Memi[$1+3]		# number of character sizes
define	CCP_POLYLINE	Memi[$1+4]		# device supports polyline
define	CCP_POLYMARKER	Memi[$1+5]		# device supports polymarker
define	CCP_FILLAREA	Memi[$1+6]		# device supports fillarea
define	CCP_CELLARRAY	Memi[$1+7]		# device supports cell array
define	CCP_ZRES	Memi[$1+8]		# device resolution in Z
define	CCP_FILLSTYLE	Memi[$1+9]		# number of fill styles
define	CCP_ROAM	Memi[$1+10]		# device supports roam
define	CCP_ZOOM	Memi[$1+11]		# device supports zoom
define	CCP_SELERASE	Memi[$1+12]		# device has selective erase
define	CCP_PIXREP	Memi[$1+13]		# device supports pixel replic.
define	CCP_STARTFRAME	Memi[$1+14]		# frame advance at metafile BOF
define	CCP_ENDFRAME	Memi[$1+15]		# frame advance at metafile EOF
	# extra space
define	CCP_CURSOR	Memi[$1+20]		# last cursor accessed
define	CCP_COLOR	Memi[$1+21]		# last color set
define	CCP_TXSIZE	Memi[$1+22]		# last text size set
define	CCP_TXFONT	Memi[$1+23]		# last text font set
define	CCP_LTYPE	Memi[$1+24]		# last line type set
define	CCP_WIDTH	Memi[$1+25]		# last line width set
define	CCP_DEVNAME	Memi[$1+26]		# name of open device
define	CCP_DEVCHAN	Memi[$1+27]		# channel for "plots(0,0,ldev)"
	# extra space
define	CCP_CHARHEIGHT	Memi[$1+30+$2-1]	# character height
define	CCP_CHARWIDTH 	Memi[$1+40+$2-1]	# character width
define	CCP_CHARSIZE	Memr[P2R($1+50+$2-1)]	# text sizes permitted
define	CCP_PLAP	($1+60)			# polyline attributes
define	CCP_PMAP	($1+64)			# polymarker attributes
define	CCP_FAAP	($1+68)			# fill area attributes
define	CCP_TXAP	($1+71)			# default text attributes

# Substructure definitions.

define	LEN_PL		4
define	PL_STATE	Memi[$1]		# polyline attributes
define	PL_LTYPE	Memi[$1+1]
define	PL_WIDTH	Memi[$1+2]
define	PL_COLOR	Memi[$1+3]

define	LEN_PM		4
define	PM_STATE	Memi[$1]		# polymarker attributes
define	PM_LTYPE	Memi[$1+1]
define	PM_WIDTH	Memi[$1+2]
define	PM_COLOR	Memi[$1+3]

define	LEN_FA		3			# fill area attributes
define	FA_STATE	Memi[$1]
define	FA_STYLE	Memi[$1+1]
define	FA_COLOR	Memi[$1+2]

define	LEN_TX		10			# text attributes
define	TX_STATE	Memi[$1]
define	TX_UP		Memi[$1+1]
define	TX_SIZE		Memi[$1+2]
define	TX_PATH		Memi[$1+3]
define	TX_SPACING	Memr[P2R($1+4)]
define	TX_HJUSTIFY	Memi[$1+5]
define	TX_VJUSTIFY	Memi[$1+6]
define	TX_FONT		Memi[$1+7]
define	TX_QUALITY	Memi[$1+8]
define	TX_COLOR	Memi[$1+9]
