# GKTRANS definitions.

define	MAX_CHARSIZES	10			# max discreet device char sizes
define	SZ_SBUF		1024			# initial string buffer size
define	SZ_MFRECORD	(1440/SZB_CHAR)		# metafile record size
define	SZ_GDEVICE	31			# maxsize forced device name
define	DEF_MAXFRAMES	16			# maximum frames/metafile

# The GKTRANS state/device descriptor.

define	LEN_GKT		81

define	GKT_SBUF	Memi[P2I($1)]		# string buffer
define	GKT_SZSBUF	Memi[P2I($1+1)]		# size of string buffer
define	GKT_NEXTCH	Memi[P2I($1+2)]		# next char pos in string buf
define	GKT_NCHARSIZES	Memi[P2I($1+3)]		# number of character sizes
define	GKT_POLYLINE	Memi[P2I($1+4)]		# device supports polyline
define	GKT_POLYMARKER	Memi[P2I($1+5)]		# device supports polymarker
define	GKT_FILLAREA	Memi[P2I($1+6)]		# device supports fillarea
define	GKT_CELLARRAY	Memi[P2I($1+7)]		# device supports cell array
define	GKT_ZRES	Memi[P2I($1+8)]		# device resolution in Z
define	GKT_FILLSTYLE	Memi[P2I($1+9)]		# number of fill styles
define	GKT_ROAM	Memi[P2I($1+10)]		# device supports roam
define	GKT_ZOOM	Memi[P2I($1+11)]		# device supports zoom
define	GKT_SELERASE	Memi[P2I($1+12)]		# device has selective erase
define	GKT_PIXREP	Memi[P2I($1+13)]		# device supports pixel replic.
define	GKT_STARTFRAME	Memi[P2I($1+14)]		# frame advance at metafile BOF
define	GKT_ENDFRAME	Memi[P2I($1+15)]		# frame advance at metafile EOF
	# extra space
define	GKT_CURSOR	Memi[P2I($1+20)]		# last cursor accessed
define	GKT_COLOR	Memi[P2I($1+21)]		# last color set
define	GKT_TXSIZE	Memi[P2I($1+22)]		# last text size set
define	GKT_TXFONT	Memi[P2I($1+23)]		# last text font set
define	GKT_TYPE	Memi[P2I($1+24)]		# last line type set
define	GKT_WIDTH	Memi[P2I($1+25)]		# last line width set
define	GKT_DEVNAME	Memi[P2I($1+26)]		# name of open device
	# extra space
define	GKT_CHARHEIGHT	Memi[P2I($1+30+$2-1)]	# character height
define	GKT_CHARWIDTH 	Memi[P2I($1+40+$2-1)]	# character width
define	GKT_CHARSIZE	Memr[P2R($1+50+$2-1)]	# text sizes permitted
define	GKT_PLAP	($1+60)			# polyline attributes
define	GKT_PMAP	($1+64)			# polymarker attributes
define	GKT_FAAP	($1+68)			# fill area attributes
define	GKT_TXAP	($1+71)			# default text attributes

# Substructure definitions.

define	LEN_PL		4
define	PL_STATE	Memi[P2I($1)]		# polyline attributes
define	PL_LTYPE	Memi[P2I($1+1)]
define	PL_WIDTH	Memi[P2I($1+2)]
define	PL_COLOR	Memi[P2I($1+3)]

define	LEN_PM		4
define	PM_STATE	Memi[P2I($1)]		# polymarker attributes
define	PM_LTYPE	Memi[P2I($1+1)]
define	PM_WIDTH	Memi[P2I($1+2)]
define	PM_COLOR	Memi[P2I($1+3)]

define	LEN_FA		3			# fill area attributes
define	FA_STATE	Memi[P2I($1)]
define	FA_STYLE	Memi[P2I($1+1)]
define	FA_COLOR	Memi[P2I($1+2)]

define	LEN_TX		10			# text attributes
define	TX_STATE	Memi[P2I($1)]
define	TX_UP		Memi[P2I($1+1)]
define	TX_SIZE		Memi[P2I($1+2)]
define	TX_PATH		Memi[P2I($1+3)]
define	TX_SPACING	Memr[P2R($1+4)]
define	TX_HJUSTIFY	Memi[P2I($1+5)]
define	TX_VJUSTIFY	Memi[P2I($1+6)]
define	TX_FONT		Memi[P2I($1+7)]
define	TX_QUALITY	Memi[P2I($1+8)]
define	TX_COLOR	Memi[P2I($1+9)]
