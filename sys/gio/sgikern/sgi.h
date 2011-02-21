# SGI global definitions.

define	MAX_CHARSIZES	10			# max discreet device char sizes
define	SZ_SBUF		1024			# initial string buffer size
define	SZ_GDEVICE	31			# maxsize forced device name
define	DEF_MAXFRAMES	16			# maximum frames/metafile

# The SGI state/device descriptor.

define	LEN_SGI		81

define	SGI_SBUF	Memi[$1]		# string buffer
define	SGI_SZSBUF	Memi[$1+1]		# size of string buffer
define	SGI_NEXTCH	Memi[$1+2]		# next char pos in string buf
define	SGI_NCHARSIZES	Memi[$1+3]		# number of character sizes
define	SGI_POLYLINE	Memi[$1+4]		# device supports polyline
define	SGI_POLYMARKER	Memi[$1+5]		# device supports polymarker
define	SGI_FILLAREA	Memi[$1+6]		# device supports fillarea
define	SGI_CELLARRAY	Memi[$1+7]		# device supports cell array
define	SGI_XRES	Memi[$1+8]		# device resolution in X
define	SGI_YRES	Memi[$1+9]		# device resolution in Y
define	SGI_ZRES	Memi[$1+10]		# device resolution in Z
define	SGI_FILLSTYLE	Memi[$1+11]		# number of fill styles
define	SGI_ROAM	Memi[$1+12]		# device supports roam
define	SGI_ZOOM	Memi[$1+13]		# device supports zoom
define	SGI_SELERASE	Memi[$1+14]		# device has selective erase
define	SGI_PIXREP	Memi[$1+15]		# device supports pixel replic.
define	SGI_STARTFRAME	Memi[$1+16]		# frame advance at metafile BOF
define	SGI_ENDFRAME	Memi[$1+17]		# frame advance at metafile EOF
	# extra space
define	SGI_CURSOR	Memi[$1+20]		# last cursor accessed
define	SGI_COLOR	Memi[$1+21]		# last color set
define	SGI_TXSIZE	Memi[$1+22]		# last text size set
define	SGI_TXFONT	Memi[$1+23]		# last text font set
define	SGI_TYPE	Memi[$1+24]		# last line type set
define	SGI_WIDTH	Memi[$1+25]		# last line width set
define	SGI_DEVNAME	Memi[$1+26]		# name of open device
	# extra space
define	SGI_CHARHEIGHT	Memi[$1+30+$2-1]	# character height
define	SGI_CHARWIDTH 	Memi[$1+40+$2-1]	# character width
define	SGI_CHARSIZE	Memr[P2R($1+50+$2-1)]	# text sizes permitted
define	SGI_PLAP	($1+60)			# polyline attributes
define	SGI_PMAP	($1+64)			# polymarker attributes
define	SGI_FAAP	($1+68)			# fill area attributes
define	SGI_TXAP	($1+71)			# default text attributes

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
