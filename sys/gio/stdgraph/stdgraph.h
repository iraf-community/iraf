# STDGRAPH definitions.

define	MAX_CHARSIZES	10			# max discreet device char sizes
define	SZ_SBUF		2048			# initial string buffer size
define	SZ_MEMORY	1024			# encoder memory size
define	SZ_GDEVICE	256			# force output to named device
define	SZ_UIFNAME	199			# user interface file name
define	SZ_MSGBUF	4096			# default size message buffer
define	FLUSH_MEMORY	117			# time to flush encoded polyline
define	LEN_STACK	20			# encoder stack size
define	NREGISTERS	12			# number of encoder registers
define	E_IOP		11			# encoder i/o pointer register
define	E_TOP		12			# encoder top memory register
define	LONG_POLYLINE	50			# big enough to post X_INT
define	PADCHAR		0			# used to gen. delays

# The user can have private copies of UI specifications in GUIDIR.
define	GUIDIR		"guidir"

# The STDGRAPH state/device descriptor.

define	LEN_SG		91

define	SG_SBUF		Memi[$1]		# string buffer
define	SG_SZSBUF	Memi[$1+1]		# size of string buffer
define	SG_NEXTCH	Memi[$1+2]		# next char pos in string buf
define	SG_NCHARSIZES	Memi[$1+3]		# number of character sizes
define	SG_POLYLINE	Memi[$1+4]		# polyline output permitted
define	SG_POLYMARKER	Memi[$1+5]		# device supports polymarker
define	SG_FILLAREA	Memi[$1+6]		# device supports fillarea
define	SG_ENCODEXY	Memi[$1+7]		# format for encoding coords
define	SG_STARTDRAW	Memi[$1+8]		# pointer to DS string
define	SG_ENDDRAW	Memi[$1+9]		# pointer to DE string
define	SG_STARTMOVE	Memi[$1+10]		# pointer to VS string
define	SG_ENDMOVE	Memi[$1+11]		# pointer to VE string
define	SG_STARTMARK	Memi[$1+12]		# pointer to MS string
define	SG_ENDMARK	Memi[$1+13]		# pointer to ME string
define	SG_STARTFILL	Memi[$1+14]		# pointer to FS string
define	SG_ENDFILL	Memi[$1+15]		# pointer to FE string
define	SG_STARTTEXT	Memi[$1+16]		# start text draw
define	SG_ENDTEXT	Memi[$1+17]		# end text draw
define	SG_CURSOR	Memi[$1+18]		# last cursor accessed
define	SG_UPDCURSOR	Memi[$1+19]		# update cursor pos before read
define	SG_CURSOR_X	Memi[$1+20]		# current cursor X position
define	SG_CURSOR_Y	Memi[$1+21]		# current cursor Y position
define	SG_COLOR	Memi[$1+22]		# last color set
define	SG_TXSIZE	Memi[$1+23]		# last text size set
define	SG_TXFONT	Memi[$1+24]		# last text font set
define	SG_PLTYPE	Memi[$1+25]		# last line type set
define	SG_FASTYLE	Memi[$1+26]		# last fill area style set
define	SG_PLWIDTH	Memi[$1+27]		# last line width set
define	SG_DEVNAME	Memi[$1+28]		# name of open device
define	SG_UIFNAME	Memi[$1+29]		# user interface file name
define	SG_UIFDATE	Memi[$1+30]		# UI file date
			# empty
define	SG_CHARHEIGHT	Memi[$1+40+$2-1]	# character height
define	SG_CHARWIDTH 	Memi[$1+50+$2-1]	# character width
define	SG_CHARSIZE	Memr[P2R($1+60+$2-1)]	# text sizes permitted
define	SG_PLAP		($1+70)			# polyline attributes
define	SG_PMAP		($1+74)			# polymarker attributes
define	SG_FAAP		($1+78)			# fill area attributes
define	SG_TXAP		($1+81)			# default text attributes

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

# TEK 4012 definitions for optimized tek coordinate encoding.

define	TEK_XRES	1024
define	TEK_YRES	780
