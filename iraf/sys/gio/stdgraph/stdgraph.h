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

define	SG_SBUF		Memi[P2I($1)]		# string buffer
define	SG_SZSBUF	Memi[P2I($1+1)]		# size of string buffer
define	SG_NEXTCH	Memi[P2I($1+2)]		# next char pos in string buf
define	SG_NCHARSIZES	Memi[P2I($1+3)]		# number of character sizes
define	SG_POLYLINE	Memi[P2I($1+4)]		# polyline output permitted
define	SG_POLYMARKER	Memi[P2I($1+5)]		# device supports polymarker
define	SG_FILLAREA	Memi[P2I($1+6)]		# device supports fillarea
define	SG_ENCODEXY	Memi[P2I($1+7)]		# format for encoding coords
define	SG_STARTDRAW	Memi[P2I($1+8)]		# pointer to DS string
define	SG_ENDDRAW	Memi[P2I($1+9)]		# pointer to DE string
define	SG_STARTMOVE	Memi[P2I($1+10)]		# pointer to VS string
define	SG_ENDMOVE	Memi[P2I($1+11)]		# pointer to VE string
define	SG_STARTMARK	Memi[P2I($1+12)]		# pointer to MS string
define	SG_ENDMARK	Memi[P2I($1+13)]		# pointer to ME string
define	SG_STARTFILL	Memi[P2I($1+14)]		# pointer to FS string
define	SG_ENDFILL	Memi[P2I($1+15)]		# pointer to FE string
define	SG_STARTTEXT	Memi[P2I($1+16)]		# start text draw
define	SG_ENDTEXT	Memi[P2I($1+17)]		# end text draw
define	SG_CURSOR	Memi[P2I($1+18)]		# last cursor accessed
define	SG_UPDCURSOR	Memi[P2I($1+19)]		# update cursor pos before read
define	SG_CURSOR_X	Memi[P2I($1+20)]		# current cursor X position
define	SG_CURSOR_Y	Memi[P2I($1+21)]		# current cursor Y position
define	SG_COLOR	Memi[P2I($1+22)]		# last color set
define	SG_TXSIZE	Memi[P2I($1+23)]		# last text size set
define	SG_TXFONT	Memi[P2I($1+24)]		# last text font set
define	SG_PLTYPE	Memi[P2I($1+25)]		# last line type set
define	SG_FASTYLE	Memi[P2I($1+26)]		# last fill area style set
define	SG_PLWIDTH	Memi[P2I($1+27)]		# last line width set
define	SG_DEVNAME	Memi[P2I($1+28)]		# name of open device
define	SG_UIFNAME	Memi[P2I($1+29)]		# user interface file name
define	SG_UIFDATE	Memi[P2I($1+30)]		# UI file date
			# empty
define	SG_CHARHEIGHT	Memi[P2I($1+40+$2-1)]	# character height
define	SG_CHARWIDTH 	Memi[P2I($1+50+$2-1)]	# character width
define	SG_CHARSIZE	Memr[P2R($1+60+$2-1)]	# text sizes permitted
define	SG_PLAP		($1+70)			# polyline attributes
define	SG_PMAP		($1+74)			# polymarker attributes
define	SG_FAAP		($1+78)			# fill area attributes
define	SG_TXAP		($1+81)			# default text attributes

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

# TEK 4012 definitions for optimized tek coordinate encoding.

define	TEK_XRES	1024
define	TEK_YRES	780
