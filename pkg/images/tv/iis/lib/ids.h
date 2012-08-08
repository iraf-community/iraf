# IDS definitions.

define	MAX_CHARSIZES	10			# max discreet device char sizes
define	SZ_SBUF		1024			# initial string buffer size
define	SZ_IDEVICE	31			# maxsize forced device name

# The IDS state/device descriptor.

define	LEN_IDS		81

define	IDS_SBUF	Memi[$1]		# string buffer
define	IDS_SZSBUF	Memi[$1+1]		# size of string buffer
define	IDS_NEXTCH	Memi[$1+2]		# next char pos in string buf
define	IDS_NCHARSIZES	Memi[$1+3]		# number of character sizes
define	IDS_POLYLINE	Memi[$1+4]		# device supports polyline
define	IDS_POLYMARKER	Memi[$1+5]		# device supports polymarker
define	IDS_FILLAREA	Memi[$1+6]		# device supports fillarea
define	IDS_CELLARRAY	Memi[$1+7]		# device supports cell array
define	IDS_ZRES	Memi[$1+8]		# device resolution in Z
define	IDS_FILLSTYLE	Memi[$1+9]		# number of fill styles
define	IDS_ROAM	Memi[$1+10]		# device supports roam
define	IDS_CANZM	Memi[$1+11]		# device supports zoom
define	IDS_SELERASE	Memi[$1+12]		# device has selective erase
define	IDS_FRAME	Memi[$1+13]		# pointer to frames area
define	IDS_BITPL	Memi[$1+14]		# pointer to bitplane area
	# extra space
define	IDS_FRCOLOR	Memi[$1+18]		# frame color
define	IDS_GRCOLOR	Memi[$1+19]		# graphics color
define	IDS_LCURSOR	Memi[$1+20]		# last cursor accessed
define	IDS_COLOR	Memi[$1+21]		# last color set
define	IDS_TXSIZE	Memi[$1+22]		# last text size set
define	IDS_TXFONT	Memi[$1+23]		# last text font set
define	IDS_TYPE	Memi[$1+24]		# last line type set
define	IDS_WIDTH	Memi[$1+25]		# last line width set
define	IDS_DEVNAME	Memi[$1+26]		# name of open device
define	IDS_CHARHEIGHT	Memi[$1+30+$2-1]	# character height
define	IDS_CHARWIDTH 	Memi[$1+40+$2-1]	# character width
define	IDS_CHARSIZE	Memr[P2R($1+50+$2-1)]	# text sizes permitted
define	IDS_PLAP	($1+60)			# polyline attributes
define	IDS_PMAP	($1+64)			# polymarker attributes
define	IDS_FAAP	($1+68)			# fill area attributes
define	IDS_TXAP	($1+71)			# default text attributes

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

define	IDS_EOD         (-2)		# flag for end of data

define	IDS_RESET	10		# escape 10
define	IDS_R_HARD	 0		# hard reset
define	IDS_R_MEDIUM	 1		# medium
define	IDS_R_SOFT	 2
define	IDS_R_SNAPDONE	 3		# end snap

define	IDS_SET_IP	11		# escape 11
define	IDS_SET_GP	12		# escape 12
define	IDS_DISPLAY_I	13		# escape 13
define	IDS_DISPLAY_G	14		# escape 14
define	IDS_SAVE	15		# escape 15
define	IDS_RESTORE	16		# escape 16

# max sizes

define	IDS_MAXIMPL	16		# maximum number of image planes
define	IDS_MAXGRPL	16		# maximum number of graphics planes
define	IDS_MAXBITPL	16		# maximum bit planes per frame
define	IDS_MAXGCOLOR	 8		# maximum number of colors (graphics)
define	IDS_MAXDATA   8192		# maximum data structure in display

define	IDS_RED		 1
define	IDS_GREEN	 2
define	IDS_BLUE	 3
define	IDS_YELLOW	 4
define	IDS_RDBL	 5
define	IDS_GRBL	 6
define	IDS_WHITE	 7
define	IDS_BLACK	 8

define	IDS_QUAD_UR	 1		# upper right quad.: split screen mode
define	IDS_QUAD_UL	 2
define	IDS_QUAD_LL	 3
define	IDS_QUAD_LR	 4

define	IDS_CONTROL	17		# escape 17
define	IDS_CTRL_LEN	 6
define	IDS_CTRL_REG	 1		# what to control
define	IDS_CTRL_RW	 2		# read/write field in control instr.
define	IDS_CTRL_N	 3		# count of DATA items
define	IDS_CTRL_FRAME	 4		# pertinent frame(s)
define	IDS_CTRL_COLOR	 5		#   and color
define	IDS_CTRL_OFFSET	 6		# generalized "register"
define	IDS_CTRL_DATA	 7		# data array

define	IDS_WRITE	 0		# write command
define	IDS_READ	 1		# read command
define	IDS_READ_WT	 2		# wait for action, then read
define	IDS_OFF		 1		# turn whatever off
define	IDS_ON		 2
define	IDS_CBLINK	 3		# cursor blink
define	IDS_CSHAPE	 4		# cursor shape

define	IDS_CSTEADY	 1		# cursor blink - steady (no blink)
define	IDS_CFAST	 2		# cursor blink - fast
define	IDS_CMEDIUM	 3		# cursor blink - medium
define	IDS_CSLOW	 4		# cursor blink - slow

define	IDS_FRAME_LUT	 1		# look-up table for image frame
define	IDS_GR_MAP	 2		# graphics color map...lookup table per
					# se makes little sense for bit plane
define	IDS_INPUT_LUT	 3		# global input lut
define	IDS_OUTPUT_LUT	 4		# final lut
define	IDS_SPLIT	 5		# split screen coordinates
define	IDS_SCROLL	 6		# scroll coordinates
define	IDS_ZOOM	 7		# zoom magnification
define	IDS_OUT_OFFSET	 8		# output bias
define	IDS_MIN		 9		# data minimum
define	IDS_MAX		10		# data maximum
define	IDS_RANGE	11		# output range select
define	IDS_HISTOGRAM	12		# output data histogram
define	IDS_ALU_FCN	13		# arithmetic feedback function
define	IDS_FEEDBACK	14		# feedback control
define	IDS_SLAVE	15		# auxillary host or slave processor

define	IDS_CURSOR	20		# cursor control - on/off/blink/shape
define	IDS_TBALL	21		# trackball control - on/off
define	IDS_DIGITIZER	22		# digitizer control - on/off
define	IDS_BLINK	23		# for blink request
define	IDS_SNAP	24		# snap function
define	IDS_MATCH	25		# match lookup tables

# snap codes ... just reuse color codes from above.
define	IDS_SNAP_RED	 IDS_RED	# snap the blue image
define	IDS_SNAP_GREEN	 IDS_GREEN	# green
define	IDS_SNAP_BLUE	 IDS_BLUE	# blue
define	IDS_SNAP_RGB	 IDS_BLACK	# rgb image --- do all three
define	IDS_SNAP_MONO	 IDS_WHITE	# do just one

# cursor parameters

define	IDS_CSET	128		# number of cursors per "group"

define	IDS_CSPECIAL	4097		# special "cursors"
			# must be > (IDS_CSET * number of cursor groups)
define	IDS_CRAW	IDS_CSPECIAL	# raw cursor read
define	IDS_BUT_RD	4098		# "cursor number" for read buttons cmd
define	IDS_BUT_WT	4099		# wait for button press, then read
define	IDS_CRAW2	4100		# A second "raw" cursor
