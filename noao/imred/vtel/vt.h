# Vacuum_telescope analysis package header file.

# General defines common to most of the programs in this package.
define	DIM_VTFD	2048		  # full disk image = 2048 x 2048 array 
define	SZB_SHORT	SZ_SHORT*SZB_CHAR # number of bytes per short integer
define	SZB_REAL	SZ_REAL*SZB_CHAR  # number of bytes per real
define	THRESHOLD	4		  # limb cutoff value, squib brightness

# Defines related to the tape format.
define 	SZ_VTHDR	20		# number of 16-bit words in vt header
define	SZ_VTREC	5120		# number of 16-bit words in vt record
define	NUM_VTREC	750		# number of records in full disk image

# Ellipse structure defines.
define	LEN_ELSTRUCT	4		# real el[LEN_ELSTRUCT]

define	E_XCENTER	$1[1]		# x-coord of center of limb ellipse
define	E_YCENTER	$1[2]		# y-coord of center of limb ellipse
define	E_XSEMIDIAMETER	$1[3]		# length of x semiaxis of limb ellipse
define	E_YSEMIDIAMETER	$1[4]		# length of y semiaxis of limb ellipse

# Defines for readvt, etc.
define	SWTH_HIGH	512		# height of each swath
define	SWTHWID_14	1700		# width of swaths 1 and 4
define	SWTHWID_23	2048		# width of swaths 2 and 3
define	HALF_DIF	174		# one half of difference in swath widths
define	SZ_TABLE	8192		# length of lookup table (16-bit words)
define	NUM_SRSTR	16		# total # of subrasters in full disk
define	LEN_HDRDAT	10		# length of header data
define	NUM_SRSTR_X	4		# number of subrasters in x direction
define	NUM_SRSTR_Y	4		# number of subrasters in y direction
define	SRSTR_WID	512		# width of each subraster
define	IS_DATA		1		# subswath data indicator
define	DTSTRING	100		# length of date/time string

# Defines for rmap, etc.
define	DIM_IN_RAS	150		# y dimension for input image subraster
define	DIM_SQUAREIM	180		# x or y dimension of daily projection

# Defines for merge, etc.
define	DIM_XCARMAP	360		# x dimension of carrington map
define	SZ_WTBL		180		# size of weight table for merge

# Mscan text (pixelfont) structure.
define	LEN_TXSTRUCT	10

define	TX_XPOS		Memi[$1]	# x position of start of text
define	TX_YPOS		Memi[$1+1]	# y position of start of text
define	TX_VALUE	Memi[$1+2]	# value to write text with
define	PRINT_TEXT	Memi[$1+3]	# to text, or not to text  (1=yes,0=no)
define	ZERO_BGND	Memi[$1+4]	# fill background w/ VALU? (1=yes,0=no)
define	BGND_VALU	Memi[$1+5]	# background value to use

# Vacuum telescope header struture.
define	VT_LENHSTRUCT	10

define	VT_HMONTH	Memi[$1]	# month of observation (1-12)
define	VT_HDAY		Memi[$1+1]	# day of observation (1-31)
define	VT_HYEAR	Memi[$1+2]	# year (two digits)
define	VT_HTIME	Memi[$1+3]	# time (seconds since midnight)
define	VT_HWVLNGTH	Memi[$1+4]	# wavelength (angstroms)
define	VT_HOBSTYPE	Memi[$1+5]	# observation type (0,1,2,3,or 4)
define	VT_HAVINTENS	Memi[$1+6]	# average intensity
define	VT_HNUMCOLS	Memi[$1+7]	# number of columns
define	VT_HINTGPIX	Memi[$1+8]	# integrations per pixel
define	VT_HREPTIME	Memi[$1+9]	# repitition time

# I/O buffer structure.
define	VT_LENBSTRUCT	3

define	VT_BUFP		Memi[$1]	# pointer, top of i/o buf
define	VT_BP		Memi[$1+1]	# pointer, current position in i/o buf
define	VT_BUFBOT	Memi[$1+2]	# pointer, current bottom of i/o buf
