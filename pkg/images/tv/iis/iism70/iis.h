# This file contains the hardware definitions for the iis model 70/f
# at Kitt Peak.

# Define header
define	LEN_IISHDR	8		# Length of IIS header

define	XFERID		  $1[1]		# transfer id
define	THINGCT		  $1[2]		# thing count
define	SUBUNIT		  $1[3]		# subuint select
define	CHECKSUM		  $1[4]		# check sum
define	XREG		  $1[5]		# x register
define	YREG		  $1[6]		# y register
define	ZREG		  $1[7]		# z register
define	TREG		  $1[8]		# t register

# Transfer ID definitions
define	IREAD		  100000B
define	IWRITE		       0B
define	PACKED		   40000B
define	BYPASSIFM	   20000B
define	BYTE		   10000B
define	ADDWRITE	    4000B
define	ACCUM		    2000B
define	BLOCKXFER	    1000B
define	VRETRACE	     400B
define	MUX32		     200B

# Subunits
define	REFRESH			1
define	LUT			2
define	OFM			3
define	IFM			4
define	FEEDBACK		5
define	SCROLL			6
define	VIDEOM			7
define	SUMPROC			8
define	GRAPHICS		9
define	CURSOR		       10
define	ALU		       11
define	ZOOM		       12
define	IPB		       15

# Command definitions
define	COMMAND		  100000B
define	ADVXONTC	  100000B		# Advance x on thing count
define	ADVXONYOV	   40000B		# Advance x on y overflow
define	ADVYONXOV	  100000B		# Advance y on x overflow
define	ADVYONTC	   40000B		# Advance y on thing count
define	ERASE		  100000B		# Erase

# 4 - Button Trackball
define	PUSH		   40000B
define	BUTTONA		     400B
define	BUTTONB		    1000B
define	BUTTONC		    2000B
define	BUTTOND		    4000B

# Display channels
define	CHAN1		       1B
define	CHAN2		       2B
define	CHAN3		       4B
define	CHAN4		      10B
define	ALLCHAN		      17B
define	GRCHAN		  100000B
define	GRCHNUM		      16

define	LEN_IISFRAMES		4
define	IISFRAMES	CHAN1, CHAN2, CHAN3, CHAN4

# Center coordinates for zoom/scroll
define	IIS_XCEN	      256
define	IIS_YCEN	      255
# Inverted Y center is just IIS_YDIM - IIS_YCEN
define	IIS_YCEN_INV	      256

# Colors

# these are bit plane mappings
define	BLUE		       1B
define	GREEN		       2B
define	RED		       4B
define	MONO		       7B
# next colors used by snap code ... used as array indexes.
define	BLU			1
define	GR			2
define	RD			3


# Bit plane selections
define	BITPL0		       1B
define	BITPL1		       2B
define	BITPL2		       4B
define	BITPL3		      10B
define	BITPL4		      20B
define	BITPL5		      40B
define	BITPL6		     100B
define	BITPL7		     200B
define	ALLBITPL	     377B

# IIS Sizes
define	IIS_XDIM	      512
define	IIS_YDIM	      512
define	MCXSCALE      	       64	# Metacode x scale
define	MCYSCALE      	       64	# Metacode y scale
define	SZB_IISHDR	       16	# Size of IIS header in bytes
define	LEN_ZOOM		3	# Zoom parameters
define	LEN_CURSOR		3	# Cursor parameters
define	LEN_SELECT	       12	# frame select
define	LEN_LUT		      256	# Look up table
define	LEN_OFM		     1024	# Output function look up table
define	LEN_IFM		     8192	# Input function look up table
define	LEN_VIDEOM	     2048	# videometer output memory
define	LEN_GRAM	      256	# graphics ram
define	MAXX		      512	# maximum x register + 1

# IIS Status Words
define	IIS_FILSIZE		(IIS_XDIM * IIS_YDIM * SZB_CHAR)
define	IIS_BLKSIZE		1
define	IIS_OPTBUFSIZE		32768
define	IIS_MAXBUFSIZE		32768
