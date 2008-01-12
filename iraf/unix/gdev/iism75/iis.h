# IIS.H -- Hardware definitions for the IIS models 70 and 75.

# Define header
define	LEN_IISHDR	8		# Length of IIS header

define	XFERID		$1[1]		# transfer id
define	THINGCT		$1[2]		# thing count
define	SUBUNIT		$1[3]		# subuint select
define	CHECKSUM	$1[4]		# check sum
define	XREG		$1[5]		# x register
define	YREG		$1[6]		# y register
define	ZREG		$1[7]		# z register
define	TREG		$1[8]		# t register

# Transfer ID definitions
define	BYTEORDER	      20B
define	PMA		      40B
define	ACCELERATE	     100B
define	REPEAT		     200B
define	IREAD		  100000B
define	IWRITE		  000000B
define	PACKED		   40000B
define	BYPASSIFM	   20000B
define	PAGEMODE	   10000B
define	ADDWRITE	    4000B
define	ACCUM		    2000B
define	BLOCKXFER	    1000B
define	VRETRACE	     400B

define	M70_BYTE	   10000B
define	M70_MUX32	     200B

# Subunits
define	REFRESH			1
define	LUT			2
define	OFM			3
define	IFM			4
define	FEEDBACK		5
define	SCROLLZOOM		6
define	VIDEOM			7
define	SUMPROC			8
define	GRAPHICS		9
define	CURSOR		       10
define	ALU		       11

define	M70_SCROLL		6
define	M70_ZOOM	       12

# Command definitions
define	COMMAND		  100000B
define	ERASE		  100000B		# Erase

define	SCROLL		       1B
define	ZOOM		   10000B
define	WRAP		    1000B

define	M70_ADVXONTC	  100000B		# Advance x on thing count
define	M70_ADVXONYOV	   40000B		# Advance x on y overflow
define	M70_ADVYONXOV	  100000B		# Advance y on x overflow
define	M70_ADVYONTC	   40000B		# Advance y on thing count

define	M75_ADVXONTC	     400B		# Advance x on thing count
define	M75_ADVXONYOV	     200B		# Advance x on y overflow
define	M75_ADVYONXOV	    2000B		# Advance y on x overflow
define	M75_ADVYONTC	    4000B		# Advance y on thing count

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
define	GRCHAN		  100000B

define	LEN_IISFRAMES		4
define	IISFRAMES		CHAN1, CHAN2, CHAN3, CHAN4

# Colors
define	BLUE		       1B
define	GREEN		       2B
define	RED		       4B
define	MONO		       7B

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
define	MCXSCALE      	       64	# Metacode x scale
define	MCYSCALE      	       64	# Metacode y scale
define	IIS_XDIM	      512
define	IIS_YDIM	      512
define	SZB_IISHDR	       16	# Size of IIS header in bytes
