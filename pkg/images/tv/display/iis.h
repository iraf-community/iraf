# This file contains the hardware definitions for the iis model 70/f
# at Kitt Peak.

# Define header
define  LEN_IISHDR      8               # Length of IIS header

define  XFERID            $1[1]         # transfer id
define  THINGCT           $1[2]         # thing count
define  SUBUNIT           $1[3]         # subuint select
define  CHECKSUM          $1[4]         # check sum
define  XREG              $1[5]         # x register
define  YREG              $1[6]         # y register
define  ZREG              $1[7]         # z register
define  TREG              $1[8]         # t register


# Transfer ID definitions
define  IREAD             100000B
define  IWRITE                 0B
define  PACKED             40000B
define  SAMPLE             40000B
define  BYPASSIFM          20000B
define  BYTE               10000B
define  ADDWRITE            4000B
define  ACCUM               2000B
define  BLOCKXFER           1000B
define  VRETRACE             400B
define  MUX32                200B
define  IMT800               100B       # [IMTOOL SPECIAL]

# Subunits
define  REFRESH                 1
define  LUT                     2
define  OFM                     3
define  IFM                     4
define  FEEDBACK                5
define  SCROLL                  6
define  VIDEOM                  7
define  SUMPROC                 8
define  GRAPHICS                9
define  CURSOR                  10
define  ALU                     11
define  ZOOM                    12
define  IMCURSOR                20B
define  WCS	                21B

# Command definitions
define  COMMAND           100000B
define  ADVXONTC          100000B               # Advance x on thing count
define  ADVXONYOV          40000B               # Advance x on y overflow
define  ADVYONXOV         100000B               # Advance y on x overflow
define  ADVYONTC           40000B               # Advance y on thing count
define  ERASE             100000B               # Erase

# 4 - Button Trackball
define  PUSH               40000B
define  BUTTONA              400B
define  BUTTONB             1000B
define  BUTTONC             2000B
define  BUTTOND             4000B

# Display channels
define  CHAN1                  1B
define  CHAN2                  2B
define  CHAN3                  4B
define  CHAN4                 10B
define  CHAN5                 20B
define  CHAN6                 40B
define  CHAN7                100B
define  CHAN8                200B
define  CHAN9                400B
define  CHAN10              1000B
define  CHAN11              2000B
define  CHAN12              4000B
define  CHAN13             10000B
define  CHAN14             20000B
define  CHAN15             40000B
define  CHAN16            100000B
define  GRCHAN            100000B

define  LEN_IISFRAMES          16
define  IISFRAMES       CHAN1, CHAN2, CHAN3, CHAN4, CHAN5, CHAN6, CHAN7, CHAN8, CHAN9, CHAN10, CHAN11, CHAN12, CHAN13, CHAN14, CHAN15, CHAN16

# Colors

define  BLUE                   1B
define  GREEN                  2B
define  RED                    4B
define  MONO                   7B

# Bit plane selections
define  BITPL0                 1B
define  BITPL1                 2B
define  BITPL2                 4B
define  BITPL3                10B
define  BITPL4                20B
define  BITPL5                40B
define  BITPL6               100B
define  BITPL7               200B
define  ALLBITPL             377B

# IIS Sizes
define  IIS_XDIM              512
define  IIS_YDIM              512
define  MCXSCALE               64       # metacode x scale
define  MCYSCALE               64       # metacode y scale
define  SZB_IISHDR             16       # size of IIS header in bytes
define  SZB_IMCURVAL          160       # size of imcursor value buffer, bytes
define  LEN_ZOOM                3       # zoom parameters
define  LEN_CURSOR              3       # cursor parameters
define  LEN_SPLIT              12       # split screen
define  LEN_LUT               256       # look up table
define  LEN_OFM              1024       # output function look up table
define	SZ_OLD_WCSTEXT	      320	# old max WCS text chars
define	SZ_WCSTEXT	     1024	# max WCS text chars

# IIS Status Words
define  IIS_FILSIZE             (IIS_XDIM * IIS_YDIM * SZB_CHAR)
define  IIS_BLKSIZE             1024
define  IIS_OPTBUFSIZE          (IIS_XDIM * SZB_CHAR)
define  IIS_MAXBUFSIZE          32768
