# EXPORT.H -- Main include file for the task structure.

# Main task structure.
define	SZ_EXPSTRUCT	40
define	SZ_EXPSTR	(20*SZ_LINE)
define  EX_UNDEFINED    -999
define	MAX_OBEXPR	250
define  MAX_OPERANDS    50


define	EX_FD		Memi[$1]	    # output binary file descriptor
define	EX_HEADER	Memi[$1+1]	    # write an output header?
define	EX_OUTTYPE	Memi[$1+2]	    # outtype parameter value
define	EX_INTERLEAVE	Memi[$1+3]	    # interleave parameter value
define	EX_BSWAP	Memi[$1+4]	    # bswap parameter value
define	EX_VERBOSE	Memi[$1+5]	    # verbose parameter value

define	EX_FORMAT	Memi[$1+6]	    # format parameter code
define	EX_BLTIN	Memi[$1+7]	    # buitlin format code
define	EX_COLOR	Memi[$1+8]	    # does format support color?
define	EX_OROWS	Memi[$1+9]	    # no. rows in output image
define	EX_OCOLS	Memi[$1+10]	    # no. cols in output image

define	EX_IMDIM	Memi[$1+11]	    # input image list dimensionality
define	EX_IMTYPE	Memi[$1+12]	    # input image list type
define	EX_NIMAGES	Memi[$1+13]	    # number of images to convert
define	EX_NCOLS	Memi[$1+14]	    # number of columns in image
define	EX_NLINES	Memi[$1+15]	    # number of lines in image
define	EX_NEXPR	Memi[$1+16]	    # number of outbands expressions
define	EX_NIMOPS	Memi[$1+17]	    # image operand array (ptr)
define	EX_IMOPS	Memi[$1+18]	    # image operand array (ptr)

define	EX_OUTFLAGS	Memi[$1+20]	    # output format flags
define	EX_BFNPTR	Memi[$1+21]	    # binary file name (ptr)
define	EX_HDRPTR	Memi[$1+22]	    # user-defined head file (ptr)
define	EX_OTPTR	Memi[$1+23]	    # output type string (ptr)
define	EX_OBPTR	Memi[$1+24]	    # outbands expression string (ptr)
define	EX_CMPTR	Memi[$1+25]	    # colormap filename (ptr)
define	EX_LUTPTR	Memi[$1+26]	    # LUT filename (ptr)
define	EX_TIMPTR	Memi[$1+27]	    # temp image name (ptr)
define	EX_PSDPI	Memr[P2R($1+28)]    # EPS dpi resolution
define	EX_PSSCALE	Memr[P2R($1+29)]    # EPS scale
define	EX_BRIGHTNESS	Memr[P2R($1+30)]    # display brightness value
define	EX_CONTRAST	Memr[P2R($1+31)]    # display contrast value

define 	EX_CMAP		Memi[$1+32]	    # colormap struct (ptr)
define 	EX_NCOLORS	Memi[$1+33]	    # no. of colors in colormap
define 	EX_LUT		Memi[$1+34]	    # LUT struct (ptr)
define 	EX_NLUTEL	Memi[$1+35]	    # no. of indices in lut
define 	EX_OBANDS	Memi[$1+36]  	    # outbands array (ptr)


# Handy macros.
define	HDRFILE		Memc[EX_HDRPTR($1)]
define	LUTFILE		Memc[EX_LUTPTR($1)]
define	CMAPFILE	Memc[EX_CMPTR($1)]
define	BFNAME		Memc[EX_BFNPTR($1)]
define	TIMNAME		Memc[EX_TIMPTR($1)]
define	OBANDS		Memi[EX_OBANDS($1)+$2-1]
define	IMOP		Memi[EX_IMOPS($1)+$2-1]


# Define the outbands struct.
define	LEN_OUTBANDS	5
define	OB_EXPSTR	Memi[$1]	# expression string (ptr)
define	OB_WIDTH	Memi[$1+1]	# expression width
define	OB_HEIGHT	Memi[$1+2]	# expression height

define	O_EXPR		Memc[OB_EXPSTR(OBANDS($1,$2))]
define	O_WIDTH		OB_WIDTH(OBANDS($1,$2))
define	O_HEIGHT	OB_HEIGHT(OBANDS($1,$2))


# Operand structure.
define  LEN_OPERAND     10
define  IO_IMPTR        Memi[$1]                # image descriptor
define  IO_BAND         Memi[$1+1]              # image band
define  IO_LINE         Memi[$1+2]              # image line

define  IO_TAG          Memi[$1+3]              # operand tag name
define  IO_TYPE         Memi[$1+4]              # operand type
define  IO_NBYTES       Memi[$1+5]              # number of bytes
define  IO_NPIX         Memi[$1+6]              # number of pixels
define  IO_DATA         Memi[$1+7]              # pixel ptr
define  IO_ISIM         Memi[$1+8]              # is data an image ptr?

define  OP_TAG          Memc[IO_TAG($1)]

#-----------------------------------------------------------------------------
# Useful Macro Definitions.

define	bitset		(and($1,$2)==($2))

# Format flags.
define	FMT_RAW		1		# write a generic binary raster
define	FMT_LIST	2		# list pixels values to the screen
define	FMT_BUILTIN	3		# write a builtin format

# OUTPUT FLAGS:
# Byte swapping flags.
define  S_NONE          0000B    	# swap nothing
define  S_ALL           0001B    	# swap everything
define  S_I2            0002B    	# swap short ints
define  S_I4            0004B    	# swap long ints
define  SWAP_STR        "|no|none|yes|i2|i4|"

# Pixel storage flags.
define	PIXEL_STORAGE	0001B		# { {RGB} {RGB} ... {RGB} ... }
define	LINE_STORAGE	0002B		# { {RRRR} {GGG} {BBB} .... {RRR} ... }
define	BAND_STORAGE	0004B		# { {RR..RRR} {GG...GGG} {BB..BBB} }

# Output flags.
define  OF_CMAP        00010B    	# a colormap was defined
define  OF_MKCMAP      00020B    	# compute a colormap
define  OF_BAND        00040B    	# force band storage
define  OF_LINE        00100B    	# force line storage
define  OF_FLIPX       00200B    	# flip image in X
define  OF_FLIPY       00400B    	# flip image in Y
define	OF_IEEE        01000B		# write IEEE floating point

# Header flags.
define  HDR_NONE        1               # no output header
define  HDR_SHORT       2               # write a short header
define  HDR_LONG        3               # write a verbose header
define  HDR_USER        4               # user defined a file

# Pixtype pixel types.
define  PT_BYTE         1               # byte data (no conversion)
define  PT_UINT         2               # unsigned integer
define  PT_INT          3               # signed integer
define  PT_IEEE         4               # ieee floating point
define  PT_NATIVE       5               # native floating point
define  PT_SKIP         6               # skip

# EPS output params.
define	EPS_DPI		72		# dpi resolution
define	EPS_SCALE	1.0		# output scale

# Define colormap/grayscale macros and parameters.
define  CMAP_SIZE       256             # Output colormap length
define  CMAP_MAX        255             # Maximum map value
define  CMAP            Memc[$1+($2*CMAP_SIZE)+$3-1]

define  R_COEFF         0.299           # NTSC grayscale coefficients
define  G_COEFF         0.587
define  B_COEFF         0.114

define  EX_RED          0		# color flags
define  EX_GREEN        1
define  EX_BLUE         2

define  SAMPLE_SIZE     10000		# default zscale() sample size
define  CONTRAST        0.25		# default zscale() contrast
define  SAMP_LEN        40		# default zscale() sample length

