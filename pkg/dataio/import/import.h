# IMPORT.H - Data structure definition file for the IMPORT task.

define	SZ_IMPSTRUCT	40		    # size of the import structure
define	SZ_EXPR		(20*SZ_LINE)        # max size of an expression
define  SZ_COMMENT      1024		    # size of a database format comment
define	LEN_UA		20000		    # minimum user header length
define	MAX_OPERANDS	1024

# Input format parameters.
define	IP_INTERLEAVE 	Memi[$1]	    # type of data interleaving
define	IP_HSKIP 	Memi[$1+1]	    # bytes to skip before data
define	IP_TSKIP 	Memi[$1+2]	    # bytes to skip after data
define	IP_BSKIP 	Memi[$1+3]	    # bytes between image bands
define	IP_LSKIP 	Memi[$1+4]	    # bytes to skip at front of line
define	IP_LPAD 	Memi[$1+5]	    # bytes to skip at end of line
define	IP_SWAP 	Memi[$1+6]	    # type of byte swapping
define	IP_NPIXT	Memi[$1+7]	    # number of pixtypes
define	IP_PIXTYPE	Memi[$1+8]	    # pixtype ptr to operands
define  IP_NDIM 	Memi[$1+9]	    # number of input axes
define  IP_AXLEN 	Memi[($1+10)+$2-1]  # input axis dimension

# Output parameters.
define	IP_OUTPUT 	Memi[$1+20]	    # type of output generated
define	IP_OUTTYPE 	Memi[$1+21]	    # output pixel type
define	IP_NBANDS 	Memi[$1+22]	    # no. of outbands expr string
define	IP_OUTBANDS 	Memi[$1+23]	    # outbands expr string (ptr)
define	IP_IMHEADER 	Memi[$1+24]	    # file w/ header info (ptr)
define	IP_VERBOSE 	Memi[$1+25]	    # verbose output flag

define	IP_FORMAT 	Memi[$1+26]	    # format param
define	IP_BLTIN 	Memi[$1+27]	    # format is a 'builtin'
define	IP_FCODE	Memi[$1+28]	    # builtin format code
define	IP_FSYM 	Memi[$1+29]	    # symtab pointer to db record
define	IP_IM 		Memi[$1+30]	    # output image pointer
define	IP_FD 		Memi[$1+31]	    # binary file pointer
define	IP_OFFSET	Memi[$1+32]	    # binary file offset
define	IP_FLIP		Memi[$1+33]	    # output image orientation flag
define	IP_COMPTR	Memi[$1+34]	    # comment block pointer

define	IP_BUFPTR	Memi[$1+35]   	    # array of image buffers (ptr)
define	IP_NPTRS	Memi[$1+36]   	    # number of image buffer
define	IP_SZBUF	Memi[$1+37]   	    # size of image buffer (lines)

define	IP_CMAP		Memi[$1+38]   	    # image colormap (ptr)
define	IP_USE_CMAP	Memi[$1+39]   	    # use the image colormap?

# Useful Macros
define	PTYPE		Memi[IP_PIXTYPE($1)+$2-1]
define	OBANDS		Memi[IP_OUTBANDS($1)+$2-1]
define	COMMENT		Memc[IP_COMPTR($1)]
define	BUFFER		Memi[IP_BUFPTR($1)+$2-1]


#-----------------------------------------------------------------------------

# Outbands structure
define  LEN_OUTBANDS    2
define	OB_EXPR		Memi[$1]		# expression string
define	OB_OP		Memi[$1+1]		# operand struct pointer
define	O_EXPR		Memc[OB_EXPR(OBANDS($1,$2))]
define	O_OP		OB_OP(OBANDS($1,$2))

# Operand structure
define	SZ_TAG		15
define  LEN_OPERAND     6
define  IO_TAG          Memi[$1]                # operand tag name
define  IO_TYPE         Memi[$1+1]              # operand type
define  IO_NBYTES       Memi[$1+2]              # number of bytes
define  IO_NPIX         Memi[$1+3]              # number of pixels
define  IO_DATA         Memi[$1+4]              # line of pixels
define	OP_TAG		Memc[IO_TAG($1)]


# Format type flags
define	IP_NONE		1		# format derived from task params
define	IP_SENSE	2		# format divined from database
define	IP_NAME		3		# format derived from database
define	IP_BUILTIN	4		# format derived from database

# Output type flags
define	IP_IMAGE	5		# generate an output image
define	IP_LIST		6		# list pixels (according to 'outbands')
define	IP_INFO		7		# print info about image format

# Byte swapping flags
define	S_NONE		000B		# swap nothing
define	S_ALL		001B		# swap everything
define	S_I2		002B		# swap short ints
define	S_I4		004B		# swap long ints
define  SWAP_STR        "|no|none|yes|i2|i4|"

# Image flipping flags
define	FLIP_NONE	000B		# don't flip the image
define	FLIP_X		001B		# flip image in X
define	FLIP_Y		002B		# flip image in Y

# Pixtype pixel types
define	PT_BYTE		1		# byte data (no conversion)
define	PT_UINT		2		# unsigned integer
define	PT_INT		3		# signed integer
define	PT_IEEE		4		# ieee floating point
define	PT_NATIVE	5		# native floating point
define	PT_SKIP		6		# skip

# Default task parameters.
define	DEF_SWAP 	S_NONE
define	DEF_INTERLEAVE	0
define	DEF_HSKIP	0
define	DEF_TSKIP	0
define	DEF_BSKIP	0
define	DEF_LSKIP	0
define	DEF_LPAD 	0

# Useful macros.
define	BAND_INTERLEAVED    ((IP_NPIXT($1)==1)&&(IP_INTERLEAVE($1)==0))
define	LINE_INTERLEAVED    ((IP_NPIXT($1)==1)&&(IP_INTERLEAVE($1)>1))
define	PIXEL_INTERLEAVED   ((IP_NPIXT($1)>1)&&(IP_INTERLEAVE(ip)==0))

# NTSC grayscale coefficients.
define  R_COEFF         0.299                   
define  G_COEFF         0.587
define  B_COEFF         0.114

# Colormap definitions.
define  CMAP_SIZE       256     	# Output colormap length
define  CMAP_MAX        255     	# Maximum map value
define  CMAP            Memc[$1+($2*CMAP_SIZE)+$3-1]

define  IP_RED          0
define  IP_GREEN        1
define  IP_BLUE         2

