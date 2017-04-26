# PRINTF.H -- Printf definitions.

define	SZ_OBUF			1024	# fmtio output buffer
define	USE_DEFAULT		(-999)	# flag to use default value
define	DECIMAL			10	# default radix
define	OCTAL			8
define	HEX			16
define	TABSTOP			8
define	START_OF_FORMAT		'%'	# begin a format specification
define	GET_FIELD		'*'	# get W,D,or C field from parg call
define	OVFL_CHAR		'*'	# output "***" if not enough room

# Format specification characters.  ("%w.dc")

define	FMT_BOOL		'b'	# print a boolean ("yes" or "no")
define	FMT_CHAR		'c'	# print a character constant
define	FMT_DECIMAL		'd'	# print a decimal integer
define	FMT_EXPON		'e'	# print in Fortran 'E' format
define	FMT_FIXED		'f'	# print in Fortran 'F' (fixed) format
define	FMT_GENERAL		'g'	# print the smaller of E or F formats
define	FMT_HMS			'h'	# print as "nn:nn:nn.nnn"
define	FMT_MINSEC		'm'	# print as "nn:nn.nnn"
define	FMT_OCTAL		'o'	# print octal integer
define	FMT_RADIX		'r'	# "%w.drN": print integer, radix N
define	FMT_STRING		's'	# print a string
define	FMT_TOCOLUMN		't'	# "%Nt": advance to column N
define	FMT_UNSIGNED		'u'	# print an unsigned integer
define	FMT_WHITESPACE		'w'	# "%Nw": output N blanks
define	FMT_HEX			'x'	# print a hex integer
define	FMT_COMPLEX		'z'	# print a complex number "(r,r)"


# for fmt_init()

define	FMT_INITIALIZE		0
define	REGULAR_FILE		1
define	STRING_FILE		2
define	CL_PARAM		3


# FPRFMT states (while interpreting a "%w.dC" format spec)

define	FMT_START		1
define	GET_WIDTH_1		2
define	GET_WIDTH_2		3
define	GET_DECPL		4
define	GET_FMTCHAR		5
define	GET_RADIX		6
define	GET_OPERAND		7

define	NOT_DONE_YET		0		# return values
define	ALL_DONE		1
