# Lexer structure lengths
define	LEN_ID		SZ_LINE			# string length
define	LEN_CODE	SZ_LINE			# code length
define	LEN_LEX		(3+LEN_ID+LEN_CODE)	# total structure length

# Lexer structure
define	LEX_TOK		Memi[$1+0]		# token
define	LEX_VAL		Memr[P2R($1+1)]		# numeric value (if any)
define	LEX_ID		Memc[P2C($1+2)]		# character string
define	LEX_CLEN	Memi[$1+LEN_ID+2]	# code length
define	LEX_CODE	($1+LEN_ID+3)		# RPN code buffer


# Minimum number of characters required when a keyword is abbreviated
define	ABBREVIATE	3


# Keywords
define	KEYWORDS	"|catalog|observations|extinction|transformations|\
			 |fit|constant|delta|\
			 |error|weight|minimum|maximum|\
			 |set|derivative|plot|"

# Keyword codes
define	K_CATALOG		1	# catalog section
define	K_OBSERVATION		2	# observation section
define	K_EXTINCTION		3	# extinction section
define	K_TRANSFORMATION	4	# transformation section
	# newline		5
define	K_FIT			6	# fitting parameter declaration
define	K_CONSTANT		7	# constant parameter declaration
define	K_DELTA			8	# parameter delta declaration
	# newline		9
define	K_ERROR			10	# error column/equation
define	K_WEIGHT		11	# weight column/equation
define	K_MIN			12	# minimum equation (error/weight)
define	K_MAX			13	# maximum equation (error/weight)
	# newline		14
define	K_SET			15	# set equation
define	K_DERIVATIVE		16	# derivative equation
define	K_PLOT			17	# plot equations


# Functions
define	FUNCTIONS	"|abs|acos|asin|atan|cos|exp|log|log10|sin|\
			 |sqrt|tan|"

# Function codes
define	K_ABS			1	# absolute value
define	K_ACOS			2	# arccosine
define	K_ASIN			3	# arcsine
define	K_ATAN			4	# arctangent
define	K_COS			5	# cosine
define	K_EXP			6	# sine
define	K_LOG			7	# natural logarithm
define	K_LOG10			8	# decimal logarithm
define	K_SIN			9	# sine
	# newline		10
define	K_SQRT			11	# square root
define	K_TAN			12	# tangent
