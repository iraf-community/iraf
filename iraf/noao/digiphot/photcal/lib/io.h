# The include file for PHOTCAL i/o

# Formatting definitions for MKCATALOG

define	MKCAT_KYCATALOG	   "CATALOG:"	# the catalog title keyword
define	MKCAT_KYNCOLS	   "NCOLS:"	# the number of columns keyword 
define	MKCAT_KYHDRLENGTH  "HDRLENGTH:" # the column header length keyword

define	MKCAT_MAXCOLWIDTH	79	# the maximum column width
define	MKCAT_IDCOLWIDTH 	15	# the default id column width
define	MKCAT_COLWIDTH	 	10	# the default data column width
define	MKCAT_NCOLS		10	# the default number of columns
define	MKCAT_SZGAP		2	# the size of the intercolumn gap

define	MKCAT_COMMENTSTR  "# "		# the comment string
define	MKCAT_CONTSTR	  "* "		# the continuation string
define	MKCAT_BLANKSTR    "  "		# the blank string
define	MKCAT_NULLSTR	  ""		# the null string
define	MKCAT_SZSTR	  2		# the size of MKCAT strings

define	MKCAT_COMMENTCHAR '#'		# the comment character


# Define the initial sizes for internal table storage.

define	LEN_CATDAT	100
define	LEN_CATOBS	100
define	LEN_OBSOBS	100


# General catalog and observations formatting definitions

define	COMMENT		"^\#"		# the comment string
define	CONTINUATION	"^\* "		# the continuation character

# Maximum number of continuation lines. This constant is used only for
# memory allocation and not to check for the number of lines.

define	MAX_CONT	10


# Record names for FITPARAMS output file. These are the record names
# used by the DTTEXT procedures to identify records in a text database.

define	STATUS		"status"		# fit status
define	WEIGHTING	"weights"		# weights definition
define	ERRORS		"errors"		# error definition
define	VARIANCE	"variance"		# fit variance
define	STDEV		"stdeviation"		# fit standard deviation
define	AVSQERROR	"avsqerror"		# average error squared
define	AVERROR		"averror"		# average error
define	AVSQSCATTER	"avsqscatter"		# additional scatter-squared
define	AVSCATTER	"avscatter"		# additional scatter
define	MSQ		"msq"			# fit mean-square
define	RMS		"rms"			# fit root-mean-square
define	CHISQR		"chisqr"		# fit reduced chi-squared
define	REFERENCE	"reference"		# reference equation name
define	FITTING		"fitting"		# fitting equation name
define	DERIVATIVES	"derivatives"		# derivative equation names
define	PARAMETERS	"parameters"		# parameter names
define	VALUES		"values"		# parameter values
define	ERRORS		"errors"		# parameter errors


# Define the permitted output types for EVALFIT and INVERTFIT

define	TYPE_ALL	1
define	TYPE_PROGRAM	2
define	TYPE_STANDARDS	3
define	TYPE_STRING	",all,program,standards,"


# Define the permitted output error types for EVALFIT and INVERTFIT

define	ERR_UNDEFINED	1
define	ERR_OBSERRORS	2
define	ERR_EQUATIONS	3
define	ERR_OPTIONS	",undefined,obserrors,equations,"
