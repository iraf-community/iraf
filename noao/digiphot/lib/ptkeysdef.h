# Header file for apselect keywords

define	LEN_KEYSTRUCT	30

# numbers of keys

define	KY_NKEYS	Memi[$1]	# total number of keys
define	KY_NPKEYS	Memi[$1+1]	# number of parameter keys
define	KY_NSTORE	Memi[$1+2]	# amount of storage space for keys
#define	KY_NOKEYS	Memi[$1+3]	# number of defined keys before table

# keyword strings

define	LEN_KWORDS	Memi[$1+4]	# length of the keyword string
define	KY_WORDS	Memi[$1+5]	# pointer to the keywords string
define	KY_VALUES	Memi[$1+6]	# pointer to the values string
define	KY_UNITS	Memi[$1+7]	# pointer to the units string
define	KY_FORMATS	Memi[$1+8]	# pointer to the format string

# indices

define	KY_PTRS		Memi[$1+9]	# pointer to values array
define	KY_NELEMS	Memi[$1+10]	# pointer to number of elems array
define	KY_TYPES	Memi[$1+11]	# pointer to the keyword data type array
define	KY_KINDICES	Memi[$1+12]	# pointer to the keyword indices
define	KY_UINDICES	Memi[$1+13]	# pointer to the unit indices
define	KY_FINDICES	Memi[$1+14]	# pointer to the format indices
define	KY_NPLINE	Memi[$1+15]	# pointer to values per line array
define	KY_NCONTINUE	Memi[$1+16]	# pointer to max no of continuations

# select buffers

define	KY_NSELECT	Memi[$1+18]	# number of selected keys
define  KY_SELECT	Memi[$1+19]	# indices to selected fields
define	KY_ELEM_SELECT	Memi[$1+20]	# index of element to be selected
define	KY_LEN_SELECT	Memi[$1+21]	# lengths of the selected fields
define	KY_NAME_SELECT	Memi[$1+22]	# pointer to string of selected names
define	KY_UNIT_SELECT	Memi[$1+23]	# pointer to string of selected units
define	KY_FMT_SELECT	Memi[$1+24]	# pointer to string of selected formats


# test for apphot/daophot database format

define	KY_CHAR_IRAF	"^\#K IRAF"

# some important character strings

define	KY_CHAR_KEYWORD "#K "	
define	KY_CHAR_NAME	"#N "	
define	KY_CHAR_UNITS	"#U "	
define	KY_CHAR_FORMAT	"#F "	
define	KY_LEN_STR	3

define	KY_CHAR_POUND	'#'	
define	KY_CHAR_NEWLINE	'\n'	
define	KY_CHAR_CONT	'\\'	


# some useful constants

define	KY_SZPAR	23		# assumed number of chars in a parameter
define	KY_NPARS	50		# initial guess at number of parameters
define	KY_NLINES	20		# maximum number of lines per record

define	KY_MAXNKEYWORDS	150		# maximum number of keywords.
#define	KY_MAXNNAMES	150		# maximum number of column names
define	KY_MAXNRANGES	150		# maximum number of reanges

# names of parameter which can be extracted from the keys database

define	KY_INDEX	1
define	KY_DATATYPE	2
define	KY_LENGTH	3
define	KY_ELEMENT	4
define  KY_NUMELEMS	5
define	KY_UNITSTR	6
define	KY_FMTSTR	7
