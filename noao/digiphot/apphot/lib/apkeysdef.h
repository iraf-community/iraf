# Header file for apselect keywords

define	LEN_KEYSTRUCT	25

# numbers of keys
define	KY_NKEYS	Memi[$1]	# total number of keys
define	KY_NOKEYS	Memi[$1+1]	# number of defined keys before table
define	KY_NPKEYS	Memi[$1+2]	# number of parameter keys
define	KY_NSTORE	Memi[$1+3]	# amount of storage space for keys
define	KY_NSELECT	Memi[$1+4]	# number of selected keys

# length of keyword string dictionary
define	LEN_KWORDS	Memi[$1+5]	# length of the keyword string

# keyword strings
define	KY_WORDS	Memi[$1+8]	# pointer to the keywords string
define	KY_VALUES	Memi[$1+9]	# pointer to the values string
define	KY_UNITS	Memi[$1+10]	# pointer to the units string
define	KY_FORMATS	Memi[$1+11]	# pointer to the format string

# indices
define	KY_PTRS		Memi[$1+12]	# pointer to values array
define	KY_NELEMS	Memi[$1+13]	# pointer to number of elems array
define	KY_TYPES	Memi[$1+14]	# pointer to the keyword data type array
define	KY_KINDICES	Memi[$1+15]	# pointer to the keyword indices
define	KY_UINDICES	Memi[$1+16]	# pointer to the unit indices
define	KY_FINDICES	Memi[$1+17]	# pointer to the format indices

define	KY_NPLINE	Memi[$1+18]	# pointer to values per line array

define  KY_SELECT	Memi[$1+19]	# indices to selected fields
define	KY_LEN_SELECT	Memi[$1+20]	# lengths of the selected fields

# define some constants

define	SZ_PAR		20		# assumed number of chars in a parameter
define	NPARS		50		# initial guess at number of parameters
define	NLINES		20		# lines per record
