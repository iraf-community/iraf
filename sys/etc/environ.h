# ENVIRON.H -- Global defines for the environment list package.

# Strings may optionally be quoted in SET stmts with either ' or ".
define	IS_QUOTE	($1 == '\'' || $1 == '"')

# Size limiting definitions.

define	NTHREADS	199		# number of hash threads
define	MAX_HASHCHARS	18		# max chars to use for hashing
define	LEN_ENVBUF	20480		# storage for environment list
define	INC_ENVBUF	4096		# increment if overflow occurs
define	MAX_SZKEY	32		# max chars in a key
define	MIN_SZVALUE	20		# min allocated space for value
define	MAX_SZVALUE	4096		# max chars in value string
define	MAX_LENLISTELEM	(4+(MAX_SZKEY+1+MAX_SZVALUE+1+SZ_SHORT-1)/SZ_SHORT)

# List element structure, stored in ENVBUF, which is allocated as an array of
# type SHORT integer.  Each list element is aligned on a short integer boundary
# within the array.  E_NEXT points to the next element in a thread, whereas
# E_LASTELEM points to the last element in the envbuf (which is a stack).

define	E_NEXT		Mems[$1]	# next element in thread (list)
define	E_LASTELEM	Mems[$1+1]	# next element in envbuf
define	E_REDEF		Mems[$1+2]	# set if element is redefined
define	E_LEN		Mems[$1+3]	# nchars allocated for value string
define	E_SETP		(($1+4-1)*SZ_SHORT+1)	# char pointer to name field
define	E_SET		Memc[E_SETP($1)]	# "name=value" string
define	E_SETOFFSET	4
