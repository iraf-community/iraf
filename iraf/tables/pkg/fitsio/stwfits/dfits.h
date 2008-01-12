# DFITS Definitions
# Define size of tables in memory
 
define	MAX_TABLE	30	# number of keywords and formats
define	MAX_CARDS	100	# number of cards
 
# Define sizes of keywords and formats
 
define	SZ_KEYWORD	8	# length of keywords in characters
define	SZ_FORMAT	8	# length of formats in characters (%-dd.ddc)
 
# Define test of formats
 
define	IS_STRING	($1 == 's')
define	IS_INTEGER	($1 == 'd' || $1 == 'o' || $1 == 'x')
define	IS_FLOAT	(($1 >= 'e' && $1 <= 'h') || $1 == 'm')
define	IS_FORMAT	(IS_STRING($1) || IS_INTEGER($1) || IS_FLOAT($1))
 
# Define possible formats
 
define	FORMAT_DICT	"defghmosx"
