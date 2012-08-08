# Grow parameter structure

define	SKY_LEN		8		# Length of parameter structure
define	SKY_STRLEN	9		# Length of string

define	SKY_TYPE	Memi[$1]	# Type of sky
define	SKY_SKF		Memi[$1+1]	# Sky fit parameters
define	SKY_SKB		Memi[$1+2]	# Sky block parameters
define	SKY_STR		Memc[P2C($1+3)]	# String


define	SKY_TYPES	"|fit|block|"
define	SKY_FIT		1		# Sky fitting algorithm
define	SKY_BLOCK	2		# Sky block algorithm
