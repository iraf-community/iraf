# Sky surface algorithm definitions.

define	SKF_LEN			16	# Length of parameter structure
define	SKF_STRLEN		9	# Length of string

define	SKF_STEP	Memr[P2R($1)]	# Number of sky lines to sample
define	SKF_LMIN	Memr[P2R($1+1)]	# Minimum number of lines to fit
define	SKF_FUNC1D	Memi[$1+2]	# 1D Fitting function
define	SKF_FUNC2D	Memi[$1+3]	# 2D Fitting function
define	SKF_XORDER	Memi[$1+4]	# Sky fitting x order
define	SKF_YORDER	Memi[$1+5]	# Sky fitting y order
define	SKF_XTERMS	Memi[$1+6]	# Sky fitting cross terms
define	SKF_BLK1D	Memi[$1+7]	# Sky block size for 1D averages
define	SKF_HCLIP	Memr[P2R($1+8)]	# Sky fitting high sigma clip
define	SKF_LCLIP	Memr[P2R($1+9)]	# Sky fitting low sigma clip
define	SKF_NITER	Memi[$1+10]	# Number of iterations
define	SKF_STR		Memc[P2C($1+11)]	# String


define	SKFLMIN			10	# Minimum number of lines to fit
define	SKFFUNC1D	"chebyshev"	# 1D fitting function
define	SKFFUNC2D	"chebyshev"	# 2D fitting function
define	SKFXTERMS	"half"		# Cross terms
define	SKFNITER		5	# Number of iterations
