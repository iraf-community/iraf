# The ICFIT data structure

define	IC_NGKEYS	5		# Number of graph keys
define	IC_LENSTRUCT	44		# Length of ICFIT structure
define	IC_SZSAMPLE	1024		# Size of sample string

# User fitting parameters
define	IC_FUNCTION	Memi[$1]	# Function type
define	IC_ORDER	Memi[$1+1]	# Order of function
define	IC_SAMPLE	Memi[$1+2]	# Pointer to sample string
define	IC_NAVERAGE	Memi[$1+3]	# Sampling averaging bin
define	IC_NITERATE	Memi[$1+4]	# Number of rejection interation
define	IC_XMIN		Memr[P2R($1+5)]	# Minimum value for curve
define	IC_XMAX		Memr[P2R($1+6)]	# Maximum value for curve
define	IC_LOW		Memr[P2R($1+7)]	# Low rejection value
define	IC_HIGH		Memr[P2R($1+8)]	# Low rejection value
define	IC_GROW		Memr[P2R($1+9)]	# Rejection growing radius

# ICFIT parameters used for fitting
define	IC_NFIT		Memi[$1+10]	# Number of fit points
define	IC_NREJECT	Memi[$1+11]	# Number of rejected points
define	IC_RG		Memi[$1+12]	# Pointer for ranges
define	IC_XFIT		Memi[$1+13]	# Pointer to ordinates of fit points
define	IC_YFIT		Memi[$1+14]	# Pointer to abscissas of fit points
define	IC_WTSFIT	Memi[$1+15]	# Pointer to weights of fit points
define	IC_REJPTS	Memi[$1+16]	# Pointer to rejected points

# ICFIT parameters used for interactive graphics
define	IC_NEWX		Memi[$1+17]	# New x fit points?
define	IC_NEWY		Memi[$1+18]	# New y points?
define	IC_NEWWTS	Memi[$1+19]	# New weights?
define	IC_NEWFUNCTION	Memi[$1+20]	# New fitting function?
define	IC_COLOR	Memi[$1+21]	# Fit color
define	IC_OVERPLOT	Memi[$1+22]	# Overplot next plot?
define	IC_FITERROR	Memi[$1+23]	# Error in fit
define	IC_MARKREJ	Memi[$1+24]	# Mark rejected points?
define	IC_LABELS	Memi[$1+25+$2-1]# Graph axis labels
define	IC_UNITS	Memi[$1+27+$2-1]# Graph axis units
define	IC_HELP		Memi[$1+29]	# Pointer to help file name
define	IC_GP		Memi[$1+30]	# GIO pointer
define	IC_GT		Memi[$1+31]	# GTOOLS pointer

# ICFIT key definitions
define	IC_GKEY		Memi[$1+32]			# Graph key
define	IC_AXES		Memi[$1+33+($2-1)*2+$3-1]	# Graph axis codes

# Default help file and prompt
define	IC_DEFHELP	"noao$lib/scr/icgfit.key"
define	IC_DEFHTML	"noao$lib/scr/icgfit.html"
define	IC_PROMPT	"icfit cursor options"
