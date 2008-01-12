# The ICFIT data structure

define	IC_NGKEYS	5		# Number of graph keys
define	IC_LENSTRUCT	44		# Length of ICFIT structure
define	IC_SZSAMPLE	1024		# Size of sample string

# User fitting parameters
define	IC_FUNCTION	Memi[P2I($1)]	# Function type
define	IC_ORDER	Memi[P2I($1+1)]	# Order of function
define	IC_SAMPLE	Memi[P2I($1+2)]	# Pointer to sample string
define	IC_NAVERAGE	Memi[P2I($1+3)]	# Sampling averaging bin
define	IC_NITERATE	Memi[P2I($1+4)]	# Number of rejection interation
define	IC_XMIN		Memr[P2R($1+5)]	# Minimum value for curve
define	IC_XMAX		Memr[P2R($1+6)]	# Maximum value for curve
define	IC_LOW		Memr[P2R($1+7)]	# Low rejection value
define	IC_HIGH		Memr[P2R($1+8)]	# Low rejection value
define	IC_GROW		Memr[P2R($1+9)]	# Rejection growing radius

# ICFIT parameters used for fitting
define	IC_NFIT		Memi[P2I($1+10)]	# Number of fit points
define	IC_NREJECT	Memi[P2I($1+11)]	# Number of rejected points
define	IC_RG		Memi[P2I($1+12)]	# Pointer for ranges
define	IC_XFIT		Memi[P2I($1+13)]	# Pointer to ordinates of fit points
define	IC_YFIT		Memi[P2I($1+14)]	# Pointer to abscissas of fit points
define	IC_WTSFIT	Memi[P2I($1+15)]	# Pointer to weights of fit points
define	IC_REJPTS	Memi[P2I($1+16)]	# Pointer to rejected points

# ICFIT parameters used for interactive graphics
define	IC_NEWX		Memi[P2I($1+17)]	# New x fit points?
define	IC_NEWY		Memi[P2I($1+18)]	# New y points?
define	IC_NEWWTS	Memi[P2I($1+19)]	# New weights?
define	IC_NEWFUNCTION	Memi[P2I($1+20)]	# New fitting function?
define	IC_COLOR	Memi[P2I($1+21)]	# Fit color
define	IC_OVERPLOT	Memi[P2I($1+22)]	# Overplot next plot?
define	IC_FITERROR	Memi[P2I($1+23)]	# Error in fit
define	IC_MARKREJ	Memi[P2I($1+24)]	# Mark rejected points?
define	IC_LABELS	Memi[P2I($1+25+$2-1)]# Graph axis labels
define	IC_UNITS	Memi[P2I($1+27+$2-1)]# Graph axis units
define	IC_HELP		Memi[P2I($1+29)]	# Pointer to help file name
define	IC_GP		Memi[P2I($1+30)]	# GIO pointer
define	IC_GT		Memi[P2I($1+31)]	# GTOOLS pointer

# ICFIT key definitions
define	IC_GKEY		Memi[P2I($1+32)]			# Graph key
define	IC_AXES		Memi[P2I($1+33+($2-1)*2+$3-1)]	# Graph axis codes

# Default help file and prompt
define	IC_DEFHELP	"noao$base/scr/icgfit.key"
define	IC_DEFHTML	"noao$base/scr/icgfit.html"
define	IC_PROMPT	"icfit cursor options"
