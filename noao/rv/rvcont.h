# Include file for the CONTINPARS structure.  A pointer is allocated in
# the main RV structure into this one.  This sub-structure contains the
# parameters used for continuum removal.

define	SZ_CONT_STRUCT	     16

define	CON_INTERACTIVE	     Memi[RV_CONT($1)]		# Process interactively?
define	CON_CNFUNC	     Memi[RV_CONT($1)+1]	# Interpolation func
define	CON_ORDER	     Memi[RV_CONT($1)+2]	# Order of func
define	CON_LOWREJECT	     Memr[P2R(RV_CONT($1)+3)]	# Low rejection
define	CON_HIGHREJECT	     Memr[P2R(RV_CONT($1)+4)]	# High rejection
define  CON_REPLACE	     Memi[RV_CONT($1)+5]	# Function type (ptr)
define	CON_NITERATE	     Memi[RV_CONT($1)+6]	# No. of iterations
define	CON_GROW	     Memr[P2R(RV_CONT($1)+7)]	# Growth radius
define  CON_SAMPLE	     Memi[RV_CONT($1)+8]	# Sample string (ptr)
define  CON_NAVERAGE	     Memi[RV_CONT($1)+9]	# Npts to average
define  CON_FUNC	     Memi[RV_CONT($1)+10]	# Function type (ptr)
define  CON_MARKREJ	     Memi[RV_CONT($1)+11]	# Mark rejected points

######################  END  OF  STRUCTURE  DEFINITIONS  ######################

# Continuum fitting functions
define	CN_INTERP_MODE	"|spline3|legendre|chebyshev|spline1|"
define	CN_SPLINE3		1
define	CN_LEGENDRE		2
define	CN_CHEBYSHEV		3
define	CN_SPLINE1		4
