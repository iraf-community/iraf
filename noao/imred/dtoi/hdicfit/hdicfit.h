# Definition file for DTOI task hdfit which uses the hdicfit subdirectory.

define	NSPOTS		64	   # Initially, max number of calibration spots
define	NVALS_FIT	200	   # Number of points in vector of fitted points
define	WT_NONE		0	   # No weighting used in fit
define	WT_USER		1	   # User specifies weighting in fit
define	WT_CALC		2	   # Weighting in fit calculated from std dev
define	MIN_DEN		EPSILONR   # Density used for setting curfit minval
define	HD_NONE		1	   # Ind var is density - no transformation
define	HD_LOGO		2	   # Ind var is log opacitance
define	HD_K50		3	   # Ind var is Kaiser transform w/ alpha=0.50
define	HD_K75		4	   # Ind var is Kaiser transform w/ alpha=0.75
define	UDELETE		100	   # Point deleted by user flag
define	PDELETE		101	   # Point deleted by program
define	NDELETE		102	   # Point not deleted 
define	ADDED_PT	0.0	   # Indication of added point in sdev array

# The ICFIT data structure - modified for use with the DTOI package.

define	IC_NGKEYS	5		# Number of graph keys
define	IC_LENSTRUCT	47		# Length of ICFIT structure

# User fitting parameters
define	IC_FUNCTION	Memi[$1]	# Function type
define	IC_ORDER	Memi[$1+1]	# Order of function
define	IC_SAMPLE	Memi[$1+2]	# Pointer to sample string
define	IC_NAVERAGE	Memi[$1+3]	# Sampling averaging bin
define	IC_NITERATE	Memi[$1+4]	# Number of rejection interation
define	IC_TRANSFORM	Memi[$1+5]	# Type of transformation ** DTOI ONLY **
define	IC_XMIN		Memr[P2R($1+6)]	# Minimum value for curve
define	IC_XMAX		Memr[P2R($1+7)]	# Maximum value for curve
define	IC_LOW		Memr[P2R($1+8)]	# Low rejection value
define	IC_HIGH		Memr[P2R($1+9)]	# Low rejection value
define	IC_GROW		Memr[P2R($1+10)]# Rejection growing radius

# ICFIT parameters used for fitting
define	IC_NFIT		Memi[$1+11]	# Number of fit points
define	IC_NREJECT	Memi[$1+12]	# Number of rejected points
define	IC_RG		Memi[$1+13]	# Pointer for ranges
define	IC_XFIT		Memi[$1+14]	# Pointer to ordinates of fit points
define	IC_YFIT		Memi[$1+15]	# Pointer to abscissas of fit points
define	IC_WTSFIT	Memi[$1+16]	# Pointer to weights of fit points
define	IC_REJPTS	Memi[$1+17]	# Pointer to rejected points

# ICFIT parameters used for interactive graphics
define	IC_NEWX		Memi[$1+18]	# New x fit points?
define	IC_NEWY		Memi[$1+19]	# New y points?
define	IC_NEWWTS	Memi[$1+20]	# New weights?
define	IC_NEWFUNCTION	Memi[$1+21]	# New fitting function?
define	IC_NEWTRANSFORM Memi[$1+22]	# New transform?  ** DTOI ONLY **
define	IC_OVERPLOT	Memi[$1+23]	# Overplot next plot?
define	IC_FITERROR	Memi[$1+24]	# Error in fit
define	IC_LABELS	Memi[$1+25+$2-1]# Graph axis labels
define	IC_UNITS	Memi[$1+27+$2-1]# Graph axis units

define	IC_FOG		Memr[P2R($1+29)]# *** DTOI ONLY *** value of fog level
define	IC_NEWFOG	Memi[$1+30]	# Flag for change in fog
define	IC_RESET	Memi[$1+31]	# Flag for resetting variables
define	IC_UPDATE	Memi[$1+32]
define	IC_EBARS	Memi[$1+33]	# Flag for plotting error bars
define	IC_RFOG		Memr[P2R($1+34)]# Reference value of fog for resetting

# ICFIT key definitions
define	IC_GKEY		Memi[$1+35]			# Graph key
define	IC_AXES		Memi[$1+36+($2-1)*2+$3-1]	# Graph axis codes
