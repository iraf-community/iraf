# RADPROF header file

define	LEN_RPSTRUCT	30

# Radprof data parameters

define  AP_RPXCUR	Memr[P2R($1)]	# X image center in pixels
define	AP_RPYCUR	Memr[P2R($1+1)]	# Y image center in pixels
define	AP_RPIX		Memi[$1+2]	# Pointer to radial profile pixels
define	AP_RPXC		Memr[P2R($1+3)]	# X center of subraster
define	AP_RPYC		Memr[P2R($1+4)]	# Y center of subraster
define	AP_RPNX		Memi[$1+5]	# Subraster length in x
define	AP_RPNY		Memi[$1+6]	# Subraster length in y
define  AP_ORPXCUR	Memr[P2R($1+7)]	# output system X image center in pixels
define	AP_ORPYCUR	Memr[P2R($1+8)]	# output system Y image center in pixels

# Radprof fit parameters

define	AP_RPRADIUS	Memr[P2R($1+9)]	# Radprof radius in scale
define	AP_RPSTEP	Memr[P2R($1+10)]# Radprof step size in scale
define	AP_RPORDER	Memi[$1+11]	# Order of the spline3 fit
define	AP_RPKSIGMA	Memr[P2R($1+12)]# Radprof K-rejection criterion in sigma
define	AP_RPNREJECT	Memi[$1+13]	# Maximum number of rejection cycles

# Radprof answers

define	AP_RPNPTS	Memi[$1+14]	# Number of points in radial profile
define	AP_RPDIST	Memi[$1+15]	# Pointer to radial distance array
define	AP_INTENSITY	Memi[$1+16]	# Pointer to fitted intensity array
define	AP_DINTENSITY	Memi[$1+17]	# Pointer incremental intensity array
define	AP_TINTENSITY	Memi[$1+18]	# Pointer to total intensity array
define	AP_INORM	Memr[P2R($1+19)]# Normalization for intensity profile
define	AP_TINORM	Memr[P2R($1+20)]# Normalization for total intensity
define	AP_DNORM	Memr[P2R($1+21)]# Normalization for incremental inten
define	AP_RPFWHM	Memr[P2R($1+22)]# Fitted profile FWHM
define	AP_RPNDATA	Memi[$1+23]	# Number of data points
define	AP_RPNDATAREJ	Memi[$1+24]	# Number of points rejected from fit
define	AP_RPNBAD	Memi[$1+25]	# Number of bad pixel points
