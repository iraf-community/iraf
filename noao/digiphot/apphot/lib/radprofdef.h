# RADPROF header file

define	LEN_RPSTRUCT	30

# Radprof data parameters

define  AP_RPXCUR	Memr[$1]	# X image center in pixels
define	AP_RPYCUR	Memr[$1+1]	# Y image center in pixels
define	AP_RPIX		Memi[$1+2]	# Pointer to radial profile pixels
define	AP_RPXC		Memr[$1+3]	# X center of subraster
define	AP_RPYC		Memr[$1+4]	# Y center of subraster
define	AP_RPNX		Memi[$1+5]	# Subraster length in x
define	AP_RPNY		Memi[$1+6]	# Subraster length in y

# Radprof fit parameters

define	AP_RPRADIUS	Memr[$1+7]	# Radprof radius in scale
define	AP_RPSTEP	Memr[$1+8]	# Radprof step size in scale
define	AP_RPORDER	Memi[$1+9]	# Order of the spline3 fit
define	AP_RPKSIGMA	Memr[$1+10]	# Radprof K-rejection criterion in sigma
define	AP_RPNREJECT	Memi[$1+11]	# Maximum number of rejection cycles

# Radprof answers

define	AP_RPNPTS	Memi[$1+12]	# Number of points in radial profile
define	AP_RPDIST	Memi[$1+13]	# Pointer to radial distance array
define	AP_INTENSITY	Memi[$1+14]	# Pointer to fitted intensity array
define	AP_DINTENSITY	Memi[$1+15]	# Pointer incremental intensity array
define	AP_TINTENSITY	Memi[$1+16]	# Pointer to total intensity array
define	AP_INORM	Memr[$1+17]	# Normalization for intensity profile
define	AP_TINORM	Memr[$1+18]	# Normalization for total intensity
define	AP_DNORM	Memr[$1+19]	# Normalization for incremental inten
define	AP_RPFWHM	Memr[$1+20]	# Fitted profile FWHM
define	AP_RPNDATA	Memi[$1+21]	# Number of data points
define	AP_RPNDATAREJ	Memi[$1+22]	# Number of points rejected from fit
define	AP_RPNBAD	Memi[$1+23]	# Number of bad pixel points
