# FITSKY structure

define LEN_SKYSTRUCT		(30 + SZ_FNAME + 1)

# sky fitting parameters

define	AP_ANNULUS	Memr[$1]	# Annulus inner radius in scale
define	AP_DANNULUS	Memr[$1+1]	# Annulus width in scale
define	AP_SKYFUNCTION	Memi[$1+2]	# Sky fitting algorithm
define	AP_K1		Memr[$1+3]	# K-sigma histogram rejection
define	AP_K2		Memr[$1+4]	# K-sigma rejection pixels
define	AP_SMAXITER	Memi[$1+5]	# Maximum number of iterations
define	AP_BINSIZE	Memr[$1+6]	# Histogram binsize in sky sigma
define	AP_SMOOTH	Memi[$1+7]	# Smooth histogram ?
define	AP_RGROW	Memr[$1+8]	# Regions growing radius in scale
define	AP_SKYBACKGROUND  Memr[$1+9]	# User defined sky value
define	AP_SNREJECT	Memi[$1+10]	# Maximum number of rejection cycles

# sky buffer definitions

define	AP_SKYPIX	Memi[$1+11]	# Pointer to sky pixels
define	AP_INDEX	Memi[$1+12]	# Pointer to ordering array
define	AP_COORDS	Memi[$1+13]	# Pointer to sky coordinates array
define	AP_NSKYPIX	Memi[$1+14]	# Number of sky pixels
define	AP_NBADSKYPIX	Memi[$1+15]	# Number of bad sky pixels
define	AP_LENSKYBUF	Memi[$1+16]	# Length of sky buffers
define	AP_SXCUR	Memr[$1+17]	# X center of sky annulus
define	AP_SYCUR	Memr[$1+18]	# Y center of sky annulus
define	AP_SXC		Memr[$1+19]	# X center of sky subraster
define	AP_SYC		Memr[$1+20]	# Y center of sky subraster
define	AP_SNX		Memi[$1+21]	# X dimension of sky subraster
define	AP_SNY		Memi[$1+22]	# Y dimension of sky subraster

# fitsky output

define	AP_SKY_MODE		Memr[$1+23]	# computed sky value
define	AP_SKY_SIG		Memr[$1+24]	# computed sky sigma
define	AP_SKY_SKEW		Memr[$1+25]	# skew of sky pixels
define	AP_NSKY			Memi[$1+26]	# number of sky pix
define	AP_NSKY_REJECT		Memi[$1+27]	# number of pixels

define	AP_SSTRING		Memc[P2C($1+28)]# salgorithm string

# default setup values for sky fitting

define	DEF_SKYFUNCTION		AP_MODE
define	DEF_ANNULUS		10.
define	DEF_DANNULUS		10.
define	DEF_K2			3.0
define	DEF_SMAXITER		10
define	DEF_SNREJECT		50
define	DEF_K1			3.0
define	DEF_SMOOTH		NO
define	DEF_BINSIZE		0.10
define	DEF_RGROW		0.0
define	DEF_SKYVALUE		0.0
