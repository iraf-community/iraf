# FITSKY structure

define LEN_SKYSTRUCT		(36 + SZ_FNAME + 1)

# sky fitting parameters

define	AP_SKYFUNCTION	  Memi[$1]	   # sky fitting algorithm
define	AP_ANNULUS	  Memr[P2R($1+1)]  # inner radius of annulus in scale 
define	AP_DANNULUS	  Memr[P2R($1+2)]  # annulus width in scale
define	AP_SKYBACKGROUND  Memr[P2R($1+3)]  # user defined sky value
define	AP_K1		  Memr[P2R($1+4)]  # K-sigma histogram rejection
define	AP_BINSIZE	  Memr[P2R($1+5)]  # histogram binsize in sky sigma
define	AP_SMOOTH	  Memi[$1+6]	   # Smooth histogram ?
define	AP_SMAXITER	  Memi[$1+7]	   # maximum number of iterations
define	AP_SLOCLIP	  Memr[P2R($1+8)]  # lower clipping percentile
define	AP_SHICLIP	  Memr[P2R($1+9)]  # lower clipping percentile
define	AP_SNREJECT	  Memi[$1+10]	   # maximum number of rejection cycles
define	AP_SLOREJECT	  Memr[P2R($1+11)] # lower K-sigma rejection for pixels
define	AP_SHIREJECT	  Memr[P2R($1+12)] # higher K-sigma rejection for pixels
define	AP_RGROW	  Memr[P2R($1+13)] # region growing radius in scale

# sky buffer definitions

define	AP_SKYPIX	Memi[$1+14]	   # pointer to sky pixels
define	AP_INDEX	Memi[$1+15]	   # pointer to ordering array
define	AP_COORDS	Memi[$1+16]	   # pointer to sky coordinates array
define	AP_SWGT		Memi[$1+17]	   # pointer to sky weights
define	AP_NSKYPIX	Memi[$1+18]	   # number of sky pixels
define	AP_NBADSKYPIX	Memi[$1+19]	   # number of bad sky pixels
define	AP_LENSKYBUF	Memi[$1+20]	   # length of sky buffers
define	AP_SXCUR	Memr[P2R($1+21)]   # x center of sky annulus
define	AP_SYCUR	Memr[P2R($1+22)]   # y center of sky annulus
define	AP_SXC		Memr[P2R($1+23)]   # x center of sky subraster
define	AP_SYC		Memr[P2R($1+24)]   # y center of sky subraster
define	AP_SNX		Memi[$1+25]	   # x dimension of sky subraster
define	AP_SNY		Memi[$1+26]	   # y dimension of sky subraster

# fitsky output

define	AP_OSXCUR	Memr[P2R($1+27)]   # x center of sky annulus
define	AP_OSYCUR	Memr[P2R($1+28)]   # y center of sky annulus
define	AP_SKY_MODE	Memr[P2R($1+29)]   # computed sky value
define	AP_SKY_SIG	Memr[P2R($1+30)]   # computed sky sigma
define	AP_SKY_SKEW	Memr[P2R($1+31)]   # computed sky skew
define	AP_NSKY		Memi[$1+32]	   # number of sky pix
define	AP_NSKY_REJECT	Memi[$1+33]	   # number of rejected sky pix

define	AP_SSTRING	Memc[P2C($1+34)]   # salgorithm string

# default setup values for sky fitting

define	DEF_SKYFUNCTION		AP_MODE
define	DEF_SKYVALUE		0.0
define	DEF_ANNULUS		10.
define	DEF_DANNULUS		10.
define	DEF_SLOCLIP		0.0
define	DEF_SHICLIP		0.0
define	DEF_SNREJECT		50
define	DEF_SLOREJECT		3.0
define	DEF_SHIREJECT		3.0
define	DEF_SMAXITER		10
define	DEF_RGROW		0.0
define	DEF_K1			3.0
define	DEF_BINSIZE		0.10
define	DEF_SMOOTH		NO
