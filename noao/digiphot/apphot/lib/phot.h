# APPHOT header file

# phot errors (# 41 - 60)

define	AP_OK			0	# no error
define	AP_APERT_NOAPERT	41	# no aperture area
define  AP_APERT_OUTOFBOUNDS	42	# 1 or more apertures out of bounds
define	AP_APERT_NOSKYMODE	43	# INDEF valued sky
define	AP_APERT_NEGMAG		44	# 0 or -ve magnitude
define	AP_APERT_BADDATA	45	# bad pixels in aperture

# phot parameters and answers (# 61 - 80)

define	ZMAG		61
define	APERTS		63
define	NAPERTS		64
define	MAGS		65
define	MAGERRS		66
define	PXCUR		69
define	PYCUR		70
define	AREAS		72
define	SUMS		73
define	PWEIGHTS	74
define	PWSTRING	75
define	APSTRING	76

# define the phot keywords

define	KY_ZMAG		"zmag"
define	KY_PWSTRING	"weighting"
define	KY_APERTS	"apertures"

# define the phot units

define	UN_ZMAG		"zeropoint"
define	UN_PWSTRING	"model"
define	UN_APERTS	"scaleunit"

# define the weighting functions

define	AP_PWCONSTANT	1	# uniform weighting
define	AP_PWCONE	2	# a cone of given fwhm is used
define	AP_PWGAUSS	3	# a Gaussian of given fwhm is used

# phot strings

define	PCMDS		"|apertures|zmag|mkapert|weighting|"
define	PSHOWARGS	"|center|sky|phot|data|"
define	PWFUNCS		"|constant|cone|gauss|"
define	QCMDS		"|show|cboxwidth|annulus|dannulus|apertures|zmag|epadu|exposure|radplots|image|output|coords|airmass|filter|"

define	PCMD_APERTURES		1
define	PCMD_ZMAG		2
define	PCMD_MKAPERT		3
define	PCMD_WEIGHTING		4

define	PCMD_CENTER		1
define	PCMD_SKY		2
define	PCMD_PHOT		3
define	PCMD_DATA		4

define	QCMD_SHOW		1
define	QCMD_CBOXWIDTH		2
define	QCMD_ANNULUS		3
define	QCMD_DANNULUS		4
define	QCMD_APERTURES		5
define	QCMD_ZMAG		6
define	QCMD_EPADU		7
define	QCMD_EXPOSURE		8
define	QCMD_RADPLOTS		9
define	QCMD_IMAGE		10
define	QCMD_OUTPUT		11
define	QCMD_COORDS		12
define	QCMD_AIRMASS		13
define	QCMD_FILTER		14

# miscellaneous

define	MAX_NAPERTS	100
