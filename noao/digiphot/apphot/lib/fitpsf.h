# FITPSF header file

# fitpsf functions

define	AP_RADGAUSS		1	# Radial Gaussian
define  AP_ELLGAUSS		2	# Elliptical Gaussian
define	AP_MOMENTS		3	# Moment analysis

# fitpsf errors (# 121 - 140)

define	AP_OK			0	# no error
define	AP_NOPSFAREA		121	# No pixels for PSF
define	AP_PSF_OUTOFBOUNDS	122	# PSF aperture extends oob
define	AP_NPSF_TOO_SMALL	123	# Too few PSF points
define	AP_PSF_SINGULAR		124	# Fit is singular
define	AP_PSF_NOCONVERGE	125	# Solution does not converge

# fitpsf parameters (# 81 - 100)

define	PSFUNCTION	81
define	MAXNPARS	82
define	PK2		83
define	PMAXITER	84
define	PSFAPERT	85
define	NPARS		86
define	PARS		87
define	PERRS		88
define	PNREJECT	89
define	PFXCUR		90
define	PFYCUR		91
define	PSFSTRING	92

# define fitpsf keywords

define	KY_PSFUNCTION	"function"
define	KY_PSFAPERT	"box"
define	KY_PK2		"kreject"
define	KY_PMAXITER	"maxiter"
define	KY_PNREJECT	"nreject"
define	KY_PSFSTRING	"function"

# define fitpsf units

define	UN_PSFUNCTION	"model"
define	UN_PSFAPERT	"scaleunit"
define	UN_PK2		"sigma"
define	UN_PMAXITER	"number"
define	UN_PNREJECT	"number"
define	UN_PSFSTRING	"model"

# fitpsf string constants

define	PFSHOWARGS	"|data|fit|"
define	PSFFUNCS	"|radgauss|elgauss|moments|"
define	PSFCMDS		"|function|box|kreject|maxiter|nreject|mkbox|"

define	PFCMD_DATA		1
define	PFCMD_FIT		2

define	PFCMD_FUNCTION		1
define	PFCMD_BOX		2
define	PFCMD_KREJECT		3
define	PFCMD_MAXITER		4
define	PFCMD_NREJECT		5
define	PFCMD_MKBOX		6
