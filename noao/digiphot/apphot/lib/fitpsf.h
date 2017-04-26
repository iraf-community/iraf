# FITPSF header file

# fitpsf functions

define	AP_RADGAUSS		1	# Radial Gaussian
define  AP_ELLGAUSS		2	# Elliptical Gaussian
define	AP_MOMENTS		3	# Moment analysis

# fitpsf errors (# 401 - 500)

define	AP_OK			0	# no error
define	AP_NOPSFAREA		401	# No pixels for PSF
define	AP_PSF_OUTOFBOUNDS	402	# PSF aperture extends oob
define	AP_NPSF_TOO_SMALL	403	# Too few PSF points
define	AP_PSF_SINGULAR		404	# Fit is singular
define	AP_PSF_NOCONVERGE	405	# Solution does not converge

# fitpsf parameters (# 401 - 500)

define	PSFUNCTION	401
define	MAXNPARS	402
define	PK2		403
define	PMAXITER	404
define	PSFAPERT	405
define	NPARS		406
define	PARS		407
define	PERRS		408
define	PNREJECT	409
define	PFXCUR		410
define	PFYCUR		411
define	OPFXCUR		412
define	OPFYCUR		413
define	PSFSTRING	414

# define fitpsf keywords

define	KY_PSFUNCTION	"function"
define	KY_PSFAPERT	"box"
define	KY_PK2		"kreject"
define	KY_PMAXITER	"maxiter"
define	KY_PNREJECT	"nreject"
define	KY_PSFSTRING	"function"

# define fitpsf units

#define	UN_PSFUNCTION	"model"
#define	UN_PSFAPERT	"scaleunit"
#define	UN_PK2		"sigma"
#define	UN_PMAXITER	"number"
#define	UN_PNREJECT	"number"
#define	UN_PSFSTRING	"model"

define	UN_PSFSCALEUNIT	"scaleunit"
define	UN_PSFNUMBER	"number"
define	UN_PSFMODEL	"model"
define	UN_PSFSIGMA	"sigma"

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
