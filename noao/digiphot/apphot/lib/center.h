# CENTER defintions

# centering algorithms

define	AP_CENTROID1D	1	# 1D centroiding
define	AP_GAUSS1D	2	# 1D Gaussian
define	AP_NONE		3	# No centering
define	AP_OFILT1D	4	# 1-D optimal filtering

# centering errors (# 101 - 200)

define	AP_OK			0	# No error
define	AP_CTR_NOAREA		101	# No pixels for centering
define	AP_CTR_OUTOFBOUNDS	102	# Centering aperture extends oob
define	AP_CTR_LOWSNRATIO	103	# S/N ratio too low for centering
define	AP_CTR_NTOO_SMALL	104	# Too few histogram bins
define	AP_CTR_SINGULAR		105	# Fit is singular
define	AP_CTR_NOCONVERGE	106	# Solution does not converge
define	AP_CTR_BADSHIFT		107	# Max shift parameter exceeded
define	AP_CTR_BADDATA		108	# Bad data in centering subraster

# centering parameters (# 101 - 200)

define	CAPERT		101
define	CENTERFUNCTION	102
define	MINSNRATIO	103
define	MAXSHIFT	104
define	CLEAN		105
define	RCLEAN		106
define	RCLIP		107
define	SIGMACLEAN	108
define	OXINIT		109
define	OYINIT		110
define	XCENTER		111
define	YCENTER		112
define	OXCENTER	113
define	OYCENTER	114
define	XERR		115
define	YERR		116
define	CMAXITER	117
define	CXCUR		118
define	CYCUR		119
define	XSHIFT		120
define	YSHIFT		121
define	OXSHIFT		122
define	OYSHIFT		123
define	CSTRING		124
define	CDATALIMIT	125
define	CTHRESHOLD	126

# center keywords

define	KY_CAPERT	"cboxwidth"
define	KY_CSTRING	"calgorithm"
define	KY_MINSNRATIO	"minsnratio"
define	KY_MAXSHIFT	"maxshift"
define	KY_CLEAN	"clean"
define	KY_RCLEAN	"rclean"
define	KY_RCLIP	"rclip"
define	KY_SIGMACLEAN	"kclean"
define	KY_CMAXITER	"cmaxiter"
define	KY_CTHRESHOLD	"cthreshold"

# center unit strings

#define	UN_CAPERT	"scaleunit"
#define	UN_CSTRING	"algorithm"
#define	UN_MINSNRATIO	"number"
#define	UN_MAXSHIFT	"scaleunit"
#define	UN_CLEAN	"switch"
#define	UN_RCLEAN	"scaleunit"
#define	UN_RCLIP	"scaleunit"
#define	UN_SIGMACLEAN	"sigma"
#define	UN_CMAXITER	"number"
#define	UN_CTHRESHOLD	"sigma"

define	UN_CSCALEUNIT	"scaleunit"
define	UN_CNUMBER	"number"
define	UN_CSIGMA	"sigma"
define	UN_CSWITCH	"switch"
define	UN_CALGORITHM	"algorithm"

# center string commands

define	CSHOWARGS	"|data|center|"
define	CFUNCS		"|centroid|gauss|none|ofilter|"
define	CCMDS		"|cboxwidth|calgorithm|maxshift|minsnratio|cmaxiter|clean|rclean|rclip|kclean|mkcenter|cthreshold|"

define	CCMD_CBOXWIDTH		1
define	CCMD_CALGORITHM		2
define	CCMD_MAXSHIFT		3
define	CCMD_MINSNRATIO		4
define	CCMD_CMAXITER		5
define	CCMD_CLEAN		6
define	CCMD_RCLEAN		7
define	CCMD_RCLIP		8
define	CCMD_KCLEAN		9
define	CCMD_MKCENTER		10
define	CCMD_CTHRESHOLD		11

define	CCMD_DATA		1
define	CCMD_CENTER		2
