# CENTER defintions

# centering algorithms

define	AP_CENTROID1D	1	# 1D centroiding
define	AP_GAUSS1D	2	# 1D Gaussian
define	AP_NONE		3	# No centering
define	AP_OFILT1D	4	# 1-D optimal filtering

# centering errors (# 21 - 40)

define	AP_OK			0	# No error
define	AP_CTR_NOAREA		21	# No pixels for centering
define	AP_CTR_OUTOFBOUNDS	22	# Centering aperture extends oob
define	AP_CTR_LOWSNRATIO	23	# S/N ratio too low for centering
define	AP_CTR_NTOO_SMALL	24	# Too few histogram bins
define	AP_CTR_SINGULAR		25	# Fit is singular
define	AP_CTR_NOCONVERGE	26	# Solution does not converge
define	AP_CTR_BADSHIFT		27	# Max shift parameter exceeded
define	AP_CTR_BADDATA		28	# Bad data in centering subraster

# centering parameters (# 41 - 60)

define	CAPERT		41
define	CENTERFUNCTION	42
define	MINSNRATIO	43
define	MAXSHIFT	44
define	CLEAN		45
define	RCLEAN		46
define	RCLIP		47
define	SIGMACLEAN	48
define	XCENTER		49
define	YCENTER		50
define	XERR		51
define	YERR		52
define	CMAXITER	53
define	CXCUR		54
define	CYCUR		55
define	XSHIFT		56
define	YSHIFT		57
define	CSTRING		59
define	CDATALIMIT	60

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

# center unit strings

define	UN_CAPERT	"scaleunit"
define	UN_CSTRING	"algorithm"
define	UN_MINSNRATIO	"number"
define	UN_MAXSHIFT	"scaleunit"
define	UN_CLEAN	"switch"
define	UN_RCLEAN	"scaleunit"
define	UN_RCLIP	"scaleunit"
define	UN_SIGMACLEAN	"sigma"
define	UN_CMAXITER	"number"

# center string commands

define	CSHOWARGS	"|data|center|"
define	CFUNCS		"|centroid|gauss|none|ofilter|"
define	CCMDS		"|cboxwidth|calgorithm|maxshift|minsnratio|cmaxiter|clean|rclean|rclip|kclean|mkcenter|"

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

define	CCMD_DATA		1
define	CCMD_CENTER		2
