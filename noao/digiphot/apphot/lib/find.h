# Find header file

# find parameters (# 701 - 800)

define	RATIO		701
define	THETA		702
define	NSIGMA		703
define	SHARPLO		704
define	SHARPHI		705
define	ROUNDLO		706
define	ROUNDHI		707
define	THRESHOLD	708

# find keyword units

define	KY_RATIO	"ratio"
define	KY_THETA	"theta"
define	KY_NSIGMA	"nsigma"
define	KY_SHARPLO	"sharplo"
define	KY_SHARPHI	"sharphi"
define	KY_ROUNDLO	"roundlo"
define	KY_ROUNDHI	"roundhi"
define	KY_THRESHOLD	"threshold"

# find parameter units

#define	UN_RATIO	"number"
#define	UN_THETA	"degrees"
#define	UN_NSIGMA	"sigma"
#define	UN_SHARPLO	"number"
#define	UN_SHARPHI	"number"
#define	UN_ROUNDLO	"number"
#define	UN_ROUNDHI	"number"
#define	UN_THRESHOLD	"sigma"

define	UN_FNUMBER	"number"
define	UN_FSIGMA	"sigma"
define	UN_FDEGREES	"degrees"

# define daofind commands

define FSHOWARGS	"|data|find|"
define FCMDS  "|nsigma|ratio|sharplo|sharphi|roundlo|roundhi|mkdetections|\
theta|threshold|"

# define daofind command strings

define	FCMD_NSIGMA		1
define	FCMD_RATIO		2
define	FCMD_SHARPLO		3
define	FCMD_SHARPHI		4
define	FCMD_ROUNDLO		5
define	FCMD_ROUNDHI		6
define	FCMD_MKDETECTIONS	7
define	FCMD_THETA		8
define	FCMD_THRESHOLD		9

define	FCMD_DATA		1
define	FCMD_FIND		2


# define the gaussian sums structure

define	LEN_GAUSS		10

define	GAUSS_SUMG		1
define	GAUSS_SUMGSQ		2
define	GAUSS_PIXELS		3
define	GAUSS_DENOM		4
define	GAUSS_SGOP		5

# miscellaneous

define	FWHM_TO_SIGMA		0.42467
define	RMIN			2.001
