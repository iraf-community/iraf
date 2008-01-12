# RADPROF header file

# radprof error codes (# 901 - 1000)

define	AP_OK			0
define	AP_RP_NOPROFILE		901
define	AP_RP_OUTOFBOUNDS	902
define	AP_RP_NPTS_TOO_SMALL	903
define	AP_RP_SINGULAR		904
define	AP_RP_NOSKYMODE		905

# radprof fit parameters and answers (# 901 - 1000)

define	RPXCUR			901
define	RPYCUR			902
define	ORPXCUR			903
define	ORPYCUR			904
define	RPRADIUS		905
define	RPSTEP			906
define	RPKSIGMA		907
define	RPNREJECT		908
define	RPORDER			909
define	INORM			910
define	TNORM			911
define	DNORM			912
define	RPFWHM			913
define	RPNPTS			914
define	RPNDATA			915
define	RPNDATAREJ		916

# define radprof keywords

define	KY_RPRADIUS	"radius"
define	KY_RPSTEP	"stepsize"
define	KY_RPORDER	"order"
define	KY_RPKSIGMA	"kreject"
define	KY_RPNREJECT	"nreject"

# define radprof units

#define	UN_RPRADIUS	"scaleunit"
#define	UN_RPSTEP	"scaleunit"
#define	UN_RPORDER	"number"
#define	UN_RPKSIGMA	"sigma"
#define	UN_RPNREJECT	"number"

define	UN_RSCALEUNIT	"scaleunit"
define	UN_RNUMBER	"number"
define	UN_RSIGMA	"sigma"

# radprof strings

define	RPSHOWARGS	"|center|sky|phot|fit|data|"
define	RPCMDS		"|radius|stepsize|order|kreject|nreject|"

define	RCMD_CENTER	1
define	RCMD_SKY	2
define	RCMD_PHOT	3
define	RCMD_FIT	4
define	RCMD_DATA	5

define	RCMD_RADIUS	1
define	RCMD_STEPSIZE	2
define	RCMD_ORDER	3
define	RCMD_KREJECT 	4
define	RCMD_NREJECT 	5
