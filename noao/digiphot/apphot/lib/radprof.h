# RADPROF header file

# radprof fit parameters and answers (# 161 - 180)

define	RPXCUR			161
define	RPYCUR			162
define	RPRADIUS		163
define	RPSTEP			164
define	RPKSIGMA		165
define	RPNREJECT		166
define	RPORDER			167
define	INORM			168
define	TNORM			169
define	DNORM			170
define	RPFWHM			171
define	RPNPTS			172
define	RPNDATA			173
define	RPNDATAREJ		174


# radprof error codes (# 161 - 180)

define	AP_OK			0
define	AP_RP_NOPROFILE		161
define	AP_RP_OUTOFBOUNDS	162
define	AP_RP_NPTS_TOO_SMALL	163
define	AP_RP_SINGULAR		164
define	AP_RP_NOSKYMODE		165

# define radprof keywords

define	KY_RPRADIUS	"radius"
define	KY_RPSTEP	"stepsize"
define	KY_RPORDER	"order"
define	KY_RPKSIGMA	"kreject"
define	KY_RPNREJECT	"nreject"

# define radprof units

define	UN_RPRADIUS	"scaleunit"
define	UN_RPSTEP	"scaleunit"
define	UN_RPORDER	"number"
define	UN_RPKSIGMA	"sigma"
define	UN_RPNREJECT	"number"

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
