# FITSKY definitions

# define the sky fitting algorithms

define	AP_CONSTANT	1	# Use a constant sky
define	AP_MODE		2	# Compute the mode of the sky pixels
define	AP_CENTROID	3	# Compute the peak of the histogram
define	AP_SKYFILE	4	# Get values from a file
define	AP_HISTPLOT	5	# Mark the sky on a histogram plot
define	AP_MEDIAN	6	# Take median of sky pixels
define	AP_RADPLOT	7	# Mark sky on radial profile plot
define	AP_GAUSS	8	# Non-linear fit to a Gaussian
define	AP_OFILT	9	# Optimal filtering
define	AP_CROSSCOR	10	# Cross correlation method
define	AP_MEAN		11	# Compute the mean of the sky pixels

# define sky fitting errors (# 201 - 300)

define	AP_OK			0	# No error
define	AP_NOSKYAREA		201	# Width of the annulus is <= 0.0 pixels
define	AP_SKY_OUTOFBOUNDS	202	# Sky annulus out of bounds
define	AP_NOHISTOGRAM		203	# Cannot make a histogram
define	AP_FLAT_HIST		204	# Histogram is flat or concave
define	AP_NSKY_TOO_SMALL	205	# Too few points for fit
define	AP_SKY_SINGULAR		206	# Fit is singular
define	AP_SKY_NOCONVERGE	207	# Solution does not converge
define	AP_NOGRAPHICS		208	# NULL graphics pointer
define	AP_NOSKYFILE		209	# NULL sky file descriptor
define	AP_EOFSKYFILE		210	# End of sky file
define	AP_BADSKYSCAN		211	# Incomplete scan of sky file
define	AP_BADPARAMS		212	# Non-physical parameters

# sky fitting parameters (# 201 - 300)

define	SKYFUNCTION	201
define	ANNULUS		202
define	DANNULUS	203
define	SKY_BACKGROUND	204
define	K1		205
define	BINSIZE		206
define	SMOOTH		207
define	SMAXITER	208
define	SLOCLIP		209
define	SHICLIP		210
define	SNREJECT	211
define	SLOREJECT	212
define	SHIREJECT	213
define	RGROW		214
define	SKY_MODE	215
define	SKY_SIGMA	216
define	SKY_SKEW	217
define	NSKY		218
define	NSKY_REJECT	219
define	SXCUR		220
define	SYCUR		221
define	OSXCUR		222
define	OSYCUR		223
define	SSTRING		224

# define sky fitting keywords

define	KY_SSTRING		"salgorithm"
define	KY_ANNULUS		"annulus"
define	KY_DANNULUS		"dannulus"
define	KY_SKY_BACKGROUND	"skyvalue"
define	KY_K1			"khist"
define	KY_BINSIZE		"binsize"
define	KY_SMOOTH		"smooth"
define	KY_SMAXITER		"smaxiter"
define	KY_SLOCLIP		"sloclip"
define	KY_SHICLIP		"shiclip"
define	KY_SNREJECT		"snreject"
define	KY_SLOREJECT		"sloreject"
define	KY_SHIREJECT		"shireject"
define	KY_RGROW		"rgrow"

# define sky fitting units strings

#define	UN_SSTRING		"algorithm"
#define	UN_ANNULUS		"scaleunit"
#define	UN_DANNULUS		"scaleunit"
#define	UN_SKY_BACKGROUND	"counts"
#define	UN_K1			"sigma"
#define	UN_BINSIZE		"sigma"
#define	UN_SMOOTH		"switch"
#define	UN_SMAXITER		"number"
#define	UN_SLOCLIP		"percent"
#define	UN_SHICLIP		"percent"
#define	UN_SNREJECT		"number"
#define	UN_SLOREJECT		"sigma"
#define	UN_SHIREJECT		"sigma"
#define	UN_RGROW		"scaleunit"

define	UN_SALGORITHM		"algorithm"
define	UN_SSCALEUNIT		"scaleunit"
define	UN_SSIGMA		"sigma"
define	UN_SNUMBER		"number"
define	UN_SPERCENT		"percent"
define	UN_SCOUNTS		"counts"
define	UN_SSWITCH		"switch"

# fitsky string definitions

define	SSHOWARGS	"|data|sky|"
define	SFUNCS		"|constant|mode|centroid|file|histplot|median|\
radplot|gauss|ofilter|crosscor|mean|"
define	SCMDS		"|annulus|dannulus|salgorithm|khist|sloreject|\
shireject|smaxiter|binsize|smooth|rgrow|snreject|skyvalue|mksky|sloclip|\
shiclip|"

define	SCMD_ANNULUS		1
define	SCMD_DANNULUS		2
define	SCMD_SALGORITHM 	3
define	SCMD_KHIST		4
define	SCMD_SLOREJECT		5
define	SCMD_SHIREJECT		6
define	SCMD_SMAXITER		7
define	SCMD_BINSIZE		8
define	SCMD_SMOOTH		9
define	SCMD_RGROW		10
define	SCMD_SNREJECT		11
define	SCMD_SKYVALUE 		12
define	SCMD_MKSKY		13
define	SCMD_SLOCLIP		14
define	SCMD_SHICLIP		15

define	SCMD_DATA		1
define	SCMD_SKY		2
