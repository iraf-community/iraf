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

# define sky fitting errors (# 1 - 20)

define	AP_OK			0	# No error
define	AP_NOSKYAREA		1	# Width of the annulus is <= 0.0 pixels
define	AP_SKY_OUTOFBOUNDS	2	# Sky annulus out of bounds
define	AP_NOHISTOGRAM		3	# Cannot make a histogram
define	AP_FLAT_HIST		4	# Histogram is flat or concave
define	AP_NSKY_TOO_SMALL	5	# Too few points for fit
define	AP_SKY_SINGULAR		6	# Fit is singular
define	AP_SKY_NOCONVERGE	7	# Solution does not converge
define	AP_NOGRAPHICS		8	# NULL graphics pointer
define	AP_NOSKYFILE		9	# NULL sky file descriptor
define	AP_EOFSKYFILE		10	# End of sky file
define	AP_BADSKYSCAN		11	# Incomplete scan of sky file
define	AP_BADPARAMS		12	# Non-physical parameters

# sky fitting parameters (# 21 - 40)

define	ANNULUS		21
define	DANNULUS	22
define	SKYFUNCTION	23
define	K1		24
define	K2		25
define	SMAXITER	26
define	BINSIZE		27
define	SMOOTH		28
define	RGROW		29
define	SKY_BACKGROUND	30
define	SKY_MODE	31
define	SKY_SIGMA	32
define	SKY_SKEW	33
define	NSKY		34
define	NSKY_REJECT	35
define	SNREJECT	36
define	SXCUR		37
define	SYCUR		38
define	SSTRING		39

# define sky fitting keywords

define	KY_ANNULUS		"annulus"
define	KY_DANNULUS		"dannulus"
define	KY_SSTRING		"salgorithm"
define	KY_K1			"khist"
define	KY_K2			"skreject"
define	KY_SNREJECT		"snreject"
define	KY_SMAXITER		"smaxiter"
define	KY_BINSIZE		"binsize"
define	KY_SMOOTH		"smooth"
define	KY_RGROW		"rgrow"
define	KY_SKY_BACKGROUND	"skyvalue"

# define sky fitting units strings

define	UN_ANNULUS		"scaleunit"
define	UN_DANNULUS		"scaleunit"
define	UN_SSTRING		"algorithm"
define	UN_K1			"sigma"
define	UN_K2			"sigma"
define	UN_SNREJECT		"number"
define	UN_SMAXITER		"number"
define	UN_BINSIZE		"sigma"
define	UN_SMOOTH		"switch"
define	UN_RGROW		"scaleunit"
define	UN_SKY_BACKGROUND	"counts"

# fitsky string definitions

define	SSHOWARGS	"|data|sky|"
define	SFUNCS		"|constant|mode|centroid|file|histplot|median|radplot|gauss|ofilter|crosscor|mean|"
define	SCMDS		"|annulus|dannulus|salgorithm|khist|skreject|smaxiter|binsize|smooth|rgrow|snreject|skyvalue|mksky|"

define	SCMD_ANNULUS		1
define	SCMD_DANNULUS		2
define	SCMD_SALGORITHM 	3
define	SCMD_KHIST		4
define	SCMD_SKREJECT		5
define	SCMD_SMAXITER		6
define	SCMD_BINSIZE		7
define	SCMD_SMOOTH		8
define	SCMD_RGROW		9
define	SCMD_SNREJECT		10
define	SCMD_SKYVALUE 		11
define	SCMD_MKSKY		12

define	SCMD_DATA		1
define	SCMD_SKY		2
