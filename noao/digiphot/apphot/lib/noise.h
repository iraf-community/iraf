# Noise model header file

# noise parameters (# 121 - 140)

define	SKYSIGMA	121
define	THRESHOLD	122
define	EPADU		123
define	NOISEFUNCTION	124
define	GAIN		125
define	NSTRING		126
define	CCDREAD		127
define	READNOISE	128
define	CTHRESHOLD	129

# noise keywords

define	KY_SKYSIGMA	"sigma"
define	KY_THRESHOLD	"threshold"
define	KY_EPADU	"epadu"
define	KY_GAIN		"gain"
define	KY_NSTRING	"noise"
define	KY_CCDREAD	"ccdread"
define	KY_READNOISE	"readnoise"
define	KY_CTHRESHOLD	"cthreshold"

# noise parameter units

define	UN_SKYSIGMA	"counts"
define	UN_THRESHOLD	"counts"
define	UN_EPADU	"e-/adu"
define	UN_GAIN		"keyword"
define	UN_NSTRING	"model"
define	UN_CCDREAD	"keyword"
define	UN_READNOISE	"e-"
define	UN_CTHRESHOLD	"counts"

# noise functions

define	AP_NCONSTANT	1
define	AP_NPOISSON	2

# noise model strings

define	NCMDS "|noise|cthreshold|sigma|epadu|gain|ccdread|readnoise|threshold|"
define	NFUNCS	"|constant|poisson|"

# noise string cases

define	NCMD_NOISE	1
define	NCMD_CTHRESHOLD	2
define	NCMD_SIGMA	3
define	NCMD_EPADU	4
define	NCMD_GAIN	5
define	NCMD_CCDREAD	6
define	NCMD_READNOISE	7
define	NCMD_THRESHOLD	8
