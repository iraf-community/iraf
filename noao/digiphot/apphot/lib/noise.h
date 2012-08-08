# Noise model header file

# noise parameters (# 601 - 700)

define	SKYSIGMA	601
define	EPADU		603
define	NOISEFUNCTION	604
define	GAIN		605
define	NSTRING		606
define	CCDREAD		607
define	READNOISE	608

# noise keywords

define	KY_SKYSIGMA	"sigma"
define	KY_EPADU	"epadu"
define	KY_GAIN		"gain"
define	KY_NSTRING	"noise"
define	KY_CCDREAD	"ccdread"
define	KY_READNOISE	"readnoise"

# noise parameter units

#define	UN_SKYSIGMA	"counts"
#define	UN_EPADU	"e-/adu"
#define	UN_GAIN		"keyword"
#define	UN_NSTRING	"model"
#define	UN_CCDREAD	"keyword"
#define	UN_READNOISE	"e-"

define	UN_NKEYWORD	"keyword"
define	UN_NCOUNTS	"counts"
define	UN_NELECTRONS	"e-"
define	UN_NMODEL	"model"
define	UN_NEPADU	"e-/adu"

# noise functions

define	AP_NCONSTANT	1
define	AP_NPOISSON	2

# noise model strings

define	NCMDS "|noise|sigma|epadu|gain|ccdread|readnoise|"
define	NFUNCS	"|constant|poisson|"

# noise string cases

define	NCMD_NOISE	1
define	NCMD_SIGMA	2
define	NCMD_EPADU	3
define	NCMD_GAIN	4
define	NCMD_CCDREAD	5
define	NCMD_READNOISE	6
