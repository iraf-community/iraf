# APPHOT header file

define	LEN_APNOISE		(10 + 3 * SZ_FNAME + 3)

# noise model parameters

define	AP_NOISEFUNCTION	Memi[$1]	# Noise function
define	AP_THRESHOLD 		Memr[$1+1]	# Detection threshold sky
define	AP_SKYSIGMA		Memr[$1+2]	# Sky sigma in counts
define	AP_EPADU		Memr[$1+3]	# Photons per adu
define	AP_READNOISE		Memr[$1+4]	# CCD readnoise in adu
define	AP_CTHRESHOLD		Memr[$1+5]	# Threshold above data min
define	AP_GAIN			Memc[P2C($1+6)]	# Gain keyword

define	AP_NSTRING		Memc[P2C($1+6+SZ_FNAME+1)]
define	AP_CCDREAD		Memc[P2C($1+6+2*SZ_FNAME+2)]

# noise model defaults

define	DEF_THRESHOLD		0.0
define	DEF_SKYSIGMA		INDEFR
define	DEF_EPADU		1.0
define	DEF_CTHRESHOLD		0.0
define	DEF_CCDREAD		""
define	DEF_GAIN		""
define	DEF_READNOISE		INDEFR
