# APPHOT header file

define	LEN_APNOISE		(10 + 3 * SZ_FNAME + 3)

# noise model parameters

define	AP_NOISEFUNCTION	Memi[$1]	# Noise function
define	AP_SKYSIGMA		Memr[P2R($1+2)]	# Sky sigma in counts
define	AP_EPADU		Memr[P2R($1+3)]	# Photons per adu
define	AP_READNOISE		Memr[P2R($1+4)]	# CCD readnoise in adu
define	AP_GAIN			Memc[P2C($1+6)]	# Gain keyword
define	AP_NSTRING		Memc[P2C($1+6+SZ_FNAME+1)] # Noise model
define	AP_CCDREAD		Memc[P2C($1+6+2*SZ_FNAME+2)] # Readnoise

# noise model defaults

define	DEF_SKYSIGMA		INDEFR
define	DEF_EPADU		1.0
define	DEF_CCDREAD		""
define	DEF_GAIN		""
define	DEF_READNOISE		INDEFR
