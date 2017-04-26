# FIND header file

define	LEN_FIND		(10)

# find model parameters

define	AP_RATIO	Memr[P2R($1)]	# Gaussian axis ratio
define	AP_THETA 	Memr[P2R($1+1)]	# Gaussian position angle
define	AP_NSIGMA	Memr[P2R($1+2)]	# Size of convolution kernel
define	AP_SHARPLO	Memr[P2R($1+3)]	# Lower sharpness bound
define	AP_SHARPHI	Memr[P2R($1+4)]	# Upper sharpness bound
define	AP_ROUNDLO	Memr[P2R($1+5)]	# Lower roundness bound
define	AP_ROUNDHI	Memr[P2R($1+6)]	# Higher roundness bound
define	AP_THRESHOLD	Memr[P2R($1+7)]	# Threshold in sigma for detection


# noise model defaults

define	DEF_RATIO		1.0
define	DEF_THETA		0.0
define	DEF_NSIGMA		1.5
define	DEF_SHARPLO		0.2
define	DEF_SHARPHI		1.0
define	DEF_ROUNDLO		-1.0
define	DEF_ROUNDHI		1.0
define	DEF_THRESHOLD		0.0
