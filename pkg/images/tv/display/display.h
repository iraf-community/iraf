# Display modes:

define	RGB		1	# True color mode
define	FRAME		2	# Single frame mode

# Color selections:

define	BLUE		1B	# BLUE Select
define	GREEN		2B	# GREEN Select
define	RED		4B	# RED Select
define	MONO		7B	# RED + GREEN + BLUE

# Size limiting parameters.

define	MAXCHAN		2
define	SAMPLE_SIZE	600

# If a logarithmic greyscale transformation is desired, the input range Z1:Z2
# will be mapped into the range 1.0 to 10.0 ** MAXLOG before taking the log
# to the base 10.

define	MAXLOG		3

# The following parameter is used to compare display pixel coordinates for
# equality.  It determines the maximum permissible magnification.  The machine
# epsilon is not used because the computations are nontrivial and accumulation
# of error is a problem.

define	DS_TOL		(1E-4)

# These parameters are needed for user defined transfer functions.

define	U_MAXPTS	4096
define	U_Z1		0
define	U_Z2		4095

# BPDISPLAY options:

define	BPDISPLAY	"|none|overlay|interpolate|"
define	BPDNONE		1	# Ignore bad pixel mask
define	BPDOVRLY	2	# Overlay bad pixels
define	BPDINTERP	3	# Interpolate bad pixels
