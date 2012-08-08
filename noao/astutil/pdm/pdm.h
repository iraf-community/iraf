# The PDM data structure and other definitions

define	PDM_LENSTRUCT	49		# Length of PDM structure

# Double precision.
define	PDM_PMIN	Memd[P2D($1)]		# Minimum period to search
define	PDM_PMAX	Memd[P2D($1+2)]		# Maximum period to search
define	PDM_FMIN	Memd[P2D($1+4)]		# Minimum frequency to search
define	PDM_FMAX	Memd[P2D($1+6)]		# Maximum frequency to search
define	PDM_MINR	Memd[P2D($1+8)]		# Period to remember (min)
define	PDM_NSIGMA	Memd[P2D($1+10)]	# num std dev. for range break
define	PDM_SUMSQ	Memd[P2D($1+12)]	# Sum of squares of the data
define	PDM_DVAR	Memd[P2D($1+14)]	# Variance (s ** 2) of the data
define	PDM_AMPL	Memd[P2D($1+16)]	# Amplitude of light curve
define	PDM_EPOCH	Memd[P2D($1+18)]	# Epoch of first maxima in data

# Pointers
define	PDM_ICD		Memi[$1+20]	# ICFIT pointer for data fit
define	PDM_ICP		Memi[$1+21]	# ICFIT pointer for phasecurve fit
define	PDM_CVD		Memi[$1+22]	# CURFIT pointer for data fit
define	PDM_CVP		Memi[$1+23]	# CURFIT pointer for phasecurve fit
define	PDM_GP		Memi[$1+24]	# PDM graphics GIO pointer
define	PDM_LFD		Memi[$1+25]	# Log file descriptor
define	PDM_PFD		Memi[$1+26]	# Plot file descriptor
define	PDM_GT		Memi[$1+27]	# PDM gtools pointer
define	PDM_XP		Memi[$1+28]	# Pointer to data ordinates
define	PDM_ODYP	Memi[$1+29]	# Pointer to original data abscissas
define	PDM_DYP		Memi[$1+30]	# Pointer to working data abscissas
define	PDM_INUSEP	Memi[$1+31]	# Pointer to in-use array
define	PDM_ERRP	Memi[$1+32]	# Pointer to error bar array
define	PDM_XTHP	Memi[$1+33]	# Pointer to theta plot ordinates
define	PDM_YTHP	Memi[$1+34]	# Pointer to theta plot abscissas
define	PDM_XPHP	Memi[$1+35]	# Pointer to phasecurve plot ordinates
define	PDM_YPHP	Memi[$1+36]	# Pointer to phasecurve plot abscissas
define	PDM_PHERRP	Memi[$1+37]	# Pointer to phasecurve plot errors
define	PDM_SORTP	Memi[$1+38]	# Pointer to array defining sort
define	PDM_SAMPLEP	Memi[$1+39]	# Pointer to sample (range) string
define	PDM_RG		Memi[$1+40]	# Pointer to range (sample) structure

# Constants
define	PDM_NPT		Memi[$1+41]	# Number of data points
define	PDM_NTHPT	Memi[$1+42]	# Number of theta points
define	PDM_NRANGE	Memi[$1+43]	# Number of ranges.

# Other
define	PDM_RESID	Memi[$1+44]	# Using residuals? flag
define	PDM_RANGE	Memi[$1+45]	# Using ranges? flag
define	PDM_DEBUG	Memb[$1+46]	# Debug? flag
define	PDM_PLUSPOINT	Memi[$1+47]	# Threshold data number to use plus
define	PDM_EB		Memi[$1+48]	# Use error bars? flag

# Macro definitions
define	PDM_X		Memd[PDM_XP($1)+$2-1]		# data ordinates
define	PDM_ODY		Memd[PDM_ODYP($1)+$2-1]		# orig data abscissas
define	PDM_DY		Memd[PDM_DYP($1)+$2-1]		# working data abscissas
define	PDM_ERR		Memr[PDM_ERRP($1)+$2-1]		# error bars
define	PDM_INUSE	Memi[PDM_INUSEP($1)+$2-1]	# in-use array
define	PDM_XTH		Memd[PDM_XTHP($1)+$2-1]		# theta plot ordinates
define	PDM_YTH		Memd[PDM_YTHP($1)+$2-1]		# theta plot abscissas
define	PDM_XPH		Memd[PDM_XPHP($1)+$2-1]		# phase plot ordinates
define	PDM_YPH		Memd[PDM_YPHP($1)+$2-1]		# phase plot abscissas
define	PDM_PHERR	Memr[PDM_PHERRP($1)+$2-1]	# phase plot errors
define	PDM_SORT	Memi[PDM_SORTP($1)+$2-1]	# array defining sort
define	PDM_SAMPLE	Memc[PDM_SAMPLEP($1)]		# sample (range) string


# Plot types. (ptype)
define	DATAPLOT	0		# data plot
define	THETAPPLOT	1		# theta period plot
define	THETAFPLOT	2		# theta frequency plot
define	PHASEPLOT	3		# phase plot

define	MAX_RANGES	20		# maximum number of range segments
define	PDM_SZ_TITLE	(4*SZ_LINE)	# size of pdm plot title buffer
define	BIN10		100		# numpts for bin 5/10 split
					# otherwise, plot pluses
define	HELP	"noao$lib/scr/pdm.key"	# where help file is
