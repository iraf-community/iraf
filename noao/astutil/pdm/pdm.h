# The PDM data structure and other definitions

define	PDM_LENSTRUCT	36		# Length of PDM structure

# Pointers
define	PDM_ICD		Memi[$1]	# ICFIT pointer for data fit
define	PDM_ICP		Memi[$1+1]	# ICFIT pointer for phasecurve fit
define	PDM_CVD		Memi[$1+2]	# CURFIT pointer for data fit
define	PDM_CVP		Memi[$1+3]	# CURFIT pointer for phasecurve fit
define	PDM_GP		Memi[$1+4]	# PDM graphics GIO pointer
define	PDM_LFD		Memi[$1+5]	# Log file descriptor
define	PDM_PFD		Memi[$1+6]	# Plot file descriptor
define	PDM_GT		Memi[$1+7]	# PDM gtools pointer
define	PDM_XP		Memi[$1+8]	# Pointer to data ordinates
define	PDM_ODYP	Memi[$1+9]	# Pointer to original data abscissas
define	PDM_DYP		Memi[$1+10]	# Pointer to working data abscissas
define	PDM_INUSEP	Memi[$1+11]	# Pointer to in-use array
define	PDM_XTHP	Memi[$1+12]	# Pointer to theta plot ordinates
define	PDM_YTHP	Memi[$1+13]	# Pointer to theta plot abscissas
define	PDM_XPHP	Memi[$1+14]	# Pointer to phasecurve plot ordinates
define	PDM_YPHP	Memi[$1+15]	# Pointer to phasecurve plot abscissas
define	PDM_SORTP	Memi[$1+16]	# Pointer to array defining sort
define	PDM_SAMPLEP	Memi[$1+17]	# Pointer to sample (range) string
define	PDM_RG		Memi[$1+18]	# Pointer to range (sample) structure

# Macro definitions
define	PDM_X		Memr[PDM_XP($1)+$2-1]		# data ordinates
define	PDM_ODY		Memr[PDM_ODYP($1)+$2-1]		# orig data abscissas
define	PDM_DY		Memr[PDM_DYP($1)+$2-1]		# working data abscissas
define	PDM_INUSE	Memi[PDM_INUSEP($1)+$2-1]	# in-use array
define	PDM_XTH		Memr[PDM_XTHP($1)+$2-1]		# theta plot ordinates
define	PDM_YTH		Memr[PDM_YTHP($1)+$2-1]		# theta plot abscissas
define	PDM_XPH		Memr[PDM_XPHP($1)+$2-1]		# phase plot ordinates
define	PDM_YPH		Memr[PDM_YPHP($1)+$2-1]		# phase plot abscissas
define	PDM_SORT	Memi[PDM_SORTP($1)+$2-1]	# array defining sort
define	PDM_SAMPLE	Memc[PDM_SAMPLEP($1)]		# sample (range) string

# Constants
define	PDM_NPT		Memi[$1+19]	# Number of data points
define	PDM_NTHPT	Memi[$1+20]	# Number of theta points
define	PDM_NRANGE	Memi[$1+21]	# Number of ranges.
define	PDM_PMIN	Memr[$1+22]	# Minimum period to search
define	PDM_PMAX	Memr[$1+23]	# Maximum period to search
define	PDM_FMIN	Memr[$1+24]	# Minimum frequency to search
define	PDM_FMAX	Memr[$1+25]	# Maximum frequency to search
define	PDM_MINR	Memr[$1+26]	# Period to remember (min)
define	PDM_NSIGMA	Memr[$1+27]	# num std deviations for range break

# Other
define	PDM_RESID	Memi[$1+28]	# Using residuals? flag
define	PDM_RANGE	Memi[$1+29]	# Using ranges? flag
define	PDM_DEBUG	Memb[$1+30]	# Debug? flag

# Sum of the squares and the variance.
define	PDM_SUMSQ	Memr[$1+31]	# Sum of squares of the data
define	PDM_DVAR	Memr[$1+32]	# Variance (s ** 2) of the data

define	PDM_AMPL	Memr[$1+33]	# Amplitude of light curve
define	PDM_EPOCH	Memr[$1+34]	# Epoch of first maxima in data
define	PDM_PLUSPOINT	Memr[$1+35]	# Threshold data number to use plus


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
