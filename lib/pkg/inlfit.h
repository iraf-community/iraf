# The user callable header file for the INLFIT pacakge

# -------------------------------------------------------------------------
# Definitions to retrieve INLFIT parameters (attributes) with the in_get(),
# and in_put() procedures.

# Integer valued parameters (in_geti, in_puti)

define	INLFUNCTION		1		# Fitting function
define	INLDERIVATIVE		2		# Fitting function derivatives
define	INLNPARAMS		3		# Total number of parameters
define	INLNFPARAMS		4		# Number of fitting parameters
define	INLNVARS		5		# Number of variables
define	INLNPTS			6		# Number of variables
define	INLMAXITER		7		# Max. number of iterations
define	INLNREJECT		8		# Number of rejection iterations
define	INLNREJPTS		9		# Number of rejected points
define	INLUAXES		10		# User plot function
define	INLUCOLON		11		# User colon function
define	INLUFIT			12		# User fit function
define	INLOVERPLOT		13		# Overplot next plot ?
define	INLPLOTFIT		14		# Overplot fit ?
define	INLFITERROR		15		# Error fit code
define	INLGKEY			16		# Graph key


# Real/double valued parameters (in_get[rd], in_put[rd])

define	INLTOLERANCE		20		# Tolerance of convergence
define	INLLOW			21		# Low rejection value
define	INLHIGH			22		# High rejection value
define	INLGROW			23		# Rejection growing radius


# Pointer valued parameters (in_getp, in_getp)

define	INLNL			30		# NLFIT descriptor
define	INLPARAM		31		# Parameter vector
define	INLDPARAM		32		# Parameter change vector
define	INLPLIST		33		# Parameter list
define	INLREJPTS		34		# Rejected points
define	INLXMIN			35		# Minimum value for curve
define	INLXMAX			36		# Maximum value for curve
define	INLSFLOAT		37		# Floating point substructure
define	INLSGAXES		38		# Graphics substructure


# String valued parameters (in_gstr, in_pstr)

define	INLLABELS		40		# standard axis labels
define	INLUNITS		41		# standard axis units
define	INLFLABELS		42		# Function labels
define	INLFUNITS		43		# Function units
define	INLPLABELS		44		# Parameter labels
define	INLPUNITS		45		# Parameter units
define	INLVLABELS		46		# Variable labels
define	INLVUNITS		47		# Variable units
define	INLUSERLABELS		48		# User plot labels
define	INLUSERUNITS		49		# User plot units
define	INLHELP			50		# Help file name
define	INLPROMPT		51		# Help prompt


# -------------------------------------------------------------------------
# Graphic key and axis definitions. These are used to retrieve key/axis
# attributes using the in_gkey() and in_pkey() procedures.

# Max number of graph keys

define	INLNGKEYS		5


# Axis codes.

define	INLXAXIS		1		# X axis
define	INLYAXIS		2		# Y axis


# Graph key/axis codes.

define	KEY_FUNCTION	1 			# Function
define	KEY_FIT		2			# Fit
define	KEY_RESIDUALS	3			# Residuals
define	KEY_RATIO	4			# Ratio
define	KEY_NONLINEAR	5			# Non-linear part
define	KEY_VARIABLE	6			# Variable (user or default)
define	KEY_UAXIS	7			# User plot function
define	KEY_MIN		KEY_FUNCTION		# Min. key type
define	KEY_MAX		KEY_UAXIS		# Max. key type
