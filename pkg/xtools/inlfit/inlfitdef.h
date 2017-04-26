# The INLFIT data structure and private definitions.

# Pointer Mem

define	MEMP		Memi


# Default help file and prompt

define	IN_DEFHELP	"lib$scr/inlgfit.key"
define	IN_DEFPROMPT	"inlfit cursor options"


# Graphic key/axis types
define	KEY_TYPES	"|function|fit|residuals|ratio|nonlinear|var|user|"


# ---------------------------------------------------------------------- 
# INLFIT structure definition.

# Structure length.
define	LEN_INLSTRUCT	37

# NLFIT parameters. These parameters are stored in the INLFIT structure,
# and passed without change to the NLFIT package. The NLFIT descriptor
# is stored here as well.

#define	IN_TYPE		Memi[$1+0]	# calculation type (TY_REAL, TY_DOUBLE)
define	IN_FUNC		Memi[$1+1]	# fitting function
define	IN_DFUNC	Memi[$1+2]	# derivative function
define	IN_NPARAMS	Memi[$1+3]	# number of parameters
define	IN_NFPARAMS	Memi[$1+4]	# number of fitted parameters
define	IN_PARAM	MEMP[$1+5]	# pointer to parameter vector
define	IN_DPARAM	MEMP[$1+6]	# pointer to par. change vector
define	IN_PLIST	MEMP[$1+7]	# parameter list
define	IN_MAXITER	Memi[$1+8]	# max number of iterations

# INLFIT parameters used to keep track of the number of variables and
# number of points in the fit. These numbers are used to decide buffer
# reallocation.

define	IN_NVARS	Memi[$1+9]	# number of variables
define	IN_NPTS		Memi[$1+10]	# number of points

# INLFIT floating point substructure. This substructure is used to
# store a pointer to a separate buffer, containing floating point
# numbers.

define	IN_SFLOAT	MEMP[$1+11]	# pointer to subs. with reals/doubles

# INLFIT parameters used for automatic data rejection. The rejection
# limits and the grow radius are stored in the floating point substructure.

define	IN_NREJECT	Memi[$1+12]	# number of rejection iteration

# INLFIT parameters used to store the rejected point counter, and a
# pointer to the rejected point list.

define	IN_NREJPTS	Memi[$1+13]	# number of rejected points
define	IN_REJPTS	MEMP[$1+14]	# pointer to buffer with rejected pts.

# INLFIT parameters used to store user defined procedures addresses.
# These parameters are used by the zcall*() procedures.

define	IN_UAXES	Memi[$1+15]	# plot function
define	IN_UCOLON	Memi[$1+16]	# default colon command
define	IN_UFIT		Memi[$1+17]	# default interactive fit command

# INLFIT parameters used to store pointers to separate buffers, containing
# the minimum and maximum values of all the input variables. The number
# of variables is kept as well.

define	IN_XMIN		MEMP[$1+18]	# pointer to buffer with min. values
define	IN_XMAX		MEMP[$1+19]	# pointer to buffer with max. values

# INLFIT flags.

define	IN_OVERPLOT	Memi[$1+20]	# overplot next plot ?
define	IN_PLOTFIT	Memi[$1+21]	# overplot fit ?
define	IN_FITERROR	Memi[$1+22]	# error fit code

# INLFIT string parameters used for interactive graphics. These are
# pointers to the actual strings.

define	IN_LABELS	MEMP[$1+23]	# standard axis labels
define	IN_UNITS	MEMP[$1+24]	# standard axis units
define	IN_FLABELS	MEMP[$1+25]	# function and fit labels
define	IN_FUNITS	MEMP[$1+26]	# function and fit units
define	IN_PLABELS	MEMP[$1+27]	# parameter labels
define	IN_PUNITS	MEMP[$1+28]	# parameter units
define	IN_VLABELS	MEMP[$1+29]	# variable labels
define	IN_VUNITS	MEMP[$1+30]	# variable units
define	IN_USERLABELS	MEMP[$1+31]	# user plot labels
define	IN_USERUNITS	MEMP[$1+32]	# user plot units
define	IN_HELP		MEMP[$1+33]	# help file name
define	IN_PROMPT	MEMP[$1+34]	# help prompt

# INLFIT graph key definitions.

define	IN_GKEY		Memi[$1+35]	# current graph key
define	IN_SGAXES	MEMP[$1+36]	# pointer to subs. with graph keys 

# next free location	($1 + 37) == LEN_INLSTRUCT


# ---------------------------------------------------------------------- 
# Floating point number substructures (real, double). This is an easy way
# to avoid having to deal with mixed floating point types in the main
# structure. The macro parameter is the main structure pointer. The
# substructure used depends on the calculation type.

# Substructure length

define	LEN_INLFLOAT	4

# Real version

define	IN_TOLR		Memr[IN_SFLOAT($1)+0]	# tolerance of convergence
define	IN_LOWR		Memr[IN_SFLOAT($1)+1]	# low rejection value
define	IN_HIGHR	Memr[IN_SFLOAT($1)+2]	# high rejection value
define	IN_GROWR	Memr[IN_SFLOAT($1)+3]	# rejection growing radius

# Double precission version

define	IN_TOLD		Memd[IN_SFLOAT($1)+0]	# tolerance of convergence
define	IN_LOWD		Memd[IN_SFLOAT($1)+1]	# low rejection value
define	IN_HIGHD	Memd[IN_SFLOAT($1)+2]	# high rejection value
define	IN_GROWD	Memd[IN_SFLOAT($1)+3]	# rejection growing radius


# ---------------------------------------------------------------------- 
# Graph axes substructure. The macro parameters are the pointer to the
# main structure, and the key number. The actual size of the graph axes
# buffer will be equal to the maximum number of keys (IN_GKEYS) times
# the substructure length (LEN_INLGRAPH). The type is one of the possible
# codes for KEY_TYPES, and the number is used to keep track of the variable
# or user supplied function numbers.

# Substructure length

define	LEN_INLGRAPH	4

# Substructure definition

define	IN_GXTYPE	Memi[IN_SGAXES($1)+($2-1)*LEN_INLGRAPH+0] # x axis type
define	IN_GXNUMBER	Memi[IN_SGAXES($1)+($2-1)*LEN_INLGRAPH+1] # x axis num.
define	IN_GYTYPE	Memi[IN_SGAXES($1)+($2-1)*LEN_INLGRAPH+2] # y axis type
define	IN_GYNUMBER	Memi[IN_SGAXES($1)+($2-1)*LEN_INLGRAPH+3] # y axis num.
