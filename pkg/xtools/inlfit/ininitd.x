.help ininit
INLFIT memory allocation procedures. All the calls to malloc() and realloc()
are grouped in this file. Acces to the INLFIT structure is restricted to
the in_get() and in_put() procedures, except for buffer allocation and
initialization.
.nf

User entry points:

    in_initd (in, func, dfunc, param, dparam, nparams, plist, nfparams)

Low level entry point:

    in_bfinitd (in, npts, nvars)
.fi
.endhelp

include	<pkg/inlfit.h>
include "inlfitdef.h"


# IN_INIT -- Initialize INLFIT parameter structure.

procedure in_initd (in, func, dfunc, param, dparam, nparams, plist, nfparams)

pointer	in			# INLFIT pointer
int	func			# fitting function address
int	dfunc			# derivative function address
double	param[nparams]		# parameter values
double	dparam[nparams]		# initial guess at uncertenties in parameters
int	nparams			# number of parameters
int	plist[nparams]		# list of active parameters
int	nfparams		# number of fitting paramters

begin
#	# Debug.
#	call eprintf (
#	    "in_init: in=%d, func=%d, dfunc=%d, npars=%d, nfpars=%d\n")
#	    call pargi (in)
#	    call pargi (func)
#	    call pargi (dfunc)
#	    call pargi (nparams)
#	    call pargi (nfparams)

	# Allocate the structure memory.
	call malloc (in, LEN_INLSTRUCT, TY_STRUCT)

	# Allocate memory for parameter values, changes, and list.
	call malloc (IN_PARAM  (in), nparams, TY_DOUBLE)
	call malloc (IN_DPARAM (in), nparams, TY_DOUBLE)
	call malloc (IN_PLIST  (in), nparams, TY_INT)

	# Allocate space for strings. All strings are limited
	# to SZ_LINE or SZ_FNAME.
	call malloc (IN_LABELS(in), SZ_LINE, TY_CHAR)
	call malloc (IN_UNITS(in), SZ_LINE, TY_CHAR)
	call malloc (IN_FLABELS(in), SZ_LINE, TY_CHAR)
	call malloc (IN_FUNITS(in), SZ_LINE, TY_CHAR)
	call malloc (IN_PLABELS(in), SZ_LINE, TY_CHAR)
	call malloc (IN_PUNITS(in), SZ_LINE, TY_CHAR)
	call malloc (IN_VLABELS(in), SZ_LINE, TY_CHAR)
	call malloc (IN_VUNITS(in), SZ_LINE, TY_CHAR)
	call malloc (IN_USERLABELS(in), SZ_LINE, TY_CHAR)
	call malloc (IN_USERUNITS(in), SZ_LINE, TY_CHAR)
	call malloc (IN_HELP(in), SZ_FNAME, TY_CHAR)
	call malloc (IN_PROMPT(in), SZ_FNAME, TY_CHAR)

	# Allocate space for floating point and graph substructures.
	call malloc (IN_SFLOAT (in), LEN_INLFLOAT, TY_DOUBLE)
	call malloc (IN_SGAXES (in), INLNGKEYS * LEN_INLGRAPH, TY_INT)

	# Enter procedure parameters into the structure.
	call in_puti (in, INLFUNCTION, func)
	call in_puti (in, INLDERIVATIVE, dfunc)
	call in_puti (in, INLNPARAMS, nparams)
	call in_puti (in, INLNFPARAMS, nfparams)
	call amovd  (param, Memd[IN_PARAM(in)], nparams)
	call amovd  (dparam, Memd[IN_DPARAM(in)], nparams)
	call amovi   (plist, Memi[IN_PLIST(in)], nparams)

	# Set defaults, just in case.
	call in_putd (in, INLTOLERANCE, double (0.01))
	call in_puti  (in, INLMAXITER, 3)
	call in_puti  (in, INLNREJECT, 0)
	call in_putd (in, INLLOW, double (3.0))
	call in_putd (in, INLHIGH, double (3.0))
	call in_putd (in, INLGROW, double (0.0))

	# Initialize the character strings.
	call in_pstr (in, INLLABELS, KEY_TYPES)
	call in_pstr (in, INLUNITS, "")
	call in_pstr (in, INLFLABELS, "")
	call in_pstr (in, INLFUNITS, "")
	call in_pstr (in, INLPLABELS, "")
	call in_pstr (in, INLPUNITS, "")
	call in_pstr (in, INLVLABELS, "")
	call in_pstr (in, INLVUNITS, "")
	call in_pstr (in, INLUSERLABELS, "")
	call in_pstr (in, INLUSERUNITS,  "")
	call in_pstr (in, INLHELP, IN_DEFHELP)
	call in_pstr (in, INLPROMPT, IN_DEFPROMPT)

	# Initialize user defined functions.
	call in_puti (in, INLUAXES,  INDEFI)
	call in_puti (in, INLUCOLON, INDEFI)
	call in_puti (in, INLUFIT,   INDEFI)

	# Initialize graph key, and axes.
	call in_puti (in, INLGKEY, 2)
	call in_pkey (in, 1, INLXAXIS, KEY_FUNCTION, INDEFI)
	call in_pkey (in, 1, INLYAXIS, KEY_FIT, INDEFI)
	call in_pkey (in, 2, INLXAXIS, KEY_FUNCTION, INDEFI)
	call in_pkey (in, 2, INLYAXIS, KEY_RESIDUALS, INDEFI)
	call in_pkey (in, 3, INLXAXIS, KEY_FUNCTION, INDEFI)
	call in_pkey (in, 3, INLYAXIS, KEY_RATIO, INDEFI)
	call in_pkey (in, 4, INLXAXIS, KEY_VARIABLE, 1)
	call in_pkey (in, 4, INLYAXIS, KEY_RESIDUALS, INDEFI)
	call in_pkey (in, 5, INLXAXIS, KEY_FUNCTION, INDEFI)
	call in_pkey (in, 5, INLYAXIS, KEY_RESIDUALS, INDEFI)

	# Initialize flags and counters.
	call in_puti (in, INLOVERPLOT, NO)
	call in_puti (in, INLPLOTFIT, NO)
	call in_puti (in, INLNREJPTS, 0)
	call in_puti (in, INLNVARS, 0)
	call in_puti (in, INLNPTS, 0)

	# Initialize pointers.
	call in_putp (in, INLREJPTS, NULL)
	call in_putp (in, INLXMIN,   NULL)
	call in_putp (in, INLXMAX,   NULL)
end


# IN_BFINIT -- Initialize the rejected point counter, number of variables,
# rejected point list, and the buffers containing the minimum and maximum
# variable values. The rejected point list and limit value buffers are
# reallocated, if necessary.

procedure in_bfinitd (in, npts, nvars)

pointer	in			# INLFIT descriptor
int	npts			# number of points
int	nvars			# number of variables

int	in_geti()

begin
#	# Debug.
#	call eprintf ("in_bfinit: in=%d, npts=%d, nvars=%d\n")
#	    call pargi (in)
#	    call pargi (npts)
#	    call pargi (nvars)

	# Clear rejected point counter, and initialize number of variables.
	call in_puti (in, INLNREJPTS, 0)

	# Reallocate space for rejected point list and initialize it.
	if (in_geti (in, INLNPTS) != npts) {
	    call in_puti (in, INLNPTS, npts)
	    call realloc (IN_REJPTS (in), npts, TY_INT)
	}
	call amovki  (NO, Memi[IN_REJPTS(in)], npts)

	# Reallocate space for minimum and maximum variable values.
	# Initialization is made afterwards.
	if (in_geti (in, INLNVARS) != nvars) {
	    call in_puti (in, INLNVARS, nvars)
	    call realloc (IN_XMIN (in), nvars, TY_DOUBLE)
	    call realloc (IN_XMAX (in), nvars, TY_DOUBLE)
	}
end
