.help inget
	int    = in_geti (in, param)
	pointer= in_getp (in, param)
	real   = in_getr (in, param)
	double = in_getd (in, param)
	         in_gstr (in, param, str, maxch)
	         in_gkey (in, key, axis, type, varnum)
.endhelp

include	<pkg/inlfit.h>
include "inlfitdef.h"

# IN_GETI -- Get integer valued parameters.

int procedure in_geti (in, param)

pointer	in			# INLFIT pointer
int	param			# parameter to get

begin
	switch (param) {
	case INLFUNCTION:
	    return (IN_FUNC (in))
	case INLDERIVATIVE:
	    return (IN_DFUNC (in))
	case INLNPARAMS:
	    return (IN_NPARAMS (in))
	case INLNFPARAMS:
	    return (IN_NFPARAMS (in))
	case INLNVARS:
	    return (IN_NVARS (in))
	case INLNPTS:
	    return (IN_NPTS (in))
	case INLMAXITER:
	    return (IN_MAXITER (in))
	case INLNREJECT:
	    return (IN_NREJECT(in))
	case INLNREJPTS:
	    return (IN_NREJPTS (in))
	case INLUAXES:
	    return (IN_UAXES (in))
	case INLUCOLON:
	    return (IN_UCOLON (in))
	case INLUFIT:
	    return (IN_UFIT (in))
	case INLOVERPLOT:
	    return (IN_OVERPLOT (in))
	case INLPLOTFIT:
	    return (IN_PLOTFIT (in))
	case INLFITERROR:
	    return (IN_FITERROR (in))
	case INLGKEY:
	    return (IN_GKEY (in))
	default:
	    call error (0, "INLFIT, in_geti: Unknown parameter")
	}
end



# IN_GET[RD] -- Get real/double valued parameters.

real procedure in_getr (in, param)

pointer	in			# INLFIT pointer
int	param			# parameter to get

begin
	switch (param) {
	case INLTOLERANCE:
	    return (IN_TOLR (in))
	case INLLOW:
	    return (IN_LOWR (in))
	case INLHIGH:
	    return (IN_HIGHR (in))
	case INLGROW:
	    return (IN_GROWR (in))
	default:
	    call error (0, "INLFIT, in_get[rd]: Unknown parameter")
	}
end

# IN_GET[RD] -- Get real/double valued parameters.

double procedure in_getd (in, param)

pointer	in			# INLFIT pointer
int	param			# parameter to get

begin
	switch (param) {
	case INLTOLERANCE:
	    return (IN_TOLD (in))
	case INLLOW:
	    return (IN_LOWD (in))
	case INLHIGH:
	    return (IN_HIGHD (in))
	case INLGROW:
	    return (IN_GROWD (in))
	default:
	    call error (0, "INLFIT, in_get[rd]: Unknown parameter")
	}
end



# IN_GETP -- Get pointer valued parameters.

pointer procedure in_getp (in, param)

pointer	in			# INLFIT pointer
int	param			# parameter to get

begin
	switch (param) {
	case INLPARAM:
	    return (IN_PARAM (in))
	case INLDPARAM:
	    return (IN_DPARAM (in))
	case INLPLIST:
	    return (IN_PLIST (in))
	case INLSFLOAT:
	    return (IN_SFLOAT (in))
	case INLREJPTS:
	    return (IN_REJPTS (in))
	case INLXMIN:
	    return (IN_XMIN (in))
	case INLXMAX:
	    return (IN_XMAX (in))
	case INLSGAXES:
	    return (IN_SGAXES (in))
	default:
	    call error (0, "INLFIT, in_getp: Unknown parameter")
	}
end


# IN_GETC -- Get character pointer valued parameters.

pointer procedure in_getc (in, param)

pointer	in			# INLFIT pointer
int	param			# parameter to get

begin
	switch (param) {
	case INLLABELS:
	    return (IN_LABELS (in))
	case INLUNITS:
	    return (IN_UNITS (in))
	case INLFLABELS:
	    return (IN_FLABELS (in))
	case INLFUNITS:
	    return (IN_FUNITS (in))
	case INLPLABELS:
	    return (IN_PLABELS (in))
	case INLPUNITS:
	    return (IN_PUNITS (in))
	case INLVLABELS:
	    return (IN_VLABELS (in))
	case INLVUNITS:
	    return (IN_VUNITS (in))
	case INLUSERLABELS:
	    return (IN_USERLABELS (in))
	case INLUSERUNITS:
	    return (IN_USERUNITS (in))
	case INLHELP:
	    return (IN_HELP (in))
	case INLPROMPT:
	    return (IN_PROMPT (in))
	default:
	    call error (0, "INLFIT, in_getc: Unknown parameter")
	}
end


# IN_GSTR -- Get string valued parameters.

procedure in_gstr (in, param, str, maxch)

pointer	in			# INLFIT pointer
int	param			# parameter to get
char	str[maxch]		# string value
int	maxch			# maximum number of characters

begin
	switch (param) {
	case INLLABELS:
	    call strcpy (Memc[IN_LABELS (in)], str, maxch)
	case INLUNITS:
	    call strcpy (Memc[IN_UNITS (in)], str, maxch)
	case INLFLABELS:
	    call strcpy (Memc[IN_FLABELS (in)], str, maxch)
	case INLFUNITS:
	    call strcpy (Memc[IN_FUNITS (in)], str, maxch)
	case INLPLABELS:
	    call strcpy (Memc[IN_PLABELS (in)], str, maxch)
	case INLPUNITS:
	    call strcpy (Memc[IN_PUNITS (in)], str, maxch)
	case INLVLABELS:
	    call strcpy (Memc[IN_VLABELS (in)], str, maxch)
	case INLVUNITS:
	    call strcpy (Memc[IN_VUNITS (in)], str, maxch)
	case INLUSERLABELS:
	    call strcpy (Memc[IN_USERLABELS (in)], str, maxch)
	case INLUSERUNITS:
	    call strcpy (Memc[IN_USERUNITS (in)], str, maxch)
	case INLHELP:
	    call strcpy (Memc[IN_HELP (in)], str, maxch)
	case INLPROMPT:
	    call strcpy (Memc[IN_PROMPT (in)], str, maxch)
	default:
	    call error (0, "INLFIT, in_gstr: Unknown parameter")
	}
end


# IN_GKEY -- Get key parameters.

procedure in_gkey (in, key, axis, type, varnum)

pointer	in			# INLFIT pointer
int	key			# key to get
int	axis			# axis number
int	type			# axis type (output)
int	varnum			# axis variable number (output)

begin
	# Check ranges
	if (key < 1 || key > INLNGKEYS)
	    call error (0, "INLFIT, in_pkey: Illegal key")

	# Get data
	if (axis == INLXAXIS) {
	    type   = IN_GXTYPE   (in, key)
	    varnum = IN_GXNUMBER (in, key)
	} else if (axis == INLYAXIS) {
	    type   = IN_GYTYPE   (in, key)
	    varnum = IN_GYNUMBER (in, key)
	} else
	    call error (0, "INLFIT, in_gkey: Illegal axis")
end
