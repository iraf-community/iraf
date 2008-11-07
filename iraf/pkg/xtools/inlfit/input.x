.help input
	in_puti (in, param, ival)
	in_putr (in, param, rval)
	in_putd (in, param, dval)
	in_putp (in, param, pval)
	in_pstr (in, param, str)
	in_pkey (in, key, axis, type, varnum)
.endhelp

include	<pkg/inlfit.h>
include "inlfitdef.h"


# IN_PUTI -- Put integer valued parameters.

procedure in_puti (in, param, ival)

pointer	in			# INLFIT pointer
int	param			# parameter to put
int	ival			# integer value

long	lval

begin
	lval = ival
	call in_putl (in,param,lval)
end


procedure in_putl (in, param, lval)

pointer	in			# INLFIT pointer
int	param			# parameter to put
long	lval			# long value

begin
	switch (param) {
	case INLNPARAMS:
	    IN_NPARAMS (in) = lval
	case INLNFPARAMS:
	    IN_NFPARAMS (in) = lval
	case INLNVARS:
	    IN_NVARS (in) = lval
	case INLNPTS:
	    IN_NPTS (in) = lval
	case INLMAXITER:
	    IN_MAXITER (in) = lval
	case INLNREJECT:
	    IN_NREJECT (in) = lval
	case INLNREJPTS:
	    IN_NREJPTS (in) = lval
	case INLUCOLON:
	    IN_UCOLON (in) = lval
	case INLUFIT:
	    IN_UFIT (in) = lval
	case INLOVERPLOT:
	    IN_OVERPLOT (in) = lval
	case INLPLOTFIT:
	    IN_PLOTFIT (in) = lval
	case INLFITERROR:
	    IN_FITERROR (in) = lval
	case INLGKEY:
	    if (lval < 1 || lval > INLNGKEYS)
		call error (0, "INLFIT, in_puti: Bad key number (INLGKEY)")
	    IN_GKEY (in) = lval
	default:
	    call error (0, "INLFIT, in_puti: Unknown parameter")
	}
end



# IN_PUT[RD] -- Put real/double valued parameters.

procedure in_putr (in, param, rval)

pointer	in			# INLFIT pointer
int	param			# parameter to put
real	rval			# value

begin
	switch (param) {
	case INLTOLERANCE:
	    IN_TOLR (in) = rval
	case INLLOW:
	    IN_LOWR (in) = rval
	case INLHIGH:
	    IN_HIGHR (in) = rval
	case INLGROW:
	    IN_GROWR (in) = rval
	default:
	    call error (0, "INLFIT, in_put[rd]: Unknown parameter")
	}
end

# IN_PUT[RD] -- Put real/double valued parameters.

procedure in_putd (in, param, dval)

pointer	in			# INLFIT pointer
int	param			# parameter to put
double	dval			# value

begin
	switch (param) {
	case INLTOLERANCE:
	    IN_TOLD (in) = dval
	case INLLOW:
	    IN_LOWD (in) = dval
	case INLHIGH:
	    IN_HIGHD (in) = dval
	case INLGROW:
	    IN_GROWD (in) = dval
	default:
	    call error (0, "INLFIT, in_put[rd]: Unknown parameter")
	}
end



# IN_PUTP -- Put pointer valued parameters.

procedure in_putp (in, param, pval)

pointer	in			# INLFIT pointer
int	param			# parameter to put
pointer	pval			# pointer value

begin
	switch (param) {
	case INLFUNCTION:
	    IN_FUNC (in) = pval
	case INLDERIVATIVE:
	    IN_DFUNC (in) = pval
	case INLUAXES:
	    IN_UAXES (in) = pval
	case INLPARAM:
	    IN_PARAM (in) = pval
	case INLDPARAM:
	    IN_DPARAM (in) = pval
	case INLPLIST:
	    IN_PLIST (in) = pval
	case INLSFLOAT:
	    IN_SFLOAT (in) = pval
	case INLREJPTS:
	    IN_REJPTS (in) = pval
	case INLXMIN:
	    IN_XMIN (in) = pval
	case INLXMAX:
	    IN_XMAX (in) = pval
	case INLSGAXES:
	    IN_SGAXES (in) = pval
	default:
	    call error (0, "INLFIT, in_putp: Unknown parameter")
	}
end


# IN_PSTR -- Put string valued parameters.

procedure in_pstr (in, param, str)

pointer	in			# INLFIT pointer
int	param			# parameter to put
char	str[ARB]		# string value

begin
	switch (param) {
	case INLLABELS:
	    call strcpy (str, Memc[IN_LABELS (in)], SZ_LINE)
	case INLUNITS:
	    call strcpy (str, Memc[IN_UNITS (in)], SZ_LINE)
	case INLFLABELS:
	    call strcpy (str, Memc[IN_FLABELS (in)], SZ_LINE)
	case INLFUNITS:
	    call strcpy (str, Memc[IN_FUNITS (in)], SZ_LINE)
	case INLPLABELS:
	    call strcpy (str, Memc[IN_PLABELS (in)], SZ_LINE)
	case INLPUNITS:
	    call strcpy (str, Memc[IN_PUNITS (in)], SZ_LINE)
	case INLVLABELS:
	    call strcpy (str, Memc[IN_VLABELS (in)], SZ_LINE)
	case INLVUNITS:
	    call strcpy (str, Memc[IN_VUNITS (in)], SZ_LINE)
	case INLUSERLABELS:
	    call strcpy (str, Memc[IN_USERLABELS (in)], SZ_LINE)
	case INLUSERUNITS:
	    call strcpy (str, Memc[IN_USERUNITS (in)], SZ_LINE)
	case INLHELP:
	    call strcpy (str, Memc[IN_HELP (in)], SZ_FNAME)
	case INLPROMPT:
	    call strcpy (str, Memc[IN_PROMPT (in)], SZ_FNAME)
	default:
	    call error (0, "INLFIT, in_pstr: Unknown parameter")
	}
end


# IN_PKEY -- Put key parameters.

procedure in_pkey (in, key, axis, type, varnum)

pointer	in			# INLFIT pointer
int	key			# key to put
int	axis			# axis number
int	type			# axis type
int	varnum			# axis variable number

begin
	# Check ranges
	if (key < 1 || key > INLNGKEYS)
	    call error (0, "INLFIT, in_pkey: Illegal key")
	if (type < KEY_MIN || type > KEY_MAX)
	    call error (0, "INLFIT, in_pkey: Illegal key type")
	
	# Enter data
	if (axis == INLXAXIS) {
	    IN_GXTYPE   (in, key) = type
	    IN_GXNUMBER (in, key) = varnum
	} else if (axis == INLYAXIS) {
	    IN_GYTYPE   (in, key) = type
	    IN_GYNUMBER (in, key) = varnum
	} else
	    call error (0,"INLFIT, in_pkey: Illegal axis number")
end
