# ECF_EVAL -- Evaluate wavelength at a given order and pixel position.

double procedure ecf_eval (ecf, order, x)

pointer	ecf			# GSURFIT pointer
int	order			# Order
double	x			# X point

int	ecf_oeval()
double	y, dgseval()
include	"ecffit.com"

begin
	y = ecf_oeval (ecf, order)
	if (ecf == NULL)
	    return (x + shift / y)
	else
	    return ((dgseval (ecf, x, y) + shift) / y)
end


# ECF_VECTOR -- Evaluate echelle dispersion function for a vector of points of
# the same order.

procedure ecf_vector (ecf, order, x, fit, npts)

pointer	ecf			# GSURFIT pointer
int	order			# Order
double	x[npts]			# X points
double	fit[npts]		# Fitted points
int	npts			# Number of points

double	yval
pointer	sp, y
int	ecf_oeval()
include	"ecffit.com"

begin
	call smark (sp)
	call salloc (y, npts, TY_DOUBLE)

	yval = ecf_oeval (ecf, order)
	if (ecf == NULL)
	    call amovd (x, fit, npts)
	else {
	    call amovkd (yval, Memd[y], npts)
	    call dgsvector (ecf, x, Memd[y], fit, npts)
	    call adivkd (fit, yval, fit, npts)
	}
	if (shift != 0.)
	    call aaddkd (fit, shift / yval, fit, npts)

	call sfree (sp)
end


# ECF_OEVAL -- Evaluate the fit order.

int procedure ecf_oeval (ecf, order)

pointer	ecf			# GSURFIT pointer
int	order			# User order

include	"ecffit.com"

begin
	return (slope * order + offset)
end
