# ECF_GSHIFT -- Return the shift for the given order.

double procedure ecf_gshift (ecf, order)

pointer	ecf			# GSURFIT pointer
int	order			# User order

include	"ecffit.com"

begin
	return (shift / (slope * order + offset))
end


# ECF_PSHIFT -- Put the shift for the given order.

procedure ecf_pshift (ecf, order, shft)

pointer	ecf			# GSURFIT pointer
int	order			# User order
double	shft			# Shift at given order

include	"ecffit.com"

begin
	shift = shft * (slope * order + offset)
end


procedure ecf_vector (ecf, order, x, fit, npts)

pointer	ecf			# GSURFIT pointer
int	order			# Order
double	x[npts]			# X points
double	fit[npts]		# Fitted points
int	npts			# Number of points

double	yval
pointer	sp, y

include	"ecffit.com"

begin
	call smark (sp)
	call salloc (y, npts, TY_DOUBLE)

	yval = slope * order + offset
	call amovkd (yval, Memd[y], npts)
	call dgsvector (ecf, x, Memd[y], fit, npts)
	call adivkd (fit, yval, fit, npts)
	if (shift != 0.)
	    call aaddkd (fit, shift / yval, fit, npts)

	call sfree (sp)
end
