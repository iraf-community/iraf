# ECF_OEVAL -- Evaluate the fit order.

double procedure ecf_oeval (ecf, order)

pointer	ecf			# GSURFIT pointer
int	order			# User order

include	"ecffit.com"

begin
	return (slope * order + offset)
end
