include	"ecidentify.h"

# EC_PEAK -- Find the peak value above continuum.

double procedure ec_peak (ec, pix)

pointer	ec			# ID pointer
double	pix			# Pixel position
double	peak			# Peak value

int	c, l, u

begin
	if (IS_INDEFD(pix))
	    return (INDEFD)

	c = nint (pix)
	l = max (1, nint (pix - EC_FWIDTH(ec)))
	u = min (EC_NPTS(ec), nint (pix + EC_FWIDTH(ec)))
	peak = IMDATA(ec,c) - (IMDATA(ec,l) +
	    IMDATA(ec,u)) / 2.

	return (peak)
end
