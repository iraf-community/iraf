include	"identify.h"

# ID_PEAK -- Find the peak value above continuum.

double procedure id_peak (id, pix)

pointer	id			# ID pointer
double	pix			# Pixel position
double	peak			# Peak value

int	c, l, u

begin
	if (IS_INDEFD(pix))
	    return (INDEFD)

	c = nint (pix)
	l = max (1, nint (pix - ID_FWIDTH(id)))
	u = min (ID_NPTS(id), nint (pix + ID_FWIDTH(id)))
	peak = IMDATA(id,c) - (IMDATA(id,l) + IMDATA(id,u)) / 2.

	return (peak)
end
