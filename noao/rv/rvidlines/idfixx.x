include	<smw.h>

# ID_FIXX - Adjust so that pixel indices are increasing.

procedure id_fixx (sh, x1, x2, y1, y2, i1, i2)

pointer	sh
real	x1, x2, y1, y2
int	i1, i2

double	z, z1, z2, shdr_wl(), shdr_lw()

begin
	z1 = x1
	z2 = x2
	z1 = max (0.5D0, min (double (SN(sh)+.499), shdr_wl(sh, z1)))
	z2 = max (0.5D0, min (double (SN(sh)+.499), shdr_wl(sh, z2)))
	if (z1 > z2) {
	    z = y1; y1 = y2; y2 = z
	    z = z1; z1 = z2; z2 = z
	}

	x1 = shdr_lw (sh, z1)
	x2 = shdr_lw (sh, z2)
	i1 = nint (z1)
	i2 = nint (z2)
end
