include	"identify.h"


# ID_VELOCITY -- Compute velocity.

procedure id_velocity (id, interactive)

pointer	id			# ID pointer
int	interactive		# Called interactively?

int	i, n
double	z, sumz, sumz2, sumw, zerr, zhelio, v, verr

begin
	sumz = 0
	sumw = 0
	n = 0
	for (i=1; i <= ID_NFEATURES(id); i = i + 1) {
	    if (IS_INDEFD (USER(id,i)) || WTS(id,i) == 0.)
		next
	    z = (FIT(id,i) - USER(id,i)) / USER(id,i)
	    sumz = sumz + WTS(id,i) * z
	    sumw = sumw + WTS(id,i)
	    n = n + 1
	}

	if (sumw > 0.) {
	    zhelio = ID_ZHELIO(id)
	    sumz = sumz / sumw

	    sumz2 = 0.
	    for (i=1; i <= ID_NFEATURES(id); i = i + 1) {
		if (IS_INDEFD (USER(id,i)) || WTS(id,i) == 0.)
		    next
		z = (FIT(id,i) - USER(id,i)) / USER(id,i)
		sumz2 = sumz2 + WTS(id,i) * (z - sumz) ** 2
	    }
	    if (sumz2 > 0.)
		sumz2 = sqrt (sumz2 / sumw)
	    else
		sumz2 = 0.
	    zerr = sumz2
	    if (n > 1)
		zerr = zerr / sqrt (n - 1.)

	    if (interactive == YES) {
		v = (sumz + zhelio) * VLIGHT
		verr = zerr * VLIGHT

		if (zhelio == 0D0)
		    call printf (
			"%s%s: Lines=%3d, Vobs=%.5g (%.5g), Zobs=%.5g (%.5g)\n")
		else
		    call printf (
		    "%s%s: Lines=%3d, Vhelio=%.5g (%.5g), Zhelio=%.5g (%.5g)\n")

		    call pargstr (Memc[ID_IMAGE(id)])
		    call pargstr (Memc[ID_SECTION(id)])
		    call pargi (n)
		    call pargd (v)
		    call pargd (verr)
		    call pargd (sumz + zhelio)
		    call pargd (zerr)
	    }
	    ID_REDSHIFT(id) = sumz
	    ID_RMSRED(id) = sumz2
	    ID_ZHELIO(id) = zhelio
	}
end
