include	<smw.h>
include	<units.h>
include	"identify.h"


# ID_VELOCITY -- Compute velocity.

procedure id_velocity (id, interactive)

pointer	id			# ID pointer
int	interactive		# Called interactively?

int	i, n
double	z, sumz, sumz2, sumw, zerr, zhelio, v, verr, id_zval()

begin
	sumz = 0
	sumw = 0
	n = 0
	for (i=1; i <= ID_NFEATURES(id); i = i + 1) {
	    if (IS_INDEFD (USER(id,i)) || WTS(id,i) == 0.)
		next
	    z = id_zval (id, FIT(id,i), USER(id,i))
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
		z = id_zval (id, FIT(id,i), USER(id,i))
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


# ID_ZVAL -- Compute Z value.

double procedure id_zval (id, x, xref)

pointer	id		#I Identify pointer
double	x		#I Coordinate
double	xref		#I Reference coordinate
double	z		#O Z value

double	y, yref
pointer	un

begin
	y = x
	yref = xref

	un = UN(ID_SH(id))
	if (UN_LOG(un) == YES) {
	    y = 10D0 ** y
	    yref = 10D0 ** yref
	}
	if (UN_INV(un) == YES) {
	    y = 1D0 / y
	    yref = 1D0 / yref
	}

	switch (UN_CLASS(un)) {
	case UN_WAVE:
	    z = (y - yref) / yref
	case UN_FREQ, UN_ENERGY:
	    z = (yref - y) / y
	case UN_VEL:
	    y = sqrt ((1 + y) / (1 - y))
	    yref = sqrt ((1 + yref) / (1 - yref))
	    z = (y - yref) / yref
	case UN_DOP:
	    y = y + 1
	    yref = yref + 1
	    z = (y - yref) / yref
	}

	return (z)
end



# ID_ZSHIFTD -- Shift coordinate by redshift.

double procedure id_zshiftd (id, x, dir)

pointer	id		#I Identify pointer
double	x		#I Coordinate
int	dir		#I Direction (0=to rest, 1=from rest)
double	y		#O Shifted coordinate

pointer	un

begin
	y = x

	un = UN(ID_SH(id))
	if (UN_LOG(un) == YES)
	    y = 10D0 ** y
	if (UN_INV(un) == YES)
	    y = 1D0 / y

	switch (UN_CLASS(un)) {
	case UN_WAVE:
	    if (dir == 0)
		y = y / (1 + ID_REDSHIFT(id))
	    else
		y = y * (1 + ID_REDSHIFT(id))
	case UN_FREQ, UN_ENERGY:
	    if (dir == 0)
		y = y * (1 + ID_REDSHIFT(id))
	    else
		y = y / (1 + ID_REDSHIFT(id))
	case UN_VEL:
	    y = sqrt ((1 + y) / (1 - y))
	    if (dir == 0)
		y = y / (1 + ID_REDSHIFT(id))
	    else
		y = y * (1 + ID_REDSHIFT(id))
	    y = y ** 2
	    y = (y - 1) / (y + 1)
	case UN_DOP:
	    y = (y + 1)
	    if (dir == 0)
		y = (y + 1) / (1 + ID_REDSHIFT(id)) - 1
	    else
		y = (y + 1) * (1 + ID_REDSHIFT(id)) - 1
	}

	if (UN_INV(un) == YES)
	    y = 1D0 / y
	if (UN_LOG(un) == YES)
	    y = log10 (y)

	return (y)
end


# ID_ZSHIFTR -- Shift coordinate by redshift.

real procedure id_zshiftr (id, x, dir)

pointer	id		#I Identify pointer
real	x		#I Coordinate
int	dir		#I Direction (0=to rest, 1=from rest)

double	id_zshiftd()

begin
	return (real (id_zshiftd (id, double(x), dir)))
end
