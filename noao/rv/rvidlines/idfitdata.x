include	<smw.h>
include	"identify.h"

# ID_FITDATA -- Compute fit coordinates from pixel coordinates.

procedure id_fitdata (id)

pointer	id				# ID pointer
int	i

begin
	call mfree (ID_FITDATA(id), TY_DOUBLE)
	call malloc (ID_FITDATA(id), ID_NPTS(id), TY_DOUBLE)

	if (ID_CV(id) == NULL)
	    call achtrd (Memr[SX(ID_SH(id))], FITDATA(id,1), ID_NPTS(id))
	else {
	    call dcvvector (ID_CV(id), PIXDATA(id,1), FITDATA(id,1),
		ID_NPTS(id))
	    if (FITDATA(id,2) > FITDATA(id,1)) {
	        do i = 3, ID_NPTS(id)
		    if (FITDATA(id,i) < FITDATA(id,i-1))
			call error (1, "Coordinate solution is not monotonic")
	    } else {
	        do i = 3, ID_NPTS(id)
		    if (FITDATA(id,i) > FITDATA(id,i-1))
			call error (1, "Coordinate solution is not monotonic")
	    }
	}
	if (ID_SHIFT(id) != 0.)
	    call aaddkd (FITDATA(id,1), ID_SHIFT(id), FITDATA(id,1),ID_NPTS(id))
end


# ID_FITFEATURES -- Compute fit coordinates for features.

procedure id_fitfeatures (id)

pointer	id				# ID pointer
int	i

double	id_fitpt()

begin
	if (ID_NFEATURES(id) < 1)
	    return

	if (ID_CV(id) == NULL)
	    do i = 1, ID_NFEATURES(id)
		FIT(id,i) = id_fitpt (id, PIX(id,i))
	else {
	    call dcvvector (ID_CV(id), PIX(id,1), FIT(id,1), ID_NFEATURES(id))
	    if (ID_SHIFT(id) != 0.)
	        call aaddkd (FIT(id,1), ID_SHIFT(id), FIT(id,1), ID_NFEATURES(id))
	}
end


# ID_FITPT -- Compute fit coordinates from pixel coordinates.

double procedure id_fitpt (id, pix)

pointer	id			# ID pointer
double	pix			# Pixel coordinate

double	fit

double	smw_c1trand(), shdr_lw(), dcveval()

begin
	if (ID_CV(id) == NULL) {
	    fit = smw_c1trand (ID_PL(id), pix)
	    fit = shdr_lw (ID_SH(id), fit)
	} else
	    fit = dcveval (ID_CV(id), pix)
	fit = fit + ID_SHIFT(id)

	return (fit)
end


# FIT_TO_PIX -- Transform fit coordinate to pixel coordinate.

define	DXMIN	.01

double procedure fit_to_pix (id, fitcoord)

pointer	id				# ID pointer
double	fitcoord		# Fit coordinate to be transformed
double	pixcoord		# Pixel coordinate returned

int	i, np1
double	dx

double	smw_c1trand(), id_fitpt()

begin
	np1 = NP1(ID_SH(id)) - 1
	if (FITDATA(id,1) < FITDATA(id,ID_NPTS(id))) {
	    if ((fitcoord<FITDATA(id,1)) || (fitcoord>FITDATA(id,ID_NPTS(id))))
	        return (INDEFD)

	    for (i = 1; fitcoord > FITDATA(id,i); i = i + 1)
	        ;

	    if (FITDATA(id,i) == fitcoord)
	        return (PIXDATA(id,i))

	    pixcoord = smw_c1trand (ID_LP(id), double(i+np1-.5))
	    dx = smw_c1trand (ID_LP(id), double(i+np1+.5)) - pixcoord
	    while (dx > DXMIN) {
	        dx = dx / 2
	        if (id_fitpt (id, pixcoord) < fitcoord)
		    pixcoord = pixcoord + dx
	        else
		    pixcoord = pixcoord - dx
	    }
	} else {
	    if ((fitcoord<FITDATA(id,ID_NPTS(id))) || (fitcoord>FITDATA(id,1)))
	        return (INDEFD)

	    for (i = 1; fitcoord < FITDATA(id,i); i = i + 1)
	        ;

	    if (FITDATA(id,i) == fitcoord)
	        return (PIXDATA(id,i))

	    pixcoord = smw_c1trand (ID_LP(id), double(i+np1-.5))
	    dx = smw_c1trand (ID_LP(id), double(i+np1+.5)) - pixcoord
	    while (dx > DXMIN) {
	        dx = dx / 2
	        if (id_fitpt (id, pixcoord) < fitcoord)
		    pixcoord = pixcoord - dx
	        else
		    pixcoord = pixcoord + dx
	    }
	}

	return (pixcoord)
end
