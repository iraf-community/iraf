# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.


include	"/usr/vesa/spec/terpdef.h"

# Quickie benchmark, 1-d sequential image interpolators.

define	MAX_PIXELS	2048
define	SZ_NAME		20


task	interp


procedure interp()

real	data[MAX_PIXELS], x[MAX_PIXELS], y[MAX_PIXELS]
real	coeff[2 * MAX_PIXELS + SZ_ASI]
char	interp_name[SZ_NAME]
int	i, j, npix, nlines, interpolant
bool	streq()
int	clgeti()

begin
	npix = min (MAX_PIXELS, clgeti ("npix"))
	nlines = npix				# square image
	call clgstr ("interpolant", interp_name, SZ_NAME)

	if (streq (interp_name, "nearest"))
	    interpolant = IT_NEAR
	else if (streq (interp_name, "linear"))
	    interpolant = IT_LINEAR
	else if (streq (interp_name, "poly3"))
	    interpolant = IT_POLY3
	else if (streq (interp_name, "poly5"))
	    interpolant = IT_POLY5
	else if (streq (interp_name, "spline3"))
	    interpolant = IT_SPLINE3
	else
	    call error (0, "Unknown interpolant keyword")

	call asiset (coeff, II_INIT, 0)
	call asiset (coeff, II_INTERPOLANT, interpolant)
	call asiset (coeff, II_ALLGOODPIX, YES)

	do i = 1, npix {			# initialize data, x arrays
	    data[i] = max(1.0, min(real(npix), real(i) + 0.15151))
	    x[i] = i
	}

	do j = 1, nlines {			# interpolate npix ** 2
	    call asifit (data, npix, coeff)
	    call asieva (x, y, npix, coeff)
	}
end
