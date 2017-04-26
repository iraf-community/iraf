task test = t_test

include <math/gsurfit.h>

procedure t_test()

int	i, j, k
int	xorder, yorder, xterms, stype
int	ncoeff, maxorder, xincr, npts, stype1, ier
pointer	gs, ags, sgs
double	dx, dy, const, accum, sum, rms1, rms2
double	x[121], y[121], z[121], w[121], zfit[121], coeff[121], save[121]
int	clgeti()
double	dgseval(), dgsgcoeff()

begin
	# Generate x and y grid.
	dy = -1.0d0
	npts = 0
	do i = 1, 9 {
	    dx = -1.0d0
	    do j = 1, 9 {
		x[npts+1] = dx
		y[npts+1] = dy
		npts = npts + 1
		dx = dx + 0.25d0
	    }
	    dy = dy + 0.25d0
	}

	stype = clgeti ("stype")
	xorder = clgeti ("xorder")
	yorder = clgeti ("yorder")
	xterms = clgeti ("xterms")
	call printf ("\n\nSURFACE: %d XORDER: %d YORDER: %d XTERMS: %d\n")
	    call pargi (stype)
	    call pargi (xorder)
	    call pargi (yorder)
	    call pargi (xterms)

	# Generate data
	if (stype > 3) {
	    switch (xterms) {
	    case GS_XNONE:

		do i = 1, npts {
		    sum = 0.0d0
		    do j = 2, yorder
			sum = sum + j * y[i] ** (j - 1)
		    do j = 2, xorder
			sum = sum + j * x[i] ** (j - 1)
		    z[i] = sum
		}

	    case GS_XHALF:

		maxorder = max (xorder + 1, yorder + 1)
		do i = 1, npts {
		    sum = 0.0d0
		    xincr = xorder
		    do j = 1, yorder {
			const = j * y[i] ** (j - 1)
			accum= 0.0d0
			do k = 1, xincr {
			    if (j > 1 || k > 1)
				accum = accum + k * x[i] ** (k - 1)
			}
			sum = sum + const * accum
			if ((j + xorder + 1) > maxorder)
			    xincr = xincr - 1
		    }
		    z[i] = sum
		}

	    case GS_XFULL:

		do i = 1, npts {
		    sum = 0.0d0
		    do j = 1, yorder {
			const = j * y[i] ** (j - 1)
			accum = 0.0d0
			do k = 1, xorder {
			    if (j > 1 || k > 1)
				accum = accum + k * x[i] ** (k - 1)
			}
			sum = sum + const * accum
		    }
		    z[i] = sum
		}
	    }

	    stype1 = stype - 3
	} else {
	    switch (xterms) {
	    case GS_XNONE:

		do i = 1, npts {
		    sum = 0.0d0
		    do j = 2, yorder
			sum = sum + j * y[i] ** (j - 1)
		    do j = 1, xorder
			sum = sum + j * x[i] ** (j - 1)
		    z[i] = sum
		}

	    case GS_XHALF:

		maxorder = max (xorder + 1, yorder + 1)
		do i = 1, npts {
		    sum = 0.0d0
		    xincr = xorder
		    do j = 1, yorder {
			const = j * y[i] ** (j - 1)
			accum= 0.0d0
			do k = 1, xincr {
			    accum = accum + k * x[i] ** (k - 1)
			}
			sum = sum + const * accum
			if ((j + xorder + 1) > maxorder)
			    xincr = xincr - 1
		    }
		    z[i] = sum
		}

	    case GS_XFULL:

		do i = 1, npts {
		    sum = 0.0d0
		    do j = 1, yorder {
			const = j * y[i] ** (j - 1)
			accum = 0.0d0
			do k = 1, xorder {
			    accum = accum + k * x[i] ** (k - 1)
			}
			sum = sum + const * accum
		    }
		    z[i] = sum
		}
	    }

	    stype1 = stype
	}

	# Print out the data.
	call printf ("\nXIN:\n")
	do i = 1, npts {
	    call printf ("%6.3f ")
		call pargd (x[i])
	    if (mod (i, 9) == 0)
		call printf ("\n")
	}
	call printf ("\n")

	call printf ("\nYIN:\n")
	do i = 1, npts {
	    call printf ("%6.3f ")
		call pargd (y[i])
	    if (mod (i, 9) == 0)
		call printf ("\n")
	}
	call printf ("\n")

	call printf ("\nZIN:\n")
	do i = 1, npts {
	    call printf ("%6.3f ")
		call pargd (z[i])
	    if (mod (i, 9) == 0)
		call printf ("\n")
	}
	call printf ("\n")

	# Fit surface.
	call dgsinit (gs, stype1, xorder, yorder, xterms, -1.0d0, 1.0d0,
	    -1.0d0, 1.0d0)
	if (stype > 3) {
	    call dgsset (gs, GSXREF, 0d0)
	    call dgsset (gs, GSYREF, 0d0)
	    call dgsset (gs, GSZREF, 0d0)
	}
	call dgsfit (gs, x, y, z, w, npts, WTS_UNIFORM, ier) 
	call printf ("\nFIT ERROR CODE: %d\n")
	    call pargi (ier)

	# Evaluate the fit and its rms.
	call dgsvector (gs, x, y, zfit, npts)
	call printf ("\nZFIT:\n")
	do i = 1, npts {
	    call printf ("%6.3f ")
		call pargd (zfit[i])
	    if (mod (i, 9) == 0)
		call printf ("\n")
	}
	call printf ("\n")
	rms1 = 0.0d0
	do i = 1, npts
	    rms1 = rms1 + (z[i] - zfit[i]) ** 2 
	rms1 = sqrt (rms1 / (npts - 1))
	rms2 = 0.0d0
	do i = 1, npts
	    rms2 = rms2 + (z[i] - dgseval (gs, x[i], y[i])) ** 2
	rms2 = sqrt (rms2 / (npts - 1))
	#call printf ("\nRMS: vector = %0.14g point = %0.14g\n\n")
	call printf ("\nRMS: vector = %0.4f point = %0.4f\n\n")
	    call pargd (rms1)
	    call pargd (rms2)

	# Print the coefficients.
	call dgscoeff (gs, coeff, ncoeff)
	call printf ("GSFIT coeff:\n")
	call printf ("first %0.14g %0.14g\n")
	    call pargd (dgsgcoeff (gs, 1, 1))
	    call pargd (dgsgcoeff (gs, xorder, 1))
	do i = 1, ncoeff {
	    call printf ("%d  %0.14g\n")
		call pargi (i)
		call pargd (coeff[i])
	}
	call printf ("last %0.14g %0.14g\n")
	    call pargd (dgsgcoeff (gs, 1, yorder))
	    call pargd (dgsgcoeff (gs, xorder, yorder))
	call printf ("\n")

	call dgsfree (gs)
	return

	# Evaluate the first derivatives.
	call dgsder (gs, x, y, zfit, npts, 1, 0)
	call printf ("\nZDER: 1 0\n")
	do i = 1, npts {
	    call printf ("%0.7g ")
		call pargd (zfit[i])
	    if (mod (i, 9) == 0)
		call printf ("\n")
	}
	call printf ("\n")

	call dgsder (gs, x, y, zfit, npts, 0, 1)
	call printf ("\nZDER: 0 1\n")
	do i = 1, npts {
	    call printf ("%0.7g ")
		call pargd (zfit[i])
	    if (mod (i, 9) == 0)
		call printf ("\n")
	}
	call printf ("\n")

	call dgsder (gs, x, y, zfit, npts, 1, 1)
	call printf ("\nZDER: 1 1\n")
	do i = 1, npts {
	    call printf ("%0.7g ")
		call pargd (zfit[i])
	    if (mod (i, 9) == 0)
		call printf ("\n")
	}
	call printf ("\n")

	# Refit the surface point by point.
	call dgszero (gs)
	do i = 1, npts {
	    call dgsaccum (gs, x[i], y[i], z[i], w[i], WTS_UNIFORM)
	}
	if (stype > 3)
	    call dgssolve1 (gs, ier)
	else
	    call dgssolve (gs, ier)
	call printf ("\nACCUM FIT ERROR CODE: %d\n")
	    call pargi (ier)
	call dgsrej (gs, x[1], y[1], z[1], w[1], WTS_UNIFORM)
	call dgsrej (gs, x[npts], y[npts], z[npts], w[npts], WTS_UNIFORM)
	call dgsaccum (gs, x[1], y[1], z[1], w[1], WTS_UNIFORM)
	call dgsaccum (gs, x[npts], y[npts], z[npts], w[npts], WTS_UNIFORM)
	call dgssolve (gs, ier)
	call printf ("\nREJ FIT ERROR CODE: %d\n")
	    call pargi (ier)

	call dgscoeff (gs, coeff, ncoeff)
	call printf ("GSACCUM coeff:\n")
	call printf ("first %0.14g %0.14g\n")
	    call pargd (dgsgcoeff (gs, 1, 1))
	    call pargd (dgsgcoeff (gs, xorder, 1))
	do i = 1, ncoeff {
	    call printf ("%d  %0.14g\n")
		call pargi (i)
		call pargd (coeff[i])
	}
	call printf ("last %0.14g %0.14g\n")
	    call pargd (dgsgcoeff (gs, 1, yorder))
	    call pargd (dgsgcoeff (gs, xorder, yorder))
	call printf ("\n")

	# Save and restore.
	call dgssave (gs, save)
	call dgsfree (gs)
	call dgsrestore (gs, save)

	call dgscoeff (gs, coeff, ncoeff)
	call printf ("RESTORE coeff:\n")
	call printf ("first %0.14g %0.14g\n")
	    call pargd (dgsgcoeff (gs, 1, 1))
	    call pargd (dgsgcoeff (gs, xorder, 1))
	do i = 1, ncoeff {
	    call printf ("%d  %0.14g\n")
		call pargi (i)
		call pargd (coeff[i])
	}
	call printf ("last %0.14g %0.14g\n")
	    call pargd (dgsgcoeff (gs, 1, yorder))
	    call pargd (dgsgcoeff (gs, xorder, yorder))
	call printf ("\n")

	# Add two surfaces.
	call dgsadd (gs, gs, ags)
	call dgscoeff (ags, coeff, ncoeff)
	call printf ("GSADD coeff:\n")
	call printf ("first %0.14g %0.14g\n")
	    call pargd (dgsgcoeff (ags, 1, 1))
	    call pargd (dgsgcoeff (ags, xorder, 1))
	do i = 1, ncoeff {
	    call printf ("%d  %0.14g\n")
		call pargi (i)
		call pargd (coeff[i])
	}
	call printf ("last %0.14g %0.14g\n")
	    call pargd (dgsgcoeff (ags, 1, yorder))
	    call pargd (dgsgcoeff (ags, xorder, yorder))
	call printf ("\n")

	# Subtract two surfaces.
	call dgssub (gs, gs, sgs)
	call dgscoeff (sgs, coeff, ncoeff)
	call printf ("GSSUB coeff:\n")
	call printf ("first %0.14g %0.14g\n")
	    call pargd (dgsgcoeff (sgs, 1, 1))
	    call pargd (dgsgcoeff (sgs, xorder, 1))
	do i = 1, ncoeff {
	    call printf ("%d  %0.14g\n")
		call pargi (i)
		call pargd (coeff[i])
	}
	call printf ("last %0.14g %0.14g\n")
	    call pargd (dgsgcoeff (sgs, 1, yorder))
	    call pargd (dgsgcoeff (sgs, xorder, yorder))
	call printf ("\n")

	call dgsfree (gs)
	call dgsfree (ags)
	call dgsfree (sgs)
end
