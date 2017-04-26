include	<math/gsurfit.h>


# IGS_SOLVE1 -- Fit z = f(x, y).

define	SFTYPES		"|chebyshev|legendre|"		# Surface types

procedure igs_solve1 (sf, x, y, z, w, npts)

pointer	sf			# GSURFIT pointer
real	x[npts]			# X points
real	y[npts]			# Y points
real	z[npts]			# Z points
real	w[npts]			# Weights
int	npts			# Number of points

int	i, nfunc, ix, iy
pointer	sf1, sf2, resids

int	strdic()

include	"igsfit.com"

begin
	# Determine the function type.

	nfunc = strdic (function, function, SZ_LINE, SFTYPES)

	# Fit the first surface.

	ix = min (2, xorder)
	iy = min (2, yorder)
	call xgsinit (sf1, nfunc, ix, iy, NO, xmin, xmax, ymin, ymax)
	call xgsfit (sf1, x, y, z, w, npts, WTS_USER, i)

	switch (i) {
	case SINGULAR:
	    call eprintf ("Singular solution\n")
	case NO_DEG_FREEDOM:
	    call error (0, "No degrees of freedom")
	}

	# Evaluate the first surface and fit the residuals.

	call malloc (resids, npts, TY_REAL)
	call xgsvector (sf1, x, y, Memr[resids], npts)
	call asubr (z, Memr[resids], Memr[resids], npts)

	call xgsinit (sf2, nfunc, xorder, yorder, YES, xmin,xmax,ymin,ymax)
	call xgsfit (sf2, x, y, Memr[resids], w, npts, WTS_USER, i)

	switch (i) {
	case SINGULAR:
	    call eprintf ("Singular solution\n")
	case NO_DEG_FREEDOM:
	    call error (0, "No degrees of freedom")
	}

	# Add the two surfaces and free memory.

	call xgsadd (sf1, sf2, sf)
	call xgsfree (sf1)
	call xgsfree (sf2)
	call mfree (resids, TY_REAL)
end


# IGS_SOLVE2 -- Fit z = x + f(y).


procedure igs_solve2 (sf, x, y, z, w, npts)

pointer	sf			# GSURFIT pointer
real	x[npts]			# X points
real	y[npts]			# Y points
real	z[npts]			# Z points
real	w[npts]			# Weights
int	npts			# Number of points

int	i, nfunc
real	a
pointer	sf1

int	strdic()
real	xgsgcoeff()

include	"igsfit.com"

begin
	nfunc = strdic (function, function, SZ_LINE, SFTYPES)
	call xgsinit (sf1, nfunc, 1, yorder, NO, xmin, xmax, ymin, ymax)

	call asubr (z, x, z, npts)
	call xgsfit (sf1, x, y, z, w, npts, WTS_USER, i)
	call aaddr (z, x, z, npts)

	switch (i) {
	case SINGULAR:
	    call eprintf ("Singular solution\n")
	case NO_DEG_FREEDOM:
	    call error (0, "No degrees of freedom")
	}

	call xgsfree (sf)
	call xgsinit (sf, nfunc, 2, yorder, NO, xmin, xmax, ymin, ymax)
	a = xgsgcoeff (sf1, 1, 1)

	a = a + (xmin + xmax) / 2
	call xgsscoeff (sf, 1, 1, a)

	a = (xmax - xmin) / 2
	call xgsscoeff (sf, 2, 1, a)

	do i = 2, yorder {
	    a = xgsgcoeff (sf1, 1, i)
	    call xgsscoeff (sf, 1, i, a)
	}

	call xgsfree (sf1)
end

# IGS_SOLVE3 -- Fit z = y + f(x).

procedure igs_solve3 (sf, x, y, z, w, npts)

pointer	sf			# GSURFIT pointer
real	x[npts]			# X points
real	y[npts]			# Y points
real	z[npts]			# Z points
real	w[npts]			# Weights
int	npts			# Number of points

int	i, nfunc
real	a
pointer	sf1

int	strdic()
real	xgsgcoeff()

include	"igsfit.com"

begin
	nfunc = strdic (function, function, SZ_LINE, SFTYPES)
	call xgsinit (sf1, nfunc, xorder, 1, NO, xmin, xmax, ymin, ymax)

	call asubr (z, y, z, npts)
	call xgsfit (sf1, x, y, z, w, npts, WTS_USER, i)
	call aaddr (z, y, z, npts)

	switch (i) {
	case SINGULAR:
	    call eprintf ("Singular solution\n")
	case NO_DEG_FREEDOM:
	    call error (0, "No degrees of freedom")
	}

	call xgsfree (sf)
	call xgsinit (sf, nfunc, xorder, 2, NO, xmin, xmax, ymin, ymax)
	a = xgsgcoeff (sf1, 1, 1)

	a = a + (ymin + ymax) / 2
	call xgsscoeff (sf, 1, 1, a)

	a = (ymax - ymin) / 2
	call xgsscoeff (sf, 1, 2, a)

	do i = 2, xorder {
	    a = xgsgcoeff (sf1, i, 1)
	    call xgsscoeff (sf, i, 1, a)
	}

	call xgsfree (sf1)
end
