include	<imhdr.h>
include	<mach.h>
include	<math.h>
include	<math/iminterp.h>
include	<math/nlfit.h>
include	"starfocus.h"


# STF_FIND      -- Find the object and return the data raster and object center.
# STF_BKGD      -- Compute the background.
# STF_PROFILE   -- Compute enclosed flux profile, derivative, and moments.
# STF_NORM      -- Renormalized enclosed flux profile
# STF_WIDTHS    -- Set widths.
# STF_I2R       -- Radius from sample index.
# STF_R2I       -- Sample index from radius.
# STF_R2N       -- Number of subsamples from radius.
# STF_MODEL     -- Return model values.
# STF_DFWHM     -- Direct FWHM from profile.
# STF_FWHMS     -- Measure FWHM vs level.
# STF_RADIUS    -- Measure the radius at the specified level.
# STF_FIT       -- Fit model.
# STF_GAUSS1    -- Gaussian function used in NLFIT.
# STF_GAUSS2    -- Gaussian function and derivatives used in NLFIT.
# STF_MOFFAT1   -- Moffat function used in NLFIT.
# STF_MOFFAT2   -- Moffat function and derivatives used in NLFIT.


# STF_FIND -- Find the object and return the data raster and object center.
# Centering uses centroid of marginal distributions of data above the mean.

procedure stf_find (sf, sfd, im)

pointer	sf			#I Starfocus pointer
pointer	sfd			#I Object pointer
pointer	im			#I Image pointer

long	lseed
int	i, j, k, x1, x2, y1, y2, nx, ny, npts
real	radius, buffer, width, xc, yc, xlast, ylast, r1, r2
real	mean, sum, sum1, sum2, sum3, asumr(), urand()
pointer	data, ptr, imgs2r()
errchk	imgs2r

begin
	radius = max (3., SFD_RADIUS(sfd))
	buffer = SF_SBUF(sf)
	width = SF_SWIDTH(sf)

	xc = SFD_X(sfd)
	yc = SFD_Y(sfd)
	r1 = radius + buffer + width
	r2 = radius

	# Iterate on the center finding.
	do k = 1, 3 {

	    # Extract region around current center.
	    xlast = xc
	    ylast = yc

	    x1 = max (1-NBNDRYPIX, nint (xc - r2))
	    x2 = min (IM_LEN(im,1)+NBNDRYPIX, nint (xc + r2))
	    nx = x2 - x1 + 1
	    y1 = max (1-NBNDRYPIX, nint (yc - r2))
	    y2 = min (IM_LEN(im,2)+NBNDRYPIX, nint (yc + r2))
	    ny = y2 - y1 + 1
	    npts = nx * ny
	    data = imgs2r (im, x1, x2, y1, y2)

	    # Find center of gravity of marginal distributions above mean.
	    npts = nx * ny
	    sum = asumr (Memr[data], npts)
	    mean = sum / nx
	    sum1 = 0.
	    sum2 = 0.

	    do i = x1, x2 {
		ptr = data + i - x1
		sum3 = 0.
		do j = y1, y2 {
		    sum3 = sum3 + Memr[ptr]
		    ptr = ptr + nx
		}
		sum3 = sum3 - mean
		if (sum3 > 0.) {
		    sum1 = sum1 + i * sum3
		    sum2 = sum2 + sum3
		}
	    }
	    if (sum2 <= 0)
		call error (1, "Centering failed to converge")
	    xc = sum1 / sum2
	    if (xlast - xc > 0.2 * nx)
		xc = xlast - 0.2 * nx
	    if (xc - xlast > 0.2 * nx)
		xc = xlast + 0.2 * nx

	    ptr = data
	    mean = sum / ny
	    sum1 = 0.
	    sum2 = 0.
	    do j = y1, y2 {
		sum3 = 0.
		do i = x1, x2 {
		    sum3 = sum3 + Memr[ptr]
		    ptr = ptr + 1
		}
		sum3 = sum3 - mean
		if (sum3 > 0.) {
		    sum1 = sum1 + j * sum3
		    sum2 = sum2 + sum3
		}
	    }
	    if (sum2 <= 0)
		call error (1, "Centering failed to converge")
	    yc = sum1 / sum2
	    if (ylast - yc > 0.2 * ny)
		yc = ylast - 0.2 * ny
	    if (yc - ylast > 0.2 * ny)
		yc = ylast + 0.2 * ny

	    if (nint(xc) == nint(xlast) && nint(yc) == nint(ylast))
		break
	}

	# Get a new centered raster if necessary.
	if (nint(xc) != nint(xlast) || nint(yc) != nint(ylast) || r2 < r1) {
	    x1 = max (1-NBNDRYPIX, nint (xc - r1))
	    x2 = min (IM_LEN(im,1)+NBNDRYPIX, nint (xc + r1))
	    nx = x2 - x1 + 1
	    y1 = max (1-NBNDRYPIX, nint (yc - r1))
	    y2 = min (IM_LEN(im,2)+NBNDRYPIX, nint (yc + r1))
	    ny = y2 - y1 + 1
	    npts = nx * ny
	    data = imgs2r (im, x1, x2, y1, y2)
	}

	# Add a dither for integer data.  The random numbers are always
	# the same to provide reproducibility.

	i = IM_PIXTYPE(im)
	if (i == TY_SHORT || i == TY_INT || i == TY_LONG) {
	    lseed = 1
	    do i = 0, npts-1
		Memr[data+i] = Memr[data+i] + urand(lseed) - 0.5
	}

	SFD_DATA(sfd) = data
	SFD_X1(sfd) = x1
	SFD_X2(sfd) = x2
	SFD_Y1(sfd) = y1
	SFD_Y2(sfd) = y2
	SFD_X(sfd) = xc
	SFD_Y(sfd) = yc
end


# STF_BKGD -- Compute the background.
# A mode is estimated from the minimum slope in the sorted background pixels
# with a bin width of 5%.

procedure stf_bkgd (sf, sfd)

pointer	sf			#I Parameter structure
pointer	sfd			#I Star structure

int	i, j, x1, x2, y1, y2, xc, yc, nx, ny, npts, ns, nsat
real	sat, bkgd, miso
real	r, r1, r2, r3, dx, dy, dz
pointer	sp, data, bdata, ptr

begin
	data = SFD_DATA(sfd)
	x1 = SFD_X1(sfd)
	x2 = SFD_X2(sfd)
	y1 = SFD_Y1(sfd)
	y2 = SFD_Y2(sfd)
	xc = SFD_X(sfd)
	yc = SFD_Y(sfd)

	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	npts = nx * ny

	ns = 0
	nsat = 0
	r1 = SFD_RADIUS(sfd) ** 2
	r2 = (SFD_RADIUS(sfd) + SF_SBUF(sf)) ** 2
	r3 = (SFD_RADIUS(sfd) + SF_SBUF(sf) + SF_SWIDTH(sf)) ** 2
	sat = SF_SAT(sf)
	if (IS_INDEF(sat))
	    sat = MAX_REAL

	call smark (sp)
	call salloc (bdata, npts, TY_REAL)

	ptr = data
	do j = y1, y2 {
	    dy = (yc - j) ** 2
	    do i = x1, x2 {
		dx = (xc - i) ** 2
		r = dx + dy
		if (r <= r1) {
		    if (Memr[ptr] >= sat)
			nsat = nsat + 1
		} else if (r >= r2 && r <= r3) {
		    Memr[bdata+ns] = Memr[ptr]
		    ns = ns + 1
		}
		ptr = ptr + 1
	    }
	}

	if (ns > 9) {
	    call asrtr (Memr[bdata], Memr[bdata], ns)
	    r = Memr[bdata+ns-1] - Memr[bdata]
	    bkgd = Memr[bdata] + r / 2
	    miso = r / 2

	    j = 1 + 0.50 * ns
	    do i = 0, ns - j {
		dz = Memr[bdata+i+j-1] - Memr[bdata+i]
		if (dz < r) {
		    r = dz
		    bkgd = Memr[bdata+i] + dz / 2
		    miso = dz / 2
		}
	    }
	} else {
	    bkgd = 0.
	    miso = 0.
	}

	SFD_BKGD1(sfd) = bkgd
	SFD_BKGD(sfd) = bkgd
	SFD_MISO(sfd) = miso
	SFD_NSAT(sfd) = nsat

	call sfree (sp)
end


# STF_PROFILE -- Compute enclosed flux profile, derivative, direct FWHM, and
# profile moments..
# 1.  The flux profile is normalized at the maximum value.
# 2.  The radial profile is computed from the numerical derivative of the
#     enclose flux profile.

procedure stf_profile (sf, sfd)

pointer	sf			#I Parameter structure
pointer	sfd			#I Star structure

int	np
real	radius, xc, yc

int	i, j, k, l, m, ns, nx, ny, x1, x2, y1, y2
real	bkgd, miso, sigma, peak
real	r, r1, r2, r3, dx, dy, dx1, dx2, dy1, dy2, dz, xx, yy, xy, ds, da
pointer	sp, data, profile, ptr, asi, msi, gs
int	stf_r2n()
real	asieval(), msieval(), gseval(), stf_i2r(), stf_r2i()
errchk	asiinit, asifit, msiinit, msifit, gsrestore

real	gsdata[24]
data	gsdata/ 1., 4., 4., 1., 0., 0.6726812, 1., 2., 1.630641, 0.088787,
		0.00389378, -0.001457133, 0.3932125, -0.1267456, -0.004864541,
		0.00249941, 0.03078612, 0.02731274, -4.875850E-4, 2.307464E-4,
		-0.002134843, 0.007603908, -0.002552385, -8.010564E-4/

begin
	data = SFD_DATA(sfd)
	x1 = SFD_X1(sfd)
	x2 = SFD_X2(sfd)
	y1 = SFD_Y1(sfd)
	y2 = SFD_Y2(sfd)
	xc = SFD_X(sfd)
	yc = SFD_Y(sfd)
	bkgd = SFD_BKGD(sfd)
	miso = SFD_MISO(sfd)
	radius = SFD_RADIUS(sfd)
	np = SFD_NP(sfd)

	nx = x2 - x1 + 1
	ny = y2 - y1 + 1

	# Use an image interpolator fit to the data.
	call msiinit (msi, II_BISPLINE3)
	call msifit (msi, Memr[data], nx, ny, nx)

	# To avoid trying to interpolate outside the center of the
	# edge pixels, a requirement of the interpolator functions,
	# we reset the data limits.
	x1 = x1 + 1
	x2 = x2 - 1
	y1 = y1 + 1
	y2 = y2 - 1

	# Compute the enclosed flux profile, its derivative, and moments.
	call smark (sp)
	call salloc (profile, np, TY_REAL)
	call aclrr (Memr[profile], np)

	xx = 0.
	yy = 0.
	xy = 0.
	do j = y1, y2 {
	    ptr = data + (j-y1+1)*nx + 1
	    dy = j - yc
	    do i = x1, x2 {
		dx = i - xc

		# Set the subpixel sampling which may be a function of radius.
		r = sqrt (dx * dx + dy * dy)
		ns = stf_r2n (r)
		ds = 1. / ns
		da = ds * ds
		dz = 0.5 + 0.5 * ds

		# Sum the interpolator values over the subpixels and compute
		# an offset to give the correct total for the pixel.

		r2 = 0.
		dy1 = dy - dz
		do l = 1, ns {
		    dy1 = dy1 + ds
		    dy2 = dy1 * dy1
		    dx1 = dx - dz
		    do k = 1, ns {
			dx1 = dx1 + ds
			dx2 = dx1 * dx1
			r1 = msieval (msi, dx1+xc-x1+2, dy1+yc-y1+2)
			r2 = r2 + r1
		    }
		}

		r1 = Memr[ptr] - bkgd
		ptr = ptr + 1
		r2 = r1 - r2 * da

		# Accumulate the enclosed flux over the sub pixels.
		dy1 = dy - dz
		do l = 1, ns {
		    dy1 = dy1 + ds
		    dy2 = dy1 * dy1
		    dx1 = dx - dz
		    do k = 1, ns {
			dx1 = dx1 + ds
			dx2 = dx1 * dx1
			r = max (0., sqrt (dx2 + dy2) - ds / 2)
			if (r < radius) {
			    r1 = da * (msieval (msi, dx1+xc-x1+2, dy1+yc-y1+2) +
				r2)

			    # Use approximation for fractions of a subpixel.
			    for (m=stf_r2i(r)+1; m<=np; m=m+1) {
				r3 = (stf_i2r (real(m)) - r) / ds
				if (r3 >= 1.)
				    break
				Memr[profile+m-1] = Memr[profile+m-1] + r3 * r1
			    }

			    # The subpixel is completely within these radii.
			    for (; m<=np; m=m+1)
				Memr[profile+m-1] = Memr[profile+m-1] + r1

			    # Accumulate the moments above an isophote.
			    if (r1 > miso) {
				xx = xx + dx2 * r1
				yy = yy + dy2 * r1
				xy = xy + dx1 * dy1 * r1
			    }
			}
		    }
		}
	    }
	}

	call msifree (msi)

	# Compute the ellipticity and position angle from the moments.
	r = (xx + yy)
	if (r > 0.) {
	    r1 = (xx - yy) / r
	    r2 = 2 * xy / r
	    SFD_E(sfd) = sqrt (r1**2 + r2**2)
	    SFD_PA(sfd) = RADTODEG (atan2 (r2, r1) / 2.)
	} else {
	    SFD_E(sfd) = 0.
	    SFD_PA(sfd) = 0.
	}

	# The magnitude and profile normalization is from the max enclosed flux.
	call alimr (Memr[profile], np, r, SFD_M(sfd))
	if (SFD_M(sfd) <= 0.)
	    call error (1, "Invalid flux profile")
	call adivkr (Memr[profile], SFD_M(sfd), Memr[profile], np)

	# Fit interpolator to the enclosed flux profile.
	call asiinit (asi, II_SPLINE3)
	call asifit (asi, Memr[profile], np)
	SFD_ASI1(sfd) = asi

	# Estimate a gaussian sigma (actually sqrt(2)*sigma) and if it is
	# it is small subtract the gaussian so that the image interpolator
	# can more accurately estimate subpixel values.

	#call stf_radius (sf, sfd, SF_LEVEL(sf), r)
	#sigma = r / sqrt (log (1/(1-SF_LEVEL(sf))))
	call stf_radius (sf, sfd, 0.8, r)
	r = r / SF_SCALE(sf)
	sigma = 2 * r * sqrt (log(2.) / log (1/(1-0.8)))
	if (sigma < 5.) {
	    if (sigma <= 2.) {
		call gsrestore (gs, gsdata)
		dx = xc - nint (xc)
		dy = yc - nint (yc)
		r = sqrt (dx * dx + dy * dy)
		dx = 1.
		ds = abs (sigma - gseval (gs, r, dx))
		for (da = 1.; da <= 2.; da = da + .01) {
		    dz = abs (sigma - gseval (gs, r, da))
		    if (dz < ds) {
			ds = dz
			dx = da
		    }
		}
		sigma = dx
		call gsfree (gs)
	    }

	    sigma = sigma / (2 * sqrt (log(2.)))
	    sigma = sigma * sigma

	    # Compute the peak that gives the correct central pixel value.
	    i = nint (xc)
	    j = nint (yc)
	    dx = i - xc
	    dy = j - yc
	    r = sqrt (dx * dx + dy * dy)
	    ns = stf_r2n (r)
	    ds = 1. / ns
	    da = ds * ds
	    dz = 0.5 + 0.5 * ds

	    r1 = 0.
	    dy1 = dy - dz
	    do l = 1, ns {
		dy1 = dy1 + ds
		dy2 = dy1 * dy1
		dx1 = dx - dz
		do k = 1, ns {
		    dx1 = dx1 + ds
		    dx2 = dx1 * dx1
		    r2 = (dx2 + dy2) / sigma
		    if (r2 < 25.)
			r1 = r1 + exp (-r2)
		}
	    }
	    ptr = data + (j - y1 + 1) * nx + (i - x1 + 1)
	    peak = (Memr[ptr] - bkgd) / (r1 * da)

	    # Subtract the gaussian from the data.
	    do j = y1, y2 {
		ptr = data + (j - y1 + 1) * nx + 1
		dy = j - yc
		do i = x1, x2 {
		    dx = i - xc
		    r = sqrt (dx * dx + dy * dy)
		    ns = stf_r2n (r)
		    ds = 1. / ns
		    da = ds * ds
		    dz = 0.5 + 0.5 * ds

		    r1 = 0.
		    dy1 = dy - dz
		    do l = 1, ns {
			dy1 = dy1 + ds
			dy2 = dy1 * dy1
			dx1 = dx - dz
			do k = 1, ns {
			    dx1 = dx1 + ds
			    dx2 = dx1 * dx1
			    r2 = (dx2 + dy2) / sigma
			    if (r2 < 25.)
				r1 = r1 + peak * exp (-r2)
			}
		    }
		    Memr[ptr] = Memr[ptr] - r1 * da
		    ptr = ptr + 1
		}
	    }

	    # Fit the image interpolator to the residual data.
	    call msiinit (msi, II_BISPLINE3)
	    call msifit (msi, Memr[data], nx, ny, nx)

	    # Recompute the enclosed flux profile and moments
	    # using the gaussian plus image interpolator fit to the residuals.

	    call aclrr (Memr[profile], np)

	    xx = 0.
	    yy = 0.
	    xy = 0.
	    do j = y1, y2 {
		ptr = data + (j - y1 + 1) * nx + 1
		dy = j - yc
		do i = x1, x2 {
		    dx = i - xc
		    r = sqrt (dx * dx + dy * dy)
		    ns = stf_r2n (r)
		    ds = 1. / ns
		    da = ds * ds
		    dz = 0.5 + 0.5 * ds

		    # Compute interpolator correction.
		    r2 = 0.
		    dy1 = dy - dz
		    do l = 1, ns {
			dy1 = dy1 + ds
			dx1 = dx - dz
			do k = 1, ns {
			    dx1 = dx1 + ds
			    r1 = msieval (msi, dx1+xc-x1+2, dy1+yc-y1+2)
			    r2 = r2 + r1
			}
		    }

		    r1 = Memr[ptr] - bkgd
		    ptr = ptr + 1
		    r2 = r1 - r2 * da

		    # Accumulate the enclosed flux and moments.
		    dy1 = dy - dz
		    do l = 1, ns {
			dy1 = dy1 + ds
			dy2 = dy1 * dy1
			dx1 = dx - dz
			do k = 1, ns {
			    dx1 = dx1 + ds
			    dx2 = dx1 * dx1
			    r3 = (dx2 + dy2) / sigma
			    if (r3 < 25.)
				r3 = peak * exp (-r3)
			    else
				r3 = 0.
			    r = max (0., sqrt (dx2 + dy2) - ds / 2)
			    if (r < radius) {
				r1 = msieval (msi, dx1+xc-x1+2, dy1+yc-y1+2)
				r1 = da * (r1 + r2 + r3)

				for (m=stf_r2i(r)+1; m<=np; m=m+1) {
				    r3 = (stf_i2r (real(m)) - r) / ds
				    if (r3 >= 1.)
					break
				    Memr[profile+m-1] = Memr[profile+m-1] +
					r3 * r1
				}
				for (; m<=np; m=m+1)
				    Memr[profile+m-1] = Memr[profile+m-1] + r1

				if (r1 > miso) {
				    xx = xx + dx2 * r1
				    yy = yy + dy2 * r1
				    xy = xy + dx1 * dy1 * r1
				}
			    }
			}
		    }
		}
	    }

	    call msifree (msi)

	    # Recompute the moments, magnitude, normalized flux, and interp.
	    r = (xx + yy)
	    if (r > 0.) {
		r1 = (xx - yy) / r
		r2 = 2 * xy / r
		SFD_E(sfd) = sqrt (r1**2 + r2**2)
		SFD_PA(sfd) = RADTODEG (atan2 (r2, r1) / 2.)
	    } else {
		SFD_E(sfd) = 0.
		SFD_PA(sfd) = 0.
	    }

	    call alimr (Memr[profile], np, r, SFD_M(sfd))
	    if (SFD_M(sfd) <= 0.)
		call error (1, "Invalid flux profile")
	    call adivkr (Memr[profile], SFD_M(sfd), Memr[profile], np)

	    call asifit (asi, Memr[profile], np)
	    SFD_ASI1(sfd) = asi
	}

	# Compute derivative of enclosed flux profile and fit an image
	# interpolator.

	dx = 0.25
	Memr[profile] = 0.
	ns = 0
	do i = 1, np {
	    r = stf_i2r (real(i))
	    r2 = stf_r2i (r + dx)
	    if (r2 > np) {
		k = i
		break
	    }
	    r1 = stf_r2i (r - dx)
	    if (r1 < 1) {
		if (i > 1) {
		    dy = asieval (asi, real(i)) / r**2
		    Memr[profile] = (ns * Memr[profile] + dy) / (ns + 1)
		    ns = ns + 1
		}
		j = i
	    } else {
		dy = (asieval (asi, r2) - asieval (asi, r1)) /
		    (4 * r * dx)
		Memr[profile+i-1] = dy
	    }
	}
	do i = 2, j
	    Memr[profile+i-1] = (Memr[profile+j] - Memr[profile]) / j *
		(i - 1) + Memr[profile]
	do i = k, np
	    Memr[profile+i-1] = Memr[profile+k-2]

	call adivkr (Memr[profile], SF_SCALE(sf)**2, Memr[profile], np)
	call alimr (Memr[profile], np, SFD_YP1(sfd), SFD_YP2(sfd))
	call asiinit (asi, II_SPLINE3)
	call asifit (asi, Memr[profile], np)
	SFD_ASI2(sfd) = asi
	#SF_XP1(sf) = j+1
	SF_XP1(sf) = 1
	SF_XP2(sf) = k-1

	call sfree (sp)
end


# STF_NORM -- Renormalize the enclosed flux profile.

procedure stf_norm (sf, sfd, x, y)

pointer	sf			#I Parameter structure
pointer	sfd			#I Star structure
real	x			#I Radius
real	y			#I Flux

int	npmax, np
pointer	asi

int	i, j, k
real	r, r1, r2, dx, dy
pointer	sp, profile
real	asieval(), stf_i2r(), stf_r2i()
errchk	asifit

begin
	npmax = SFD_NPMAX(sfd)
	np = SFD_NP(sfd)
	asi = SFD_ASI1(sfd)

	call smark (sp)
	call salloc (profile, npmax, TY_REAL)

	# Renormalize the enclosed flux profile.
	if (IS_INDEF(x) || x <= 0.) {
	    dy = SFD_BKGD(sfd) - SFD_BKGD1(sfd)
	    SFD_BKGD(sfd) = SFD_BKGD(sfd) - dy
	    do i = 1, npmax
		Memr[profile+i-1] = asieval (asi, real(i)) +
		    dy * stf_i2r(real(i)) ** 2
	    call alimr (Memr[profile], np, r1, r2)
	    call adivkr (Memr[profile], r2, Memr[profile], npmax)
	} else if (IS_INDEF(y)) {
	    r = max (1., min (real(np), stf_r2i (x)))
	    r2 = asieval (asi, r)
	    if (r2 <= 0.)
		return
	    do i = 1, npmax
		Memr[profile+i-1] = asieval (asi, real(i))
	    call adivkr (Memr[profile], r2, Memr[profile], npmax)
	} else {
	    r = max (1., min (real(np), stf_r2i (x)))
	    r1 = asieval (asi, r)
	    dy = (y - r1) / x ** 2
	    SFD_BKGD(sfd) = SFD_BKGD(sfd) - dy
	    do i = 1, npmax
		Memr[profile+i-1] = asieval (asi, real(i)) +
		    dy * stf_i2r(real(i)) ** 2
	}

	call asifit (asi, Memr[profile], npmax)
	SFD_ASI1(sfd) = asi

	# Compute derivative of enclosed flux profile and fit an image
	# interpolator.

	dx = 0.25
	do i = 1, npmax {
	    r = stf_i2r (real(i))
	    r2 = stf_r2i (r + dx)
	    if (r2 > np) {
		k = i
		break
	    }
	    r1 = stf_r2i (r - dx)
	    if (r1 < 1) {
		if (i > 1) {
		    dy = asieval (asi, real(i)) / r**2
		    Memr[profile] = dy
		}
		j = i
	    } else {
		dy = (asieval (asi, r2) - asieval (asi, r1)) /
		    (4 * r * dx)
		Memr[profile+i-1] = dy
	    }
	}
	do i = 2, j
	    Memr[profile+i-1] = (Memr[profile+j] - Memr[profile]) / j *
		(i - 1) + Memr[profile]
	do i = k, npmax
	    Memr[profile+i-1] = Memr[profile+k-2]

	call adivkr (Memr[profile], SF_SCALE(sf)**2, Memr[profile], np)
	call alimr (Memr[profile], np, SFD_YP1(sfd), SFD_YP2(sfd))
	asi = SFD_ASI2(sfd)
	call asifit (asi, Memr[profile], np)
	SFD_ASI2(sfd) = asi
	#SF_XP1(sf) = min (j+1, np)
	SF_XP1(sf) = 1
	SF_XP2(sf) = min (k-1, np)

	call sfree (sp)
end


# STF_WIDTHS -- Set the widhts.

procedure stf_widths (sf, sfd)

pointer	sf			#I Main data structure
pointer	sfd			#I Star data structure

errchk	stf_radius, stf_dfwhm, stf_fit

begin
	call stf_radius (sf, sfd, SF_LEVEL(sf), SFD_R(sfd))
	call stf_dfwhm (sf, sfd)
	call stf_fit (sf, sfd)

	switch (SF_WCODE(sf)) {
	case 1:
	    SFD_W(sfd) = SFD_R(sfd)
	case 2:
	    SFD_W(sfd) = SFD_DFWHM(sfd)
	case 3:
	    SFD_W(sfd) = SFD_GFWHM(sfd)
	case 4:
	    SFD_W(sfd) = SFD_MFWHM(sfd)
	}
end


# STF_I2R -- Compute radius from sample index.

real procedure stf_i2r (i)

real	i			#I Index
real	r			#O Radius

begin
	if (i < 20)
	    r = 0.05 * i
	else if (i < 30)
	    r = 0.1 * i - 1
	else if (i < 40)
	    r = 0.2 * i - 4
	else if (i < 50)
	    r = 0.5 * i - 16
	else
	    r = i - 41
	return (r)
end


# STF_R2I -- Compute sample index from radius.

real procedure stf_r2i (r)

real	r			#I Radius
real	i			#O Index

begin
	if (r < 1)
	    i = 20 * r
	else if (r < 2)
	    i = 10 * (r + 1)
	else if (r < 4)
	    i = 5 * (r + 4)
	else if (r < 9)
	    i = 2 * (r + 16)
	else
	    i = r + 41
	return (i)
end


# STF_R2N -- Compute number of subsamples from radius.

int procedure stf_r2n (r)

real	r			#I Radius
int	n			#O Number of subsamples

begin
	if (r < 1)
	    n = 20
	else if (r < 2)
	    n = 10
	else if (r < 4)
	    n = 5
	else if (r < 9)
	    n = 2
	else
	    n = 1
	return (n)
end


# STF_MODEL -- Return model value.

procedure stf_model (sf, sfd, r, profile, flux)

pointer	sf			#I Main data structure
pointer	sfd			#I Star data structure
real	r			#I Radius at level
real	profile			#I Profile value
real	flux			#I Enclosed flux value

real	x, x1, x2, r1, r2, dr

begin
	dr = 0.25 * SF_SCALE(sf)
	r1 = r - dr
	r2 = r + dr
	if (r1 < 0.) {
	    r1 = dr
	    r2 = r1 + dr
	}

	switch (SF_WCODE(sf)) {
	case 3:
	    x = r**2 / (2. * SFD_SIGMA(sfd)**2)
	    if (x < 20.)
		flux = 1 - exp (-x)
	    else
		flux = 0.

	    x1 = r1**2 / (2. * SFD_SIGMA(sfd)**2)
	    x2 = r2**2 / (2. * SFD_SIGMA(sfd)**2)
	    if (x2 < 20.) {
		x1 = 1 - exp (-x1)
		x2 = 1 - exp (-x2)
	    } else {
		x1 = 1.
		x2 = 1.
	    }
	    if (r <= dr) {
		x1 = x1 / dr ** 2
		x2 = x2 / (4 * dr ** 2)
		profile = (x2 - x1) / dr * r + x1
	    } else {
		profile = (x2 - x1) / (4 * r * dr)
	    }
	default:
	    x = 1 + (r / SFD_ALPHA(sfd)) ** 2
	    flux = 1 - x ** (1 - SFD_BETA(sfd))

	    x1 = 1 + (r1 / SFD_ALPHA(sfd)) ** 2
	    x2 = 1 + (r2 / SFD_ALPHA(sfd)) ** 2
	    x1 = 1 - x1 ** (1 - SFD_BETA(sfd))
	    x2 = 1 - x2 ** (1 - SFD_BETA(sfd))
	    if (r <= dr) {
		x1 = x1 / dr ** 2
		x2 = x2 / (4 * dr ** 2)
		profile = (x2 - x1) / dr * r + x1
	    } else {
		profile = (x2 - x1) / (4 * r * dr)
	    }
	}
end


# STF_DFWHM -- Direct FWHM from profile.

procedure stf_dfwhm (sf, sfd)

pointer	sf			#I Main data structure
pointer	sfd			#I Star data structure

int	np
real	r, rpeak, profile, peak, asieval(), stf_i2r()
pointer	asi

begin
	asi = SFD_ASI2(sfd)
	np = SFD_NP(sfd)

	rpeak = 1.
	peak = 0.
	for (r=1.; r <= np; r = r + 0.01) {
	    profile = asieval (asi, r)
	    if (profile > peak) {
		rpeak = r
		peak = profile
	    }
	}
	
	peak = peak / 2.
	for (r=rpeak; r <= np && asieval (asi, r) > peak; r = r + 0.01)
	    ;

	SFD_DFWHM(sfd) = 2 * stf_i2r (r) * SF_SCALE(sf)
end


# STF_FWHMS -- Measure FWHM vs level.

procedure stf_fwhms (sf, sfd)

pointer	sf			#I Main data structure
pointer	sfd			#I Star data structure

int	i
real	level, r

begin
	do i = 1, 19 {
	    level = i * 0.05
	    call stf_radius (sf, sfd, level, r)
	    switch (SF_WCODE(sf)) {
	    case 3:
		SFD_FWHM(sfd,i) = 2 * r * sqrt (log (2.) / log (1/(1-level)))
	    default:
		r = r / sqrt ((1.-level)**(1./(1.-SFD_BETA(sfd))) - 1.)
		SFD_FWHM(sfd,i) = 2 * r * sqrt (2.**(1./SFD_BETA(sfd))-1.)
	    }
	}
end


# STF_RADIUS -- Measure the radius at the specified level.

procedure stf_radius (sf, sfd, level, r)

pointer	sf			#I Main data structure
pointer	sfd			#I Star data structure
real	level			#I Level to measure
real	r			#O Radius

int	np
pointer	asi
real	f, fmax, rmax, asieval(), stf_i2r()

begin
	np = SFD_NP(sfd)
	asi = SFD_ASI1(sfd)

	for (r=1; r <= np && asieval (asi, r) < level; r = r + 0.01)
	    ;
	if (r > np) {
	    fmax = 0.
	    rmax = 0.
	    for (r=1; r <= np; r = r + 0.01) {
		f = asieval (asi, r)
		if (f > fmax) {
		    fmax = f
		    rmax = r
		}
	    }
	    r = rmax
	}
	r = stf_i2r (r) * SF_SCALE(sf)
end


# STF_FIT -- Fit models to enclosed flux.

procedure stf_fit (sf, sfd)

pointer	sf			#I Main data structure
pointer	sfd			#I Star data structure

int	i, j, n, np, pfit[2]
real	beta, z, params[3]
pointer	asi, nl
pointer	sp, x, y, w

int	locpr()
real	asieval(), stf_i2r()
extern	stf_gauss1(), stf_gauss2(), stf_moffat1(), stf_moffat2()
errchk	nlinitr, nlfitr

data	pfit/2,3/

begin
	np = SFD_NP(sfd)
	asi = SFD_ASI1(sfd)

	call smark (sp)
	call salloc (x, np, TY_REAL)
	call salloc (y, np, TY_REAL)
	call salloc (w, np, TY_REAL)

	n = 0
	j = 0
	do i = 1, np {
	    z = 1. - max (0., asieval (asi, real(i)))
	    if (n > np/3 && z < 0.5)
		break
	    if ((n < np/3 && z > 0.01) || z > 0.5)
		j = n

	    Memr[x+n] = stf_i2r (real(i)) * SF_SCALE(sf)
	    Memr[y+n] = z
	    Memr[w+n] = 1.
	    n = n + 1
	}

	# Gaussian.
	np = 1
	params[2] = Memr[x+j] / sqrt (2. * log (1./min(0.99,Memr[y+j])))
	params[1] = 1
	call nlinitr (nl, locpr (stf_gauss1), locpr (stf_gauss2),
	    params, params, 2, pfit, np, .001, 100)
	call nlfitr (nl, Memr[x], Memr[y], Memr[w], n, 1, WTS_USER, i)
	if (i != SINGULAR && i != NO_DEG_FREEDOM) {
	    call nlpgetr (nl, params, i)
	    if (params[2] < 0.)
		params[2] = Memr[x+j] / sqrt (2. * log (1./min(0.99,Memr[y+j])))
	}
	SFD_SIGMA(sfd) = params[2]
	SFD_GFWHM(sfd) = 2 * SFD_SIGMA(sfd) * sqrt (2. * log (2.))

	# Moffat.
	if (SF_BETA(sf) < 1.1) {
	    call nlfreer (nl)
	    call sfree (sp)
	    call error (1, "Cannot measure FWHM - Moffat beta too small")
	}

	beta = SF_BETA(sf)
	if (IS_INDEFR(beta)) {
	    beta = 2.5
	    np = 2
	} else {
	    np = 1
	}
	params[3] = 1 - beta
	params[2] = Memr[x+j] / sqrt (min(0.99,Memr[y+j])**(1./params[3]) - 1.)
	params[1] = 1
	call nlinitr (nl, locpr (stf_moffat1), locpr (stf_moffat2),
	    params, params, 3, pfit, np, .001, 100)
	call nlfitr (nl, Memr[x], Memr[y], Memr[w], n, 1, WTS_USER, i)
	if (i != SINGULAR && i != NO_DEG_FREEDOM) {
	    call nlpgetr (nl, params, i)
	    if (params[2] < 0.) {
		params[3] = 1. - beta
		params[2] = Memr[x+j] /
		    sqrt (min(0.99,Memr[y+j])**(1./params[3]) - 1.)
	    }
	}
	SFD_ALPHA(sfd) = params[2]
	SFD_BETA(sfd) = 1 - params[3]
	SFD_MFWHM(sfd) = 2 * SFD_ALPHA(sfd) * sqrt (2.**(1./SFD_BETA(sfd))-1.)

	call nlfreer (nl)
	call sfree (sp)
end


# STF_GAUSS1 -- Gaussian function used in NLFIT.  The parameters are the
# amplitude and sigma and the input variable is the radius.

procedure stf_gauss1 (x, nvars, p, np, z)

real	x[nvars]		#I Input variables
int	nvars			#I Number of variables
real	p[np]			#I Parameter vector
int	np			#I Number of parameters
real	z			#O Function return

real	r2

begin
	r2 = x[1]**2 / (2 * p[2]**2)
	if (abs (r2) > 20.)
	    z = 0.
	else
	    z = p[1] * exp (-r2)
end


# STF_GAUSS2 -- Gaussian function and derivatives used in NLFIT.  The parameters
# are the amplitude and sigma and the input variable is the radius.

procedure stf_gauss2 (x, nvars, p, dp, np, z, der)

real	x[nvars]		#I Input variables
int	nvars			#I Number of variables
real	p[np]			#I Parameter vector
real	dp[np]			#I Dummy array of parameters increments
int	np			#I Number of parameters
real	z			#O Function return
real	der[np]			#O Derivatives

real	r2

begin
	r2 = x[1]**2 / (2 * p[2]**2)
	if (abs (r2) > 20.) {
	    z = 0.
	    der[1] = 0.
	    der[2] = 0.
	} else {
	    der[1] = exp (-r2)
	    z = p[1] * der[1]
	    der[2] = z * 2 * r2 / p[2]
	}
end


# STF_MOFFAT1 -- Moffat function used in NLFIT.  The parameters are the
# amplitude, alpha squared, and beta and the input variable is the radius.

procedure stf_moffat1 (x, nvars, p, np, z)

real	x[nvars]		#I Input variables
int	nvars			#I Number of variables
real	p[np]			#I Parameter vector
int	np			#I Number of parameters
real	z			#O Function return

real	y

begin
	y = 1 + (x[1] / p[2]) ** 2
	if (abs (y) > 20.)
	    z = 0.
	else
	    z = p[1] * y ** p[3]
end


# STF_MOFFAT2 -- Moffat function and derivatives used in NLFIT.  The
# parameters are the amplitude, alpha squared, and beta and the input
# variable is the radius.

procedure stf_moffat2 (x, nvars, p, dp, np, z, der)

real	x[nvars]		#I Input variables
int	nvars			#I Number of variables
real	p[np]			#I Parameter vector
real	dp[np]			#I Dummy array of parameters increments
int	np			#I Number of parameters
real	z			#O Function return
real	der[np]			#O Derivatives

real	y

begin
	y = 1 + (x[1] / p[2]) ** 2
	if (abs (y) > 20.) {
	    z = 0.
	    der[1] = 0.
	    der[2] = 0.
	    der[3] = 0.
	} else {
	    der[1] = y ** p[3]
	    z = p[1] * der[1]
	    der[2] = -2 * z / y * p[3] / p[2] * (x[1] / p[2]) ** 2
	    der[3] = z * log (y)
	}
end
