include	<mach.h>
include	<gset.h>
include	<math/curfit.h>
include	"apertures.h"


# AP_PROFILE -- Determine spectrum profile with pixel rejection.
#
# The profile is determined by dividing each dispersion point by an estimate
# of the spectrum and then smoothing and normalizing to unit integral.
# This routine has two algorithms (procedures) for smoothing, one for nearly
# aligned spectra and one for tilted spectra.  The selection is determined
# by the calling program and signaled by whether there is a variation in
# the profile offsets.  For both smoothing algorithms the same iterative
# rejection algorithm may be used to eliminate deviant points from affecting
# the profile.  This rejection is selected by the "clean" parameter.
# A plot of the final profile along the dispersion may be made for the
# special plotfile "debugfits" or "debugall".
#
# Dispersion points with saturated pixels are ignored as well a when the
# total sky subtracted flux is negative.

procedure ap_profile (im, ap, dbuf, nc, nl, c1, l1, sbuf, svar, profile, nx, ny,
	xs, ys, asi)

pointer	im		# IMIO pointer
pointer	ap		# Aperture structure
pointer	dbuf		# Data buffer
int	nc, nl		# Size of data buffer
int	c1, l1		# Origin of data buffer
pointer	sbuf		# Sky values (NULL if none)
pointer	svar		# Sky variances
real	profile[ny,nx]	# Profile (returned)
int	nx, ny		# Size of profile array
int	xs[ny], ys	# Origin of profile array
pointer	asi		# Image interpolator for edge pixel weighting

real	gain		# Gain
real	rdnoise		# Readout noise
real	saturation	# Maximum value for an unsaturated pixel
bool	clean		# Clean cosmic rays?
real	lsigma, usigma	# Rejection sigmas.

int	fd, ix, iy, ix1, ix2, xs1, xs2, nsum
int	i, niterate, ixrej, iyrej, nrej, nreject
real	p, s, chisq, tfac, rrej, predict, var0, var, vmin, resid, wt1, wt2, dat
pointer	sp, str, spec, x1, x2, y, reject, xreject, data, sky, cv, gp

int	apgeti()
real	apgetr(), ap_cveval(), apgimr()
bool	apgetb()
errchk	salloc, ap_horne, ap_marsh, apgimr, ap_asifit

begin
	# Allocate memory. Adjust pointers to be one indexed.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (spec, ny, TY_REAL)
	call salloc (x1, ny, TY_REAL)
	call salloc (x2, ny, TY_REAL)
	call salloc (y, ny, TY_REAL)
	call salloc (reject, nx*ny, TY_BOOL)
	if (sbuf == NULL) {
	    call salloc (sky, nx, TY_REAL)
	    sky = sky - 1
	}
	spec=spec-1; x1=x1-1; x2=x2-1; y=y-1

	# Get task parameters.
	gain = apgimr ("gain", im)
	rdnoise = apgimr ("readnoise", im) ** 2
	saturation = apgetr ("saturation")
	if (!IS_INDEF(saturation))
	    saturation = saturation * gain
	lsigma = apgetr ("lsigma")
	usigma = apgetr ("usigma")
	clean = apgetb ("clean")
	if (clean)
	    niterate = apgeti ("niterate")
	else
	    niterate = 0

	# Initialize.
	if (rdnoise == 0.)
	    vmin = 1.
	else
	    vmin = rdnoise
	if (sbuf == NULL) {
	    call aclrr (Memr[sky+1], nx)
	    var0 = rdnoise
	}
	cv = AP_CV(ap)

	# Set aperture limits and initialize rejection flags.
	call alimi (xs, ny, xs1, xs2)
	i = AP_AXIS(ap)
	p = AP_CEN(ap,i) + AP_LOW(ap,i)
	s = AP_CEN(ap,i) + AP_HIGH(ap,i)
	xreject = reject
	do iy = 1, ny {
	    dat = ap_cveval (cv, real (iy + ys - 1)) - c1 + 1
	    Memr[x1+iy] = p + dat
	    Memr[x2+iy] = s + dat
	    Memr[x1+iy] = max (0.5, Memr[x1+iy]) + c1 - xs[iy]
	    Memr[x2+iy] = min (nc + 0.49, Memr[x2+iy]) + c1 - xs[iy]
	    ix1 = nint (Memr[x1+iy])
	    ix2 = nint (Memr[x2+iy])
	    Memr[y+iy] = iy
	    do ix = 1, nx {
		if (ix < ix1 || ix > ix2)
		    Memb[xreject] = false
		else
		    Memb[xreject] = true
		xreject = xreject + 1
	    }
	}

	# Estimate spectrum by summing across the aperture with partial
	# pixel estimates at the aperture edges.  The initial profile
	# estimates are obtained by normalizing by the spectrum estimate.
	# Profiles where the spectrum is below sky are set to zero.

	call aclrr (profile, nx * ny)
	nrej = 0
	do iy = 1, ny {
	    if (Memr[x1+iy] >= Memr[x2+iy]) {
		Memr[spec+iy] = 0.
	        do ix = 1, nx
		    profile[iy,ix] = 0.
		next
	    }

	    call ap_asifit (dbuf+(iy+ys-1-l1)*nc, nc, xs[iy]-c1+1,
		Memr[x1+iy]-c1+xs[iy], Memr[x2+iy]-c1+xs[iy], data, asi)
	    if (sbuf != NULL)
		sky = sbuf + (iy - 1) * nx - 1
	    call ap_edge (asi, Memr[x1+iy]+1, Memr[x2+iy]+1, wt1, wt2)
	    ix1 = nint (Memr[x1+iy])
	    ix2 = nint (Memr[x2+iy])
	    s = 0.
	    do ix = ix1, ix2 {
		if (!IS_INDEF(saturation))
		    if (Memr[data+ix] > saturation) {
			s = 0.
			nrej = nrej + 1
			break;
		    }
		dat = Memr[data+ix] - Memr[sky+ix]
		if (ix1 == ix2)
		    dat = wt1 * dat
		else if (ix == ix1)
		    dat = wt1 * dat
		else if (ix == ix2)
		    dat = wt2 * dat
		s = s + dat
	    }

	    if (s > 0.) {
	        do ix = ix1, ix2
		    profile[iy,ix] = max (0., (Memr[data+ix]-Memr[sky+ix])/s)
	    } else {
	        do ix = ix1, ix2
		    profile[iy,ix] = 0.
	    }
	    Memr[spec+iy] = s
	}

	if (nrej == ny)
	    call error (1, "All profiles contain saturated pixels")
	else if (nrej > 0) {
	    call sprintf (Memc[str], SZ_LINE,
		"EXTRACT: %d profiles with saturated pixels in aperture %d")
		call pargi (nrej)
		call pargi (AP_ID(ap))
	    if (nrej < ny / 3)
	        call ap_log (Memc[str], YES, NO, NO)
	    else
	        call ap_log (Memc[str], YES, NO, YES)
	}

	# Smooth the profile and possibly reject deviant pixels.
	nreject = 0
	tfac = 2.
	do i = 0, niterate {

	    # Estimate profile.
	    if (xs1 == xs2)
	        call ap_horne (im, cv, dbuf, nc, nl, c1, l1, Memr[spec+1], sbuf,
		    svar, Memb[reject], profile, nx, ny, xs, ys,
		    Memr[x1+1], Memr[x2+1])
	    else
	        call ap_marsh (im, dbuf, nc, nl, c1, l1, Memr[spec+1], sbuf,
		    svar, Memb[reject], profile, nx, ny, xs, ys,
		    Memr[x1+1], Memr[x2+1])

	    if (i == niterate)
		break

	    # Reject pixels.  The rejection threshold is based on the overall
	    # chi square.  Pixels are rejected on the basis of the current
	    # chi square and the largest residual not rejected is compared
	    # against the final chi square to possibly trigger another round
	    # of rejections.

	    chisq = 0.; nsum = 0; ixrej = 0; iyrej = 0; rrej = 0.; nrej = 0
	    do iy = 1, ny {
	        s = Memr[spec+iy]
		if (s <= 0.)
		    next
		call ap_asifit (dbuf+(iy+ys-1-l1)*nc, nc, xs[iy]-c1+1,
		    Memr[x1+iy]-c1+xs[iy], Memr[x2+iy]-c1+xs[iy], data, asi)
		if (sbuf != NULL) {
		    sky = sbuf + (iy - 1) * nx - 1
		    var0 = rdnoise + Memr[svar+iy-1]
		}
		call ap_edge (asi, Memr[x1+iy]+1, Memr[x2+iy]+1, wt1, wt2)
		xreject = reject + (iy - 1) * nx - 1
	        ix1 = nint (Memr[x1+iy])
	        ix2 = nint (Memr[x2+iy])
	        do ix = ix1, ix2 {
	            if (Memb[xreject+ix]) {
	                nsum = nsum + 1
	                predict = max (0., s * profile[iy,ix] + Memr[sky+ix])
	                var = max (vmin, var0 + predict)
	                resid = (Memr[data+ix] - predict) / sqrt (var)
	                chisq = chisq + resid**2
		        if (resid < -tfac*lsigma || resid > tfac*usigma) {
		            if (ix < ix1 || ix > ix2)
			        p = 0.
			    else if (ix1 == ix2)
		                p = wt1
		            else if (ix == ix1)
		                p = wt1
		            else if (ix == ix2)
		                p = wt2
		            else 
			        p = 1
		            Memr[spec+iy] = Memr[spec+iy] -
				p * (Memr[data+ix] - predict)
	                    nrej = nrej + 1
			    Memb[xreject+ix] = false
		        } else if (abs (resid) > abs (rrej)) {
		            rrej = resid
		            if (ix < ix1 || ix > ix2)
			        p = 0.
			    else if (ix1 == ix2)
		                p = wt1
		            else if (ix == ix1)
			        p = wt1
		            else if (ix == ix2)
			        p = wt2
		            else 
			        p = 1
		            dat = p * (Memr[data+ix] - predict)
		            ixrej = ix
		            iyrej = iy
			}
	            }
	        }
	    }

	    if (nsum == 0)
		call error (1, "All pixels rejected")
	    tfac = sqrt (chisq / nsum)
	    if (rrej < -tfac * lsigma || rrej > tfac * usigma) {
	        Memr[spec+iyrej] = Memr[spec+iyrej] - dat
		xreject = reject + (iyrej - 1) * nx - 1
	        Memb[xreject+ixrej] = false
	        nrej = nrej + 1
	    }

	    nreject = nreject + nrej
	    if (nrej == 0)
		break
	}

	# These plots are too big for production work but can be turned on
	# for debugging.

	call ap_popen (gp, fd, "fits")
	if (gp != NULL) {
	    ix1 = xs1
	    ix2 = xs2 + nx - 1
	    if (xs1 != xs2) {
		ix1 = ix1 + 1
		ix2 = ix2 - 1
	    }
	    do ix = ix1, ix2 {
		nrej = 0
		do iy = 1, ny {
		    i = ix - xs[iy] + 1
		    if (i < 1 || i > nx)
			next
		    if (Memr[spec+iy] <= 0.)
			next
		    data = dbuf + (iy + ys - 1 - l1) * nc + ix - c1 - 1
		    if (sbuf != NULL)
		        s = Memr[sbuf+(iy-1)*nx+i-1]
		    else
			s = Memr[sky+i]
		    nrej = nrej + 1
		    Memr[y+nrej] = iy + ys - 1
		    Memr[x1+nrej] = max (-.1, min (1.1,
			(Memr[data+1] - s) / Memr[spec+iy]))
		    Memr[x2+nrej] = profile[iy,i]
		}
		call gclear (gp)
		call gascale (gp, Memr[x1+1], nrej, 2)
		call grscale (gp, Memr[x2+1], nrej, 2)
		call gswind (gp, Memr[y+1], Memr[y+nrej], INDEF, INDEF)
		if (AP_AXIS(ap) == 1) {
		    call sprintf (Memc[str], SZ_LINE, "Column %d")
			call pargi (ix)
		    call glabax (gp, Memc[str], "Line", "Profile")
		} else {
		    call sprintf (Memc[str], SZ_LINE, "Line %d")
			call pargi (ix)
		    call glabax (gp, Memc[str], "Column", "Profile")
		}
		call gpmark (gp, Memr[y+1], Memr[x1+1], nrej, GM_POINT, 1., 1.)
		call gpline (gp, Memr[y+1], Memr[x2+1], nrej)
	    }
	}
	call ap_pclose (gp, fd)

	# Log the number of rejected pixels.
	if (clean) {
	    call sprintf (Memc[str], SZ_LINE,
		"EXTRACT: %d pixels rejected for profile from aperture %d")
		call pargi (nreject)
		call pargi (AP_ID(ap))
	    call ap_log (Memc[str], YES, NO, NO)
	}

	call sfree (sp)
end


# AP_HORNE -- Determine profile by fitting a low order function parallel to
# dispersion along image lines or columns after dividing by a spectrum
# estimate.  An initial profile estimate and a rejection array are
# required for setting the weights.  This is a straightforward algorithm
# similar to images.fit1d except that it is noninteractive.  The fitting
# function is fixed at a cubic spline and the number of pieces is set by
# the amount of tilt such that there is one cubic spline piece per
# passage across the tilted spectrum plus an amount based on the order
# of the tracing function.  It is named after Keith Horne
# since this is what is outlined in his paper.  The profile array is used
# cleverly to minimize memory requirements.  The storage order of the
# profile array, which is transposed relative to the data, is determined
# by this procedure.

procedure ap_horne (im, cvtrace, dbuf, nc, nl, c1, l1, spec, sbuf, svar, reject,
	profile, nx, ny, xs, ys, x1, x2)

pointer	im			# IMIO pointer
pointer	cvtrace			# Trace pointer
pointer	dbuf			# Data buffer
int	nc, nl			# Size of data buffer
int	c1, l1			# Origin of data buffer
real	spec[ny]		# Spectrum estimate
pointer	sbuf			# Sky values (NULL if none)
pointer	svar			# Sky variances
bool	reject[nx,ny]		# Rejection flags
real	profile[ny,nx]		# Initial profile in, improved profile out
int	nx, ny			# Size of profile array
int	xs[ny], ys		# Origin of profile array
real	x1[ny], x2[ny]		# Aperture limits in profile array

int	cvtype			# Curfit type
int	order			# Order of curfit function.
real	rdnoise			# Readout noise in RMS data numbers.

int	ix, iy, ierr
real	p, s, sk, var, vmin, var0, wmin
pointer	sp, y, w, cv, dbuf1, data, sky

#int	apgeti()
int	cvstati()
real	apgimr()
errchk	salloc, apgimr

begin
	call smark (sp)
	call salloc (y, ny, TY_REAL)
	call salloc (w, ny, TY_REAL)

	# Get CL parameters
	#cvtype = apgeti ("e_function")
	#order = apgeti ("e_order")
	rdnoise = apgimr ("readnoise", im) ** 2

	# Initialize.
	call alimr (x1, ny, p, s)
	cvtype = SPLINE3
	order = int (s - p + 1) + max (0, cvstati (cvtrace, CVNCOEFF) - 2)
	#order = min (20, order)
	order = 2 * order
	call cvinit (cv, cvtype, order, 1., real (ny))
	do iy = 1, ny
	    Memr[y+iy-1] = iy
	if (rdnoise == 0.)
	    vmin = 1.
	else
	    vmin = rdnoise
	dbuf1 = dbuf + (ys - l1 - 1) * nc - c1 - 1
	if (sbuf == NULL) {
	    sk = 0.
	    var0 = rdnoise
	}

	# For each line parallel to the dispersion divide by a spectrum
	# estimate and then fit the smoothing function.  Use the input
	# profile and rejection array to set the weights.

	do ix = 1, nx { 
	    data = dbuf1 + ix
	    if (sbuf != NULL)
		sky = sbuf - nx - 1 + ix
	    wmin = MAX_REAL
	    do iy = 1, ny {
	        s = spec[iy]
	        if (s > 0. && reject[ix,iy]) {
		    if (sbuf != NULL) {
			sk = Memr[sky+iy*nx]
			var0 = rdnoise + Memr[svar+iy-1]
		    }
		    p = profile[iy,ix]
		    var = max (vmin, var0 + max (0., s * p + sk))
		    var = (s ** 2) / var
		    wmin = min (wmin, var)
		    Memr[w+iy-1] = var
		    profile[iy,ix] = (Memr[data+iy*nc+xs[iy]] - sk) / s
		} else
		    Memr[w+iy-1] = 0.
	    }
	    if (wmin == MAX_REAL)
		call amovkr (1., Memr[w], ny)
	    else
		call amaxkr (Memr[w], wmin / 10., Memr[w], ny)
	    call cvfit (cv, Memr[y], profile[1,ix], Memr[w], ny, WTS_USER, ierr)
	    call cvvector (cv, Memr[y], profile[1,ix], ny)
	    call amaxkr (profile[1,ix], 0., profile[1,ix], ny)
	}

	call cvfree (cv)
	call sfree (sp)
end


# AP_MARSH -- Determine profile by Marsh algorithm (PASP V101, P1032, 1989).
# This algorithm fits low order polynomials to weighted points sampled
# at uniform intervals parallel to the aperture trace.  The polynomials
# are coupled through the weights and so requires a 2D matrix inversion.
# This is a relatively slow algorithm but does provide low order smoothing
# for arbitrary profile shapes in highly tilted spectra.  An estimate
# of the profile, a rejection array, sky and sky variance, and aperture
# limit arrays are required.

procedure ap_marsh (im, dbuf, nc, nl, c1, l1, spec, sbuf, svar, reject,
	profile, nx, ny, xs, ys, x1, x2)

pointer	im			# IMIO pointer
pointer	dbuf			# Data buffer
int	nc, nl			# Size of data buffer
int	c1, l1			# Origin of data buffer
real	spec[ny]		# Spectrum estimate
pointer	sbuf			# Sky values (NULL if none)
pointer	svar			# Sky variances
bool	reject[nx,ny]		# Rejection flags
real	profile[ny,nx]		# Initial profile in, improved profile out
int	nx, ny			# Size of profile array
int	xs[ny], ys		# Origin of profile array
real	x1[ny], x2[ny]		# Aperture limits in profile array

real	spix			# Polynomial pixel separation
int	npols			# Number of polynomials
int	order			# Order of function.
real	rdnoise			# Readout noise in RMS data numbers.

int	il, jl, kl, ll, ix, iy, ix1, ix2, nside, nadd
int	ip, ip1, ip2, index1, index2, index3
real	p, s, s2, dat, sk, var, vmin, var0
real	dx0, dx1, dx2, dx3, dx4, xj, xk, xt, xz, qj, qk, xadd
double	sum1, sum2
pointer	sp, work, work1, work2, work3, work4, ysum, data, sky

int	apgeti()
real	apgetr(), apgimr()
errchk	salloc, apgimr

begin
	# Get CL parameters
	#npols = apgeti ("npols")
	spix = apgetr ("polysep")
	order = apgeti ("polyorder")
	rdnoise = apgimr ("readnoise", im) ** 2

	# Set dimensions.
	npols = (x2[1] - x1[1] + 2) / spix
	spix = (x2[1] - x1[1] + 2) / real (npols)
	nside = npols * order
	nadd = nside * nside
	if (spix > 1.)
	    call error (4, "Polynomial separation too large")

	# Allocate memory.  One index pointers.
	call smark (sp)
	call salloc (work, nadd+3*nside, TY_REAL)
	call salloc (work4, nside, TY_INT)
	call salloc (ysum, ny, TY_REAL)
	work = work - 1
	work1 = work + nadd
	work2 = work1 + nside
	work3 = work2 + nside
	work4 = work4 - 1
	ysum=ysum-1
	if (sbuf == NULL) {
	    call salloc (sky, nx, TY_REAL)
	    sky = sky - 1
	}

	# Initialize.
	call aclrr (Memr[work+1], nadd+3*nside)
	call aclri (Memi[work4+1], nside)
	if (rdnoise == 0.)
	    vmin = 1.
	else
	    vmin = rdnoise
	if (sbuf == NULL) {
	    call aclrr (Memr[sky+1], nx)
	    var0 = rdnoise
	}

	# Factors for weights.
	dx0 = 0.5 - spix
	dx1 = abs (dx0)
	dx2 = 1. - (dx0 / spix) ** 2
	dx3 = 0.5 + spix
	dx4 = sqrt (2.) * spix

	# Accumulate least terms for least squares matrix equation AX = B.

	# First accumulate B.
	do jl = 0, npols-1 {
 	    do iy = 1, ny {
	        if (spec[iy] <= 0.)
		    next

		xj = x1[iy] - 1 + spix * (real (jl) + 0.5)
		ix1 = nint (xj - spix)
		ix2 = nint (xj + spix)
		if (ix1 < 1 || ix2 > nx) {
		    Memr[ysum+iy] = 0.
		    next
		}

		data = dbuf + (iy + ys - 1 - l1) * nc + xs[iy] - c1 - 1
		if (sbuf != NULL) {
		    sky = sbuf + (iy - 1) * nx - 1
		    var0 = rdnoise + Memr[svar+iy-1]
		}

		# Evaluate qj, the contribution of polynomial number jl+1
		# for the pixel ix1,jj.  Four cases are considered.  The
		# first two account for the triangular interpolation
		# function partially overlapping a pixel, on one side
		# only.  The third is for the function wholly inside a
		# pixel, and finally for the pixel wholly covered by the
		# interpolation function.

	        s = spec[iy]
	        sum1 = 0.
	        do ix = ix1, ix2 {
		    if (!reject[ix,iy])
			next
	            p = profile[iy,ix]
		    sk = Memr[sky+ix]
                    dat = Memr[data+ix] - sk
		    var = max (vmin, var0 + max (0., s * p + sk))

                    xz = xj - real (ix)
                    xt = abs (xz)
                    if (xt >= dx1) {
                        if (xt >= 0.5)
                            qj = ((xt - dx3) / dx4) ** 2
                        else
                            qj = 1.- ((xt - dx0) / dx4) ** 2
                        
                    } else if (xt <= dx0)
                        qj = 1.
                    else
                        qj = dx2 - (xz / spix) ** 2
                    sum1 = sum1 + qj * s * dat / var
                }
	        Memr[ysum+iy] = sum1
	    }

	    index1 = order * jl
	    do il = 1, order {
	        sum1 = 0.
	        ip = il - 1
	        do iy = 1, ny
	            if (spec[iy] > 0.)
			sum1 = sum1 + Memr[ysum+iy] * ((real (iy) / ny) ** ip)
	        Memr[work1+index1+il] = sum1
	    }
	}

	# Now accumulate matrix A.  Since it is symmetric we only need to
	# evaluate half of it.  Since it is banded we only need to evaluate
	# contribution if two polynomial terms can be affected by the same
	# pixel.

	ip1 = nside - 1
	ip2 = order * ip1
	do jl = 0, npols-1 {
	    do kl = 0, jl {
		if (spix * (jl - kl - 2) > 0.)
		    next
		do iy = 1, ny {
		    if (spec[iy] <= 0.)
			next
		    if (sbuf != NULL) {
			sky = sbuf + (iy - 1) * nx - 1
			var0 = rdnoise + Memr[svar+iy-1]
		    }

		    # Compute left and right limits of polynomials jl+1
		    # and kl+1 for this value of y Evaluate sum over row
		    # of qj[jl+1] times qj[kl+1] where qj[i] is fraction
		    # of polynomial i which contributes to to pixel ix,jj.

		    xj = x1[iy] - 1 + spix * (real (jl) + 0.5)
		    xk = x1[iy] - 1 + spix * (real (kl) + 0.5)
		    ix1 = nint (xj - spix)
		    ix2 = nint (xk + spix)

                    if (ix2 < ix1 || ix1 < 1 || ix2 > nx) {
			Memr[ysum+iy] = 0.
			next
		    }

                    s  = spec[iy]
                    s2 = s * s
                    sum1 = 0.
                    do ix = ix1, ix2 {
                        if (reject[ix,iy]) {
                            p = profile[iy,ix]
			    sk = Memr[sky+ix]
			    var = max (vmin, var0 + max (0., s * p + sk))

                            xz = xj - real (ix)
                            xt = abs (xz)
                            if (xt >= dx1) {
                                if (xt >= 0.5)
                                    qj = ((xt-dx3)/dx4)**2
                                else
                                    qj = 1.- ((xt-dx0)/dx4)**2
                            } else if (xt <= dx0)
                                qj = 1.
                            else
                                qj = dx2 - (xz / spix) ** 2
                            if (kl != jl) {
                                xz = xk - real (ix)
                                xt = abs (xz)
                                if (xt >= dx1) {
                                    if (xt >= 0.5)
                                        qk = ((xt-dx3)/dx4)**2
                                    else
                                        qk = 1.-((xt-dx0)/dx4)**2
                                } else if (xt <= dx0)
                                    qk = 1.
                                else
                                    qk = dx2 - (xz / spix) ** 2
                            } else
                                qk = qj
                            sum1 = sum1 + qj * qk * s2 / var
                        }
                    }
                    Memr[ysum+iy] = sum1
		}

	        do il = 1, order {
	            do ll = 1, il {
	                sum1 = 0.
	                ip = il + ll - 2
	                do iy = 1, ny
	                    if (spec[iy] > 0.)
	                        sum1 = sum1 +
				    Memr[ysum+iy] * ((real (iy) / ny)**ip)
	                index1 = nside * (order*jl+il-1) + order * kl + ll
	                Memr[work+index1] = sum1
	                if (ll != il) {
	                    ip = ip1 * (ll - il)
	                    index2 = index1 + ip
	                    Memr[work+index2] = sum1
	                } else
	                    index2 = index1 
	                if (kl != jl) {
	                    index3 = index2 + ip2 * (kl - jl)
	                    Memr[work+index3] = sum1
	                    if (ll != il)
	                        Memr[work+index3-ip] = sum1
	                }
	            }
	        }
	    }
	}

	# Solve matrix equation AX = B for X.  A is a real symmetric,
	# positive definite matrix, dimension (order*npols)**2.  X is 
	# the vector representing the coefficients fitted to the 
	# normalized profile.  Coefficients are reordered for later speed.

	call hfti (Memr[work+1], nside, nside, nside, Memr[work1+1], 1, 1,
	    0.01, ip, p, Memr[work2+1], Memr[work3+1], Memi[work4+1])

	do jl = 1, order {
	    do il = 1, npols {
	        index1 = order * (il - 1) + jl
	        index2 = npols * (jl - 1) + il
	        Memr[work+index2] = Memr[work1+index1]
	    }
	}

	# Evaluate fit and make profile positive only.
	do iy = 1, ny {
	    ix1 = nint (x1[iy])
	    ix2 = nint (x2[iy])
	    xadd = x1[iy] - 1
	    s = 0.
	    do ix = 1, nx {
	        xj = real (ix) - xadd - 0.5
	        xk = real (ix) - xadd + 0.5
	        ip1 = int (xj / spix + 0.5)
	        ip2 = int (xk / spix + 1.5)
	        ip1 = max (1, min (ip1, npols))
	        ip2 = max (1, min (ip2, npols))
	        sum1 = 0.
	        do jl = 0, order-1 {
	            index1 = npols * jl
	            sum2 = 0.
	            do il = ip1, ip2 {
	                xz = xadd + spix * (real (il-1) + 0.5) - real (ix)
	                xt = abs (xz)
	                if (xt >= dx1) {
	                    if (xt >= 0.5)
	                        qj = ((xt - dx3) / dx4) ** 2
	                    else
	                        qj = 1. - ((xt - dx0) / dx4) ** 2
	                } else if (xt <= dx0)
	                    qj = 1.
	                else
	                    qj = dx2 - (xz / spix) ** 2
	                sum2 = sum2 + qj * Memr[work+index1+il]
	            }
	            sum1 = sum1 + sum2 * ((real (iy)/ ny) ** jl)
	        }
	        profile[iy,ix] = max (0.d0, sum1)
	    }
	}

	call sfree (sp)
end
