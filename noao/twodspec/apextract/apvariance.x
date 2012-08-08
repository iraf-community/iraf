include	<gset.h>
include	"apertures.h"


# AP_VARIANCE -- Variance weighted extraction based on profile and CCD noise.
# If desired reject deviant pixels.  In addition to the variance weighted
# spectrum, the unweighted and uncleaned "raw" spectrum is extracted and
# a sigma spectrum is returned.  Wavelengths with saturated pixels are
# flagged with 0 value and negative sigma if cleaning.

procedure ap_variance (im, ap, dbuf, nc, nl, c1, l1, sbuf, svar, profile,
	nx, ny, xs, ys, spec, raw, specsig, nsubaps, asi)

pointer	im			# IMIO pointer
pointer	ap			# Aperture structure
pointer	dbuf			# Data buffer
int	nc, nl			# Size of data buffer
int	c1, l1			# Origin of data buffer
pointer	sbuf			# Sky values (NULL if none)
pointer	svar			# Sky variance
real	profile[ny,nx]		# Profile (returned)
int	nx, ny			# Size of profile array
int	xs[ny], ys		# Origin of profile array
real	spec[ny,nsubaps]	# Spectrum
real	raw[ny,nsubaps]		# Raw spectrum
real	specsig[ny,nsubaps]	# Sky variance in, spectrum sigma out
int	nsubaps			# Number of subapertures
pointer	asi			# Image interpolator for edge pixel weighting

real	rdnoise			# Readout noise in RMS data numbers.
real	gain			# Gain in photons per data number.
real	saturation		# Maximum value for an unsaturated pixel.
bool	clean			# Clean cosmic rays?
real	nclean			# Number of pixels to clean
real	lsigma, usigma		# Rejection sigmas.

bool	sat
int	fd, iterate, niterate, nrej, irej, nreject
int	i, ix, iy, ix1, ix2
real	low, high, step, shift, x1, x2, wt1, wt2, s, w, dat, sk, var, var0
real	sum, wsum, wvsum, sum1, sum2, total1, total2
real	vmin, resid, rrej
pointer	cv, gp
pointer	sp, str, work, wt, xplot, yplot, eplot, fplot, data, sky, data1

real	apgetr(), apgimr(), ap_cveval()
bool	apgetb()
errchk	apgimr, ap_asifit

begin
	# Get task parameters.
	gain = apgimr ("gain", im)
	rdnoise = apgimr ("readnoise", im) ** 2
	saturation = apgetr ("saturation")
	if (!IS_INDEF(saturation))
	    saturation = saturation * gain
	clean = apgetb ("clean")
	lsigma = apgetr ("lsigma")
	usigma = apgetr ("usigma")
	call ap_popen (gp, fd, "clean")

	# Allocate memory and one index.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (work, 6*nx, TY_REAL)
	wt = work - 1
	xplot = wt + nx
	yplot = xplot + nx
	eplot = yplot + nx
	fplot = eplot + nx
	data1 = fplot + nx
	if (sbuf == NULL) {
	    call salloc (sky, nx, TY_REAL)
	    call aclrr (Memr[sky], nx)
	    sky = sky - 1
	    var0 = rdnoise
	}

	# Initialize
	if (rdnoise == 0.)
	    vmin = 1.
	else
	    vmin = rdnoise
	if (clean) {
	    nclean = apgetr ("nclean")
	    if (nclean < 1.)
	        niterate = max (1., nclean * nx)
	    else
		niterate = max (1., min (real (nx), nclean))
	} else
	    niterate = 0

	call aclrr (spec, ny * nsubaps)
	call aclrr (raw, ny * nsubaps)
	call amovkr (-1., specsig, ny * nsubaps)

	i = AP_AXIS(ap)
	low = AP_CEN(ap,i) + AP_LOW(ap,i)
	high = AP_CEN(ap,i) + AP_HIGH(ap,i)
	step = (high - low) / nsubaps
	cv = AP_CV(ap)

	# For each line compute the weighted spectrum and then iterate
	# to reject deviant pixels.  Rejected pixels are flagged by negative
	# variance.

	nreject = 0
	total1 = 0.
	total2 = 0.
	do iy = 1, ny {
	    shift = ap_cveval (cv, real (iy + ys - 1)) - c1 + 1
	    x1 = max (0.5, low + shift) + c1 - xs[iy]
	    x2 = min (nc + 0.49, high + shift) + c1 - xs[iy]
	    if (x1 >= x2)
		next
	    ix1 = nint (x1)
	    ix2 = nint (x2)

	    call ap_asifit (dbuf+(iy+ys-1-l1)*nc, nc, xs[iy]-c1+1,
		low+shift, high+shift, data, asi)
#	    data = dbuf + (iy + ys - 1 - l1) * nc + xs[iy] - c1 - 1
	    if (sbuf != NULL) {
		sky = sbuf + (iy - 1) * nx - 1
		var0 = rdnoise + Memr[svar+iy-1]
	    }

	    # Set pixel weights for summing.
#	    if (asi != NULL)
#		call asifit (asi, Memr[data], nc-xs[iy]+c1)
	    call ap_edge (asi, x1+1, x2+1, wt1, wt2)

	    # First estimate spectrum by summing across the aperture.
	    # Accumulate the raw spectrum and set up various arrays for
	    # plotting and later access.

	    sat = false
	    sum = 0.
	    wsum = 0.
	    wvsum = 0.
	    do ix = ix1, ix2 {
		if (ix1 == ix2)
		    w = wt1
		else if (ix == ix1)
		    w = wt1
		else if (ix == ix2)
		    w = wt2
		else
		    w = 1.
		dat = Memr[data+ix]
		if (!IS_INDEF(saturation))
		    if (dat > saturation)
			sat = true
		sk = Memr[sky+ix]
		raw[iy,1] = raw[iy,1] + w * (dat - sk)

	        Memr[xplot+ix] = ix + xs[iy] - 1
	        Memr[yplot+ix] = dat - sk
		Memr[data1+ix] = dat - sk
		Memr[wt+ix] = w
		var = max (vmin, var0 + max (0., dat))
		w = profile[iy,ix] / var
		var = sqrt (var)
		Memr[eplot+ix] = var
		sum = sum + w * (dat - sk)
		wsum = wsum + w * profile[iy,ix]
		wvsum = wvsum + (w * var) ** 2
	    }
	    if (wsum > 0.) {
	        spec[iy,1] = sum / wsum
	        specsig[iy,1] = sqrt (wvsum) / abs (wsum)
	    } else {
	        spec[iy,1] = 0.
	        specsig[iy,1] = -1.
	    }

	    sum = 0.
	    wsum = 0.
	    wvsum = 0.
	    sum1 = 0.
	    sum2 = 0.
	    do ix = ix1, ix2 {
		sum1 = sum1 + Memr[wt+ix] * Memr[data1+ix]
		if (Memr[eplot+ix] <= 0.)
		    next
		sk = Memr[sky+ix]
	        s = max (0., spec[iy,1]) * profile[iy,ix]
	        var = max (vmin, var0 + (s + sk))
	        w = profile[iy,ix] / var
		var = sqrt (var)
	        Memr[eplot+ix] = var
	        Memr[fplot+ix] = s
	        sum = sum + w * Memr[data1+ix]
	        wsum = wsum + w * profile[iy,ix]
	        wvsum = wvsum + (w * var) ** 2
	    }
	    if (wsum > 0.) {
	        spec[iy,1] = sum / wsum
	        specsig[iy,1] = sqrt (wvsum) / abs (wsum)
		sum2 = sum2 + spec[iy,1]
	    } else {
	         spec[iy,1] = 0.
	         specsig[iy,1] = -1.
		 sum1 = 0.
		 sum2 = 0.
	    }

	    # Reject cosmic rays one at a time.
	    nrej = 0
	    do iterate = 1, niterate {
	        irej = 0
	        rrej = 0.

		# Compute revised variance estimate using profile model 
		# skip rejected pixels, find worst pixel.

	        do ix = ix1, ix2 {
	            if (Memr[eplot+ix] <= 0.)
			next
	            s = max (0., spec[iy,1]) * profile[iy,ix]
		    sk = Memr[sky+ix]
	            var = sqrt (max (vmin, var0 + max (0., s + sk)))
	            Memr[fplot+ix] = s
	            Memr[eplot+ix] = var

		    resid = (Memr[data1+ix] - Memr[fplot+ix]) / var
		    if (abs (resid) > abs (rrej)) {
			rrej = resid
			irej = ix
	            }
	        }

		# Reject worst outlier.

	        if (rrej <= -lsigma || rrej >= usigma) {
	            Memr[eplot+irej] = -Memr[eplot+irej]
		    Memr[data1+irej] = Memr[fplot+irej]
	            nrej = nrej + 1
	        } else
		    break

		# Update spectrum estimate excluding rejected pixels.
	        sum = 0.
	        wsum = 0.
	        wvsum = 0.
		sum1 = 0.
		sum2 = 0.
	        do ix = ix1, ix2 {
		    sum1 = sum1 + Memr[wt+ix] * Memr[data1+ix]
	            if (Memr[eplot+ix] <= 0.)
			next
	            w = profile[iy,ix] / Memr[eplot+ix]**2
	            sum = sum + w * Memr[data1+ix]
	            wsum = wsum + w * profile[iy,ix]
	            wvsum = wvsum + (w * Memr[eplot+ix]) ** 2
	        }

	        if (wsum > 0.) {
	            spec[iy,1] = sum / wsum
	            specsig[iy,1] = sqrt (wvsum) / abs (wsum)
		    sum2 = sum2 + spec[iy,1]
	        } else {
	            spec[iy,1] = 0.
	            specsig[iy,1] = -1.
		    sum1 = 0.
		    sum2 = 0.
	        }
	    }

	    nreject = nreject + nrej
	    total1 = total1 + sum1
	    total2 = total2 + sum2

	    # Calculate subapertures if desired.
	    if (nsubaps > 1) {
		do i = 1, nsubaps {
		    x1 = max (0.5, low + (i - 1) * step + shift) + c1 - xs[iy]
		    x2 = min (nc + 0.49, low + i * step + shift) + c1 - xs[iy]
		    if (x1 >= x2) {
			spec[iy,i] = 0.
			raw[iy,i] = 0.
			specsig[iy,i] = -1.
			next
		    }
		    ix1 = nint (x1)
		    ix2 = nint (x2)
		    call ap_edge (asi, x1+1, x2+1, wt1, wt2)

	            sum = 0.
	            wvsum = 0.
		    raw[iy,i] = 0.
	            do ix = ix1, ix2 {
			if (ix1 == ix2)
			    w = wt1
			else if (ix == ix1)
			    w = wt1
			else if (ix == ix2)
			    w = wt2
			else
			    w = 1.
			raw[iy,i] = raw[iy,i] + w * Memr[yplot+ix]
	                if (Memr[eplot+ix] <= 0.)
			    next
	                w = profile[iy,ix] / Memr[eplot+ix]**2
	                sum = sum + w * Memr[data1+ix]
	                wvsum = wvsum + (w * Memr[eplot+ix]) ** 2
	            }

	            if (wsum > 0.) {
	                spec[iy,i] = sum / wsum
	                specsig[iy,i] = sqrt (wvsum) / abs (wsum)
	            } else {
	                spec[iy,i] = 0.
	                specsig[iy,i] = -1.
	            }
		}
	    }

	    # Flag points with saturated pixels.
	    if (sat)
	        do i = 1, nsubaps
		    specsig[iy,i] = -specsig[iy,i]

	    # Plot profile with cosmic rays if desired.
	    if (gp != NULL && nrej > 0 && spec[iy,1] > 0.) {
		call sprintf (Memc[str], SZ_LINE, "Profile %4d")
		    call pargi (iy)
		s = Memr[yplot+ix1] - abs (Memr[eplot+ix1])
		w = Memr[yplot+ix1] + abs (Memr[eplot+ix1])
		do ix = ix1+1, ix2 {
		    s = min (s, Memr[yplot+ix] - abs (Memr[eplot+ix]))
		    w = max (w, Memr[yplot+ix] + abs (Memr[eplot+ix]))
		}
		sum = w - s
		x1 = ix1 + xs[iy] - 2
		x2 = ix2 + xs[iy]
		s = s - 0.1 * sum
		w = w + 0.1 * sum
		call gclear (gp)
		call gswind (gp, x1, x2, s, w)
		call glabax (gp, Memc[str], "", "")

	        do ix = ix1, ix2 {
	            if (Memr[eplot+ix] > 0.) {
			call gmark (gp, Memr[xplot+ix], Memr[yplot+ix],
			    GM_PLUS, 2., 2.)
			call gmark (gp, Memr[xplot+ix], Memr[yplot+ix],
			    GM_VEBAR, 2., -6.*Memr[eplot+ix])
	            } else {
			call gmark (gp, Memr[xplot+ix], Memr[yplot+ix],
			    GM_CROSS, 2., 2.)
			call gmark (gp, Memr[xplot+ix], Memr[yplot+ix],
			    GM_VEBAR, 1., 6.*Memr[eplot+ix])
	            }
	        }
		call gpline (gp, Memr[xplot+ix1], Memr[fplot+ix1], ix2-ix1+1)
	    }
	}

	# To avoid any bias, scale weighted extraction to same total flux
	# as raw spectrum (with rejected pixels replaced by fit).

	if (total1 * total2 <= 0.) {
	    call sprintf (Memc[str], SZ_LINE,
		"EXTRACT: WARNING - Aperture %d:")
		call pargi (AP_ID(ap))
	    call ap_log (Memc[str], YES, NO, YES)
	    call sprintf (Memc[str], SZ_LINE,
		"   Total variance weighted spectrum flux is %g")
		call pargr (total2)
	    call ap_log (Memc[str], YES, NO, YES)
	    call sprintf (Memc[str], SZ_LINE,
		"   Total unweighted spectrum flux is %g")
		call pargr (total1)
	    call ap_log (Memc[str], YES, NO, YES)
	    call sprintf (Memc[str], SZ_LINE,
		"   Variance spectrum bias factor ignored")
	    call ap_log (Memc[str], YES, NO, YES)
	} else {
	    sum = total1 / total2
	    call amulkr (spec, sum, spec, ny * nsubaps)
	    call amulkr (specsig, sum, specsig, ny * nsubaps)
	    if (sum < .5 || sum > 2) {
		call sprintf (Memc[str], SZ_LINE,
		    "EXTRACT: WARNING - Aperture %d:")
		    call pargi (AP_ID(ap))
		call ap_log (Memc[str], YES, NO, YES)
		call sprintf (Memc[str], SZ_LINE,
		    "   Total variance weighted spectrum flux is %g")
		    call pargr (total2)
		call ap_log (Memc[str], YES, NO, YES)
		call sprintf (Memc[str], SZ_LINE,
		    "   Total unweighted spectrum flux is %g")
		    call pargr (total1)
		call ap_log (Memc[str], YES, NO, YES)
		call sprintf (Memc[str], SZ_LINE,
		    "EXTRACT: Aperture %d variance spectrum bias factor is %g")
		    call pargi (AP_ID(ap))
		    call pargr (total1 / total2)
		call ap_log (Memc[str], YES, NO, YES)
	    } else {
		call sprintf (Memc[str], SZ_LINE,
		    "EXTRACT: Aperture %d variance spectrum bias factor is %g")
		    call pargi (AP_ID(ap))
		    call pargr (total1 / total2)
		call ap_log (Memc[str], YES, NO, NO)
	    }
	}

	# Log the number of rejected pixels.
	if (clean) {
	    call sprintf (Memc[str], SZ_LINE,
		"EXTRACT: %d pixels rejected from aperture %d")
		call pargi (nreject)
		call pargi (AP_ID(ap))
	    call ap_log (Memc[str], YES, NO, NO)
	}

	call ap_pclose (gp, fd)
	call sfree (sp)
end
