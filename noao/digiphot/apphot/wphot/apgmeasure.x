include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/center.h"
include "../lib/fitsky.h"

define	CONVERSION	2.772588

# AP_GMEASURE -- Procedure to measure the fluxes and effective areas of a set of
# apertures assuming a Gaussian weighting function.

procedure ap_gmeasure (ap, im, wx, wy, c1, c2, l1, l2, aperts, sums, areas,
        naperts)

pointer	ap			# pointer to apphot structure
pointer	im			# pointer to image
real	wx, wy			# center of subraster
int	c1, c2			# column limits
int	l1, l2			# line limits
real	aperts[ARB]		# array of apertures
real	sums[ARB]		# array of sums
real	areas[ARB]		# aperture areas
int	naperts			# number of apertures

int	i, j, k, nx, yindex, positive
pointer	sp, buf, sump, sumpw
real	xc, yc, apmaxsq, fwhmpsf, skyval, varsky, dy2, r2, r, fctn
real	sig2, prof, var, weight, norm

int	apstati()
pointer	imgs2r()
real	apstatr()

begin
	# Initialize.
	call smark (sp)
	call salloc (sump, naperts, TY_REAL)
	call salloc (sumpw, naperts, TY_REAL)
	call aclrr (Memr[sump], naperts)
	call aclrr (Memr[sumpw], naperts)

	# Setup some apphot parameters.
	fwhmpsf = apstatr (ap, FWHMPSF) * apstatr (ap, SCALE)
	sig2 = fwhmpsf ** 2 / CONVERSION
	positive = apstati (ap, POSITIVE)
	if (positive == YES) {
	    skyval = apstatr (ap, SKY_MODE)
	    varsky = apstatr (ap, SKY_SIGMA) ** 2
	} else {
	    skyval = 0.0
	    varsky = (apstatr (ap, READNOISE) / apstatr (ap, EPADU)) ** 2
	}

	# Get array boundary parameters.
	nx = c2 - c1 + 1
	xc = wx - c1 + 1
	yc = wy - l1 + 1
	apmaxsq = (aperts[naperts] + 0.5) ** 2

	# Clear out the accumulaters
	call aclrr (sums, naperts)
	call aclrr (areas, naperts)

	# Loop over the pixels.
	do j = l1, l2 {
	    buf = imgs2r (im, c1, c2, j, j)
	    if (buf == EOF) {
		call sfree (sp)
		return
	    }
	    yindex = j - l1 + 1
	    dy2 = (yindex - yc) ** 2
	    do i = 1, nx {
		r2 = (i - xc) ** 2 + dy2
		if (r2 > apmaxsq)
		    next
		var = abs (Memr[buf+i-1] - skyval) + varsky
		if (var <= 0.0)
		    next
		r = sqrt (r2) - 0.5
		prof = max (exp (-r2 / sig2), 0.0)
		if (prof <= 0.0)
		    next
		weight = prof / var
		do k = 1, naperts {
		    if (r > aperts[k])
			next
		    fctn = max (0.0, min (1.0, aperts[k] - r))
		    sums[k] = sums[k] + weight * fctn * Memr[buf+i-1]
		    areas[k] = areas[k] + weight * fctn
		    Memr[sump+k-1] = Memr[sump+k-1] + prof
		    Memr[sumpw+k-1] = Memr[sumpw+k-1] + weight * prof
		}
	    }
	}

	# Normalize.
	do k = 1, naperts {
	    if (Memr[sumpw+k-1] <= 0.0)
	        norm = 0.0
	    else
	        norm = Memr[sump+k-1] / Memr[sumpw+k-1]
	    sums[k] = sums[k] * norm
	    areas[k] = areas[k] * norm
	}

	call sfree (sp)
end


# AP_BGMEASURE -- Procedure to measure the fluxes and effective areas of a set
# of apertures assuming a Gaussian weighting function.

procedure ap_bgmeasure (ap, im, wx, wy, c1, c2, l1, l2, datamin, datamax,
	aperts, sums, areas, naperts, minapert)

pointer	ap			# pointer to apphot structure
pointer	im			# pointer to image
real	wx, wy			# center of subraster
int	c1, c2			# column limits
int	l1, l2			# line limits
real	datamin			# minimum good data
real	datamax			# maximum good data
real	aperts[ARB]		# array of apertures
real	sums[ARB]		# array of sums
real	areas[ARB]		# aperture areas
int	naperts			# number of apertures
int	minapert		# minimum aperture fo bad pixels

int	i, j, k, nx, yindex, kindex, positive
pointer	sp, buf, sump, sumpw
real	xc, yc, apmaxsq, fwhmpsf, skyval, varsky, dy2, r2, r, fctn
real	pixval, sig2, prof, var, weight, norm

int	apstati()
pointer	imgs2r()
real	apstatr()

begin
	# Initialize.
	call smark (sp)
	call salloc (sump, naperts, TY_REAL)
	call salloc (sumpw, naperts, TY_REAL)
	call aclrr (Memr[sump], naperts)
	call aclrr (Memr[sumpw], naperts)
	minapert = naperts + 1

	# Setup some apphot parameters.
	fwhmpsf = apstatr (ap, FWHMPSF) * apstatr (ap, SCALE)
	sig2 = fwhmpsf ** 2 / CONVERSION
	positive = apstati (ap, POSITIVE)
	if (positive == YES) {
	    skyval = apstatr (ap, SKY_MODE)
	    varsky = apstatr (ap, SKY_SIGMA) ** 2
	} else {
	    skyval = 0.0
	    varsky = (apstatr (ap, READNOISE) / apstatr (ap, EPADU)) ** 2
	}

	# Get array boundary parameters.
	nx = c2 - c1 + 1
	xc = wx - c1 + 1
	yc = wy - l1 + 1
	apmaxsq = (aperts[naperts] + 0.5) ** 2

	# Clear out the accumulaters
	call aclrr (sums, naperts)
	call aclrr (areas, naperts)

	# Loop over the pixels.
	do j = l1, l2 {
	    buf = imgs2r (im, c1, c2, j, j)
	    if (buf == EOF) {
		call sfree (sp)
		return
	    }
	    yindex = j - l1 + 1
	    dy2 = (yindex - yc) ** 2
	    do i = 1, nx {
		r2 = (i - xc) ** 2 + dy2
		if (r2 > apmaxsq)
		    next
		pixval = Memr[buf+i-1]
		var = abs (pixval - skyval) + varsky
		if (var <= 0.0)
		    next
		r = sqrt (r2) - 0.5
		prof = max (exp (-r2 / sig2), 0.0)
		if (prof <= 0.0)
		    next
		weight = prof / var
		kindex = naperts + 1
		do k = 1, naperts {
		    if (r > aperts[k])
			next
		    kindex = min (k, kindex)
		    fctn = max (0.0, min (1.0, aperts[k] - r))
		    sums[k] = sums[k] + weight * fctn * pixval
		    areas[k] = areas[k] + weight * fctn
		    Memr[sump+k-1] = Memr[sump+k-1] + prof
		    Memr[sumpw+k-1] = Memr[sumpw+k-1] + weight * prof
		}
		if (kindex < minapert) {
		    if (pixval < datamin || pixval > datamax)
			minapert = kindex
		}
	    }
	}

	# Normalize.
	do k = 1, naperts {
	    if (Memr[sumpw+k-1] <= 0.0)
	        norm = 0.0
	    else
	        norm = Memr[sump+k-1] / Memr[sumpw+k-1]
	    sums[k] = sums[k] * norm
	    areas[k] = areas[k] * norm
	}

	call sfree (sp)
end
