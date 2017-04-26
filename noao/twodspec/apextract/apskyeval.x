include	<math/iminterp.h>
include <mach.h>
include	"apertures.h"

# Background fitting types
define	BACKGROUND	"|none|average|median|minimum|fit|"
define	B_NONE		1
define	B_AVERAGE	2
define	B_MEDIAN	3
define	B_MINIMUM	4
define	B_FIT		5

define	NSAMPLE		20	# Maximum number of background sample regions


# AP_SKYEVAL -- Evaluate sky within aperture.
#
# The sky pixels specified by the background sample string are used to
# determine a sky function at each line which is then evaluated for each
# pixel in the aperture as given by the SBUF array with starting offsets
# given by XS.  The fitting consists of either a straight average or a
# function fit using ICFIT.  The sky regions are specified relative to the
# aperture center.  To avoid systematics due to shifting of the aperture
# relative to the integer pixel positions the sky regions are linearly
# interpolated.  The average uses the integral of the interpolation
# function within the sample region endpoints.  The fit samples the
# interpolation on a pixel grid with the aperture exactly centered on
# a pixel.  A crude sky variance is computed for each line based solely
# on the variance model and the square root of the number of "pixels"
# used for the fit.  This variance is used to boost the variance of
# the sky subtracted spectrum during variance weighting.  Because sky
# noise may be significant in short slits a box car smoothing may be
# used giving a lower variance per pixel but bad errors near sky lines.
# An unweighted aperture sum of the sky is returned in case the user
# wants to save the subtracted 1D sky spectrum.

procedure ap_skyeval (im, ap, dbuf, nc, nl, c1, l1, sbuf, svar, sky, nx, ny,
	xs, ys, nsubaps, rdnoise)

pointer	im		# IMIO pointer
pointer	ap		# Aperture structure
pointer	dbuf		# Data buffer
int	nc, nl		# Size of data buffer
int	c1, l1		# Origin of data buffer
real	sbuf[nx,ny]	# Sky values
real	svar[ny]	# Sky variances
real	sky[ny,nsubaps]	# Extracted sky (out)
int	nx, ny		# Size of profile array
int	xs[ny], ys	# Origin of profile array
int	nsubaps		# Number of subapertures
real	rdnoise		# Readout noise in RMS data numbers.

int	bkg		# Background type
int	skybox		# Sky box car smoothing

int	i, j, ix1, ix2, nsample, nsky, nfit, ix, iy
real	center, xmin, xmax, a, b, c, s, avg
pointer	ic, cv, cv1, asi, sp, str, data, as, bs, x, y, w

int	apgwrd(), apgeti(), ctor()
real	ic_getr(), ap_cveval(), asieval(), asigrl(), amedr()
errchk	salloc, ic_fit

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get CL parameters and set shift and fitting function pointers.
	bkg = apgwrd ("background", Memc[str], SZ_LINE, BACKGROUND)
	skybox = apgeti ("skybox")

	cv = AP_CV(ap)
	ic = AP_IC(ap)

	# Set center and maximum limits relative to data buffer.
	# The limits are required to overlap the aperture and include
	# an extra point at each end for interpolation.  Shifts
	# and boundary limits will be enforced later.

	i = AP_AXIS(ap)
	center = AP_CEN(ap,i)
	xmin = center + min (AP_LOW(ap,i), ic_getr (ic, "xmin"))
	xmax = center + max (AP_HIGH(ap,i), ic_getr (ic, "xmax"))
	ix1 = nint (xmin) - 1
	ix2 = nint (xmax) + 1
	nfit = ix2 - ix1 + 1

	# Allocate memory and parse sample string.
	# The colons in the sample string must be changed to avoid
	# sexigesimal interpretation.

	call salloc (as, NSAMPLE, TY_REAL)
	call salloc (bs, NSAMPLE, TY_REAL)

	call ic_gstr (ic, "sample", Memc[str], SZ_LINE)
	for (i=str; Memc[i]!=EOS; i=i+1)
	    if (Memc[i] == ':')
		Memc[i] = '$'

	nsample = 0
	for (i=1; Memc[str+i-1]!=EOS; i=i+1) {
	    if (ctor (Memc[str], i, a) > 0) {
		i = i - 1
		if (Memc[str+i] == '$') {
		    i = i + 2
		    if (ctor (Memc[str], i, b) > 0) {
			i = i - 1
			Memr[as+nsample] = center + min (a, b)
			Memr[bs+nsample] = center + max (a, b)
			nsample = nsample + 1
			if (nsample == NSAMPLE)
			    break
		    }
	        }
	    }
	}

	if (nsample == 0) {
	    Memr[as] = xmin
	    Memr[bs] = xmax
	    nsample = 1
	}

	if (bkg == B_MEDIAN)
	    call salloc (y, nfit, TY_REAL)
	else if (bkg == B_FIT) {
	    call salloc (x, nfit, TY_REAL)
	    call salloc (y, nfit, TY_REAL)
	    call salloc (w, nfit, TY_REAL)
	}

	# Initialize the image interpolator.
	call asiinit (asi, II_LINEAR)

	# Determine sky at each dispersion point.
	call aclrr (svar, ny)
	do iy = 1, ny {

	    # Fit image interpolation function including extra points
	    # and apply image boundary limits.

	    i = iy + ys - 1
	    s = ap_cveval (cv, real (i))
	    ix1 = max (c1, nint (xmin + s) - 1)
	    ix2 = min (c1+nc-1, nint (xmax + s) + 1)
	    nfit = ix2 - ix1 + 1
	    if (nfit < 3) {
		call aclrr (sbuf[1,iy], nx)
		svar[iy] = 0.
		next
	    }
	    data = dbuf + (i - l1) * nc + ix1 - c1
	    if (bkg == B_AVERAGE || bkg == B_FIT) {
		iferr (call asifit (asi, Memr[data], nfit)) {
		    call aclrr (sbuf[1,iy], nx)
		    svar[iy] = 0.
		    next
		}
	    }

	    # Determine background
	    switch (bkg) {
	    case B_AVERAGE:
		# The background is computed by integrating the interpolator 
	        avg = 0.
		nsky = 0
	        c = 0.
		for (i=0; i < nsample; i=i+1) {
		    a = max (real (ix1), Memr[as+i] + s) - ix1 + 1
		    b = min (real (ix2), Memr[bs+i] + s) - ix1 + 1
		    if (b - a > 0.) {
			avg = avg + asigrl (asi, a, b)
			c = c + b - a
			nsky = nsky + nint (b) - nint(a) + 1
		    }
	        }
	        if (c > 0.)
	            avg = avg / c
		call amovkr (avg, sbuf[1,iy], nx) 
		if (nsky > 1)
		    svar[iy] = max (0., (rdnoise + avg) / (nsky - 1))
	    case B_MEDIAN:
		# The background is computed by the median pixel
	        avg = 0.
		nsky = 0
		for (i=0; i < nsample; i=i+1) {
		    a = max (real (ix1), Memr[as+i] + s) - ix1 + 1
		    b = min (real (ix2), Memr[bs+i] + s) - ix1 + 1
		    do j = nint (a), nint (b) {
			Memr[y+nsky] = Memr[data+j-1]
			nsky = nsky + 1
		    }
	        }
		if (nsky > 0)
		    avg = amedr (Memr[y], nsky)
		call amovkr (avg, sbuf[1,iy], nx) 
		if (nsky > 1)
		    svar[iy] = max (0., (rdnoise + avg) / (nsky - 1))
	    case B_MINIMUM:
		# The background is computed by the minimum pixel
	        avg = MAX_REAL
		nsky = 0
		for (i=0; i < nsample; i=i+1) {
		    a = max (real (ix1), Memr[as+i] + s) - ix1 + 1
		    b = min (real (ix2), Memr[bs+i] + s) - ix1 + 1
		    do j = nint (a), nint (b) {
			avg = min (avg, Memr[data+j-1])
			nsky = nsky + 1
		    }
	        }
		if (nsky == 0)
		    avg = 0
		call amovkr (avg, sbuf[1,iy], nx) 
		if (nsky > 1)
		    svar[iy] = max (0., (rdnoise + avg) / (nsky - 1))
	    case B_FIT:
		# The fitting is done in a coordinate system relative to
		# aperture center.

		c = center + s
	        a = ix1 + c - int (c)
	        do i = 1, nfit-1 {
	            Memr[x+i-1] = nint (1000. * (a - c)) / 1000.
		    Memr[y+i-1] = asieval (asi, a-ix1+1)
		    Memr[w+i-1] = 1.
		    a = a + 1.
	        }

	        iferr {
	            call ic_fit (ic, cv1, Memr[x], Memr[y], Memr[w], nfit-1,
	                YES, YES, YES, YES)

		    avg = 0.
	            do i = 1, nx {
		        a = xs[iy] + i - 1
			b = ap_cveval (cv1, a - c)
			avg = avg + b
			sbuf[i,iy] = b
	            }
		    avg = avg / nx
	        } then {
		    avg = 0.
		    call aclrr (sbuf[1,iy], nx)
		}

		nsky = 0.
		for (i=0; i < nsample; i=i+1) {
		    a = max (real (ix1), Memr[as+i] + s) - ix1 + 1
		    b = min (real (ix2), Memr[bs+i] + s) - ix1 + 1
		    nsky = nsky + nint (b) - nint (a) + 1
		}
		if (nsky > 1)
		    svar[iy] = max (0., (rdnoise + avg) / (nsky - 1))
	    }
	}

	# Do box car smoothing if desired.
	if (skybox > 1) {
	    ix2 = skybox ** 2
	    avg = 0.
	    a = 0.
	    iy = 1
	    for (i=1; i<=skybox; i=i+1) {
		avg = avg + sbuf[1,i]
		a = a + svar[i]
	    }
	    for (; i<=ny; i=i+1) {
		b = sbuf[1,iy]
		c = svar[iy]
		sbuf[1,iy] = avg / skybox
		svar[iy] = a / ix2
		avg = avg + sbuf[1,i] - b
		a = a + svar[i] - c
		iy = iy + 1
	    }
	    sbuf[1,iy] = avg / skybox
	    svar[iy] = a / ix2
	    i = ny - skybox + 1
	    for (iy=ny; iy > ny-skybox/2; iy=iy-1)
		svar[iy] = svar[i]
	    for (; i > 1; i=i-1) {
		svar[iy] = svar[i]
		iy = iy - 1
	    }
	    for (; iy > 1; iy=iy-1)
		svar[iy] = svar[1]

	    switch (bkg) {
	    case B_AVERAGE, B_MEDIAN, B_MINIMUM:
		i = ny - skybox + 1
	        for (iy=ny; iy > ny-skybox/2; iy=iy-1)
		    call amovkr (sbuf[1,i], sbuf[1,iy], nx)
	        for (; i > 1; i=i-1) {
		    call amovkr (sbuf[1,i], sbuf[1,iy], nx)
		    iy = iy - 1
	        }
	        for (; iy > 1; iy=iy-1)
		    call amovkr (sbuf[1,1], sbuf[1,iy], nx)
	    case B_FIT:
	        i = ny - skybox + 1
	        for (iy=ny; iy > ny-skybox/2; iy=iy-1)
		    sbuf[1,iy] = sbuf[1,i]
	        for (; i > 1; i=i-1) {
		    sbuf[1,iy] = sbuf[1,i]
		    iy = iy - 1
	        }
	        for (; iy > 1; iy=iy-1)
		    sbuf[1,iy] = sbuf[1,1]
		do ix1 = 2, nx {
		    avg = 0.
		    iy = 1
		    for (i=1; i<=skybox; i=i+1)
			avg = avg + sbuf[ix1,i]
		    for (; i<=ny; i=i+1) {
			b = sbuf[ix1,iy]
			sbuf[ix1,iy] = avg / skybox
			avg = avg + sbuf[ix1,i] - b
			iy = iy + 1
		    }
		    sbuf[ix1,iy] = avg / skybox
		    i = ny - skybox + 1
		    for (iy=ny; iy > ny-skybox/2; iy=iy-1)
			sbuf[ix1,iy] = sbuf[ix1,i]
		    for (; i > 1; i=i-1) {
			sbuf[ix1,iy] = sbuf[ix1,i]
			iy = iy - 1
		    }
		    for (; iy > 1; iy=iy-1)
			sbuf[ix1,iy] = sbuf[ix1,1]
		}
	    }
	}

	# Compute the unweighted aperture sky spectrum.
	i = AP_AXIS(ap)
	a = AP_CEN(ap,i) + AP_LOW(ap,i)
	b = AP_CEN(ap,i) + AP_HIGH(ap,i)
	c = (b - a) / nsubaps

	do iy = 1, ny {
	    data = dbuf + (iy + ys - 1 - l1) * nc + xs[iy] - c1 - 1
	    s = ap_cveval (cv, real (iy + ys - 1)) - c1 + 1
	    do i = 1, nsubaps {
		xmin = max (0.5, a + (i - 1) * c + s) + c1 - xs[iy]
		xmax = min (nc + 0.49, a + i * c + s) + c1 - xs[iy]
		if (xmin >= xmax) {
		    sky[iy,i] = 0.
		    next
		}
		ix1 = nint (xmin)
		ix2 = nint (xmax)

		if (ix1 == ix2)
		    sky[iy,i] = (xmax - xmin) * sbuf[ix1,iy]
		else {
		    sky[iy,i] = (ix1 - xmin + 0.5) * sbuf[ix1,iy]
		    sky[iy,i] = sky[iy,i] + (xmax - ix2 + 0.5) * sbuf[ix2,iy]
		}
		do ix = ix1+1, ix2-1
		    sky[iy,i] = sky[iy,i] + sbuf[ix,iy]
	    }
	}

	if (bkg == B_FIT)
	    call cvfree (cv1)
	call asifree (asi)
	call sfree (sp)
end
