include	<error.h>
include	<mach.h>
include	<gset.h>
include "rvflags.h"
include "rvpackage.h"

define	SQ2PI	2.5066283


# RV_LINEFIT - Fit a gaussian to a line between two specified endpoints.

procedure rv_linefit  (rv, x1, x2, stdlam, which)

pointer	rv					#I RV struct pointer
real	x1					#I left edge of fit
real	x2					#I right edge of fit
real	stdlam					#I standard wavelength
int	which					#I which spectrum to fit

int	ix1, ix2

begin
        if (which == OBJECT_SPECTRUM) {          # Fit at the top 
	    if (RV_DCFLAG(rv) != -1) {
           	x1 = (log10(x1) - RV_OW0(rv)) / RV_OWPC(rv) + 1
            	x2 = (log10(x2) - RV_OW0(rv)) / RV_OWPC(rv) + 1
	    }
	    ix1 = int (x1 + 0.5)		# round off
	    ix2 = int (x2 + 0.5)
	    if (OBJCONT(rv) == NO) {
	        call rv_gfit (rv, RV_GP(rv), RV_OW0(rv), RV_OWPC(rv), 
		    ix1, ix2, OBJPIXX(rv,1), OBJPIXY(rv,1), 
		    RV_NPTS(rv), stdlam, TOP)
	    } else {
	        call rv_gfit (rv, RV_GP(rv), RV_OW0(rv), RV_OWPC(rv), 
		    ix1, ix2, OBJPIXX(rv,1), OCONT_DATA(rv,1), 
		    RV_NPTS(rv), stdlam, TOP)
	    }

	} else {
            if (RV_DCFLAG(rv) != -1) {
                x1 = (log10(x1) - RV_RW0(rv)) / RV_RWPC(rv) + 1
                x2 = (log10(x2) - RV_RW0(rv)) / RV_RWPC(rv) + 1
	    }
	    ix1 = int (x1 + 0.5)		# round off
	    ix2 = int (x2 + 0.5)
	    if (REFCONT(rv) == NO) {
	        call rv_gfit (rv, RV_GP(rv), RV_RW0(rv), RV_RWPC(rv), 
		    ix1, ix2, REFPIXX(rv,1), REFPIXY(rv,1), 
		    RV_RNPTS(rv), stdlam, BOTTOM)
	    } else {
	        call rv_gfit (rv, RV_GP(rv), RV_RW0(rv), RV_RWPC(rv), 
		    ix1, ix2, REFPIXX(rv,1), RCONT_DATA(rv,1), 
		    RV_RNPTS(rv), stdlam, BOTTOM)
	    }
	}
end


# RV_GFIT -- Fit a Gaussian to a spectral line and output it's velocity
# based on a standard wavelength.

procedure rv_gfit (rv, gp, w0, wpc, left, right, pixx, pixy, ndata, lam, 
    where)

pointer	rv					#I RV task structure
pointer	gp					#I GIO pointer
real	w0, wpc					#I Dispsersion params
int	left, right				#I Fitting region endpoints
real	pixx[ARB]				#I Spectrum data
real	pixy[ARB]				#I Spectrum data
int	ndata					#I Number of points
real	lam					#I standard line wavelength
int	where					#I where to plot the line

int	i, j, npts, nlines
real	w, wyc, wx, wy, wx2, wy2, wx1, wy1, a[5]
real	x1, x2, y1, y2, range, peakx, vel, shift
real	slope, height, flux, cont, sigma, eqw, scale, chisq
bool	fit
pointer	sp, x, y, z

double	dex(), rv_shift2vel()
real	model(), rv_maxpix(), rv_minpix()
bool	fp_equalr()
errchk	dofit

define	done_	99

begin
	# Determine number of points to fit.
	npts = right - left + 1
	if (npts < 3) {
	    call eprintf ("At least 3 points are required\n")
	    return
	}

	# Allocate space for the points to be fit.
	call smark (sp)
	call salloc (x, npts, TY_REAL)
	call salloc (y, npts, TY_REAL)
	call salloc (z, npts, TY_REAL)

	# Scale the data.
	scale = 0.
	do i = 1, npts {
	    Memr[x+i-1] = dex (pixx[left+i-1])
	    Memr[y+i-1] = pixy[left+i-1]
	    scale = max (scale, abs (Memr[y+i-1]))
	}
	if (fp_equalr(scale,0.0))
	    scale = 1.0
	call adivkr (Memr[y], scale, Memr[y], npts)

	# Setup initial estimates.
	wx1 = dex (pixx[left])
	wx2 = dex (pixx[right])
	wy1 = pixy[left]
	wy2 = pixy[right]
	slope = (wy2-wy1) / (wx2-wx1) / scale
	wyc = wy1 / scale - slope * wx1
	wx = 0
	do i = 0, npts-1 {
	    w = Memr[x+i]
	    wy = Memr[y+i] - wyc - slope * w
	    if (abs (wy) > wx) {
		wx = abs (wy)
		j = i
		peakx = w
	    }
	}

	w = Memr[x+j-1]
	wy = min (0.99, max (0.01, abs (Memr[y+j-1] - wyc - slope * w) / wx))
	sigma = sqrt (-0.5 * (w-peakx)**2 / log (wy))
	w = Memr[x+j+1]
	wy = min (0.99, max (0.01, abs (Memr[y+j+1] - wyc - slope * w) / wx))
	sigma = sigma + sqrt (-0.5 * (w-peakx)**2 / log (wy))

	# Do fit.
	a[1] = w				# initial shift
	#a[2] = 0.25 * abs (Memr[x+npts-1] - Memr[x])
	a[2] = sigma / 2
	call pixind (w0, wpc, log10(w), i)
	a[3] = (pixy[i] - (wyc + slope * (w - pixy[left]))) / scale
	a[4] = 0.
	a[5] = 1.
	nlines = 1
	iferr {
	    call dofit ('a', Memr[x], Memr[y], npts, a, nlines, chisq)
	    call dofit ('b', Memr[x], Memr[y], npts, a, nlines, chisq)
	} then {
	    call erract (EA_WARN)
	    fit = false
	    goto done_
	}
	a[3] = a[3] * scale
	wyc = (wyc + slope * wx1) * scale
	slope = slope * scale

	# Compute model spectrum with continuum and plot.
	fit = true
	do i = 1, npts {
	    w = Memr[x+i-1]
	    Memr[z+i-1] = model (w, a, 5) + wyc + slope * (w - wx1)
	}

        y2 = rv_maxpix (pixy, ndata)
        y1 = rv_minpix (pixy, ndata)
	range = abs (y2 - y1)
        y2 = y2 + (.15 * range)
        y1 = y1 - (.12 * range)
	x1 = dex (pixx[1])
	x2 = dex (pixx[ndata])
        call gswind (gp, x1, x2, y1, y2)
	if (where == TOP)
            call gsview (gp, 0.115, 0.95, 0.51, 0.865)
	else
            call gsview (gp, 0.115, 0.95, 0.125, 0.50)
	call gseti (gp, G_PLTYPE, GL_DASHED)
	call gseti (gp, G_PLCOLOR, RV_LINECOLOR(rv))
	call gpline (gp, Memr[x], Memr[z], npts)
	call gseti (gp, G_PLTYPE, GL_DOTTED)
	call gseti (gp, G_PLCOLOR, C_GREEN)
	call gline (gp, wx1, wyc, wx2, wyc + slope * (wx2 - wx1))
	call gseti (gp, G_PLTYPE, GL_SOLID)
	call gseti (gp, G_PLCOLOR, C_FOREGROUND)
	call gflush (gp)

done_
	# Log computed values
	if (fit) {
	    w = a[1]
	    cont = wyc + slope * (w - wx1)
	    height = a[3]
	    sigma = a[2]
	    flux = sigma * height * SQ2PI
	    shift = (log10(w) - log10(lam)) / wpc
	    vel = real (rv_shift2vel (rv, shift))
	    if (cont > 0.)
		eqw = -flux / cont
	    else
		eqw = INDEF

	    call printf (
	        "center = %8.6g, vel = %8.4g, eqw = %6.4g, fwhm = %6.4g")
	        call pargr (w)
	        call pargr (vel)
	        call pargr (eqw)
	        call pargr (2.355 * sigma)
	}

	call sfree (sp)
end
