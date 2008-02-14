# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<imhdr.h>
include	<gset.h>
include	<math.h>
include	<math/gsurfit.h>
include	<math/nlfit.h>
include	"imexam.h"

define	FITTYPES	"|gaussian|moffat|"
define	FITGAUSS	1
define	FITMOFFAT	2


# IE_QRIMEXAM -- Radial profile plot and photometry parameters.
# If no GIO pointer is given then only the photometry parameters are printed.
# First find the center using the marginal distributions.  Then subtract
# a fit to the background.  Compute the moments within the aperture and
# fit a gaussian of fixed center and zero background.  Make the plot
# and print the photometry values.

procedure ie_qrimexam (gp, mode, ie, x, y)

pointer	gp
pointer	ie
int	mode
real	x, y

bool	center, background, medsky, fitplot, clgpsetb()
real	radius, buffer, width, magzero, rplot, beta, clgpsetr()
int	fittype, xorder, yorder, clgpseti(), strdic()

int	i, j, ns, no, np, nx, ny, npts, x1, x2, y1, y2
int	plist[3], nplist
real	bkg, xcntr, ycntr, mag, e, pa, zcntr, wxcntr, wycntr
real	params[3]
real	fwhm, dfwhm
pointer	sp, fittypes, title, coords, im, data, pp, ws, xs, ys, zs, gs, ptr, nl
double	sumo, sums, sumxx, sumyy, sumxy
real	r, r1, r2, r3, dx, dy, gseval(), amedr()
pointer	clopset(), ie_gimage(), ie_gdata(), locpr()
extern	ie_gauss(), ie_dgauss(), ie_moffat(), ie_dmoffat()
errchk	nlinit, nlfit

string	glabel "#\
   COL    LINE    RMAG     FLUX      SKY   N  RMOM ELLIP    PA     PEAK GFWHM\n"
string	mlabel "#\
   COL    LINE    RMAG     FLUX      SKY   N  RMOM ELLIP    PA     PEAK MFWHM\n"

begin
	call smark (sp)
	call salloc (fittypes, SZ_FNAME, TY_CHAR)
	call salloc (title, IE_SZTITLE, TY_CHAR)
	call salloc (coords, IE_SZTITLE, TY_CHAR)

	iferr (im = ie_gimage (ie, NO)) {
	    call erract (EA_WARN)
	    call sfree (sp)
	    return
	}

	# Open parameter set.
	if (gp != NULL) {
	    if (IE_PP(ie) != NULL)
		call clcpset (IE_PP(ie))
	}
	pp = clopset ("rimexam")

	center = clgpsetb (pp, "center")
	background = clgpsetb (pp, "background")
	radius = clgpsetr (pp, "radius")
	buffer = clgpsetr (pp, "buffer")
	width = clgpsetr (pp, "width")
	xorder = clgpseti (pp, "xorder")
	yorder = clgpseti (pp, "yorder")
	medsky = (xorder <= 0 || yorder <= 0)

	magzero = clgpsetr (pp, "magzero")
	rplot = clgpsetr (pp, "rplot")
	fitplot = clgpsetb (pp, "fitplot")
	call clgpseta (pp, "fittype", Memc[fittypes], SZ_FNAME)
	fittype = strdic (Memc[fittypes], Memc[fittypes], SZ_FNAME, FITTYPES)
	if (fittype == 0) {
	    call eprintf ("WARNING: Unknown profile fit type `%s'.\n")
		call pargstr (Memc[fittypes])
	    call sfree (sp)
	    return
	}
	beta = clgpsetr (pp, "beta")

	# If the initial center is INDEF then use the previous value.
	if (gp != NULL) {
	    if (!IS_INDEF(x))
	        IE_X1(ie) = x
	    if (!IS_INDEF(y))
	        IE_Y1(ie) = y

	    xcntr = IE_X1(ie)
	    ycntr = IE_Y1(ie)
	} else {
	    xcntr = x
	    ycntr = y
	}

	# Center
	if (center)
	    iferr (call ie_center (im, radius, xcntr, ycntr)) {
		call erract (EA_WARN)
		return
	    }

	# Crude estimage of FHWM.
	dfwhm = radius

	# Get data including a buffer and background annulus.
	if (!background) {
	    buffer = 0.
	    width = 0.
	}
	r = max (rplot, radius + buffer + width)
	x1 = xcntr - r
	x2 = xcntr + r
	y1 = ycntr - r
	y2 = ycntr + r
	iferr (data = ie_gdata (im, x1, x2, y1, y2)) {
	    call erract (EA_WARN)
	    call sfree (sp)
	    return
	}

	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	npts = nx * ny

	call salloc (xs, npts, TY_REAL)
	call salloc (ys, npts, TY_REAL)
	call salloc (ws, npts, TY_REAL)

	# Extract the background data if background subtracting.
	ns = 0
	if (background && width > 0.) {
	    call salloc (zs, npts, TY_REAL)

	    r1 = radius ** 2
	    r2 = (radius + buffer) ** 2
	    r3 = (radius + buffer + width) ** 2

	    ptr = data
	    do j = y1, y2 {
	        dy = (ycntr - j) ** 2
	        do i = x1, x2 {
		    r = (xcntr - i) ** 2 + dy
		    if (r <= r1)
		        ;
		    else if (r >= r2 && r <= r3) {
		        Memr[xs+ns] = i
		        Memr[ys+ns] = j
		        Memr[zs+ns] = Memr[ptr]
		        ns = ns + 1
		    }
		    ptr = ptr + 1
	        }
	    }
	}
 
	# Accumulate the various sums for the moments and the gaussian fit.
	no = 0
	np = 0
	zcntr = 0.
	sumo = 0.; sums = 0.; sumxx = 0.; sumyy = 0.; sumxy = 0.
	ptr = data
	gs = NULL

	if (ns > 0) {		# Background subtraction

	    # If background points are defined fit a surface and subtract
	    # the fitted background from within the object aperture. 

	    if (medsky)
		bkg = amedr (Memr[zs], ns)
	    else {
		repeat {
		    call gsinit (gs, GS_POLYNOMIAL, xorder, yorder, YES,
			real (x1), real (x2), real (y1), real (y2))
		    call gsfit (gs, Memr[xs], Memr[ys], Memr[zs], Memr[ws], ns,
			WTS_UNIFORM, i)
		    if (i == OK)
			break
		    xorder = max (1, xorder - 1)
		    yorder = max (1, yorder - 1)
		    call gsfree (gs)
		}
		bkg = gseval (gs, real(x1), real(y1))
	    }

	    do j = y1, y2 {
	        dy = j - ycntr
	        do i = x1, x2 {
		    dx = i - xcntr
		    r = sqrt (dx ** 2 + dy ** 2)
		    r3 = max (0., min (5., 2 * r / dfwhm - 1.))

		    if (medsky)
			r2 = bkg
		    else {
			r2 = gseval (gs, real(i), real(j))
			bkg = min (bkg, r2)
		    }
		    r1 = Memr[ptr] - r2

		    if (r <= radius) {
			sumo = sumo + r1
			sums = sums + r2
			sumxx = sumxx + dx * dx * r1
			sumyy = sumyy + dy * dy * r1
			sumxy = sumxy + dx * dy * r1
			zcntr = max (r1, zcntr)
			if (r <= rplot) {
			    Memr[xs+no] = r
			    Memr[ys+no] = r1
			    Memr[ws+no] = exp (-r3**2) / max (.1, r**2)
			    no = no + 1
			} else {
			    np = np + 1
			    Memr[xs+npts-np] = r
			    Memr[ys+npts-np] = r1
			    Memr[ws+npts-np] = exp (-r3**2) / max (.1, r**2)
			}
		    } else if (r <= rplot) {
		        np = np + 1
		        Memr[xs+npts-np] = r
		        Memr[ys+npts-np] = r1
		    }
		    ptr = ptr + 1
		}
	    }

	    if (gs != NULL)
	        call gsfree (gs)

	} else {		# No background subtraction
	    bkg = 0.
	    do j = y1, y2 {
	        dy = j - ycntr
	        do i = x1, x2 {
		    dx = i - xcntr
		    r = sqrt (dx ** 2 + dy ** 2)
		    r3 = max (0., min (5., 2 * r / dfwhm - 1.))
		    r1 = Memr[ptr]

		    if (r <= radius) {
			sumo = sumo + r1
			sumxx = sumxx + dx * dx * r1
			sumyy = sumyy + dy * dy * r1
			sumxy = sumxy + dx * dy * r1
			zcntr = max (r1, zcntr)
			if (r <= rplot) {
			    Memr[xs+no] = r
			    Memr[ys+no] = r1
			    Memr[ws+no] = exp (-r3**2) / max (.1, r**2)
			    no = no + 1
			} else {
			    np = np + 1
			    Memr[xs+npts-np] = r
			    Memr[ys+npts-np] = r1
			    Memr[ws+npts-np] = exp (-r3**2) / max (.1, r**2)
			}
		    } else if (r <= rplot) {
		        np = np + 1
		        Memr[xs+npts-np] = r
		        Memr[ys+npts-np] = r1
		    }
		    ptr = ptr + 1
		}
	    }
	}
	if (np > 0) {
	    call amovr (Memr[xs+npts-np], Memr[xs+no], np)
	    call amovr (Memr[ys+npts-np], Memr[ys+no], np)
	    call amovr (Memr[ws+npts-np], Memr[ws+no], np)
	}
	if (rplot <= radius) {
	    no = no + np
	    np = no - np 
	} else
	    np = no + np
	    

	# Compute the photometry and gaussian fit parameters.

	switch (fittype) {
	case FITGAUSS:
	    plist[1] = 1
	    plist[2] = 2
	    nplist = 2
	    params[2] = dfwhm**2 / (8 * log(2.))
	    params[1] = zcntr
	    call nlinitr (nl, locpr (ie_gauss), locpr (ie_dgauss), 
		params, params, 2, plist, nplist, .001, 100)
	    call nlfitr (nl, Memr[xs], Memr[ys], Memr[ws], no, 1, WTS_USER, i)
	    if (i == SINGULAR || i == NO_DEG_FREEDOM) {
		call eprintf ("WARNING: Gaussian fit did not converge\n")
		call tsleep (5)
		zcntr = INDEF
		fwhm = INDEF
	    } else {
		call nlpgetr (nl, params, i)
		if (params[2] < 0.) {
		    zcntr = INDEF
		    fwhm = INDEF
		} else {
		    zcntr = params[1]
		    fwhm = sqrt (8 * log (2.) * params[2])
		}
	    }
	case FITMOFFAT:
	    plist[1] = 1
	    plist[2] = 2
	    if (IS_INDEF(beta)) {
		params[3] = -3.0
		plist[3] = 3
		nplist = 3
	    } else {
		params[3] = -beta
		nplist = 2
	    }
	    params[2] = dfwhm / 2. / sqrt (2.**(-1./params[3]) - 1.)
	    params[1] = zcntr
	    call nlinitr (nl, locpr (ie_moffat), locpr (ie_dmoffat), 
		params, params, 3, plist, nplist, .001, 100)
	    call nlfitr (nl, Memr[xs], Memr[ys], Memr[ws], no, 1, WTS_USER, i)
	    if (i == SINGULAR || i == NO_DEG_FREEDOM) {
		call eprintf ("WARNING: Moffat fit did not converge\n")
		call tsleep (5)
		zcntr = INDEF
		fwhm = INDEF
		beta = INDEF
	    } else {
		call nlpgetr (nl, params, i)
		if (params[2] < 0.) {
		    zcntr = INDEF
		    fwhm = INDEF
		    beta = INDEF
		} else {
		    zcntr = params[1]
		    beta = -params[3]
		    fwhm = abs (params[2])*2.*sqrt (2.**(-1./params[3]) - 1.)
		}
	    }
	}

	mag = INDEF
	r = INDEF
	e = INDEF
	pa = INDEF
	if (sumo > 0.) {
	    mag = magzero - 2.5 * log10 (sumo)
	    r2 = sumxx + sumyy
	    if (r2 > 0.) {
		switch (fittype) {
		case FITGAUSS:
		    r = 2 * sqrt (log (2.) * r2 / sumo)
		case FITMOFFAT:
		    if (beta > 2.)
			r = 2 * sqrt ((beta-2.)*(2.**(1./beta)-1) * r2 / sumo)
		}
	        r1 =(sumxx-sumyy)**2+(2*sumxy)**2
		if (r1 > 0.)
		    e = sqrt (r1) / r2
	        else
		    e = 0.
	    }
	    if (e < 0.01)
		e = 0.
	    else
		pa = RADTODEG (0.5 * atan2 (2*sumxy, sumxx-sumyy))
	}

	call ie_mwctran (ie, xcntr, ycntr, wxcntr, wycntr)
	if (xcntr == wxcntr && ycntr == wycntr)
	    call strcpy ("%.2f %.2f", Memc[title], IE_SZTITLE)
	else {
	    call sprintf (Memc[title], IE_SZTITLE, "%s %s")
		if (IE_XFORMAT(ie) == '%')
		    call pargstr (IE_XFORMAT(ie))
		else
		    call pargstr ("%g")
		if (IE_YFORMAT(ie) == '%')
		    call pargstr (IE_YFORMAT(ie))
		else
		    call pargstr ("%g")
	}
	call sprintf (Memc[coords], IE_SZTITLE, Memc[title])
	    call pargr (wxcntr)
	    call pargr (wycntr)

	# Plot the radial profile and overplot the fit.
	if (gp != NULL) {
	    call sprintf (Memc[title], IE_SZTITLE,
		"%s: Radial profile at %s\n%s")
	        call pargstr (IE_IMNAME(ie))
	        call pargstr (Memc[coords])
	        call pargstr (IM_TITLE(im))

	    call ie_graph (gp, mode, pp, Memc[title], Memr[xs], Memr[ys],
		np, "", "")

	    if (fitplot && !IS_INDEF (fwhm)) {
		np = 51
		dx = rplot / (np - 1)
		do i = 0, np - 1
		    Memr[xs+i] = i * dx
		call nlvectorr (nl, Memr[xs], Memr[ys], np, 1)
		call gseti (gp, G_PLTYPE, 2)
		call gpline (gp, Memr[xs], Memr[ys], np)
		call gseti (gp, G_PLTYPE, 1)
	    }
	}

	if (IE_LASTKEY(ie) != ',') {
	    switch (fittype) {
	    case FITGAUSS:
	        call printf (glabel)
	    case FITMOFFAT:
	        call printf (mlabel)
	    }
	}

	# Print the photometry values.
	call printf (
	    "%7.2f %7.2f %7.2f %8.1f %8.2f %3d %5.2f %5.3f %5.1f %8.2f %5.2f\n")
	    call pargr (xcntr)
	    call pargr (ycntr)
	    call pargr (mag)
	    call pargd (sumo)
	    call pargd (sums / no)
	    call pargi (no)
	    call pargr (r)
	    call pargr (e)
	    call pargr (pa)
	    call pargr (zcntr)
	    call pargr (fwhm)
	if (gp == NULL) {
	    if (xcntr != wxcntr || ycntr != wycntr) {
		call printf ("%s: %s\n")
		    call pargstr (IE_WCSNAME(ie))
		    call pargstr (Memc[coords])
	    }
	}

	if (IE_LOGFD(ie) != NULL) {
	    if (IE_LASTKEY(ie) != ',') {
		switch (fittype) {
		case FITGAUSS:
		    call fprintf (IE_LOGFD(ie), glabel)
		case FITMOFFAT:
		    call fprintf (IE_LOGFD(ie), mlabel)
		}
	    }

	    call fprintf (IE_LOGFD(ie),
	    "%7.2f %7.2f %7.2f %8.1f %8.2f %3d %5.2f %5.3f %5.1f %8.2f %5.2f\n")
	        call pargr (xcntr)
	        call pargr (ycntr)
	        call pargr (mag)
	        call pargd (sumo)
	        call pargd (sums / no)
	        call pargi (no)
	        call pargr (r)
	        call pargr (e)
	        call pargr (pa)
	        call pargr (zcntr)
	        call pargr (fwhm)
	    if (xcntr != wxcntr || ycntr != wycntr) {
		call fprintf (IE_LOGFD(ie), "%s: %s\n")
		    call pargstr (IE_WCSNAME(ie))
		    call pargstr (Memc[coords])
	    }
	}

	if (gp == NULL)
	   call clcpset (pp)
	else
	    IE_PP(ie) = pp

	call nlfreer (nl)
	call sfree (sp)
end
