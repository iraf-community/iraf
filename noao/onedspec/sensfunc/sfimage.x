include	<gset.h>
include	<mach.h>
include	<imhdr.h>
include	<imset.h>
include	<math/curfit.h>
include	"sensfunc.h"
include	"../idsmtn.h"


# SF_IMAGE -- Graph fluxed image data and possible standard flux points.
# For efficiency the IMIO pointer, buffer, and associated data are kept
# since a redraw is a common occurence and generating the data is slow.

procedure sf_image (gp, wc, stds, nstds, cv, wextn, extn, nextn, ecv)

pointer	gp			# Graphics structure
int	wc			# WC of graph
pointer	stds[nstds]		# Standard star data for flux points
int	nstds			# Number of standard stars
pointer	cv			# Sensitivity function curve
real	wextn[nextn]		# Extinction table wavelengths
real	extn[nextn]		# Extinction table values
int	nextn			# Number of extinction table values
pointer	ecv			# Residual extinction curve

int	scale[SF_NGRAPHS], npts[SF_NGRAPHS]
real	wstart[SF_NGRAPHS], wend[SF_NGRAPHS]
pointer	bufs[SF_NGRAPHS]

int	i, j, n, err
real	airmass, exposure, w, dw, ext, sens, smin, smax, latitude, xmin, xmax
pointer	im, skyim, buf, gio, sp, str, ids, x, y, z
pointer	immap(), imgl1r()
real	cveval(), clgetr(), cvstatr()
bool	streq(), strne()
errchk	immap

define	plot_	99

begin
	# Return if no image name.
	if (Memc[GP_IMAGES(gp,wc)] == EOS)
	    return

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the spectrum and sky subtract if necessary.
	im = GP_IMIO(gp,wc)
	buf = bufs[wc]
	if (im != NULL) {
	    call imstats (im, IM_IMAGENAME, Memc[str], SZ_LINE)
	    if (streq (Memc[GP_IMAGES(gp,wc)], Memc[str]))
		goto plot_
	    call imunmap (im)
	}

	im = immap (Memc[GP_IMAGES(gp,wc)], READ_ONLY, 0)
	if (IM_NDIM(im) > 1) {
	    call imunmap (im)
	    call error (0, "Graphs of two dimensional formats not supported")
	}
	buf = imgl1r (im)
	if (Memc[GP_SKYS(gp,wc)] != EOS) {
	    skyim = immap (Memc[GP_SKYS(gp,wc)], READ_ONLY, 0)
	    call asubr (Memr[buf], Memr[imgl1r(skyim)], Memr[buf], IM_LEN(im,1))
	    call imunmap (skyim)
	}

	# Save the IMIO data for future redraw.
	GP_IMIO(gp,wc) = im
	npts[wc] = IM_LEN(im,1)
	bufs[wc] = buf

	# Get the header data to determine airmass, wavelength scale, etc.
	# Check the image is dispersion corrected.

	call salloc (ids, LEN_IDS, TY_STRUCT)
	call salloc (POINT(ids), MAX_NCOEFF, TY_REAL)
	call load_ids_hdr (ids, im, 1)
	if (DC_FLAG(ids) != 0) {
	    call imunmap (im)
	    call sfree (sp)
	    call printf ("-%s must be linearly dispersion corrected-")
		call pargstr (Memc[GP_IMAGES(gp,wc)])
	    return
	}
	if (IS_INDEF (ITM(ids)))
	    exposure = 1.
	else
	    exposure = ITM(ids)
	if (IS_INDEF (AIRMASS(ids))) {
	    latitude = clgetr ("latitude")
	    call get_airm (RA(ids), DEC(ids), HA(ids), ST(ids), latitude,
		AIRMASS(ids))
	}
	airmass = AIRMASS(ids)
	dw = WPC(ids)
	wstart[wc] = W0(ids)
	wend[wc] = wstart[wc] + (npts[wc] - 1) * dw

	# Apply extinction correction if needed.
	if (EX_FLAG(ids) < 0) {
	    if (ecv != NULL) {
	        xmin = cvstatr (ecv, CVXMIN)
	        xmax = cvstatr (ecv, CVXMAX)
	    }
	    do i = 1, npts[wc] {
	        w = wstart[wc] + (i - 1) * dw 
	        call intrp (1, wextn, extn, nextn, w, ext, err)
	        if (ecv != NULL)
		    ext = ext + cveval (ecv, min (xmax, max (w, xmin)))
	        Memr[buf+i-1] = Memr[buf+i-1] * 10. ** (0.4 * airmass * ext)
	    }
	} else {
	    call printf ("-%s already extinction corrected-")
	       call pargstr (Memc[GP_IMAGES(gp,wc)])
	}

	# Apply flux calibration if needed.
	if (CA_FLAG(ids) < 0) {
	    do i = 1, npts[wc] {
	        w = wstart[wc] + (i - 1) * dw 
	        sens = cveval (cv, w)
	        Memr[buf+i-1] = Memr[buf+i-1] / exposure / dw / 10.**(0.4*sens)
	    }
	} else {
	    call printf ("-%s already flux calibrated-")
		call pargstr (Memc[GP_IMAGES(gp,wc)])
	}

	call alimr (Memr[buf], npts[wc], smin, smax)
	if (smin > 0.) {
	    scale[wc] = 0
	    do i = 1, npts[wc]
		Memr[buf+i-1] = log10 (Memr[buf+i-1])
	} else if (smax > 0.) {
	    scale[wc] =  -log10 (smax)
	    w = 10. ** scale[wc]
	    call amulkr (Memr[buf], w, Memr[buf], npts[wc])
	} else
	    scale[wc] = 1

#	scale[wc] = 1 - log10 (smin)
#	w = 10. ** scale[wc]
#	call amulkr (Memr[buf], w, Memr[buf], npts[wc])

plot_
	# Plot scaled graph.
	if (scale[wc] == 0) { 
	    call sprintf (Memc[str], SZ_LINE, "%s: Log Flux")
	        call pargstr (Memc[GP_IMAGES(gp,wc)])
	} else {
	    call sprintf (Memc[str], SZ_LINE, "%s: Flux x 10E%d")
	        call pargstr (Memc[GP_IMAGES(gp,wc)])
	        call pargi (scale[wc])
	    w = 10. ** scale[wc]
	}
#	call sprintf (Memc[str], SZ_LINE, "%s: Flux x 10E%d")
#	    call pargstr (Memc[GP_IMAGES(gp,wc)])
#	    call pargi (scale[wc])
	gio = GP_GIO(gp)
	call gswind (gio, wstart[wc], wend[wc], INDEF, INDEF)
	call gascale (gio, Memr[buf], npts[wc], 2)
	call glabax (gio, Memc[str], "", "")
	call gvline (gio, Memr[buf], npts[wc], wstart[wc], wend[wc])

	call sfree (sp)

	# Check if image is one of the standard stars and plot flux points.
	do i = 1, nstds {
	    if (strne (Memc[GP_IMAGES(gp,wc)], STD_IMAGE(stds[i])))
		next
	    n = STD_NWAVES(stds[i])
	    x = STD_WAVES(stds[i])
	    y = STD_FLUXES(stds[i])
	    z = STD_DWAVES(stds[i])
	    if (scale[wc] == 0) {
	        do j = 0, n-1
		    call gmark (gio, Memr[x+j], log10 (Memr[y+j]), GM_HEBAR,
			-Memr[z+j], 1.)
	    } else {
	        do j = 0, n-1
		    call gmark (gio, Memr[x+j], w * Memr[y+j], GM_HEBAR,
			-Memr[z+j], 1.)
	    }
	}
end
