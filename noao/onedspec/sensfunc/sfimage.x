include	<gset.h>
include	<math/curfit.h>
include	"sensfunc.h"
include	<smw.h>


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

int	scale[SF_NGRAPHS], log[SF_NGRAPHS]

bool	newobs, obshead
int	i, j, n, err
real	a, t, w, dw, e, sens, latitude, smin, smax, xmin, xmax
pointer	im, mw, sh, skyim, skymw, skysh, std, gio, sp, str, x, y, z, obs
pointer	immap(), smw_openim()
real	cveval(), obsgetr(), cvstatr()
double	shdr_lw()
bool	streq(), strne()
errchk	immap, smw_openim, obsimopen

define	plot_	99

begin
	# Return if no image name.
	if (Memc[GP_IMAGES(gp,wc)] == EOS)
	    return

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the spectrum and sky subtract if necessary.
	sh = GP_SHDR(gp,wc)
	if (sh != NULL) {
	    if (streq (Memc[GP_IMAGES(gp,wc)], IMNAME(sh))) {
		if (GP_LOG(gp) == log[wc])
		    goto plot_
		else
		    call shdr_close (sh)
	    }
	}

	# Determine a valid standard star to get aperture number.
	do i = 1, nstds
	    if (STD_FLAG(stds[i]) != SF_EXCLUDE) {
		std = stds[i]
		break
	    }

	im = immap (Memc[GP_IMAGES(gp,wc)], READ_ONLY, 0)
	mw = smw_openim (im)
	call shdr_open (im, mw, 1, 1, STD_BEAM(std), SHDATA, sh)

	# Check for dispersion correction
	if (DC(sh) == DCNO) {
	    call shdr_close (sh)
	    call smw_close (mw)
	    call imunmap (im)
	    GP_SHDR(gp,wc) = NULL
	    call sfree (sp)
	    call printf ("-%s must be dispersion corrected-")
		call pargstr (Memc[GP_IMAGES(gp,wc)])
	    return
	}

	# Sky subtract if necessary
	if (Memc[GP_SKYS(gp,wc)] != EOS) {
	    skyim = immap (Memc[GP_SKYS(gp,wc)], READ_ONLY, 0)
	    skymw = smw_openim (skyim)
	    call shdr_open (skyim, skymw, 1, 1, STD_BEAM(std), SHDATA, skysh)
	    call shdr_rebin (skysh, sh)
	    call asubr (Memr[SY(sh)], Memr[SY(skysh)], Memr[SY(sh)], SN(sh))
	    call shdr_close (skysh)
	    call smw_close (skymw)
	    call imunmap (skyim)
	}

	# Set airmass and exposure time
	if (IS_INDEF (AM(sh))) {
	    obs = NULL
	    call clgstr ("observatory", Memc[str], SZ_LINE)
	    call obsimopen (obs, im, Memc[str], NO, newobs, obshead)
	    latitude = obsgetr (obs, "latitude")
	    call obsclose (obs)
	    call get_airm (RA(sh), DEC(sh), HA(sh), ST(sh), latitude,
		AM(sh))
	}
	a = AM(sh)
	if (IS_INDEF (IT(sh)))
	    t = 1.
	else
	    t = IT(sh)

	# Apply extinction correction if needed
	if (EC(sh) == ECNO) {
	    if (ecv != NULL) {
	        xmin = cvstatr (ecv, CVXMIN)
	        xmax = cvstatr (ecv, CVXMAX)
	    }
	    do i = 1, SN(sh) {
		w = Memr[SX(sh)+i-1]
	        call intrp (1, wextn, extn, nextn, w, e, err)
	        if (ecv != NULL)
		    e = e + cveval (ecv, min (xmax, max (w, xmin)))
	        Memr[SY(sh)+i-1] = Memr[SY(sh)+i-1] * 10. ** (0.4 * a * e)
	    }
	} else {
	    call printf ("-%s already extinction corrected-")
	       call pargstr (Memc[GP_IMAGES(gp,wc)])
	}

	# Apply flux calibration if needed
	if (FC(sh) == FCNO) {
	    do i = 1, SN(sh) {
	        w = Memr[SX(sh)+i-1]
		dw = abs (shdr_lw (sh, double (i+0.5)) -
		    shdr_lw (sh, double (i-0.5)))
	        sens = cveval (cv, w)
	        Memr[SY(sh)+i-1] = Memr[SY(sh)+i-1] / t / dw / 10.**(0.4*sens)
	    }
	} else {
	    call printf ("-%s already flux calibrated-")
		call pargstr (Memc[GP_IMAGES(gp,wc)])
	}

	# Set flux scaling
	call alimr (Memr[SY(sh)], SN(sh), smin, smax)
	if (smax < 0.)
	    scale[wc] = 0.
	else if (GP_LOG(gp) == NO) {
	    scale[wc] = -log10 (smax) + 1
	    w = 10. ** scale[wc]
	    call amulkr (Memr[SY(sh)], w, Memr[SY(sh)], SN(sh))
	} else {
	    scale[wc] = INDEFI
	    smin = smax / 1000.
	    w = smax
	    y = SY(sh)
	    do i = 1, SN(sh) {
		if (Memr[y] > smin)
		    w = min (w, Memr[y])
		y = y + 1
	    }
	    y = SY(sh)
	    do i = 1, SN(sh) {
		Memr[y] = log10 (max (Memr[y], w))
		y = y + 1
	    }
	}
	log[wc] = GP_LOG(gp)

	# Save the spectrum for future redraw.
	call smw_close (MW(sh))
	call imunmap (im)
	GP_SHDR(gp,wc) = sh

plot_
	# Plot scaled graph.
	smin = GP_FMIN(gp)
	smax = GP_FMAX(gp)
	if (IS_INDEFI(scale[wc])) {
	    call sprintf (Memc[str], SZ_LINE, "%s: Log Flux")
	        call pargstr (Memc[GP_IMAGES(gp,wc)])
	    if (!IS_INDEF(smin)) {
		if (smin > 0.)
		    smin = log10 (smin)
		else
		    smin = INDEF
	    }
	    if (!IS_INDEF(smax)) {
		if (smax > 0.)
		    smax = log10 (smax)
		else
		    smax = INDEF
	    }
	} else if (scale[wc] != 0) {
	    call sprintf (Memc[str], SZ_LINE, "%s: Flux x 1E%d")
	        call pargstr (Memc[GP_IMAGES(gp,wc)])
	        call pargi (scale[wc])
	    w = 10. ** scale[wc]
	    if (!IS_INDEF(smin))
		smin = w * smin
	    if (!IS_INDEF(smax))
		smax = w * smax
	} else {
	    call sprintf (Memc[str], SZ_LINE, "%s: Flux")
	        call pargstr (Memc[GP_IMAGES(gp,wc)])
	    w = 1.
	}

	gio = GP_GIO(gp)
	call gascale (gio, Memr[SX(sh)], SN(sh), 1)
	call gascale (gio, Memr[SY(sh)], SN(sh), 2)
	call gswind (gio, INDEF, INDEF, smin, smax)
	call glabax (gio, Memc[str], "", "")
	call gseti (gio, G_PLCOLOR, GP_PLCOLOR(gp))
	call gpline (gio, Memr[SX(sh)], Memr[SY(sh)], SN(sh))

	call sfree (sp)

	# Check if image is one of the standard stars and plot flux points.
	do i = 1, nstds {
	    if (strne (Memc[GP_IMAGES(gp,wc)], STD_IMAGE(stds[i])))
		next
	    n = STD_NWAVES(stds[i])
	    x = STD_WAVES(stds[i])
	    y = STD_FLUXES(stds[i])
	    z = STD_DWAVES(stds[i])
	    call gseti (gio, G_PMLTYPE, 1)
	    call gseti (gio, G_PLCOLOR, GP_CMARK(gp))
	    if (IS_INDEFI(scale[wc])) {
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
