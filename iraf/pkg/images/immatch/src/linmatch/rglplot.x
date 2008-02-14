include <mach.h>
include <gset.h>
include "linmatch.h"

define	MINFRACTION	0.01
define	FRACTION	0.05

# XP_LPLOT -- Plot the data.

int procedure rg_lplot (gd, imr, im1, ls, udelete, region, bscale, bzero,
	plot_type) 

pointer	gd			#I pointer to the graphics stream
pointer	imr			#I pointer to the reference image
pointer	im1			#I pointer to the input image
pointer	ls			#I pointer to the linmatch structure
int	udelete[ARB]		#I the user deletions array
int	region			#I the current region if applicable
real	bscale			#I the computed bscale value
real	bzero			#I the computed bzero value
int	plot_type		#I the current plot type

int	stat
int	rg_mmhplot(), rg_mmfplot(), rg_mmrplot(), rg_rifplot(), rg_rirplot()
int	rg_bzfplot(), rg_bzrplot(), rg_msfplot(), rg_msrplot()

begin
	stat = OK

	switch (plot_type) {
	case LS_MMHIST:
	    stat = rg_mmhplot (gd, imr, im1, ls, udelete, region)
	case LS_MMFIT:
	    stat = rg_mmfplot (gd, ls, udelete, bscale, bzero)
	case LS_MMRESID:
	    stat = rg_mmrplot (gd, ls, udelete, bscale, bzero)
	case LS_RIFIT:
	    stat = rg_rifplot (gd, imr, im1, ls, udelete, region)
	case LS_RIRESID:
	    stat = rg_rirplot (gd, imr, imr, ls, udelete, region)
	case LS_BSZFIT:
	    stat = rg_bzfplot (gd, ls, udelete, bscale, bzero)
	case LS_BSZRESID:
	    stat = rg_bzrplot (gd, ls, udelete, bscale, bzero)
	case LS_MAGSKYFIT:
	    stat = rg_msfplot (gd, ls, udelete, bscale, bzero)
	case LS_MAGSKYRESID:
	    stat = rg_msrplot (gd, ls, udelete, bscale, bzero)
	default:
	    stat = ERR
	}

	return (stat)
end


# RG_MMHPLOT -- Plot the histogram of the data used to compute the mean, median,# and mode.

int procedure rg_mmhplot (gd, imr, im1, ls, udelete, region)

pointer	gd			#I pointer to the graphics stream
pointer	imr			#I pointer to the reference image
pointer	im1			#I pointer to the input image
pointer	ls			#I pointer to the linmatch structure
int	udelete[ARB]		#I the user deleteions array
int	region			#I the current region if applicable

int	nbinsr, nbins1
pointer	rbuf, ibuf, sp, hgmi, hgmr, image, title, str
real	rsigma, hminr, hmaxr, dhr, isigma, hmin1, hmax1, dh1, ymin, ymax
int	rg_lstati(), rg_limget()
pointer	rg_lstatp()

begin
	# Get the data.
	if (imr == NULL || im1 == NULL) {
	    return (ERR)
	} else if (region == rg_lstati (ls,CNREGION) &&
	    rg_lstatp (ls,RBUF) != NULL && rg_lstatp(ls, IBUF) != NULL) {
	    rbuf = rg_lstatp (ls, RBUF) 
	    ibuf = rg_lstatp (ls, IBUF) 
	} else if (rg_limget (ls, imr, im1, region) == OK) {
	    rbuf = rg_lstatp (ls, RBUF) 
	    ibuf = rg_lstatp (ls, IBUF) 
	} else {
	    return (ERR)
	}

	# Get the reference image binning parameters.
	rsigma = sqrt (real(Memi[rg_lstatp(ls,RNPTS)+region-1])) *
	    Memr[rg_lstatp(ls,RSIGMA)+region-1]
	hminr = Memr[rg_lstatp(ls,RMEDIAN)+region-1] - LMODE_HWIDTH * rsigma
	hmaxr = Memr[rg_lstatp(ls,RMEDIAN)+region-1] + LMODE_HWIDTH * rsigma
	dhr = LMODE_ZBIN * rsigma
	if (dhr <= 0.0)
	    return (ERR)
	nbinsr = (hmaxr - hminr) / dhr + 1
	if (nbinsr <= 0)
	    return (ERR)

	# Get the input image binning parameters.
	isigma = sqrt (real(Memi[rg_lstatp(ls,INPTS)+region-1])) *
	    Memr[rg_lstatp(ls,ISIGMA)+region-1]
	hmin1 = Memr[rg_lstatp(ls,IMEDIAN)+region-1] - LMODE_HWIDTH * isigma
	hmax1 = Memr[rg_lstatp(ls,IMEDIAN)+region-1] + LMODE_HWIDTH * isigma
	dh1 = LMODE_ZBIN * isigma
	if (dh1 <= 0.0)
	    return (ERR)
	nbins1 = (hmax1 - hmin1) / dh1 + 1
	if (nbins1 <= 0.0)
	    return (ERR)

	# Allocate working space.
	call smark (sp)
	call salloc (hgmi, max (nbinsr, nbins1), TY_INT)
	call salloc (hgmr, max (nbinsr, nbins1), TY_REAL)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (title, 2 * SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	call gclear (gd)

	# Create the reference histogram.
	call aclri (Memi[hgmi], nbinsr)
	call ahgmr (Memr[rbuf], Memi[rg_lstatp(ls,RNPTS)+region-1],
	    Memi[hgmi], nbinsr, hminr, hmaxr)
	call achtir (Memi[hgmi], Memr[hgmr], nbinsr)
	call alimr (Memr[hgmr], nbinsr, ymin, ymax)

	# Compute the limits for the reference histogram.
	call gseti (gd, G_WCS, 1)
	call gsview (gd, 0.1, 0.9, 0.6, 0.9)
	call gswind (gd, hminr, hmaxr, ymin, ymax)
	call rg_pfill (gd, hminr, hmaxr, ymin, ymax, GF_SOLID, 0)
	call rg_lstats (ls, REFIMAGE, Memc[image], SZ_FNAME)
	call sprintf (Memc[str], SZ_LINE,
	    "Mean = %g Median = %g Mode = %g  Sigma = %g")
	    call pargr (Memr[rg_lstatp(ls,RMEAN)+region-1])
	    call pargr (Memr[rg_lstatp(ls,RMEDIAN)+region-1])
	    call pargr (Memr[rg_lstatp(ls,RMODE)+region-1])
	    call pargr (rsigma)

	# Create the title for the reference histogram.
	call sprintf (Memc[title], 2 * SZ_LINE,
        "Ref Image: %s  Region: %d%s\nNbins = %d Hmin = %g Hmax = %g Dh = %g\n%s\n")
	    call pargstr (Memc[image])
	    call pargi (region)
	    if (udelete[region] == YES)
		call pargstr (" [deleted]")
	    else if (Memi[rg_lstatp(ls,RDELETE)+region-1] != LS_NO)
		call pargstr (" [rejected]")
	    else
		call pargstr ("")
	    call pargi (nbinsr)
	    call pargr (hminr)
	    call pargr (hmaxr)
	    call pargr (dhr)
	    call pargstr (Memc[str])
	call gseti (gd, G_YNMINOR, 0)
	call glabax (gd, Memc[title], "", "")

	# Plot the reference histogram.
	call rg_lhbox (gd, Memr[hgmr], nbinsr, hminr - dhr / 2.0,
	    hmaxr + dhr / 2.0)

	# Create the input histogram.
	call aclri (Memi[hgmi], nbins1)
	call ahgmr (Memr[ibuf], Memi[rg_lstatp(ls,INPTS)+region-1],
	    Memi[hgmi], nbins1, hmin1, hmax1)
	call achtir (Memi[hgmi], Memr[hgmr], nbins1)
	call alimr (Memr[hgmr], nbins1, ymin, ymax)

	# Compute the limits for the input histogram.
	call gseti (gd, G_WCS, 2)
	call gsview (gd, 0.1, 0.9, 0.1, 0.4)
	call gswind (gd, hmin1, hmax1, ymin, ymax)
	call rg_pfill (gd, hmin1, hmax1, ymin, ymax, GF_SOLID, 0)

	# Create the title for the input histogram.
	call rg_lstats (ls, IMAGE, Memc[image], SZ_FNAME)
	call sprintf (Memc[str], SZ_LINE,
	    "Mean = %g Median = %g Mode = %g  Sigma = %g")
	    call pargr (Memr[rg_lstatp(ls,IMEAN)+region-1])
	    call pargr (Memr[rg_lstatp(ls,IMEDIAN)+region-1])
	    call pargr (Memr[rg_lstatp(ls,IMODE)+region-1])
	    call pargr (isigma)
	call sprintf (Memc[title], 2 * SZ_LINE,
	"Input Image: %s  Region: %d%s\nNbins = %d Hmin = %g Hmax = %g Dh = %g\n%s\n")
	    call pargstr (Memc[image])
	    call pargi (region)
	    if (udelete[region] == YES)
		call pargstr (" [deleted]")
	    else if (Memi[rg_lstatp(ls,RDELETE)+region-1] != NO)
		call pargstr (" [rejected]")
	    else
		call pargstr ("")
	    call pargi (nbins1)
	    call pargr (hmin1)
	    call pargr (hmax1)
	    call pargr (dh1)
	    call pargstr (Memc[str])
	call gseti (gd, G_YNMINOR, 0)
	call glabax (gd, Memc[title], "", "")

	# Plot the input histogram.
	call rg_lhbox (gd, Memr[hgmr], nbins1, hmin1 - dh1 / 2.0,
	    hmax1 + dh1 / 2.0)

	call sfree (sp)

	return (OK)
end


# RG_MMFPLOT -- Plot the fit computed from the mean, median, or mode.

int procedure rg_mmfplot (gd, ls, udelete, bscale, bzero)

pointer	gd			#I pointer to the graphics stream
pointer	ls			#I pointer to the linmatch structure
int	udelete[ARB]		#I the user deletions array
real	bscale			#I the fitted bscale value
real	bzero			#I the fitted bzero value

bool	start, finish
int	nregions, mtype
pointer	sp, title, str, imager, image1
real	xmin, xmax, ymin, ymax, diff, dxmin, dxmax, dymin, dymax, x, y
int	rg_lstati()
pointer	rg_lstatp()

begin
	nregions = rg_lstati (ls, NREGIONS)
	if (nregions <= 1)
	    return (ERR)

	# Determine the type of data to plot.
	mtype = 0
	switch (rg_lstati(ls, BSALGORITHM)) {
	case LS_MEAN:
	    mtype = LS_MEAN
	case LS_MEDIAN:
	    mtype = LS_MEDIAN
	case LS_MODE:
	    mtype = LS_MODE
	default:
	}
	switch (rg_lstati(ls, BZALGORITHM)) {
	case LS_MEAN:
	    mtype = LS_MEAN
	case LS_MEDIAN:
	    mtype = LS_MEDIAN
	case LS_MODE:
	    mtype = LS_MODE
	default:
	}
	if (mtype <= 0)
	    return (ERR)

	# Allocate working space.
	call smark (sp)
	call salloc (title, 2 * SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (imager, SZ_LINE, TY_CHAR)
	call salloc (image1, SZ_LINE, TY_CHAR)

	# Clear the plot space.
	call gclear (gd)

	# Compute the limits of the plot.
	switch (mtype) {
	case LS_MEAN:
	    call rg_galimr (Memr[rg_lstatp(ls,IMEAN)],
	        Memi[rg_lstatp(ls,RDELETE)], nregions, xmin, xmax)
	    call rg_galimr (Memr[rg_lstatp(ls,RMEAN)],
	        Memi[rg_lstatp(ls,RDELETE)], nregions, ymin, ymax)
	case LS_MEDIAN:
	    call rg_galimr (Memr[rg_lstatp(ls,IMEDIAN)],
	        Memi[rg_lstatp(ls,RDELETE)], nregions, xmin, xmax)
	    call rg_galimr (Memr[rg_lstatp(ls,RMEDIAN)],
	        Memi[rg_lstatp(ls,RDELETE)], nregions, ymin, ymax)
	case LS_MODE:
	    call rg_galimr (Memr[rg_lstatp(ls,IMODE)],
	        Memi[rg_lstatp(ls,RDELETE)], nregions, xmin, xmax)
	    call rg_galimr (Memr[rg_lstatp(ls,RMODE)],
	        Memi[rg_lstatp(ls,RDELETE)], nregions, ymin, ymax)
	}
	dxmin = xmin
	dxmax = xmax
	dymin = ymin
	dymax = ymax

	diff = xmax - xmin
	if (diff <= 0)
	    diff = MINFRACTION
	else
	    diff = max (diff, MINFRACTION * abs (xmax + xmin) / 2.0)
	xmin = xmin - diff * FRACTION
	xmax = xmax + diff * FRACTION
	diff = ymax - ymin
	if (diff <= 0.0)
	    diff = MINFRACTION
	else
	    diff = max (diff, MINFRACTION * abs (ymax + ymin) / 2.0)
	ymin = ymin - diff * FRACTION
	ymax = ymax + diff * FRACTION
	call gswind (gd, xmin, xmax, ymin, ymax)

	# Construct the titles and axis labels.
	call rg_lstats (ls, REFIMAGE, Memc[imager], SZ_FNAME)
	call rg_lstats (ls, IMAGE, Memc[image1], SZ_FNAME)
	call sprintf (Memc[str], SZ_LINE,
	    "Nregions = %d Ref Image = %g * Input Image + %g")
	    call pargi (nregions)
	    call pargr (bscale)
	    call pargr (bzero)
	call sprintf (Memc[title], 2 * SZ_LINE,
	    "Counts for %s versus Counts for %s\n%s\n")
	    call pargstr (Memc[imager])
	    call pargstr (Memc[image1])
	    call pargstr (Memc[str])
	call glabax (gd, Memc[title], "Input Image Counts",
	    "Ref Image Counts")

	# Plot the data.
	switch (mtype) {
	case LS_MEAN:
	    call rg_lxyplot (gd, Memr[rg_lstatp(ls,IMEAN)],
	        Memr[rg_lstatp(ls,RMEAN)], Memi[rg_lstatp(ls,RDELETE)],
		udelete, nregions, GM_BOX, GM_CROSS)
	case LS_MEDIAN:
	    call rg_lxyplot (gd, Memr[rg_lstatp(ls,IMEDIAN)],
	        Memr[rg_lstatp(ls,RMEDIAN)], Memi[rg_lstatp(ls,RDELETE)],
		udelete, nregions, GM_BOX, GM_CROSS)
	case LS_MODE:
	    call rg_lxyplot (gd, Memr[rg_lstatp(ls,IMODE)],
	        Memr[rg_lstatp(ls,RMODE)], Memi[rg_lstatp(ls,RDELETE)],
		udelete, nregions, GM_BOX, GM_CROSS)
	}

	# Plot the fit.
	start = false
	finish = false
	if (! IS_INDEFR(bscale) && ! IS_INDEFR(bzero)) {
	    y = bscale * dxmin + bzero
	    if (y >= ymin && y <= ymax) {
		call gamove (gd, dxmin, y)
		start = true
	    }
	    y = bscale * dxmax + bzero
	    if (y >= ymin && y <= ymax) {
		if (start) {
		    call gadraw (gd, dxmax, y)
		    finish = true
		} else {
		    call gamove (gd, dxmax, y)
		    start = true
		}
	    }
	    x = (dymin - bzero) / bscale
	    if (x >= xmin && x <= xmax) {
		if (! start) {
		    call gamove (gd, x, dymin)
		    start = true
		} else if (! finish) {
		    call gadraw (gd, x, dymin)
		    finish = true
		}
	    }
	    x = (dymax - bzero) / bscale
	    if (x >= xmin && x <= xmax) {
		if (! start) {
		    call gamove (gd, x, dymax)
		    start = true
		} else if (! finish) {
		    call gadraw (gd, x, dymax)
		    finish = true
		}
	    }
	}

	call sfree (sp)

	return (OK)
end


# RG_MMRPLOT -- Plot the residuals from the fit computed from the mean,
# median, or mode.

int procedure rg_mmrplot (gd, ls, udelete, bscale, bzero)

pointer	gd			#I pointer to the graphics stream
pointer	ls			#I pointer to the linmatch structure
int	udelete[ARB]		#I the user deletions array
real	bscale			#I the fitted bscale value
real	bzero			#I the fitted bzero value

int	nregions, mtype
pointer	sp, resid, title, imager, image1, str
real	xmin, xmax, ymin, ymax, diff
int	rg_lstati()
pointer	rg_lstatp()

begin
	nregions = rg_lstati (ls, NREGIONS)
	if (nregions <= 1)
	    return (ERR)

	# Determine the type of data to plot.
	mtype = 0
	switch (rg_lstati(ls, BSALGORITHM)) {
	case LS_MEAN:
	    mtype = LS_MEAN
	case LS_MEDIAN:
	    mtype = LS_MEDIAN
	case LS_MODE:
	    mtype = LS_MODE
	default:
	}
	switch (rg_lstati(ls, BZALGORITHM)) {
	case LS_MEAN:
	    mtype = LS_MEAN
	case LS_MEDIAN:
	    mtype = LS_MEDIAN
	case LS_MODE:
	    mtype = LS_MODE
	default:
	}
	if (mtype <= 0)
	    return (ERR)

	# Allocate working space.
	call smark (sp)

	call gclear (gd)

	# Compute the data.
	call salloc (resid, nregions, TY_REAL)
	switch (mtype) {
	case LS_MEAN:
	    call altmr (Memr[rg_lstatp(ls,IMEAN)], Memr[resid], nregions,
	        bscale, bzero)
	    call asubr (Memr[rg_lstatp(ls,RMEAN)], Memr[resid], Memr[resid],
	        nregions)
	    call rg_galimr (Memr[rg_lstatp(ls,IMEAN)],
	        Memi[rg_lstatp(ls,RDELETE)], nregions, xmin, xmax)
	    call rg_galimr (Memr[resid], Memi[rg_lstatp(ls,RDELETE)], nregions,
	        ymin, ymax)
	case LS_MEDIAN:
	    call altmr (Memr[rg_lstatp(ls,IMEDIAN)], Memr[resid], nregions,
	        bscale, bzero)
	    call asubr (Memr[rg_lstatp(ls,RMEDIAN)], Memr[resid], Memr[resid],
	        nregions)
	    call rg_galimr (Memr[rg_lstatp(ls,IMEDIAN)],
	        Memi[rg_lstatp(ls,RDELETE)], nregions, xmin, xmax)
	    call rg_galimr (Memr[resid], Memi[rg_lstatp(ls,RDELETE)], nregions,
	        ymin, ymax)
	case LS_MODE:
	    call altmr (Memr[rg_lstatp(ls,IMODE)], Memr[resid], nregions,
	        bscale, bzero)
	    call asubr (Memr[rg_lstatp(ls,RMODE)], Memr[resid], Memr[resid],
	        nregions)
	    call rg_galimr (Memr[rg_lstatp(ls,IMODE)],
	        Memi[rg_lstatp(ls,RDELETE)], nregions, xmin, xmax)
	    call rg_galimr (Memr[resid], Memi[rg_lstatp(ls,RDELETE)], nregions,
	        ymin, ymax)
	}

	# Compute the data limits.
	diff = xmax - xmin
	if (diff <= 0.0)
	    diff = MINFRACTION
	else
	    diff = max (diff, MINFRACTION * abs (xmax + xmin) / 2.0)
	xmin = xmin - diff * FRACTION
	xmax = xmax + diff * FRACTION
	diff = ymax - ymin
	if (diff <= 0.0)
	    diff = MINFRACTION
	else
	    diff = max (diff, MINFRACTION * abs (ymax + ymin) / 2.0)
	ymin = ymin - diff * FRACTION
	ymax = ymax + diff * FRACTION
	call gswind (gd, xmin, xmax, ymin, ymax)

	call salloc (title, 2 * SZ_LINE, TY_CHAR)
	call salloc (imager, SZ_FNAME, TY_CHAR)
	call salloc (image1, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call rg_lstats (ls, REFIMAGE, Memc[imager], SZ_FNAME)
	call rg_lstats (ls, IMAGE, Memc[image1], SZ_FNAME)
	call sprintf (Memc[str], SZ_LINE,
	    "Nregions = %d Ref Image = %g * Input Image + %g")
	    call pargi (nregions)
	    call pargr (bscale)
	    call pargr (bzero)
	call sprintf (Memc[title], 2 * SZ_LINE,
	    "Residuals for %s  versus Counts for %s\n%s\n")
	    call pargstr (Memc[imager])
	    call pargstr (Memc[image1])
	    call pargstr (Memc[str])
	call glabax (gd, Memc[title], "Input Image Counts",
	    "Residual Counts")

	# Plot the data.
	switch (mtype) {
	case LS_MEAN:
	    call rg_lxyplot (gd, Memr[rg_lstatp(ls,IMEAN)], Memr[resid],
	        Memi[rg_lstatp(ls,RDELETE)], udelete, nregions,
		GM_BOX, GM_CROSS)
	case LS_MEDIAN:
	    call rg_lxyplot (gd, Memr[rg_lstatp(ls,IMEDIAN)], Memr[resid],
	        Memi[rg_lstatp(ls,RDELETE)], udelete, nregions,
		GM_BOX, GM_CROSS)
	case LS_MODE:
	    call rg_lxyplot (gd, Memr[rg_lstatp(ls,IMODE)], Memr[resid],
	        Memi[rg_lstatp(ls,RDELETE)], udelete, nregions,
		GM_BOX, GM_CROSS)
	}

	# Plot the residuals 0 line.
	call gamove (gd, xmin, 0.0)
	call gadraw (gd, xmax, 0.0)

	call sfree (sp)

	return (OK)
end


# RG_RIFPLOT -- Plot the pixel to pixel fit for a region.

int procedure rg_rifplot (gd, imr, im1, ls, udelete, region)

pointer	gd			#I pointer to the graphics stream
pointer	imr			#I pointer to the reference image
pointer	im1			#I pointer to the input image
pointer	ls			#I pointer to the linmatch structure
int	udelete[ARB]		#I pointer to the user deletions array
int	region			#I the current region

bool	start, finish
int	npts
pointer	rbuf, ibuf, sp, title, str, imager, image1, resid
real	xmin, xmax, ymin, ymax, diff, bscale, bzero, datamin, datamax
real	loreject, hireject, chi, dxmin, dxmax, dymin, dymax, x, y
int	rg_lstati(), rg_limget()
pointer	rg_lstatp()
real	rg_lstatr()

begin
	# Get the data.
	if (imr == NULL || im1 == NULL) {
	    return (ERR)
	} else if (region == rg_lstati (ls,CNREGION) &&
	    rg_lstatp (ls,RBUF) != NULL && rg_lstatp(ls, IBUF) != NULL) {
	    rbuf = rg_lstatp (ls, RBUF) 
	    ibuf = rg_lstatp (ls, IBUF) 
	} else if (rg_limget (ls, imr, im1, region) == OK) {
	    rbuf = rg_lstatp (ls, RBUF) 
	    ibuf = rg_lstatp (ls, IBUF) 
	} else {
	    return (ERR)
	}

	# Initialize.
	call gclear (gd)

	# Get some constants
	npts = Memi[rg_lstatp(ls,RNPTS)+region-1]
	bscale = Memr[rg_lstatp(ls,RBSCALE)+region-1]
	bzero = Memr[rg_lstatp(ls,RBZERO)+region-1]
	chi = Memr[rg_lstatp(ls,RCHI)+region-1]
	if (IS_INDEFR(rg_lstatr(ls,DATAMIN)))
	    datamin = -MAX_REAL
	else
	    datamin = rg_lstatr (ls,DATAMIN)
	if (IS_INDEFR(rg_lstatr(ls,DATAMAX)))
	    datamax = MAX_REAL
	else
	    datamax = rg_lstatr (ls,DATAMAX)
	if (rg_lstati(ls,NREJECT) <= 0 || IS_INDEFR(rg_lstatr(ls,LOREJECT)) ||
	    IS_INDEFR(chi))
	    loreject = -MAX_REAL
	else
	    loreject = -rg_lstatr (ls,LOREJECT) * chi
	if (rg_lstati(ls,NREJECT) <= 0 || IS_INDEFR(rg_lstatr(ls,HIREJECT)) ||
	    IS_INDEFR(chi))
	    hireject = MAX_REAL
	else
	    hireject = rg_lstatr (ls,HIREJECT) * chi

	# Compute the plot limits.
	call alimr (Memr[ibuf], npts, xmin, xmax)
	call alimr (Memr[rbuf], npts, ymin, ymax)
	dxmin = xmin
	dxmax = xmax
	dymin = ymin
	dymax = ymax

	diff = xmax - xmin
	if (diff <= 0.0)
	    diff = MINFRACTION
	else
	    diff = max (diff, MINFRACTION * abs (xmax + xmin) / 2.0)
	xmin = xmin - diff * FRACTION
	xmax = xmax + diff * FRACTION
	diff = ymax - ymin
	if (diff <= 0.0)
	    diff = MINFRACTION
	else
	    diff = max (diff, MINFRACTION * abs (ymax + ymin) / 2.0)
	ymin = ymin - diff * FRACTION
	ymax = ymax + diff * FRACTION
	call gswind (gd, xmin, xmax, ymin, ymax)

	# Allocate working space.
	call smark (sp)

	# Create the plot title.
	call salloc (title, 2 * SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (imager, SZ_FNAME, TY_CHAR)
	call salloc (image1, SZ_FNAME, TY_CHAR)
	call rg_lstats (ls, REFIMAGE, Memc[imager], SZ_FNAME)
	call rg_lstats (ls, IMAGE, Memc[image1], SZ_FNAME)
	call sprintf (Memc[str], SZ_LINE,
	    "Region %d%s: Ref Image = %g * Input Image + %g")
	    call pargi (region)
	    if (udelete[region] == YES)
		call pargstr (" [deleted]")
	    else if (Memi[rg_lstatp(ls,RDELETE)+region-1] != LS_NO)
		call pargstr (" [rejected]")
	    else
		call pargstr ("")
	    call pargr (bscale)
	    call pargr (bzero)
	call sprintf (Memc[title], 2 * SZ_LINE,
	    "Counts for Image %s  versus Counts for Image %s\n%s\n\n")
	    call pargstr (Memc[imager])
	    call pargstr (Memc[image1])
	    call pargstr (Memc[str])
	call glabax (gd, Memc[title], "Input Image Counts",
	    "Ref image Counts")

	# Compute the residuals.
	call salloc (resid, npts, TY_REAL)
	if (IS_INDEFR(bscale) || IS_INDEFR(bzero))
	    call amovkr (0.0, Memr[resid], npts)
	else {
	    call altmr (Memr[ibuf], Memr[resid], npts, bscale, bzero)
	    call asubr (Memr[rbuf], Memr[resid], Memr[resid], npts)
	}

	# Plot the data.
	call rg_riplot (gd, Memr[ibuf], Memr[rbuf], Memr[resid], npts,
	    datamin, datamax, loreject, hireject, GM_BOX, GM_CROSS)

	# Plot the fit if bscale and bzero are defined.
	start = false
	finish = false
	if (! IS_INDEFR(bscale) && ! IS_INDEFR(bzero)) {
	    y = bscale * dxmin + bzero
	    if (y >= ymin && y <= ymax) {
		call gamove (gd, dxmin, y)
		start = true
	    }
	    y = bscale * dxmax + bzero
	    if (y >= ymin && y <= ymax) {
		if (start) {
		    call gadraw (gd, dxmax, y)
		    finish = true
		} else {
		    call gamove (gd, dxmax, y)
		    start = true
		}
	    }
	    x = (dymin - bzero) / bscale
	    if (x >= xmin && x <= xmax) {
		if (! start) {
		    call gamove (gd, x, dymin)
		    start = true
		} else if (! finish) {
		    call gadraw (gd, x, dymin)
		    finish = true
		}
	    }
	    x = (dymax - bzero) / bscale
	    if (x >= xmin && x <= xmax) {
		if (! start) {
		    call gamove (gd, x, dymax)
		    start = true
		} else if (! finish) {
		    call gadraw (gd, x, dymax)
		    finish = true
		}
	    }
	}

	call sfree (sp)

	return (OK)
end


# RG_RIRPLOT -- Plot the pixel to pixel fit residuals for a region.

int procedure rg_rirplot (gd, imr, im1, ls, udelete, region)

pointer	gd			#I pointer to the graphics stream
pointer	imr			#I pointer to the reference image
pointer	im1			#I pointer to the input image
pointer	ls			#I pointer to the linmatch structure
int	udelete[ARB]		#I pointer to the user deletions array
int	region			#I the current region

int	npts
pointer	rbuf, ibuf, sp, title, str, imager, image1, resid
real	xmin, xmax, ymin, ymax, diff, bscale, bzero, datamin, datamax
real	loreject, hireject, chi
int	rg_lstati(), rg_limget()
pointer	rg_lstatp()
real	rg_lstatr()

begin
	# Get the data.
	if (imr == NULL || im1 == NULL) {
	    return (ERR)
	} else if (region == rg_lstati (ls,CNREGION) &&
	    rg_lstatp (ls,RBUF) != NULL && rg_lstatp(ls, IBUF) != NULL) {
	    rbuf = rg_lstatp (ls, RBUF) 
	    ibuf = rg_lstatp (ls, IBUF) 
	} else if (rg_limget (ls, imr, im1, region) == OK) {
	    rbuf = rg_lstatp (ls, RBUF) 
	    ibuf = rg_lstatp (ls, IBUF) 
	} else {
	    return (ERR)
	}

	# Initialize.
	call gclear (gd)

	# Get some constants
	npts = Memi[rg_lstatp(ls,RNPTS)+region-1]
	bscale = Memr[rg_lstatp(ls,RBSCALE)+region-1]
	bzero = Memr[rg_lstatp(ls,RBZERO)+region-1]
	chi = Memr[rg_lstatp(ls,RCHI)+region-1]
	if (IS_INDEFR(rg_lstatr(ls,DATAMIN)))
	    datamin = -MAX_REAL
	else
	    datamin = rg_lstatr (ls,DATAMIN)
	if (IS_INDEFR(rg_lstatr(ls,DATAMAX)))
	    datamax = MAX_REAL
	else
	    datamax = rg_lstatr (ls,DATAMAX)
	if (rg_lstati(ls,NREJECT) <= 0 || IS_INDEFR(rg_lstatr(ls,LOREJECT)) ||
	    IS_INDEFR(chi))
	    loreject = -MAX_REAL
	else
	    loreject = -rg_lstatr (ls,LOREJECT) * chi
	if (rg_lstati(ls,NREJECT) <= 0 || IS_INDEFR(rg_lstatr(ls,HIREJECT)) ||
	    IS_INDEFR(chi))
	    hireject = MAX_REAL
	else
	    hireject = rg_lstatr (ls,HIREJECT) * chi

	# Allocate working space.
	call smark (sp)

	# Compute the residuals.
	call salloc (resid, npts, TY_REAL)
	if (IS_INDEFR(bscale) || IS_INDEFR(bzero))
	    call amovkr (INDEFR, Memr[resid], npts)
	else {
	    call altmr (Memr[ibuf], Memr[resid], npts, bscale, bzero)
	    call asubr (Memr[rbuf], Memr[resid], Memr[resid], npts)
	}

	# Compute the plot limits.
	call alimr (Memr[ibuf], npts, xmin, xmax)
	call alimr (Memr[resid], npts, ymin, ymax)
	diff = xmax - xmin
	if (diff <= 0.0)
	    diff = MINFRACTION
	else
	    diff = max (diff, MINFRACTION * abs (xmin + xmax) / 2.0)
	xmin = xmin - diff * FRACTION
	xmax = xmax + diff * FRACTION
	diff = ymax - ymin
	if (diff <= 0.0)
	    diff = MINFRACTION
	else
	    diff = max (diff, MINFRACTION * abs (ymin + ymax) / 2.0)
	ymin = ymin - diff * FRACTION
	ymax = ymax + diff * FRACTION
	call gswind (gd, xmin, xmax, ymin, ymax)

	# Create the plot title.
	call salloc (title, 2 * SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (imager, SZ_FNAME, TY_CHAR)
	call salloc (image1, SZ_FNAME, TY_CHAR)

	# Create the plot title.
	call rg_lstats (ls, REFIMAGE, Memc[imager], SZ_FNAME)
	call rg_lstats (ls, IMAGE, Memc[image1], SZ_FNAME)
	call sprintf (Memc[str], SZ_LINE,
	    "Region %d%s: Ref Image = %g * Input Image + %g")
	    call pargi (region)
	    if (udelete[region] == YES)
		call pargstr (" [deleted]")
	    else if (Memi[rg_lstatp(ls,RDELETE)+region-1] != LS_NO)
		call pargstr (" [rejected]")
	    else
		call pargstr ("")
	    call pargr (bscale)
	    call pargr (bzero)
	call sprintf (Memc[title], 2 * SZ_LINE,
	    "Residuals for Image %s  versus Counts for Image %s\n%s\n\n")
	    call pargstr (Memc[imager])
	    call pargstr (Memc[image1])
	    call pargstr (Memc[str])
	call glabax (gd, Memc[title], "Input Image Counts",
	    "Ref image Counts")

	# Plot the data.
	call rg_rriplot (gd, Memr[ibuf], Memr[rbuf], Memr[resid], npts,
	    datamin, datamax, loreject, hireject, GM_BOX, GM_CROSS)

	# Plot the 0 line if bscale and bzero are defined.
	if ( ! IS_INDEFR(bscale) && ! IS_INDEFR(bzero)) {
	    call gamove (gd, xmin, 0.0)
	    call gadraw (gd, xmax, 0.0)
	}

	call sfree (sp)

	return (OK)
end


# RG_BZFPLOT -- Plot the bscale and bzero values computed from the
# fit algorithm.

int procedure rg_bzfplot (gd, ls, udelete, bscale, bzero)

pointer	gd			#I pointer to the graphics stream
pointer	ls			#I pointer to the linmatch structure
int	udelete[ARB]		#I the user deletions array
real	bscale			#I the fitted bscale value
real	bzero			#I the fitted bzero value

int	i, nregions
pointer	sp, xreg, title, str, imager, image1
real	xmin, xmax, ymin, ymax, diff
int	rg_lstati()
pointer	rg_lstatp()

begin
	nregions = rg_lstati (ls, NREGIONS)
	if (nregions <= 1)
	    return (ERR)

	# Allocate working space.
	call smark (sp)

	# Set up space and info the plot title.
	call salloc (title, 2 * SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (imager, SZ_FNAME, TY_CHAR)
	call salloc (image1, SZ_FNAME, TY_CHAR)
	call rg_lstats (ls, REFIMAGE, Memc[imager], SZ_FNAME)
	if (rg_lstati(ls,BSALGORITHM) == LS_PHOTOMETRY ||
	    rg_lstati(ls,BZALGORITHM) == LS_PHOTOMETRY)
	    call rg_lstats (ls, PHOTFILE, Memc[image1], SZ_FNAME)
	else
	    call rg_lstats (ls, IMAGE, Memc[image1], SZ_FNAME)

	# Set the x array.
	call salloc (xreg, nregions, TY_REAL)
	do i = 1, nregions
	    Memr[xreg+i-1] = i
	xmin = 1.0 - FRACTION * (nregions - 1)
	xmax = nregions + FRACTION * (nregions - 1)

	call gclear (gd)

	# Determine the limits of bscale versus region.
	call alimr (Memr[rg_lstatp(ls,RBSCALE)], nregions, ymin, ymax)
	diff = ymax - ymin
	if (diff <= 0.0)
	    diff = MINFRACTION
	else
	    diff = max (diff, MINFRACTION * (ymax + ymin) / 2.0)
	ymin = ymin - FRACTION * diff
	ymax = ymax + FRACTION * diff
	call gseti (gd, G_WCS, 1)
	call gsview (gd, 0.15, 0.9, 0.6, 0.9)
	call gswind (gd, xmin, xmax, ymin, ymax)
	call rg_pfill (gd, xmin, xmax, ymin, ymax, GF_SOLID, 0)

	# Create the title for bscale versus region.
	call sprintf (Memc[str], SZ_LINE,
	    "Reference: %s  Input: %s  Bscale: %g")
	    call pargstr (Memc[imager])
	    call pargstr (Memc[image1])
	    call pargr (bscale)
	call sprintf (Memc[title], 2 * SZ_LINE,
	    "Bscale vs. Region\n%s\n")
	    call pargstr (Memc[str])
	call glabax (gd, Memc[title], "Region", "Bscale")

	# Plot the points.
	call rg_lxyplot (gd, Memr[xreg], Memr[rg_lstatp(ls,RBSCALE)],
	    Memi[rg_lstatp(ls,RDELETE)], udelete, nregions, GM_BOX, GM_CROSS)

	# Plot the fit.
	call gamove (gd, xmin, bscale)
	call gadraw (gd, xmax, bscale)

	# Determine the limits of bzero versus region.
	call alimr (Memr[rg_lstatp(ls,RBZERO)], nregions, ymin, ymax)
	diff = ymax - ymin
	if (diff <= 0.0)
	    diff = MINFRACTION
	else
	    diff = max (diff, MINFRACTION * abs (ymin + ymax) / 2.0)
	ymin = ymin - FRACTION * diff
	ymax = ymax + FRACTION * diff
	call gseti (gd, G_WCS, 2)
	call gsview (gd, 0.15, 0.9, 0.1, 0.4)
	call gswind (gd, xmin, xmax, ymin, ymax)
	call rg_pfill (gd, xmin, xmax, ymin, ymax, GF_SOLID, 0)

	# Create the title for bzero versus region.
	call sprintf (Memc[str], SZ_LINE,
	    "Reference: %s  Input: %s  Bzero: %g")
	    call pargstr (Memc[imager])
	    call pargstr (Memc[image1])
	    call pargr (bzero)
	call sprintf (Memc[title], 2 * SZ_LINE, "Bzero vs. Region\n%s\n")
	    call pargstr (Memc[str])
	call glabax (gd, Memc[title], "Region", "Bzero")

	# Plot the points.
	call rg_lxyplot (gd, Memr[xreg], Memr[rg_lstatp(ls,RBZERO)],
	    Memi[rg_lstatp(ls,RDELETE)], udelete, nregions, GM_BOX, GM_CROSS)

	# Plot the fit.
	call gamove (gd, xmin, bzero)
	call gadraw (gd, xmax, bzero)

	call sfree (sp)

	return (OK)
end


# RG_BZRPLOT -- Plot the bscale and bzero values computed from the
# fit algorithm.

int procedure rg_bzrplot (gd, ls, udelete, bscale, bzero)

pointer	gd			#I pointer to the graphics stream
pointer	ls			#I pointer to the linmatch structure
int	udelete[ARB]		#I the user deletions array
real	bscale			#I the fitted bscale value
real	bzero			#I the fitted bzero value

int	i, nregions
pointer	sp, xreg, yreg, title, str, imager, image1
real	xmin, xmax, ymin, ymax, diff
int	rg_lstati()
pointer	rg_lstatp()

begin
	nregions = rg_lstati (ls, NREGIONS)
	if (nregions <= 1)
	    return (ERR)

	# Allocate working space.
	call smark (sp)
	call salloc (xreg, nregions, TY_REAL)
	call salloc (yreg, nregions, TY_REAL)

	# Set up space and info the plot title.
	call salloc (title, 2 * SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (imager, SZ_FNAME, TY_CHAR)
	call salloc (image1, SZ_FNAME, TY_CHAR)
	call rg_lstats (ls, REFIMAGE, Memc[imager], SZ_FNAME)
	if (rg_lstati(ls,BSALGORITHM) == LS_PHOTOMETRY ||
	    rg_lstati(ls,BZALGORITHM) == LS_PHOTOMETRY)
	    call rg_lstats (ls, PHOTFILE, Memc[image1], SZ_FNAME)
	else
	    call rg_lstats (ls, IMAGE, Memc[image1], SZ_FNAME)

	# Set the x array.
	do i = 1, nregions
	    Memr[xreg+i-1] = i
	xmin = 1.0 - FRACTION * (nregions - 1)
	xmax = nregions + FRACTION * (nregions - 1)

	call gclear (gd)

	# Determine the limits of the bscale value versus region.
	call asubkr (Memr[rg_lstatp(ls,RBSCALE)], bscale, Memr[yreg], nregions)
	call alimr (Memr[yreg], nregions, ymin, ymax)
	diff = ymax - ymin
	if (diff <= 0.0)
	    diff = MINFRACTION
	else
	    diff = max (diff, MINFRACTION * (ymax + ymin) / 2.0)
	ymin = ymin - FRACTION * diff
	ymax = ymax + FRACTION * diff
	call gseti (gd, G_WCS, 1)
	call gsview (gd, 0.15, 0.9, 0.6, 0.9)
	call gswind (gd, xmin, xmax, ymin, ymax)
	call rg_pfill (gd, xmin, xmax, ymin, ymax, GF_SOLID, 0)

	# Create the title for bscale versus region.
	call sprintf (Memc[str], SZ_LINE,
	    "Reference: %s  Input: %s  Bscale: %g")
	    call pargstr (Memc[imager])
	    call pargstr (Memc[image1])
	    call pargr (bscale)
	call sprintf (Memc[title], 2 * SZ_LINE,
	    "Bscale Residuals vs. Region\n%s\n")
	    call pargstr (Memc[str])
	call glabax (gd, Memc[title], "Region", "Bscale Residuals")

	# Plot the points.
	call rg_lxyplot (gd, Memr[xreg], Memr[yreg], Memi[rg_lstatp(ls,
	    RDELETE)], udelete, nregions, GM_BOX, GM_CROSS)

	# Plot the fit.
	call gamove (gd, xmin, 0.0)
	call gadraw (gd, xmax, 0.0)

	# Determine the limits of the bscale value versus region.
	call asubkr (Memr[rg_lstatp(ls,RBZERO)], bzero, Memr[yreg], nregions)
	call alimr (Memr[yreg], nregions, ymin, ymax)
	diff = ymax - ymin
	if (diff <= 0.0)
	    diff = MINFRACTION
	else
	    diff = max (diff, MINFRACTION * (ymax + ymin) / 2.0)
	ymin = ymin - FRACTION * diff
	ymax = ymax + FRACTION * diff
	call gseti (gd, G_WCS, 2)
	call gsview (gd, 0.15, 0.9, 0.1, 0.4)
	call gswind (gd, xmin, xmax, ymin, ymax)
	call rg_pfill (gd, xmin, xmax, ymin, ymax, GF_SOLID, 0)

	# Create the title for bzero versus region.
	call sprintf (Memc[str], SZ_LINE,
	    "Reference: %s  Input: %s  Bzero: %g")
	    call pargstr (Memc[imager])
	    call pargstr (Memc[image1])
	    call pargr (bzero)
	call sprintf (Memc[title], 2 * SZ_LINE,
	    "Bzero Residuals vs. Region\n%s\n")
	    call pargstr (Memc[str])
	call glabax (gd, Memc[title], "Region", "Bzero Residuals")

	# Plot the points.
	call rg_lxyplot (gd, Memr[xreg], Memr[yreg], Memi[rg_lstatp(ls,
	    RDELETE)], udelete, nregions, GM_BOX, GM_CROSS)

	# Plot the fit.
	call gamove (gd, xmin, 0.0)
	call gadraw (gd, xmax, 0.0)

	call sfree (sp)

	return (OK)
end


# RG_MSFPLOT -- Plot the magnitude and sky values of the regions.

int procedure rg_msfplot (gd, ls, udelete, bscale, bzero)

pointer	gd			#I pointer to the graphics stream
pointer	ls			#I pointer to the linmatch structure
int	udelete[ARB]		#I the user deletions array
real	bscale			#I the fitted bscale value
real	bzero			#I the fitted bzero value

bool	start, finish
int	nregions
pointer	sp, title, str, imager, image1
real	xmin, xmax, ymin, ymax, diff, dxmin, dxmax, dymin, dymax, x, y
int	rg_lstati()
pointer	rg_lstatp()

begin
	nregions = rg_lstati (ls, NREGIONS)
	if (nregions <= 0)
	    return (ERR)

	# Allocate working space.
	call smark (sp)
	call salloc (title, 2 * SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (imager, SZ_FNAME, TY_CHAR)
	call salloc (image1, SZ_FNAME, TY_CHAR)
	call rg_lstats (ls, REFIMAGE, Memc[imager], SZ_FNAME)
	call rg_lstats (ls, PHOTFILE, Memc[image1], SZ_FNAME)

	call gclear (gd)

	# Determine the limits of the bscale value versus region.
	call alimr (Memr[rg_lstatp(ls,IMAG)], nregions, xmin, xmax)
	dxmin = xmin
	dxmax = xmax
	diff = xmax - xmin
	if (diff <= 0.0)
	    diff = MINFRACTION
	else
	    diff = max (diff, MINFRACTION * (xmax + xmin) / 2.0)
	xmin = xmin - FRACTION * diff
	xmax = xmax + FRACTION * diff
	call alimr (Memr[rg_lstatp(ls,RMAG)], nregions, ymin, ymax)
	dymin = ymin
	dymax = ymax
	diff = ymax - ymin
	if (diff <= 0.0)
	    diff = MINFRACTION
	else
	    diff = max (diff, MINFRACTION * (ymax + ymin) / 2.0)
	ymin = ymin - FRACTION * diff
	ymax = ymax + FRACTION * diff
	call gseti (gd, G_WCS, 1)
	call gsview (gd, 0.15, 0.9, 0.6, 0.9)
	call gswind (gd, xmin, xmax, ymin, ymax)
	call rg_pfill (gd, xmin, xmax, ymin, ymax, GF_SOLID, 0)

	# Create the title for bscale versus region.
	call sprintf (Memc[str], SZ_LINE,
	    "Reference magnitudes = Input magnitudes + %0.3f")
	    call pargr (-2.5 * log10 (bscale))
	call sprintf (Memc[title], 2 * SZ_LINE,
	    "Magnitudes for %s vs. Magnitudes for %s\n%s\n")
	    call pargstr (Memc[imager])
	    call pargstr (Memc[image1])
	    call pargstr (Memc[str])
	call glabax (gd, Memc[title], "Input Magnitudes",
	    "Ref Magnitudes")

	# Plot the points.
	call rg_lxyplot (gd, Memr[rg_lstatp(ls,IMAG)], Memr[rg_lstatp(ls,RMAG)],
	    Memi[rg_lstatp(ls, RDELETE)], udelete, nregions, GM_BOX, GM_CROSS)

	# Plot the fit.
	if (bscale > 0.0) {
	    call gamove (gd, dxmin, dxmin - 2.5 * log10(bscale))
	    call gadraw (gd, dxmax, dxmax - 2.5 * log10(bscale))
	}

	# Determine the limits of the bscale value versus region.
	call alimr (Memr[rg_lstatp(ls,ISKY)], nregions, xmin, xmax)
	dxmin = xmin
	dxmax = xmax
	diff = xmax - xmin
	if (diff <= 0.0)
	    diff = MINFRACTION
	else
	    diff = max (diff, MINFRACTION * (xmax + xmin) / 2.0)
	xmin = xmin - FRACTION * diff
	xmax = xmax + FRACTION * diff
	call alimr (Memr[rg_lstatp(ls,RSKY)], nregions, ymin, ymax)
	dymin = ymin
	dymax = ymax
	diff = ymax - ymin
	if (diff <= 0.0)
	    diff = 0.0
	else
	    diff = max (diff, MINFRACTION * (ymax + ymin) / 2.0)
	ymin = ymin - FRACTION * diff
	ymax = ymax + FRACTION * diff
	call gseti (gd, G_WCS, 2)
	call gsview (gd, 0.15, 0.9, 0.1, 0.4)
	call gswind (gd, xmin, xmax, ymin, ymax)
	call rg_pfill (gd, xmin, xmax, ymin, ymax, GF_SOLID, 0)

	# Create the title for bscale versus region.
	call sprintf (Memc[str], SZ_LINE,
	    "Reference skies = %g * Input skies + %g")
	    call pargr (bscale)
	    call pargr (bzero)
	call sprintf (Memc[title], 2 * SZ_LINE,
	    "Sky Values for %s vs. Sky Values for %s\n%s\n")
	    call pargstr (Memc[imager])
	    call pargstr (Memc[image1])
	    call pargstr (Memc[str])
	call glabax (gd, Memc[title], "Input Sky Values",
	    "Ref Sky Values")

	# Plot the points.
	call rg_lxyplot (gd, Memr[rg_lstatp(ls,ISKY)], Memr[rg_lstatp(ls,RSKY)],
	    Memi[rg_lstatp(ls, RDELETE)], udelete, nregions, GM_BOX, GM_CROSS)

	# Plot the fit.
	start = false
	finish = false
	if (! IS_INDEFR(bscale) && ! IS_INDEFR(bzero)) {
	    y = bscale * dxmin + bzero
	    if (y >= ymin && y <= ymax) {
		call gamove (gd, dxmin, y)
		start = true
	    }
	    y = bscale * dxmax + bzero
	    if (y >= ymin && y <= ymax) {
		if (start) {
		    call gadraw (gd, dxmax, y)
		    finish = true
		} else {
		    call gamove (gd, dxmax, y)
		    start = true
		}
	    }
	    x = (dymin - bzero) / bscale
	    if (x >= xmin && x <= xmax) {
		if (! start) {
		    call gamove (gd, x, dymin)
		    start = true
		} else if (! finish) {
		    call gadraw (gd, x, dymin)
		    finish = true
		}
	    }
	    x = (dymax - bzero) / bscale
	    if (x >= xmin && x <= xmax) {
		if (! start) {
		    call gamove (gd, x, dymax)
		    start = true
		} else if (! finish) {
		    call gadraw (gd, x, dymax)
		    finish = true
		}
	    }
	}

	call sfree (sp)

	return (OK)
end


# RG_MSRPLOT -- Plot the magnitude and sky values of the regions.

int procedure rg_msrplot (gd, ls, udelete, bscale, bzero)

pointer	gd			#I pointer to the graphics stream
pointer	ls			#I pointer to the linmatch structure
int	udelete[ARB]		#I the user deletions array
real	bscale			#I the fitted bscale value
real	bzero			#I the fitted bzero value

int	nregions
pointer	sp, yreg, title, str, imager, image1
real	xmin, xmax, ymin, ymax, diff, dmin, dmax
int	rg_lstati()
pointer	rg_lstatp()

begin
	nregions = rg_lstati (ls, NREGIONS)
	if (nregions <= 0)
	    return (ERR)

	# Allocate working space.
	call smark (sp)
	call salloc (yreg, nregions, TY_REAL)
	call salloc (title, 2 * SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (imager, SZ_FNAME, TY_CHAR)
	call salloc (image1, SZ_FNAME, TY_CHAR)
	call rg_lstats (ls, REFIMAGE, Memc[imager], SZ_FNAME)
	call rg_lstats (ls, PHOTFILE, Memc[image1], SZ_FNAME)

	call gclear (gd)

	# Determine the limits of the bscale value versus region.
	call alimr (Memr[rg_lstatp(ls,IMAG)], nregions, xmin, xmax)
	diff = xmax - xmin
	if (diff <= 0.0)
	    diff = MINFRACTION
	else
	    diff = max (diff, MINFRACTION * (xmax + xmin) / 2.0)
	dmin = xmin
	dmax = xmax
	xmin = xmin - FRACTION * diff
	xmax = xmax + FRACTION * diff
	if (bscale > 0) {
	    call aaddkr (Memr[rg_lstatp(ls,IMAG)], -2.5*log10(bscale),
	        Memr[yreg], nregions)
	    call asubr (Memr[rg_lstatp(ls,RMAG)], Memr[yreg], Memr[yreg],
		nregions)
	} else
	    call asubr (Memr[rg_lstatp(ls,RMAG)], Memr[rg_lstatp(ls,IMAG)],
	        Memr[yreg], nregions)
	call alimr (Memr[yreg], nregions, ymin, ymax)
	diff = ymax - ymin
	if (diff <= 0.0)
	    diff = MINFRACTION
	else
	    diff = max (diff, MINFRACTION * (ymax + ymin) / 2.0)
	ymin = ymin - FRACTION * diff
	ymax = ymax + FRACTION * diff
	call gseti (gd, G_WCS, 1)
	call gsview (gd, 0.15, 0.9, 0.6, 0.9)
	call gswind (gd, xmin, xmax, ymin, ymax)
	call rg_pfill (gd, xmin, xmax, ymin, ymax, GF_SOLID, 0)

	# Create the title for bscale versus region.
	call sprintf (Memc[str], SZ_LINE,
	    "Reference: %s  Input: %s  Bscale: %g")
	    call pargstr (Memc[imager])
	    call pargstr (Memc[image1])
	    call pargr (bscale)
	call sprintf (Memc[title], 2 * SZ_LINE,
	    "Residuals for %s vs. Magnitudes for %s\n%s\n")
	    call pargstr (Memc[imager])
	    call pargstr (Memc[image1])
	    call pargstr (Memc[str])
	call glabax (gd, Memc[title], "Input Magnitudes",
	    "Mag Residuals")

	# Plot the points.
	call rg_lxyplot (gd, Memr[rg_lstatp(ls,IMAG)], Memr[yreg],
	    Memi[rg_lstatp(ls, RDELETE)], udelete, nregions, GM_BOX, GM_CROSS)

	# Plot the fit.
	if (bscale > 0.0) {
	    call gamove (gd, xmin, 0.0)
	    call gadraw (gd, xmax, 0.0)
	}

	# Determine the limits of the bscale value versus region.
	call alimr (Memr[rg_lstatp(ls,ISKY)], nregions, xmin, xmax)
	diff = xmax - xmin
	if (diff <= 0.0)
	    diff = MINFRACTION
	else
	    diff = max (diff, MINFRACTION * (xmax + xmin) / 2.0)
	dmin = xmin
	dmax = xmax
	xmin = xmin - FRACTION * diff
	xmax = xmax + FRACTION * diff
	call altmr (Memr[rg_lstatp(ls,ISKY)], Memr[yreg], nregions,
	    bscale, bzero)
	call asubr (Memr[rg_lstatp(ls,RSKY)], Memr[yreg], Memr[yreg],
	    nregions)
	call alimr (Memr[yreg], nregions, ymin, ymax)
	diff = ymax - ymin
	if (diff <= 0.0)
	    diff = MINFRACTION
	else
	    diff = max (diff, MINFRACTION * (ymax + ymin) / 2.0)
	ymin = ymin - FRACTION * diff
	ymax = ymax + FRACTION * diff
	call gseti (gd, G_WCS, 2)
	call gsview (gd, 0.15, 0.9, 0.1, 0.4)
	call gswind (gd, xmin, xmax, ymin, ymax)
	call rg_pfill (gd, xmin, xmax, ymin, ymax, GF_SOLID, 0)

	# Create the title for bscale versus region.
	call sprintf (Memc[str], SZ_LINE,
	    "Reference: %s  Input: %s  Bscale: %g Bzero: %g")
	    call pargstr (Memc[imager])
	    call pargstr (Memc[image1])
	    call pargr (bscale)
	    call pargr (bzero)
	call sprintf (Memc[title], 2 * SZ_LINE,
	    "Residuals for %s vs. Sky Values for %s\n%s\n")
	    call pargstr (Memc[imager])
	    call pargstr (Memc[image1])
	    call pargstr (Memc[str])
	call glabax (gd, Memc[title], "Input Sky Values",
	    "Sky Residuals")

	# Plot the points.
	call rg_lxyplot (gd, Memr[rg_lstatp(ls,ISKY)], Memr[yreg],
	    Memi[rg_lstatp(ls, RDELETE)], udelete, nregions, GM_BOX, GM_CROSS)

	# Plot the fit.
	call gamove (gd, xmin, 0.0)
	call gadraw (gd, xmax, 0.0)

	call sfree (sp)

	return (OK)
end


# RG_LHBOX -- Draw a stepped curve of the histogram data.

procedure rg_lhbox (gp, ydata, npts, x1, x2)

pointer gp              #I the graphics descriptor
real    ydata[ARB]      #I the y coordinates of the line endpoints
int     npts            #I the number of line endpoints
real    x1, x2          #I starting and ending x coordinates

int     pixel
real    left, right, top, bottom, x, y, dx

begin
        call ggwind (gp, left, right, bottom, top)
        dx = (x2 - x1) / npts

        # Do the first vertical line.
        call gamove (gp, x1, bottom)
        call gadraw (gp, x1, ydata[1])

        # Do the first horizontal line.
        call gadraw (gp, x1 + dx, ydata[1])

        # Draw the remaining horizontal lines.
        do pixel = 2, npts {
            x = x1 + dx * (pixel - 1)
            y = ydata[pixel]
            call gadraw (gp, x, y)
            call gadraw (gp, x + dx, y)
        }

        # Draw the last vertical line.
        call gadraw (gp, x + dx, bottom)
end


# RG_PFILL -- Fill a rectangular area with a given style and color.

procedure rg_pfill (gd, xmin, xmax, ymin, ymax, fstyle, fcolor)

pointer gd                      #I pointer to the graphics stream
real    xmin, xmax              #I the x coordinate limits
real    ymin, ymax              #I the y coordinate limits
int     fstyle                  #I the fill style
int     fcolor                  #I the fill color

real    x[4], y[4]

begin
        call gseti (gd, G_FACOLOR, fcolor)
        x[1] = xmin; y[1] = ymin
        x[2] = xmax; y[2] = ymin
        x[3] = xmax; y[3] = ymax
        x[4] = xmin; y[4] = ymax
        call gfill (gd, x, y, 4, fstyle)
end


# XP_LXYPLOT -- Plot the x and y points.

procedure rg_lxyplot (gd, x, y, del, udel, npts, gmarker, dmarker)

pointer gd              # pointer to the graphics stream
real    x[ARB]          # the x coordinates
real    y[ARB]          # the y coordinates
int	del[ARB]	# the deletions array
int	udel[ARB]	# the user deletions array
int     npts            # the number of points to be marked
int     gmarker         # the good point marker type
int     dmarker         # the deleted point marker type

int     i

begin
        # Plot the points.
        do i = 1, npts {
	    if (udel[i] == YES) {
                call gmark (gd, x[i], y[i], gmarker, 2.0, 2.0)
                call gmark (gd, x[i], y[i], dmarker, 2.0, 2.0)
	    } else if (del[i] != LS_NO)
                call gmark (gd, x[i], y[i], dmarker, 2.0, 2.0)
	    else
                call gmark (gd, x[i], y[i], gmarker, 2.0, 2.0)
	}
end


# XP_RIPLOT -- Plot the reference image intensity versus the input image
# intensity.

procedure rg_riplot (gd, x, y, resid, npts, datamin, datamax, loreject,
	hireject, gmarker, dmarker)

pointer gd              #I pointer to the graphics stream
real    x[ARB]          #I the x coordinates
real    y[ARB]          #I the y coordinates
real	resid[ARB]	#I the residuals array
int     npts            #I the number of points to be marked
real	datamin		#I the good data minimum
real	datamax		#I the good data maximum
real	loreject	#I the low side rejection limit
real	hireject	#I the high side rejection limit
int     gmarker         #I the good point marker type
int     dmarker         #I the deleted point marker type

int	i

begin
	do i = 1, npts {
	    if (x[i] < datamin || x[i] > datamax)
		call gmark (gd, x[i], y[i], dmarker, 2.0, 2.0)
	    else if (y[i] < datamin || y[i] > datamax)
		call gmark (gd, x[i], y[i], dmarker, 2.0, 2.0)
	    else if (resid[i] < loreject || resid[i] > hireject)
		call gmark (gd, x[i], y[i], dmarker, 2.0, 2.0)
	    else
		call gmark (gd, x[i], y[i], gmarker, 2.0, 2.0)
	}
end


# XP_RRIPLOT -- Plot the reference image intensity versus the input image
# intensity.

procedure rg_rriplot (gd, x, y, resid, npts, datamin, datamax, loreject,
	hireject, gmarker, dmarker)

pointer gd              #I pointer to the graphics stream
real    x[ARB]          #I the x coordinates
real    y[ARB]          #I the y coordinates
real	resid[ARB]	#I the residuals array
int     npts            #I the number of points to be marked
real	datamin		#I the good data minimum
real	datamax		#I the good data maximum
real	loreject	#I the low side rejection limit
real	hireject	#I the high side rejection limit
int     gmarker         #I the good point marker type
int     dmarker         #I the deleted point marker type

int	i

begin
	do i = 1, npts {
	    if (x[i] < datamin || x[i] > datamax)
		call gmark (gd, x[i], resid[i], dmarker, 2.0, 2.0)
	    else if (y[i] < datamin || y[i] > datamax)
		call gmark (gd, x[i], resid[i], dmarker, 2.0, 2.0)
	    else if (IS_INDEFR(resid[i]))
		call gmark (gd, x[i], resid[i], dmarker, 2.0, 2.0)
	    else if (resid[i] < loreject || resid[i] > hireject)
		call gmark (gd, x[i], resid[i], dmarker, 2.0, 2.0)
	    else
		call gmark (gd, x[i], resid[i], gmarker, 2.0, 2.0)
	}
end


# RG_GALIMR -- Compute the good data limits for the plot.

procedure rg_galimr (a, index, npts, amin, amax)

real	a[ARB]			#I the input array
int	index[ARB]		#I the index array
int	npts			#I the size of the array
real	amin, amax		#O the output min and max values

int	i
real	dmin, dmax, gmin, gmax

begin
	dmin = a[1]; dmax = a[1]
	gmin = MAX_REAL; gmax = -MAX_REAL

	do i = 1, npts {
	    if (a[i] < dmin)
		dmin = a[i]
	    else if (a[i] > dmax)
		dmax = a[i]
	    if (index[i] == LS_NO) {
	        if (a[i] < gmin)
		    gmin = a[i]
	        if (a[i] > gmax)
		    gmax = a[i]
	    }
	}

	if (gmin == MAX_REAL)
	    amin = dmin
	else
	    amin = gmin
	if (gmax == -MAX_REAL)
	    amax = dmax
	else
	    amax = gmax
end
