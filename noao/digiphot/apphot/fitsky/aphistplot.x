include <gset.h>
include <pkg/gtools.h>
include "../lib/fitsky.h"

# AP_HISTPLOT -- Procedure to the sky value using a plot of the sky histogram
# and cursor readback.

int procedure ap_histplot (gd, gt, skypix, wgt, index, nskypix, k1, hwidth,
	binsize, smooth, sky_mode, sky_sigma, sky_skew, nsky, nsky_reject)

pointer	gd			# pointer to graphics stream
pointer	gt			# pointer to GTOOLS structure
real	skypix[ARB]		# array of unsorted sky pixels
real	wgt[ARB]		# array of weights for rejection
int	index[ARB]		# array of sort indices
int	nskypix			# number of sky pixels
real	k1			# rejection criterion
real	hwidth			# half width of histogram in k1 units
real	binsize			# histogram binsize in units of sigma
int	smooth			# smooth the histogram
real	sky_mode		# sky value
real	sky_sigma		# sigma of sky pixels
real	sky_skew		# skew of sky pixels
int	nsky			# number of sky pixels
int	nsky_reject		# number of rejected sky pixels

double	dsky, sumpx, sumsqpx, sumcbpx
int	i, nbins, nker, wcs, key
pointer	sp, x, hgm, shgm, cmd
real	dmin, dmax, hmin, hmax, dh, ymin, ymax, symin, symax, wx, wy
real	u1, u2, v1, v2, x1, x2, y1, y2, cut, sky_mean, sky_zero
int	clgcur(), aphigmr()
real	apmedr(), ap_asumr()

begin
	# Check for valid graphics stream.
	if (gd == NULL)
	    return (AP_NOGRAPHICS)

	# Initialize.
	nsky = nskypix
	nsky_reject = 0
	sky_mode = INDEFR
	sky_sigma = INDEFR
	sky_skew = INDEFR
	if (nskypix <= 0)
	    return (AP_SKY_OUTOFBOUNDS)

	# Compute an initial guess at the sky distribution.
	sky_zero = ap_asumr (skypix, index, nskypix) / nskypix
	call ap_ialimr (skypix, index, nskypix, dmin, dmax)
	call apfimoments (skypix, index, nskypix, sky_zero, sumpx, sumsqpx,
	    sumcbpx, sky_mean, sky_sigma, sky_skew)
	sky_mode = apmedr (skypix, index, nskypix)
	sky_mode = max (dmin, min (sky_mode, dmax))

	# Compute histogram width and binsize.
	if (! IS_INDEFR(hwidth) && hwidth > 0.0) {
	    hmin = sky_mode - k1 * hwidth
	    hmax = sky_mode + k1 * hwidth
	    dh = binsize * hwidth
	} else {
	    cut = min (sky_mode - dmin, dmax - sky_mode, k1 * sky_sigma)
	    hmin = sky_mode - cut
	    hmax = sky_mode + cut
	    dh = binsize * cut / k1
	}

	# Compute the number of histgram bins and the histogram resolution.
	if (dh <= 0.0) {
	    nbins = 1
	    dh = 0.0
	} else {
	    nbins = 2 * nint ((hmax - sky_mode) / dh) + 1
	    dh = (hmax - hmin) / (nbins - 1)
	}

	# Test for a valid histogram.
	if (dh <= 0.0 || k1 <= 0.0 || sky_sigma <= 0.0 || sky_sigma <= dh ||
	    nbins < 2) {
	    sky_mode = sky_mode
	    sky_sigma = 0.0
	    sky_skew = 0.0
	    return (AP_NOHISTOGRAM)
	}

	# Allocate temporary space.
	call smark (sp)
	call salloc (x, nbins, TY_REAL)
	call salloc (hgm, nbins, TY_REAL)
	call salloc (shgm, nbins, TY_REAL)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Compute the x array and accumulate the histogram.
	do i = 1, nbins
	    Memr[x+i-1] = i
	call amapr (Memr[x], Memr[x], nbins, 1.0, real (nbins),
	    hmin + 0.5 * dh, hmax + 0.5 * dh) 
	call aclrr (Memr[hgm], nbins)
	nsky_reject = nsky_reject + aphigmr (skypix, wgt, index, nskypix,
	    Memr[hgm], nbins, hmin, hmax)
	nsky = nskypix - nsky_reject

	# Subtract rejected pixels and recompute the moments.
	if (nsky_reject > 0) {
	    do i = 1, nskypix {
	        if (wgt[index[i]] <= 0.0) {
		    dsky = skypix[index[i]] - sky_zero
		    sumpx = sumpx - dsky
		    sumsqpx = sumsqpx - dsky ** 2
		    sumcbpx = sumcbpx - dsky ** 3
	        }
	    }
	}
	call apmoments (sumpx, sumsqpx, sumcbpx, nsky, sky_zero, sky_mean,
	    sky_sigma, sky_skew)

	# Smooth the histogram and compute the histogram plot limits.
	if (smooth == YES) {
	    nker = max (1, nint (sky_sigma / dh))
	    #call ap_lucy_smooth (Memr[hgm], Memr[shgm], nbins, nker, 2)
	    call ap_bsmooth (Memr[hgm], Memr[shgm], nbins, nker, 2)
	    call alimr (Memr[hgm], nbins, ymin, ymax)
	    call alimr (Memr[shgm], nbins, symin, symax)
	    ymin = min (ymin, symin)
	    ymax = max (ymax, symax)
	} else
	    call alimr (Memr[hgm], nbins, ymin, ymax)

	# Store the old viewport and window coordinates.
	call greactivate (gd, 0)
	call ggview (gd, u1, u2, v1, v2)
	call ggwind (gd, x1, x2, y1, y2)

	# Plot the raw and smoothed histograms.
	call ap_set_hplot (gd, gt, hmin, hmax, ymin, ymax, nbins, smooth)
	if (smooth == YES) {
	    call ap_plothist (gd, gt, Memr[x], Memr[hgm], nbins, "histogram",
	        GL_SOLID)
	    call ap_plothist (gd, gt, Memr[x], Memr[shgm], nbins, "line",
	        GL_SOLID)
	} else
	    call ap_plothist (gd, gt, Memr[x], Memr[hgm], nbins, "histogram",
	        GL_SOLID)

	# Mark the peak of the histogram with the cursor.
	call printf (
	    "Mark histogram peak (%g) [space=mark,q=quit,:.help=help:]")
	    call pargr (sky_mode)
	call gscur (gd, sky_mode, (ymin + ymax) / 2.0)
	while (clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {
		if (key == 'q')
		    break
		else
		    sky_mode = max (hmin, min (wx, hmax))
	    call printf (
	        "Mark histogram peak (%g) [space=mark,q=quit,:.help=help:]")
	        call pargr (sky_mode)
	}

	# Store the old viewport and window coordinates.
	call gsview (gd, u1, u2, v1, v2)
	call gswind (gd, x1, x2, y1, y2)

	# Close up.
	call gflush (gd)
	call gdeactivate (gd, 0)
	call sfree (sp)
	return (AP_OK)
end


# AP_PLOTHIST -- Procedure plot the histogram.

procedure ap_plothist (gd, gt, x, hgm, nbins, plottype, polytype) 

pointer	gd		# pointer to graphics stream
pointer	gt		# GTOOLS pointer
real	x[ARB]		# the histogram bin values
real	hgm[ARB]	# histogram
int	nbins		# number of bins
char	plottype[ARB]	# the plot type "histogram" or "line"
int	polytype	# polyline type

begin
	call gt_sets (gt, GTTYPE, plottype)
	call gt_seti (gt, GTLINE, polytype)
	call gt_plot (gd, gt, x, hgm, nbins)
	call gflush (gd)
end


# AP_SET_HPLOT -- Procedure to set up the histogram plot.

procedure ap_set_hplot (gd, gt, xmin, xmax, ymin, ymax, nbins, smooth)

pointer	gd		# pointer to GRAPHICS stream
pointer	gt		# pointer to GTOOLS structure
real	xmin, xmax	# min and max of x vector
real	ymin, ymax	# min and max of y vector
int	nbins		# number of bins
int	smooth		# smooth histogram

pointer	sp, str

begin
	# Initialize
	call gclear (gd)
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Set up the gtools parameter string.
	call sprintf (Memc[str], SZ_LINE,
	    "Sky Histogram: nbins = %d hmin = %g hmax = %g smooth=%b")
	    call pargi (nbins)
	    call pargr (xmin)
	    call pargr (xmax)
	    call pargi (smooth)
	call gt_sets (gt, GTPARAMS, Memc[str])

	# Set up the plot axes.
	call gt_sets (gt, GTXLABEL, "Sky Values")
	call gt_sets (gt, GTYLABEL, "Number of Pixels")
	call gt_setr (gt, GTXMIN, xmin)
	call gt_setr (gt, GTXMAX, xmax)
	call gt_setr (gt, GTYMIN, ymin)
	call gt_setr (gt, GTYMAX, ymax)
	call gt_swind (gd, gt)
	call gt_labax (gd, gt)

	call sfree (sp)
end
