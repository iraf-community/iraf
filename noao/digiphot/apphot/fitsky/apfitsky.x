include "../lib/apphotdef.h"
include "../lib/apphot.h"
include "../lib/noisedef.h"
include "../lib/fitskydef.h"
include "../lib/fitsky.h"

# APFITSKY -- Procedure to the compute the sky value in an annular region
# around a given position in the IRAF image.

int procedure apfitsky (ap, im, wx, wy, sd, gd)

pointer	ap		# pointer to the apphot structure
pointer	im		# pointer to the IRAF image
real	wx		# object x coordinate
real	wy		# object y coordinate
int	sd		# pointer to input text file containing sky values
pointer	gd		# pointer to graphics stream

int	ier, nclip, nsky, ilo, ihi
pointer	sky, nse, gt
real	x, y
int	apskybuf(), ap_mode(), ap_centroid(), ap_histplot(), ap_readsky()
int	ap_median(), ap_radplot(), ap_gauss(), ap_lgsky(), ap_crosscor()
int	ap_mean(), ap_clip()
pointer	ap_gtinit()

begin
	# Initialize.
	sky = AP_PSKY(ap)
	nse = AP_NOISE(ap)
	AP_SXCUR(sky) = wx
	AP_SYCUR(sky) = wy
        if (IS_INDEFR(wx) || IS_INDEFR(wy)) {
            AP_OSXCUR(sky) = INDEFR
            AP_OSYCUR(sky) = INDEFR
        } else {
            switch (AP_WCSOUT(ap)) {
            case WCS_WORLD, WCS_PHYSICAL:
                call ap_ltoo (ap, wx, wy, AP_OSXCUR(sky), AP_OSYCUR(sky), 1)
            case WCS_TV:
                call ap_ltov (im, wx, wy, AP_OSXCUR(sky), AP_OSYCUR(sky), 1)
            default:
                AP_OSXCUR(sky) = wx
                AP_OSYCUR(sky) = wy
            }
        }

	AP_SKY_MODE(sky) = INDEFR
	AP_SKY_SIG(sky) = INDEFR
	AP_SKY_SKEW(sky) = INDEFR
	AP_NSKY(sky) = 0
	AP_NSKY_REJECT(sky) = 0
	if (IS_INDEFR(wx) || IS_INDEFR(wy))
	    return (AP_NOSKYAREA)

	switch (AP_SKYFUNCTION(sky)) {

	case AP_MEAN:

	    # Fetch  the sky pixels.
	    ier = apskybuf (ap, im, wx, wy)
	    if (ier != AP_OK)
		return (ier)

	    # Initialze the weights.
	    call amovkr (1.0, Memr[AP_SWGT(sky)], AP_NSKYPIX(sky))

	    # Clip the data.
	    if (AP_SLOCLIP(sky) > 0.0 || AP_SHICLIP(sky) > 0.0) {
		nclip = ap_clip (Memr[AP_SKYPIX(sky)], Memi[AP_INDEX(sky)],
		    AP_NSKYPIX(sky), AP_SLOCLIP(sky), AP_SHICLIP(sky), ilo,
		    ihi)
		if (nclip >= AP_NSKYPIX(sky))
		    return (AP_NSKY_TOO_SMALL)
		nsky = AP_NSKYPIX(sky) - nclip
	    } else {
		nclip = 0
		call ap_index (Memi[AP_INDEX(sky)], AP_NSKYPIX(sky))
		ilo = 1
		nsky = AP_NSKYPIX(sky) 
	    }

	    # Compute the mean of the sky pixel distribution with pixel
	    # rejection and region growing.
	    ier = ap_mean (Memr[AP_SKYPIX(sky)], Memi[AP_COORDS(sky)],
	        Memr[AP_SWGT(sky)], Memi[AP_INDEX(sky)+ilo-1],
		nsky, AP_SNX(sky), AP_SNY(sky), AP_SLOREJECT(sky),
		AP_SHIREJECT(sky), AP_RGROW(sky) * AP_SCALE(ap),
		AP_SNREJECT(sky), AP_SKY_MODE(sky), AP_SKY_SIG(sky),
		AP_SKY_SKEW(sky), AP_NSKY(sky), AP_NSKY_REJECT(sky))
		AP_NSKY_REJECT(sky) = AP_NBADSKYPIX(sky) + nclip +
		    AP_NSKY_REJECT(sky)

	    return (ier)

	case AP_MEDIAN:

	    # Fetch  the sky pixels.
	    ier = apskybuf (ap, im, wx, wy)
	    if (ier != AP_OK)
		return (ier)

	    # Initialze the weights.
	    call amovkr (1.0, Memr[AP_SWGT(sky)], AP_NSKYPIX(sky))

	    # Clip the data.
	    if (AP_SLOCLIP(sky) > 0.0 || AP_SHICLIP(sky) > 0.0) {
		nclip = ap_clip (Memr[AP_SKYPIX(sky)], Memi[AP_INDEX(sky)],
		    AP_NSKYPIX(sky), AP_SLOCLIP(sky), AP_SHICLIP(sky), ilo,
		    ihi)
		if (nclip >= AP_NSKYPIX(sky))
		    return (AP_NSKY_TOO_SMALL)
		nsky = AP_NSKYPIX(sky) - nclip
	    } else {
		nclip = 0
		call apqsort (Memr[AP_SKYPIX(sky)], Memi[AP_INDEX(sky)],
		    Memi[AP_INDEX(sky)], AP_NSKYPIX(sky))
		ilo = 1
		nsky = AP_NSKYPIX(sky) 
	    }

	    # Compute the median of the sky pixel distribution with pixel
	    # rejection and region growing.
	    ier = ap_median (Memr[AP_SKYPIX(sky)], Memi[AP_COORDS(sky)],
	        Memr[AP_SWGT(sky)], Memi[AP_INDEX(sky)+ilo-1], nsky,
		AP_SNX(sky), AP_SNY(sky), AP_SLOREJECT(sky),
		AP_SHIREJECT(sky), AP_RGROW(sky) * AP_SCALE(ap),
		AP_SNREJECT(sky), AP_SKY_MODE(sky), AP_SKY_SIG(sky),
		AP_SKY_SKEW(sky), AP_NSKY(sky), AP_NSKY_REJECT(sky))
	    AP_NSKY_REJECT(sky) = AP_NBADSKYPIX(sky) + nclip +
	        AP_NSKY_REJECT(sky)

	    return (ier)

	case AP_MODE:

	    # Fetch the sky pixels.
	    ier = apskybuf (ap, im, wx, wy)
	    if (ier != AP_OK)
		return (ier)

	    # Initialze the weights.
	    call amovkr (1.0, Memr[AP_SWGT(sky)], AP_NSKYPIX(sky))

	    # Clip the data.
	    if (AP_SLOCLIP(sky) > 0.0 || AP_SHICLIP(sky) > 0.0) {
		nclip = ap_clip (Memr[AP_SKYPIX(sky)], Memi[AP_INDEX(sky)],
		    AP_NSKYPIX(sky), AP_SLOCLIP(sky), AP_SHICLIP(sky), ilo,
		    ihi)
		if (nclip >= AP_NSKYPIX(sky))
		    return (AP_NSKY_TOO_SMALL)
		nsky = AP_NSKYPIX(sky) - nclip
	    } else {
		nclip = 0
		call apqsort (Memr[AP_SKYPIX(sky)], Memi[AP_INDEX(sky)],
		    Memi[AP_INDEX(sky)], AP_NSKYPIX(sky))
		ilo = 1
		nsky = AP_NSKYPIX(sky) 
	    }

	    # Compute the median of the sky pixel distribution with pixel
	    # rejection and region growing.
	    ier = ap_mode (Memr[AP_SKYPIX(sky)], Memi[AP_COORDS(sky)],
	        Memr[AP_SWGT(sky)], Memi[AP_INDEX(sky)+ilo-1], nsky,
		AP_SNX(sky), AP_SNY(sky), AP_SLOREJECT(sky),
		AP_SHIREJECT(sky), AP_RGROW(sky) * AP_SCALE(ap),
		AP_SNREJECT(sky), AP_SKY_MODE(sky), AP_SKY_SIG(sky),
		AP_SKY_SKEW(sky), AP_NSKY(sky), AP_NSKY_REJECT(sky))
	    AP_NSKY_REJECT(sky) = AP_NBADSKYPIX(sky) + nclip +
	        AP_NSKY_REJECT(sky)

	    return (ier)

	case AP_CENTROID:

	    # Fetch the sky pixels.
	    ier = apskybuf (ap, im, wx, wy)
	    if (ier != AP_OK)
		return (ier)

	    # Initialze the weights.
	    call amovkr (1.0, Memr[AP_SWGT(sky)], AP_NSKYPIX(sky))

	    # Clip the data.
	    if (AP_SLOCLIP(sky) > 0.0 || AP_SHICLIP(sky) > 0.0) {
		nclip = ap_clip (Memr[AP_SKYPIX(sky)], Memi[AP_INDEX(sky)],
		    AP_NSKYPIX(sky), AP_SLOCLIP(sky), AP_SHICLIP(sky), ilo,
		    ihi)
		if (nclip >= AP_NSKYPIX(sky))
		    return (AP_NSKY_TOO_SMALL)
		nsky = AP_NSKYPIX(sky) - nclip
	    } else {
		nclip = 0
		call ap_index (Memi[AP_INDEX(sky)], AP_NSKYPIX(sky))
		ilo = 1
		nsky = AP_NSKYPIX(sky) 
	    }

	    # Compute the sky value by performing a moment analysis of the
	    # sky pixel histogram.
	    ier = ap_centroid (Memr[AP_SKYPIX(sky)], Memi[AP_COORDS(sky)],
	        Memr[AP_SWGT(sky)], Memi[AP_INDEX(sky)+ilo-1], nsky,
		AP_SNX(sky), AP_SNY(sky), AP_K1(sky), INDEFR,
		AP_BINSIZE(sky), AP_SMOOTH(sky), AP_SLOREJECT(sky),
		AP_SHIREJECT(sky), AP_RGROW(sky) * AP_SCALE(ap),
		AP_SMAXITER(sky), AP_SKY_MODE(sky), AP_SKY_SIG(sky),
		AP_SKY_SKEW(sky), AP_NSKY(sky), AP_NSKY_REJECT(sky))
	    AP_NSKY_REJECT(sky) = AP_NBADSKYPIX(sky) + nclip +
	        AP_NSKY_REJECT(sky)

	    return (ier)

	case AP_CONSTANT:

	    # Set the sky value to a constant.
	    AP_SKY_MODE(sky) = AP_SKYBACKGROUND(sky)
	    AP_SKY_SIG(sky) = AP_SKYSIGMA(nse)
	    AP_SKY_SKEW(sky) = INDEFR
	    AP_NSKY(sky) = 0
	    AP_NSKY_REJECT(sky) = 0
	    return (AP_OK)

	case AP_SKYFILE:

	    # Read the sky values from a file.
	    if (sd == NULL)
		return (AP_NOSKYFILE)
	    ier = ap_readsky (sd, x, y, AP_SKY_MODE(sky), AP_SKY_SIG(sky),
		AP_SKY_SKEW(sky), AP_NSKY(sky), AP_NSKY_REJECT(sky))
	    if (ier == EOF)
		return (AP_EOFSKYFILE)
	    else if (ier != 7)
		return (AP_BADSKYSCAN)
	    else if (AP_NSKY(sky) <= 0)
		return (AP_NOSKYAREA)
	    else
		return (AP_OK)

	case AP_RADPLOT:

	    # Check the status of the graphics stream.
	    if (gd == NULL)
		return (AP_NOGRAPHICS)

	    # Fetch the sky pixels.
	    ier = apskybuf (ap, im, wx, wy)
	    if (ier != AP_OK)
		return (ier)

	    # Clip the data.
	    if (AP_SLOCLIP(sky) > 0.0 || AP_SHICLIP(sky) > 0.0) {
		nclip = ap_clip (Memr[AP_SKYPIX(sky)], Memi[AP_INDEX(sky)],
		    AP_NSKYPIX(sky), AP_SLOCLIP(sky), AP_SHICLIP(sky), ilo,
		    ihi)
		if (nclip >= AP_NSKYPIX(sky))
		    return (AP_NSKY_TOO_SMALL)
		nsky = AP_NSKYPIX(sky) - nclip
	    } else {
		nclip = 0
		call ap_index (Memi[AP_INDEX(sky)], AP_NSKYPIX(sky))
		ilo = 1
		nsky = AP_NSKYPIX(sky) 
	    }

	    # Mark the sky level on the radial profile plot.
	    call gactivate (gd, 0)
	    gt = ap_gtinit (AP_IMROOT(ap), wx, wy)
	    ier = ap_radplot (gd, gt, Memr[AP_SKYPIX(sky)],
	        Memi[AP_COORDS(sky)], Memi[AP_INDEX(sky)+ilo-1], nsky,
		AP_SXC(sky), AP_SYC(sky), AP_SNX(sky), AP_SNY(sky),
		AP_SCALE(ap), AP_SKY_MODE(sky), AP_SKY_SKEW(sky),
		AP_SKY_SIG(sky), AP_NSKY(sky), AP_NSKY_REJECT(sky))
	    AP_NSKY_REJECT(sky) = AP_NBADSKYPIX(sky) + nclip +
	        AP_NSKY_REJECT(sky)
	    call ap_gtfree (gt)
	    call gdeactivate (gd, 0)

	    return (ier)

	case AP_HISTPLOT:

	    # Check the status of the graphics stream.
	    if (gd == NULL)
		return (AP_NOGRAPHICS)

	    # Fetch the sky pixels.
	    ier = apskybuf (ap, im, wx, wy)
	    if (ier != AP_OK)
		return (ier)

	    # Initialze the weights.
	    call amovkr (1.0, Memr[AP_SWGT(sky)], AP_NSKYPIX(sky))

	    # Clip the data.
	    if (AP_SLOCLIP(sky) > 0.0 || AP_SHICLIP(sky) > 0.0) {
		nclip = ap_clip (Memr[AP_SKYPIX(sky)], Memi[AP_INDEX(sky)],
		    AP_NSKYPIX(sky), AP_SLOCLIP(sky), AP_SHICLIP(sky),
		    ilo, ihi)
		if (nclip >= AP_NSKYPIX(sky))
		    return (AP_NSKY_TOO_SMALL)
		nsky = AP_NSKYPIX(sky) - nclip
	    } else {
		nclip = 0
		call ap_index (Memi[AP_INDEX(sky)], AP_NSKYPIX(sky))
		ilo = 1
		nsky = AP_NSKYPIX(sky) 
	    }

	    # Mark the peak of the histogram on the histogram plot.
	    #call gactivate (gd, 0)
	    gt = ap_gtinit (AP_IMROOT(ap), wx, wy)
	    ier = ap_histplot (gd, gt, Memr[AP_SKYPIX(sky)],
	        Memr[AP_SWGT(sky)], Memi[AP_INDEX(sky)+ilo-1], nsky,
		AP_K1(sky), INDEFR, AP_BINSIZE(sky), AP_SMOOTH(sky),
		AP_SKY_MODE(sky), AP_SKY_SIG(sky), AP_SKY_SKEW(sky),
		AP_NSKY(sky), AP_NSKY_REJECT(sky))	
	    AP_NSKY_REJECT(sky) = AP_NBADSKYPIX(sky) + nclip +
	        AP_NSKY_REJECT(sky)
	    call ap_gtfree (gt)
	    #call gdeactivate (gd, 0)

	    return (ier)

	case AP_OFILT:

	    # Fetch the sky pixels.
	    ier = apskybuf (ap, im, wx, wy)
	    if (ier != AP_OK)
		return (ier)

	    # Initialze the weights.
	    call amovkr (1.0, Memr[AP_SWGT(sky)], AP_NSKYPIX(sky))

	    # Clip the data.
	    if (AP_SLOCLIP(sky) > 0.0 || AP_SHICLIP(sky) > 0.0) {
		nclip = ap_clip (Memr[AP_SKYPIX(sky)], Memi[AP_INDEX(sky)],
		    AP_NSKYPIX(sky), AP_SLOCLIP(sky), AP_SHICLIP(sky),
		    ilo, ihi)
		if (nclip >= AP_NSKYPIX(sky))
		    return (AP_NSKY_TOO_SMALL)
		nsky = AP_NSKYPIX(sky) - nclip
	    } else {
		nclip = 0
		call ap_index (Memi[AP_INDEX(sky)], AP_NSKYPIX(sky))
		ilo = 1
		nsky = AP_NSKYPIX(sky) 
	    }

	    # Compute the sky value using the histogram of the sky pixels
	    # and a variation of the optimal filtering technique.
	    ier = ap_lgsky (Memr[AP_SKYPIX(sky)], Memi[AP_COORDS(sky)],
	        Memr[AP_SWGT(sky)], Memi[AP_INDEX(sky)+ilo-1], nsky,
		AP_SNX(sky), AP_SNY(sky), AP_K1(sky), INDEFR,
		AP_BINSIZE(sky), AP_SMOOTH(sky), AP_SLOREJECT(sky),
		AP_SHIREJECT(sky), AP_RGROW(sky) * AP_SCALE(ap),
		AP_SMAXITER(sky), AP_SKY_MODE(sky), AP_SKY_SIG(sky),
		AP_SKY_SKEW(sky), AP_NSKY(sky), AP_NSKY_REJECT(sky))
	    AP_NSKY_REJECT(sky) = AP_NBADSKYPIX(sky) + nclip +
	        AP_NSKY_REJECT(sky)

	    return (ier)

	case AP_GAUSS:

	    # Fetch the sky pixels.
	    ier = apskybuf (ap, im, wx, wy)
	    if (ier != AP_OK)
		return (ier)

	    # Initialze the weights.
	    call amovkr (1.0, Memr[AP_SWGT(sky)], AP_NSKYPIX(sky))

	    # Clip the data.
	    if (AP_SLOCLIP(sky) > 0.0 || AP_SHICLIP(sky) > 0.0) {
		nclip = ap_clip (Memr[AP_SKYPIX(sky)], Memi[AP_INDEX(sky)],
		    AP_NSKYPIX(sky), AP_SLOCLIP(sky), AP_SHICLIP(sky),
		    ilo, ihi)
		if (nclip >= AP_NSKYPIX(sky))
		    return (AP_NSKY_TOO_SMALL)
		nsky = AP_NSKYPIX(sky) - nclip
	    } else {
		nclip = 0
		call ap_index (Memi[AP_INDEX(sky)], AP_NSKYPIX(sky))
		ilo = 1
		nsky = AP_NSKYPIX(sky) 
	    }

	    # Compute the sky value by a fitting a skewed Gaussian function
	    # to the sky pixel histogram.
	    ier = ap_gauss (Memr[AP_SKYPIX(sky)], Memi[AP_COORDS(sky)],
	        Memr[AP_SWGT(sky)], Memi[AP_INDEX(sky)+ilo-1], nsky,
		AP_SNX(sky), AP_SNY(sky), AP_SMAXITER(sky), AP_K1(sky),
		INDEFR, AP_BINSIZE(sky), AP_SMOOTH(sky),
		AP_SLOREJECT(sky), AP_SHIREJECT(sky), AP_RGROW(sky) *
		AP_SCALE(ap), AP_SNREJECT(sky), AP_SKY_MODE(sky),
		AP_SKY_SIG(sky), AP_SKY_SKEW(sky), AP_NSKY(sky),
		AP_NSKY_REJECT(sky))
	    AP_NSKY_REJECT(sky) = AP_NBADSKYPIX(sky) + nclip +
	        AP_NSKY_REJECT(sky)

	    return (ier)

	case AP_CROSSCOR:

	    # Fetch the sky pixels.
	    ier = apskybuf (ap, im, wx, wy)
	    if (ier != AP_OK)
		return (ier)

	    # Initialze the weights.
	    call amovkr (1.0, Memr[AP_SWGT(sky)], AP_NSKYPIX(sky))

	    # Clip the data.
	    if (AP_SLOCLIP(sky) > 0.0 || AP_SHICLIP(sky) > 0.0) {
		nclip = ap_clip (Memr[AP_SKYPIX(sky)], Memi[AP_INDEX(sky)],
		    AP_NSKYPIX(sky), AP_SLOCLIP(sky), AP_SHICLIP(sky),
		    ilo, ihi)
		if (nclip >= AP_NSKYPIX(sky))
		    return (AP_NSKY_TOO_SMALL)
		nsky = AP_NSKYPIX(sky) - nclip
	    } else {
		nclip = 0
		call ap_index (Memi[AP_INDEX(sky)], AP_NSKYPIX(sky))
		ilo = 1
		nsky = AP_NSKYPIX(sky) 
	    }

	    # Fit the sky value  by computing the cross-correlation
	    # function of the histogram and an estimate of the noise
	    # distribution.
	    ier = ap_crosscor (Memr[AP_SKYPIX(sky)], Memi[AP_COORDS(sky)],
	        Memr[AP_SWGT(sky)], Memi[AP_INDEX(sky)+ilo-1], nsky,
		AP_SNX(sky), AP_SNY(sky), AP_K1(sky), INDEFR,
		AP_BINSIZE(sky), AP_SMOOTH(sky), AP_SLOREJECT(sky),
		AP_SHIREJECT(sky), AP_RGROW(sky) * AP_SCALE(ap),
		AP_SMAXITER(sky), AP_SKY_MODE(sky), AP_SKY_SIG(sky),
		AP_SKY_SKEW(sky), AP_NSKY(sky), AP_NSKY_REJECT(sky))
	    AP_NSKY_REJECT(sky) = AP_NBADSKYPIX(sky) + nclip +
	        AP_NSKY_REJECT(sky)

	    return (ier)

	default:

	    AP_SKY_MODE(sky) = INDEFR
	    AP_SKY_SIG(sky) = INDEFR
	    AP_SKY_SKEW(sky) = INDEFR
	    AP_NSKY(sky) = AP_NSKYPIX(sky)
	    AP_NSKY_REJECT(sky) = 0
	    return (AP_OK)
	}
end
