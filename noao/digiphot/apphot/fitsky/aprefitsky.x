include "../lib/apphotdef.h"
include "../lib/apphot.h"
include "../lib/noisedef.h"
include "../lib/fitskydef.h"
include "../lib/fitsky.h"

# APREFITSKY -- Procedure to fit the sky using the pixels currently stored in
# the sky fitting buffers.

int procedure aprefitsky (ap, im, gd)

pointer	ap		# pointer to the apphot structure
pointer	im		# the input image descriptor
pointer	gd		# pointer to graphics stream

int	ier, nclip, nsky, ilo, ihi
pointer	sky, nse, gt
int	ap_mode(), ap_centroid(), ap_histplot(), ap_median()
int	ap_radplot(), ap_gauss(), ap_lgsky(), ap_crosscor()
int	ap_mean(), ap_clip()
pointer	ap_gtinit()

begin
	# Initialize.
	sky = AP_PSKY(ap)
	nse = AP_NOISE(ap)
	AP_SKY_MODE(sky) = INDEFR
	AP_SKY_SIG(sky) = INDEFR
	AP_SKY_SKEW(sky) = INDEFR
	AP_NSKY(sky) = 0
	AP_NSKY_REJECT(sky) = 0
	if (IS_INDEFR(AP_SXCUR(sky)) || IS_INDEFR(AP_SYCUR(sky))) {
            AP_OSXCUR(sky) = AP_SXCUR(sky)
            AP_OSYCUR(sky) = AP_SYCUR(sky)
	} else {
            switch (AP_WCSOUT(ap)) {
            case WCS_WORLD, WCS_PHYSICAL:
                call ap_ltoo (ap, AP_SXCUR(sky), AP_SYCUR(sky), AP_OSXCUR(sky),
		    AP_OSYCUR(sky), 1)
            case WCS_TV:
                call ap_ltov (im, AP_SXCUR(sky), AP_SYCUR(sky), AP_OSXCUR(sky),
		    AP_OSYCUR(sky), 1)
            default:
                AP_OSXCUR(sky) = AP_SXCUR(sky)
                AP_OSYCUR(sky) = AP_SYCUR(sky)
            }
	}

	if (IS_INDEFR(AP_SXCUR(sky)) || IS_INDEFR(AP_SYCUR(sky)))
	    return (AP_NOSKYAREA)

	switch (AP_SKYFUNCTION(sky)) {

	case AP_MEAN:

	    # Initialize the weights.
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

	    ier = ap_mean (Memr[AP_SKYPIX(sky)], Memi[AP_COORDS(sky)],
	        Memr[AP_SWGT(sky)], Memi[AP_INDEX(sky)+ilo-1], nsky,
		AP_SNX(sky), AP_SNY(sky), AP_SLOREJECT(sky),
		AP_SHIREJECT(sky), AP_RGROW(sky) * AP_SCALE(ap),
		AP_SNREJECT(sky), AP_SKY_MODE(sky), AP_SKY_SIG(sky),
		AP_SKY_SKEW(sky), AP_NSKY(sky), AP_NSKY_REJECT(sky))
	    AP_NSKY_REJECT(sky) = AP_NBADSKYPIX(sky) + nclip +
	        AP_NSKY_REJECT(sky) 

	    return (ier)

	case AP_MEDIAN:

	    # Initialize the weights.
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

	    # Initialize the weights.
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

	    # Initialize the weights.
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

	    ier = ap_centroid (Memr[AP_SKYPIX(sky)], Memi[AP_COORDS(sky)],
	        Memr[AP_SWGT(sky)+ilo-1], Memi[AP_INDEX(sky)+ilo-1], nsky,
		AP_SNX(sky), AP_SNY(sky), AP_K1(sky), INDEFR,
		AP_BINSIZE(sky), AP_SMOOTH(sky), AP_SLOREJECT(sky),
		AP_SHIREJECT(sky), AP_RGROW(sky) * AP_SCALE(ap),
		AP_SMAXITER(sky), AP_SKY_MODE(sky), AP_SKY_SIG(sky),
		AP_SKY_SKEW(sky), AP_NSKY(sky), AP_NSKY_REJECT(sky))
	    AP_NSKY_REJECT(sky) = AP_NBADSKYPIX(sky) + nclip +
	        AP_NSKY_REJECT(sky) 

	    return (ier)

	case AP_CONSTANT:

	    AP_SKY_MODE(sky) = AP_SKYBACKGROUND(sky)
	    AP_SKY_SIG(sky) = AP_SKYSIGMA(nse)
	    AP_SKY_SKEW(sky) = INDEFR
	    AP_NSKY(sky) = 0
	    AP_NSKY_REJECT(sky) = 0
	    return (AP_OK)

	case AP_SKYFILE:

	    return (AP_OK)

	case AP_RADPLOT:

	    # Check the status of the graphics stream.
	    if (gd == NULL)
		return (AP_NOGRAPHICS)

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

	    call gactivate (gd, 0)
	    gt = ap_gtinit (AP_IMROOT(ap), AP_SXCUR(sky), AP_SYCUR(sky))
	    ier = ap_radplot (gd, gt, Memr[AP_SKYPIX(sky)],
	        Memi[AP_COORDS(sky)], Memi[AP_INDEX(sky)+ilo-1], nsky,
		AP_SXC(sky), AP_SYC(sky), AP_SNX(sky), AP_SNY(sky),
		AP_SCALE(ap), AP_SKY_MODE(sky), AP_SKY_SIG(sky),
		AP_SKY_SKEW(sky), AP_NSKY(sky), AP_NSKY_REJECT(sky))
	    AP_NSKY_REJECT(sky) = AP_NBADSKYPIX(sky) + nclip +
	        AP_NSKY_REJECT(sky) 
	    call ap_gtfree (gt)
	    call gdeactivate (gd, 0)

	    return (ier)

	case AP_HISTPLOT:

	    # Check the status of the graphics stream.
	    if (gd == NULL)
		return (AP_NOGRAPHICS)

	    # Initialize the weights.
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

	    #call gactivate (gd, 0)
	    gt = ap_gtinit (AP_IMROOT(ap), AP_SXCUR(sky), AP_SYCUR(sky))
	    ier = ap_histplot (gd, gt, Memr[AP_SKYPIX(sky)],
	        Memr[AP_SWGT(sky)], Memi[AP_INDEX(sky)+ilo-1], nsky,
		AP_K1(sky), INDEFR, AP_BINSIZE(sky), AP_SMOOTH(sky),
		AP_SKY_MODE(sky), AP_SKY_SIG(sky), AP_SKY_SKEW(sky),
		AP_NSKY(sky), AP_NSKY_REJECT(sky))	
	    AP_NSKY_REJECT(sky) = AP_NBADSKYPIX(sky) + nclip +
	        AP_NSKY_REJECT(sky) 
	    call ap_gtfree (gt)

	    return (ier)

	case AP_OFILT:

	    # Initialize the weights.
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

	    # Initialize the weights.
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

	    ier = ap_gauss (Memr[AP_SKYPIX(sky)], Memi[AP_COORDS(sky)],
	        Memr[AP_SWGT(sky)], Memi[AP_INDEX(sky)+ilo-1], nsky,
		AP_SNX(sky), AP_SNY(sky), AP_SMAXITER(sky),
		AP_K1(sky), INDEFR, AP_BINSIZE(sky), AP_SMOOTH(sky),
		AP_SLOREJECT(sky), AP_SHIREJECT(sky), AP_RGROW(sky) *
		AP_SCALE(ap), AP_SNREJECT(sky), AP_SKY_MODE(sky),
		AP_SKY_SIG(sky), AP_SKY_SKEW(sky), AP_NSKY(sky),
		AP_NSKY_REJECT(sky))
	    AP_NSKY_REJECT(sky) = AP_NBADSKYPIX(sky) + nclip +
	        AP_NSKY_REJECT(sky) 

	    return (ier)

	case AP_CROSSCOR:

	    # Initialize the weights.
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
