include "../lib/apphotdef.h"
include "../lib/noisedef.h"
include "../lib/fitskydef.h"
include "../lib/fitsky.h"

# APREFITSKY -- Procedure to fit the sky using the pixels currently stored in
# the sky fitting buffers.

int procedure aprefitsky (ap, gd)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to graphics stream

int	ier
pointer	sky, nse, gt
int	ap_mode(), ap_centroid(), ap_histplot(), ap_median()
int	ap_radplot(), ap_gauss(), ap_lgsky(), ap_crosscor()
int	ap_mean()
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
	if (IS_INDEFR(AP_SXCUR(sky)) || IS_INDEFR(AP_SYCUR(sky)))
	    return (AP_NOSKYAREA)

	switch (AP_SKYFUNCTION(sky)) {

	case AP_MEAN:

	    ier = ap_mean (Memr[AP_SKYPIX(sky)], Memi[AP_COORDS(sky)],
	        AP_NSKYPIX(sky), AP_SNX(sky), AP_SNY(sky), AP_K2(sky),
		AP_RGROW(sky) * AP_SCALE(ap), AP_SNREJECT(sky),
		AP_SKY_MODE(sky), AP_SKY_SIG(sky), AP_SKY_SKEW(sky),
		AP_NSKY(sky), AP_NSKY_REJECT(sky))
	    AP_NSKY_REJECT(sky) = AP_NBADSKYPIX(sky) + AP_NSKY_REJECT(sky) 
	    return (ier)

	case AP_MEDIAN:

	    ier = ap_median (Memr[AP_SKYPIX(sky)], Memi[AP_COORDS(sky)],
	        Memi[AP_INDEX(sky)], AP_NSKYPIX(sky), AP_SNX(sky), AP_SNY(sky),
		AP_K2(sky), AP_RGROW(sky) * AP_SCALE(ap), AP_SNREJECT(sky),
		AP_SKY_MODE(sky), AP_SKY_SIG(sky), AP_SKY_SKEW(sky),
		AP_NSKY(sky), AP_NSKY_REJECT(sky))
	    AP_NSKY_REJECT(sky) = AP_NBADSKYPIX(sky) + AP_NSKY_REJECT(sky) 
	    return (ier)

	case AP_MODE:

	    ier = ap_mode (Memr[AP_SKYPIX(sky)], Memi[AP_COORDS(sky)],
	        Memi[AP_INDEX(sky)], AP_NSKYPIX(sky), AP_SNX(sky), AP_SNY(sky),
		AP_K2(sky), AP_RGROW(sky) * AP_SCALE(ap), AP_SNREJECT(sky),
		AP_SKY_MODE(sky), AP_SKY_SIG(sky), AP_SKY_SKEW(sky),
		AP_NSKY(sky), AP_NSKY_REJECT(sky))
	    AP_NSKY_REJECT(sky) = AP_NBADSKYPIX(sky) + AP_NSKY_REJECT(sky) 
	    return (ier)

	case AP_CENTROID:

	    ier = ap_centroid (Memr[AP_SKYPIX(sky)], Memi[AP_COORDS(sky)],
	        AP_NSKYPIX(sky), AP_SNX(sky), AP_SNY(sky), AP_K1(sky),
		AP_SKYSIGMA(nse), AP_BINSIZE(sky), AP_SMOOTH(sky), AP_K2(sky),
		AP_RGROW(sky) * AP_SCALE(ap), AP_SMAXITER(sky),
		AP_SKY_MODE(sky), AP_SKY_SIG(sky), AP_SKY_SKEW(sky),
		AP_NSKY(sky), AP_NSKY_REJECT(sky))
	    AP_NSKY_REJECT(sky) = AP_NBADSKYPIX(sky) + AP_NSKY_REJECT(sky) 
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

	    if (gd == NULL)
		return (AP_NOGRAPHICS)
	    call gactivate (gd, 0)
	    gt = ap_gtinit (AP_IMNAME(ap), AP_SXCUR(sky), AP_SYCUR(sky))
	    ier = ap_radplot (gd, gt, Memr[AP_SKYPIX(sky)],
	        Memi[AP_COORDS(sky)], AP_NSKYPIX(sky), AP_SXC(sky),
		AP_SYC(sky), AP_SNX(sky), AP_SNY(sky), AP_SCALE(ap), AP_K2(sky),
		AP_SKY_MODE(sky), AP_SKY_SIG(sky), AP_SKY_SKEW(sky),
		AP_NSKY(sky), AP_NSKY_REJECT(sky))
	    call ap_gtfree (gt)
	    call gdeactivate (gd, 0)
	    return (ier)

	case AP_HISTPLOT:

	    if (gd == NULL)
		return (AP_NOGRAPHICS)
	    call gactivate (gd, 0)
	    gt = ap_gtinit (AP_IMNAME(ap), AP_SXCUR(sky), AP_SYCUR(sky))
	    ier = ap_histplot (gd, gt, Memr[AP_SKYPIX(sky)], AP_NSKYPIX(sky),
	        AP_K1(sky), AP_SKYSIGMA(nse), AP_BINSIZE(sky), AP_SMOOTH(sky),
		AP_SKY_MODE(sky), AP_SKY_SIG(sky), AP_SKY_SKEW(sky),
		AP_NSKY(sky), AP_NSKY_REJECT(sky))	
	    AP_NSKY_REJECT(sky) = AP_NBADSKYPIX(sky) + AP_NSKY_REJECT(sky) 
	    call ap_gtfree (gt)
	    call gdeactivate (gd, 0)
	    return (ier)

	case AP_OFILT:

	    ier = ap_lgsky (Memr[AP_SKYPIX(sky)], Memi[AP_COORDS(sky)],
	        AP_NSKYPIX(sky), AP_SNX(sky), AP_SNY(sky), AP_K1(sky),
		AP_SKYSIGMA(nse), AP_BINSIZE(sky), AP_SMOOTH(sky), AP_K2(sky),
		AP_RGROW(sky) * AP_SCALE(ap), AP_SMAXITER(sky),
		AP_SKY_MODE(sky), AP_SKY_SIG(sky), AP_SKY_SKEW(sky),
		AP_NSKY(sky), AP_NSKY_REJECT(sky))
	    AP_NSKY_REJECT(sky) = AP_NBADSKYPIX(sky) + AP_NSKY_REJECT(sky) 
	    return (ier)

	case AP_GAUSS:

	    ier = ap_gauss (Memr[AP_SKYPIX(sky)], Memi[AP_COORDS(sky)],
	        AP_NSKYPIX(sky), AP_SNX(sky), AP_SNY(sky), AP_SMAXITER(sky),
		AP_K1(sky), AP_SKYSIGMA(nse), AP_BINSIZE(sky), AP_SMOOTH(sky),
		AP_K2(sky), AP_RGROW(sky) * AP_SCALE(ap), AP_SNREJECT(sky),
		AP_SKY_MODE(sky), AP_SKY_SIG(sky), AP_SKY_SKEW(sky),
		AP_NSKY(sky), AP_NSKY_REJECT(sky))
	    AP_NSKY_REJECT(sky) = AP_NBADSKYPIX(sky) + AP_NSKY_REJECT(sky) 
	    return (ier)

	case AP_CROSSCOR:

	    ier = ap_crosscor (Memr[AP_SKYPIX(sky)], Memi[AP_COORDS(sky)],
	        AP_NSKYPIX(sky), AP_SNX(sky), AP_SNY(sky), AP_K1(sky),
		AP_SKYSIGMA(nse), AP_BINSIZE(sky), AP_SMOOTH(sky), AP_K2(sky),
		AP_RGROW(sky) * AP_SCALE(sky), AP_SMAXITER(sky),
		AP_SKY_MODE(sky), AP_SKY_SIG(sky), AP_SKY_SKEW(sky),
		AP_NSKY(sky), AP_NSKY_REJECT(sky))
	    AP_NSKY_REJECT(sky) = AP_NBADSKYPIX(sky) + AP_NSKY_REJECT(sky) 
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
