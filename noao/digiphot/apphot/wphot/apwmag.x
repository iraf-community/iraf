include <mach.h>
include "../lib/apphotdef.h"
include "../lib/noisedef.h"
include "../lib/photdef.h"
include "../lib/phot.h"

# AP_WMAG -- Procedure to compute the magnitudes inside a set of apertures for
# a single of object.

int procedure ap_wmag (ap, im, wx, wy, positive, skyval, skysig, nsky)

pointer	ap		# pointer to the apphot structure
pointer	im		# pointer to the IRAF image
real	wx, wy		# object coordinates
int	positive	# emission or absorption features
real	skyval		# sky value
real	skysig		# sky sigma
int	nsky		# number of sky pixels

int	c1, c2, l1, l2, ier
pointer	sp, nse, phot, temp
real	datamin, datamax, zmag
int	apmagbuf()

begin
	# Initalize.
	phot = AP_PPHOT(ap)
	nse = AP_NOISE(ap)
	AP_PXCUR(phot) = wx
	AP_PYCUR(phot) = wy
	call amovkr (0.0, Memr[AP_SUMS(phot)], AP_NAPERTS(phot)]
	call amovkr (0.0, Memr[AP_AREA(phot)], AP_NAPERTS(phot)]
	call amovkr (INDEFR, Memr[AP_MAGS(phot)], AP_NAPERTS(phot)]
	call amovkr (INDEFR, Memr[AP_MAGERRS(phot)], AP_NAPERTS(phot)]

	# Make sure the center is defined.
	if (IS_INDEFR(wx) || IS_INDEFR(wy))
	    return (AP_APERT_NOAPERT)

	# Fetch the aperture pixels.
	ier = apmagbuf (ap, im, wx, wy, c1, c2, l1, l2)
	if (ier == AP_APERT_NOAPERT)
	    return (AP_APERT_NOAPERT)

	call smark (sp)
	call salloc (temp, AP_NAPERTS(phot), TY_REAL)

	# Compute the min and max.
	if (IS_INDEFR(AP_DATAMIN(ap)))
	    datamin = -MAX_REAL
	else
	    datamin = AP_DATAMIN(ap)
	if (IS_INDEFR(AP_DATAMAX(ap)))
	    datamax = MAX_REAL
	else
	    datamax = AP_DATAMAX(ap)

	# Do photometry for all the apertures.
	AP_NMINAP(phot) = AP_NMAXAP(phot) + 1
	call amulkr (Memr[AP_APERTS(phot)], AP_SCALE(ap), Memr[temp],
	    AP_NAPERTS(phot)]
	switch (AP_PWEIGHTS(phot)) {
	case AP_PWCONSTANT:
	    if (IS_INDEFR(AP_DATAMIN(ap)) && IS_INDEFR(AP_DATAMAX(ap)))
	        call  apmeasure (im, wx, wy, c1, c2, l1, l2, Memr[temp],
	            Memr[AP_SUMS(phot)], Memr[AP_AREA(phot)], AP_NMAXAP(phot))
	    else
	        call  apbmeasure (im, wx, wy, c1, c2, l1, l2, datamin,
		    datamax, Memr[temp], Memr[AP_SUMS(phot)],
		    Memr[AP_AREA(phot)], AP_NMAXAP(phot), AP_NMINAP(phot))
	case AP_PWCONE:
	    if (IS_INDEFR(AP_DATAMIN(ap)) && IS_INDEFR(AP_DATAMAX(ap)))
	        call  ap_tmeasure (ap, im, wx, wy, c1, c2, l1, l2, Memr[temp],
	            Memr[AP_SUMS(phot)], Memr[AP_AREA(phot)], AP_NMAXAP(phot))
	    else
	        call  ap_btmeasure (ap, im, wx, wy, c1, c2, l1, l2, datamin,
		    datamax, Memr[temp], Memr[AP_SUMS(phot)],
		    Memr[AP_AREA(phot)], AP_NMAXAP(phot), AP_NMINAP(phot))
	case AP_PWGAUSS:
	    if (IS_INDEFR(AP_DATAMIN(ap)) && IS_INDEFR(AP_DATAMAX(ap)))
	        call  ap_gmeasure (ap, im, wx, wy, c1, c2, l1, l2, Memr[temp],
	            Memr[AP_SUMS(phot)], Memr[AP_AREA(phot)], AP_NMAXAP(phot))
		else
	            call  ap_bgmeasure (ap, im, wx, wy, c1, c2, l1, l2,
		        datamin, datamax, Memr[temp], Memr[AP_SUMS(phot)],
			Memr[AP_AREA(phot)], AP_NMAXAP(phot), AP_NMINAP(phot))
	default:
	    if (IS_INDEFR(AP_DATAMIN(ap)) && IS_INDEFR(AP_DATAMAX(ap)))
	        call  apmeasure (im, wx, wy, c1, c2, l1, l2, Memr[temp],
	            Memr[AP_SUMS(phot)], Memr[AP_AREA(phot)], AP_NMAXAP(phot))
	    else
	        call  apbmeasure (im, wx, wy, c1, c2, l1, l2, datamin,
		    datamax, Memr[temp], Memr[AP_SUMS(phot)],
		    Memr[AP_AREA(phot)], AP_NMAXAP(phot), AP_NMINAP(phot))
	}

	# Make sure that the sky value has been defined.
	if (IS_INDEFR(skyval))
	    ier = AP_APERT_NOSKYMODE

	else {

	    # Check for bad pixels.
	    if (AP_NMINAP(phot) <= AP_NMAXAP(phot))
		ier = AP_APERT_BADDATA

	    # Compute the magnitudes and errors.
	    if (positive == YES)
	        call apcopmags (Memr[AP_SUMS(phot)], Memr[AP_AREA(phot)],
	            Memr[AP_MAGS(phot)], Memr[AP_MAGERRS(phot)],
		    AP_NMAXAP(phot), skyval, skysig, nsky, AP_ZMAG(phot),
		    AP_NOISEFUNCTION(nse), AP_EPADU(nse))
	    else
	        call apconmags (Memr[AP_SUMS(phot)], Memr[AP_AREA(phot)],
	            Memr[AP_MAGS(phot)], Memr[AP_MAGERRS(phot)],
		    AP_NMAXAP(phot), skyval, skysig, nsky, AP_ZMAG(phot),
		    AP_NOISEFUNCTION(nse), AP_EPADU(nse), AP_READNOISE(nse))

	    # Compute correction for itime.
	    zmag = 2.5 * log10 (AP_ITIME(ap))
	    call aaddkr (Memr[AP_MAGS(phot)], zmag, Memr[AP_MAGS(phot)],
		AP_NAPERTS(phot)]
	}

	call sfree (sp)
	if (ier != AP_OK)
	    return (ier)
	else
	    return (AP_OK)
end
