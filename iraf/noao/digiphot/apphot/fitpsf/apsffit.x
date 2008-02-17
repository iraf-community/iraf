include <mach.h>
include "../lib/apphotdef.h"
include "../lib/apphot.h"
include "../lib/fitpsfdef.h"
include "../lib/noisedef.h"
include "../lib/fitpsf.h"

# APSFFIT -- Procedure to fit an analytic function to the PSF.

int procedure apsffit (ap, im, wx, wy)

pointer	ap		# pointer to the apphot structure
pointer	im		# pointer to the IRAF image
real	wx, wy		# object coordinates

int	ier, fier
pointer	psf, nse
real	datamin, datamax, dmin, dmax, threshold
int	apfbuf(), apsfradgauss(), apsfelgauss(), apsfmoments()

begin
	# Initialize.
	psf = AP_PPSF(ap)
	nse = AP_NOISE(ap)
	AP_PFXCUR(psf) = wx
	AP_PFYCUR(psf) = wy
	if (IS_INDEFR(wx) || IS_INDEFR(wy)) {
            AP_OPFXCUR(psf) = INDEFR
            AP_OPFYCUR(psf) = INDEFR
	} else {
            switch (AP_WCSOUT(ap)) {
            case WCS_WORLD, WCS_PHYSICAL:
                call ap_ltoo (ap, wx, wy, AP_OPFXCUR(psf), AP_OPFYCUR(psf), 1)
            case WCS_TV:
                call ap_ltov (im, wx, wy, AP_OPFXCUR(psf), AP_OPFYCUR(psf), 1)
            default:
                AP_OPFXCUR(psf) = wx
                AP_OPFYCUR(psf) = wy
            }
	}
	call amovkr (INDEFR, Memr[AP_PPARS(psf)], AP_MAXNPARS(psf))
	call amovkr (INDEFR, Memr[AP_PPERRS(psf)], AP_MAXNPARS(psf))

	# Fetch the buffer of pixels.
	ier = apfbuf (ap, im, wx, wy)
	if (ier == AP_NOPSFAREA)
	    return (AP_NOPSFAREA)

	# Compute the min and max of the data subraster.
	if (IS_INDEFR(AP_DATAMIN(ap)))
	    datamin = -MAX_REAL
	else
	    datamin = AP_DATAMIN(ap)
	if (IS_INDEFR(AP_DATAMAX(ap)))
	    datamax = MAX_REAL
	else
	    datamax = AP_DATAMAX(ap)

	switch (AP_PSFUNCTION(psf)) {

	case AP_RADGAUSS:

	    fier = apsfradgauss (Memr[AP_PSFPIX(psf)], AP_PNX(psf), AP_PNY(psf),
	        AP_POSITIVE(ap), AP_FWHMPSF(ap) * AP_SCALE(ap), datamin,
		datamax, AP_NOISEFUNCTION(nse), AP_EPADU(nse),
		AP_READNOISE(nse) / AP_EPADU(nse), AP_PMAXITER(psf),
		AP_PK2(psf), AP_PNREJECT(psf), Memr[AP_PPARS(psf)],
		Memr[AP_PPERRS(psf)], AP_PSFNPARS(psf))

	    Memr[AP_PPARS(psf)+1] = Memr[AP_PPARS(psf)+1] + wx - AP_PXC(psf)
	    Memr[AP_PPARS(psf)+2] = Memr[AP_PPARS(psf)+2] + wy - AP_PYC(psf) 

	case AP_ELLGAUSS:

	    fier = apsfelgauss (Memr[AP_PSFPIX(psf)], AP_PNX(psf), AP_PNY(psf),
	        AP_POSITIVE(ap), AP_FWHMPSF(ap) * AP_SCALE(ap), datamin,
		datamax, AP_NOISEFUNCTION(nse), AP_EPADU(nse),
		AP_READNOISE(nse) / AP_EPADU(nse), AP_PMAXITER(psf),
		AP_PK2(psf), AP_PNREJECT(psf), Memr[AP_PPARS(psf)],
		Memr[AP_PPERRS(psf)], AP_PSFNPARS(psf))

	    Memr[AP_PPARS(psf)+1] = Memr[AP_PPARS(psf)+1] + wx - AP_PXC(psf)
	    Memr[AP_PPARS(psf)+2] = Memr[AP_PPARS(psf)+2] + wy - AP_PYC(psf) 

	case AP_MOMENTS:

	    call alimr (Memr[AP_PSFPIX(psf)], AP_PNX(psf) * AP_PNY(psf),
	        dmin, dmax)
	    dmin = max (dmin, datamin)
	    dmax = min (dmax, datamax)
	    threshold = 0.0

	    if (AP_POSITIVE(ap) == YES)
	        fier = apsfmoments (Memr[AP_PSFPIX(psf)], AP_PNX(psf),
		    AP_PNY(psf), dmin + threshold, dmax,
		    AP_POSITIVE(ap), Memr[AP_PPARS(psf)], Memr[AP_PPERRS(psf)],
		    AP_PSFNPARS(psf))
	    else
	        fier = apsfmoments (Memr[AP_PSFPIX(psf)], AP_PNX(psf),
		    AP_PNY(psf), dmax - threshold, dmin,
		    AP_POSITIVE(ap), Memr[AP_PPARS(psf)],
		    Memr[AP_PPERRS(psf)], AP_PSFNPARS(psf))

	    Memr[AP_PPARS(psf)+1] = Memr[AP_PPARS(psf)+1] + wx - AP_PXC(psf)
	    Memr[AP_PPARS(psf)+2] = Memr[AP_PPARS(psf)+2] + wy - AP_PYC(psf) 

	default:

	    # do nothing gracefully

        }

        switch (AP_WCSOUT(ap)) {
        case WCS_WORLD, WCS_PHYSICAL:
            call ap_ltoo (ap, Memr[AP_PPARS(psf)+1], Memr[AP_PPARS(psf)+2],
		Memr[AP_PPARS(psf)+1], Memr[AP_PPARS(psf)+2], 1)
        case WCS_TV:
            call ap_ltov (im, Memr[AP_PPARS(psf)+1], Memr[AP_PPARS(psf)+2],
		Memr[AP_PPARS(psf)+1], Memr[AP_PPARS(psf)+2], 1)
        default:
	    ;
        }

	# Return the appropriate error code.
	if (fier == AP_OK) {
	    if (ier == AP_PSF_OUTOFBOUNDS)
		return (AP_PSF_OUTOFBOUNDS)
	    else
		return (AP_OK)
	} else if (fier == AP_NPSF_TOO_SMALL) {
	    call amovkr (INDEFR, Memr[AP_PPARS(psf)], AP_PSFNPARS(psf))
	    call amovkr (INDEFR, Memr[AP_PPERRS(psf)], AP_PSFNPARS(psf))
	    return (AP_NPSF_TOO_SMALL)
	} else
	    return (fier)
end
