include <mach.h>
include "../lib/apphotdef.h"
include "../lib/apphot.h"
include "../lib/fitpsfdef.h"
include "../lib/fitpsf.h"
include "../lib/noisedef.h"

# APSFREFIT -- Procedure to fit a function to the data already in the
# data buffers.

int procedure apsfrefit (ap, im)

pointer	ap		# pointer to the apphot structure
pointer	im		# the input image descriptor

int	ier
pointer	psf, nse
int	apsfradgauss(), apsfelgauss(), apsfmoments()
real	datamin, datamax, dmin, dmax, threshold

begin
	psf = AP_PPSF(ap)
	nse = AP_NOISE(ap)
	call amovkr (INDEFR, Memr[AP_PPARS(psf)], AP_MAXNPARS(psf))
	call amovkr (INDEFR, Memr[AP_PPERRS(psf)], AP_MAXNPARS(psf))

	# Compute the minimum and maximum good data value.
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

	    ier = apsfradgauss (Memr[AP_PSFPIX(psf)], AP_PNX(psf), AP_PNY(psf),
		AP_POSITIVE(ap), AP_FWHMPSF(ap) * AP_SCALE(ap), datamin,
		datamax, AP_NOISEFUNCTION(nse), AP_EPADU(nse),
		AP_READNOISE(nse) / AP_EPADU(nse), AP_PMAXITER(psf),
		AP_PK2(psf), AP_PNREJECT(psf), Memr[AP_PPARS(psf)],
		Memr[AP_PPERRS(psf)], AP_PSFNPARS(psf))

	    Memr[AP_PPARS(psf)+1] = Memr[AP_PPARS(psf)+1] + AP_PFXCUR(psf) -
		AP_PXC(psf)
	    Memr[AP_PPARS(psf)+2] = Memr[AP_PPARS(psf)+2] + AP_PFYCUR(psf) -
		AP_PYC(psf)

	case AP_ELLGAUSS:

	    ier = apsfelgauss (Memr[AP_PSFPIX(psf)], AP_PNX(psf), AP_PNY(psf),
		AP_POSITIVE(ap), AP_FWHMPSF(ap) * AP_SCALE(ap), datamin,
		datamax, AP_NOISEFUNCTION(nse), AP_EPADU(nse),
		AP_READNOISE(nse) / AP_EPADU(nse), AP_PMAXITER(psf),
		AP_PK2(psf), AP_PNREJECT(psf), Memr[AP_PPARS(psf)],
		Memr[AP_PPERRS(psf)], AP_PSFNPARS(psf))

	    Memr[AP_PPARS(psf)+1] = Memr[AP_PPARS(psf)+1] + AP_PFXCUR(psf) -
		AP_PXC(psf)
	    Memr[AP_PPARS(psf)+2] = Memr[AP_PPARS(psf)+2] + AP_PFYCUR(psf) -
		AP_PYC(psf)

	case AP_MOMENTS:

	    call alimr (Memr[AP_PSFPIX(psf)], AP_PNX(psf) * AP_PNY(psf),
		dmin, dmax)
	    dmin = max (dmin, datamin)
	    dmax = min (dmax, datamax)
	    threshold = 0.0

	    if (AP_POSITIVE(ap) == YES)
	        ier = apsfmoments (Memr[AP_PSFPIX(psf)], AP_PNX(psf),
		    AP_PNY(psf), dmin + threshold, dmax,
		    AP_POSITIVE(ap), Memr[AP_PPARS(psf)],
		    Memr[AP_PPERRS(psf)], AP_PSFNPARS(psf))
	    else 
	        ier = apsfmoments (Memr[AP_PSFPIX(psf)], AP_PNX(psf),
		    AP_PNY(psf), dmax + threshold, dmin,
		    AP_POSITIVE(ap), Memr[AP_PPARS(psf)],
		    Memr[AP_PPERRS(psf)], AP_PSFNPARS(psf))

	    Memr[AP_PPARS(psf)+1] = Memr[AP_PPARS(psf)+1] + AP_PFXCUR(psf) -
		AP_PXC(psf)
	    Memr[AP_PPARS(psf)+2] = Memr[AP_PPARS(psf)+2] + AP_PFYCUR(psf) -
		AP_PYC(psf)

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

	if (ier == AP_NPSF_TOO_SMALL) {
	    call amovkr (INDEFR, Memr[AP_PPARS(psf)], AP_PSFNPARS(psf))
	    call amovkr (INDEFR, Memr[AP_PPERRS(psf)], AP_PSFNPARS(psf))
	}

	return (ier)
end
