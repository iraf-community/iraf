include <mach.h>
include "../lib/apphotdef.h"
include "../lib/fitpsfdef.h"
include "../lib/fitpsf.h"
include "../lib/noisedef.h"

# APSFREFIT -- Procedure to fit a function to the data already in the
# data buffers.

int procedure apsfrefit (ap)

pointer	ap		# pointer to the apphot structure

int	ier
pointer	psf, nse
int	apsfradgauss(), apsfelgauss(), apsfmoments()
real	datamin, datamax, dmin, dmax

begin
	psf = AP_PPSF(ap)
	nse = AP_NOISE(ap)

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
		AP_FWHMPSF(ap) * AP_SCALE(ap), datamin, datamax,
		AP_NOISEFUNCTION(nse), AP_EPADU(nse), AP_READNOISE(nse) /
		AP_EPADU(nse), AP_PMAXITER(psf), AP_PK2(psf), AP_PNREJECT(psf),
		Memr[AP_PPARS(psf)], Memr[AP_PPERRS(psf)], AP_PSFNPARS(psf))

	    Memr[AP_PPARS(psf)+1] = Memr[AP_PPARS(psf)+1] + AP_PFXCUR(psf) -
		AP_PXC(psf)
	    Memr[AP_PPARS(psf)+2] = Memr[AP_PPARS(psf)+2] + AP_PFYCUR(psf) -
		AP_PYC(psf)
	    Memr[AP_PPARS(psf)+3] = sqrt (abs (Memr[AP_PPARS(psf)+3]))

	case AP_ELLGAUSS:

	    ier = apsfelgauss (Memr[AP_PSFPIX(psf)], AP_PNX(psf), AP_PNY(psf),
		AP_FWHMPSF(ap) * AP_SCALE(ap), datamin, datamax,
		AP_NOISEFUNCTION(nse), AP_EPADU(nse), AP_READNOISE(nse) /
		AP_EPADU(nse), AP_PMAXITER(psf), AP_PK2(psf), AP_PNREJECT(psf),
		Memr[AP_PPARS(psf)], Memr[AP_PPERRS(psf)], AP_PSFNPARS(psf))

	    Memr[AP_PPARS(psf)+1] = Memr[AP_PPARS(psf)+1] + AP_PFXCUR(psf) -
		AP_PXC(psf)
	    Memr[AP_PPARS(psf)+2] = Memr[AP_PPARS(psf)+2] + AP_PFYCUR(psf) -
		AP_PYC(psf)
	    Memr[AP_PPARS(psf)+3] = sqrt (abs (Memr[AP_PPARS(psf)+3]))
	    Memr[AP_PPARS(psf)+4] = sqrt (abs (Memr[AP_PPARS(psf)+4]))

	case AP_MOMENTS:

	    call alimr (Memr[AP_PSFPIX(psf)], AP_PNX(psf) * AP_PNY(psf),
		dmin, dmax)
	    dmin = max (dmin, datamin)
	    dmax = min (dmax, datamax)

	    if (AP_POSITIVE(ap) == YES)
	        ier = apsfmoments (Memr[AP_PSFPIX(psf)], AP_PNX(psf),
		    AP_PNY(psf), dmin + AP_THRESHOLD(nse), dmax,
		    AP_POSITIVE(ap), Memr[AP_PPARS(psf)],
		    Memr[AP_PPERRS(psf)], AP_PSFNPARS(psf))
	    else 
	        ier = apsfmoments (Memr[AP_PSFPIX(psf)], AP_PNX(psf),
		    AP_PNY(psf), dmax + AP_THRESHOLD(nse), dmin,
		    AP_POSITIVE(ap), Memr[AP_PPARS(psf)],
		    Memr[AP_PPERRS(psf)], AP_PSFNPARS(psf))

	    Memr[AP_PPARS(psf)+1] = Memr[AP_PPARS(psf)+1] + AP_PFXCUR(psf) -
		AP_PXC(psf)
	    Memr[AP_PPARS(psf)+2] = Memr[AP_PPARS(psf)+2] + AP_PFYCUR(psf) -
		AP_PYC(psf)

	default:

	    # do nothing gracefully
        }

	if (ier == AP_NPSF_TOO_SMALL) {
	    call amovkr (INDEFR, Memr[AP_PPARS(psf)], AP_PSFNPARS(psf))
	    call amovkr (INDEFR, Memr[AP_PPERRS(psf)], AP_PSFNPARS(psf))
	}

	return (ier)
end
