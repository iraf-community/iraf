include "../lib/apphotdef.h"
include "../lib/radprofdef.h"
include "../lib/photdef.h"
include "../lib/phot.h"
include "../lib/radprof.h"

# AP_RPINDEF -- Routine to return INDEF valued photometry and radial profile
# buffers.

procedure ap_rpindef (ap)

pointer	ap		# pointer to the apphot structure

pointer	phot, rprof

begin
	phot = AP_PPHOT(ap)
	rprof = AP_RPROF(ap)

	AP_RPFWHM(rprof) = INDEFR
	AP_INORM(rprof) = INDEFR
	AP_TINORM(rprof) = INDEFR

	call amovkr (INDEFR, Memr[AP_INTENSITY(rprof)], AP_RPNPTS(rprof))
	call amovkr (INDEFR, Memr[AP_TINTENSITY(rprof)], AP_RPNPTS(rprof))
	call amovkd (0.0d0, Memd[AP_AREA(phot)], AP_NAPERTS(phot))
	call amovkd (0.0d0, Memd[AP_SUMS(phot)], AP_NAPERTS(phot))
	call amovkr (INDEFR, Memr[AP_MAGS(phot)], AP_NAPERTS(phot))
	call amovkr (INDEFR, Memr[AP_MAGERRS(phot)], AP_NAPERTS(phot))
end


# AP_MAXAP -- Procedure to setup the maximum number of apertures for phot.

procedure ap_maxap (ap, pier)

pointer	ap		# pointer to the apphot structure
int	pier		# photometric error

int	i
pointer	phot, rprof
real	dxc1, dxc2, dyc1, dyc2, rdist, rapert

begin
	phot = AP_PPHOT(ap)
	rprof = AP_RPROF(ap)

	dxc1 = AP_RPXC(rprof) - 0.5
	dxc2 = AP_RPNX(rprof) - AP_RPXC(rprof) + 0.5
	dyc1 = AP_RPYC(rprof) - 0.5
	dyc2 = AP_RPNY(rprof) - AP_RPYC(rprof) + 0.5

	# Compute the maximum aperture.
	AP_NMAXAP(phot) = 0
	rdist = min (abs (dxc1), abs (dxc2), abs (dyc1), abs (dyc2))
	do i = 1, AP_NAPERTS(phot) {
	    rapert = AP_SCALE(ap) * Memr[AP_APERTS(phot)+i-1]
	    if (rapert <= rdist) {
		AP_NMAXAP(phot) = i
	    } else {
		break
	    }
	}

	if (AP_NMAXAP(phot) < AP_NAPERTS(phot))
	    pier = AP_APERT_OUTOFBOUNDS
	else
	    pier = AP_OK
end
