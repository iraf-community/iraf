include "../lib/apphotdef.h"
include "../lib/finddef.h"

# AP_FDINIT - Initialize the daofind data structure.

procedure ap_fdinit (ap, fwhmpsf, noise)

pointer	ap		# pointer to the apphot structure
real	fwhmpsf		# FWHM of the PSF
int	noise		# noise function

begin
	# Allocate space.
	call malloc (ap, LEN_APSTRUCT, TY_STRUCT)
	AP_IMNAME(ap) = EOS
	AP_CWX(ap) = INDEFR
	AP_CWY(ap) = INDEFR
	AP_WX(ap) = INDEFR
	AP_WY(ap) = INDEFR
	AP_FWHMPSF(ap) = fwhmpsf
	AP_SCALE(ap) = DEF_SCALE
	AP_POSITIVE(ap) = DEF_POSITIVE
	AP_DATAMIN(ap) = DEF_DATAMIN
	AP_DATAMAX(ap) = DEF_DATAMAX
	AP_EXPOSURE(ap) = EOS
	AP_ITIME(ap) = DEF_ITIME

	# Setup the noise structure.
	call ap_noisesetup (ap, noise)

	# Setup the display strucuture.
	call ap_dispsetup (ap)

	# Setup the find structure.
	call ap_fdsetup (ap)
	    
	# Unused structures are set to null.
	AP_PCENTER(ap) = NULL
	AP_PSKY(ap) = NULL
	AP_PPSF(ap) = NULL
	AP_PPHOT(ap) = NULL
	AP_POLY(ap) = NULL
	AP_RPROF(ap) = NULL
end


# AP_FDSETUP -- Initialize the find structure.

procedure ap_fdsetup (ap)

pointer	ap		# pointer to the apphot strucuture

pointer	fnd

begin
	call malloc (AP_PFIND(ap), LEN_FIND, TY_STRUCT)
	fnd = AP_PFIND(ap)

	AP_RATIO(fnd) = DEF_RATIO
	AP_THETA(fnd) = DEF_RATIO
	AP_NSIGMA(fnd) = DEF_NSIGMA

	AP_SHARPLO(fnd) = DEF_SHARPLO
	AP_SHARPHI(fnd) = DEF_SHARPHI
	AP_ROUNDLO(fnd) = DEF_ROUNDLO
	AP_ROUNDHI(fnd) = DEF_ROUNDLO
end
