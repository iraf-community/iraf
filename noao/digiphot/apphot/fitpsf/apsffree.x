include "../lib/apphotdef.h"
include "../lib/fitpsfdef.h"

# APSFFREE -- Procedure to free the point spread fitting structure.

procedure apsffree (ap)

pointer	ap		# pointer to the apphot structure

begin
	if (ap == NULL)
	    return
	if (AP_NOISE(ap) != NULL)
	    call ap_noisecls (ap)
	if (AP_PDISPLAY(ap) != NULL)
	    call ap_dispcls (ap)
	if (AP_PPSF(ap) != NULL)
	    call ap_psfcls (ap)
	if (AP_IMBUF(ap) != NULL)
	    call mfree (AP_IMBUF(ap), TY_REAL)
	if (AP_MW(ap) != NULL)
	    call mw_close (AP_MW(ap))
	call mfree (ap, TY_STRUCT)
end


# AP_PSFCLS -- Procedure to close up the point spread fitting function
# and arrays

procedure ap_psfcls (ap)

pointer ap		# pointer to apphot structure

pointer	psf

begin
	if (AP_PPSF(ap) == NULL)
	    return
	psf = AP_PPSF(ap)
	#if (AP_PSFPIX(psf) != NULL)
	    #call mfree (AP_PSFPIX(psf), TY_REAL)
	if (AP_PSFXPIX(psf) != NULL)
	    call mfree (AP_PSFXPIX(psf), TY_REAL)
	if (AP_PSFYPIX(psf) != NULL)
	    call mfree (AP_PSFYPIX(psf), TY_REAL)
	if (AP_PPARS(psf) != NULL)
	    call mfree (AP_PPARS(psf), TY_REAL)
	if (AP_PPERRS(psf) != NULL)
	    call mfree (AP_PPERRS(psf), TY_REAL)
	if (AP_PPSF(ap) != NULL)
	    call mfree (AP_PPSF(ap), TY_STRUCT)
end
