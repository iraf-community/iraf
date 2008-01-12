include "../lib/apphotdef.h"
include "../lib/photdef.h"

# APFREE -- Free the apphot structure.

procedure appfree (ap)

pointer	ap		# pointer to the apphot structure

begin
	if (ap == NULL)
	    return
	if (AP_NOISE(ap) != NULL)
	    call ap_noisecls (ap)
	if (AP_PCENTER(ap) != NULL)
	    call ap_ctrcls (ap)
	if (AP_PDISPLAY(ap) != NULL)
	    call ap_dispcls (ap)
	if (AP_POLY(ap) != NULL)
	    call ap_ycls (ap)
	if (AP_PPHOT(ap) != NULL)
	    call ap_photcls (ap)
	if (AP_PPSF(ap) != NULL)
	    call ap_psfcls (ap)
	if (AP_PSKY(ap) != NULL)
	    call ap_skycls (ap)
	if (AP_IMBUF(ap) != NULL)
	    call mfree (AP_IMBUF(ap), TY_REAL)
	if (AP_MW(ap) != NULL)
	    call mw_close (AP_MW(ap))
	call mfree (ap, TY_STRUCT)
end


# AP_PHOTCLS -- Procedure to close up the photometry structure and arrays.

procedure ap_photcls (ap)

pointer	ap		# pointer to apphot structure

pointer	phot

begin
	if (AP_PPHOT(ap) == NULL)
	    return
	phot = AP_PPHOT(ap)
	if (AP_APERTS(phot) != NULL)
	    call mfree (AP_APERTS(phot), TY_REAL)
	if (AP_MAGS(phot) != NULL)
	    call mfree (AP_MAGS(phot), TY_REAL)
	if (AP_MAGERRS(phot) != NULL)
	    call mfree (AP_MAGERRS(phot), TY_REAL)
	if  (AP_SUMS(phot) != NULL)
	    call mfree (AP_SUMS(phot), TY_DOUBLE)
	if (AP_AREA(phot) != NULL)
	    call mfree (AP_AREA(phot), TY_DOUBLE)

	#if (AP_APIX(phot) != NULL)
	    #call mfree (AP_APIX(phot), TY_REAL)
	#if (AP_XAPIX(phot) != NULL)
	    #call mfree (AP_XAPIX(phot), TY_REAL)
	#if (AP_YAPIX(phot) != NULL)
	    #call mfree (AP_YAPIX(phot), TY_REAL)

	call mfree (AP_PPHOT(ap), TY_STRUCT)
end
