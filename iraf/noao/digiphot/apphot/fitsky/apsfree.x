include "../lib/apphotdef.h"
include "../lib/fitskydef.h"

# APSFREE -- Procedure to free the sky fitting structure.

procedure apsfree (ap)

pointer	ap		# pointer to the apphot structure

begin
	if (ap == NULL)
	    return
	if (AP_NOISE(ap) != NULL)
	    call ap_noisecls (ap)
	if (AP_PDISPLAY(ap) != NULL)
	    call ap_dispcls (ap)
	if (AP_PSKY(ap) != NULL)
	    call ap_skycls (ap)
	if (AP_IMBUF(ap) != NULL)
	    call mfree (AP_IMBUF(ap), TY_REAL)
	if (AP_MW(ap) != NULL)
	    call mw_close (AP_MW(ap))
	call mfree (ap, TY_STRUCT)
end


# AP_SKYCLS -- Procedure to close up the sky fitting arrays.

procedure ap_skycls (ap)

pointer	ap		# pointer to the apphot structure

pointer	sky

begin
	sky = AP_PSKY(ap)
	if (sky == NULL)
	    return
	if (AP_SKYPIX(sky) != NULL)
	    call mfree (AP_SKYPIX(sky), TY_REAL)
	if (AP_INDEX(sky) != NULL)
	    call mfree (AP_INDEX(sky), TY_INT)
	if (AP_COORDS(sky) != NULL)
	    call mfree (AP_COORDS(sky), TY_INT)
	if (AP_SWGT(sky) != NULL)
	    call mfree (AP_SWGT(sky), TY_REAL)
	call mfree (AP_PSKY(ap), TY_STRUCT)
end
