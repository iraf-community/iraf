include "../lib/apphotdef.h"
include "../lib/polyphotdef.h"

# AP_YFREE -- Procedure to free the polyphot structure.

procedure ap_yfree (ap)

pointer	ap		# pointer to the apphot structure

begin
	if (ap == NULL)
	    return
	if (AP_NOISE(ap) != NULL)
	    call ap_noisecls (ap)
	if (AP_PDISPLAY(ap) != NULL)
	    call ap_dispcls (ap)
	if (AP_POLY(ap) != NULL)
	    call ap_ycls (ap)
	if (AP_PSKY(ap) != NULL)
	    call ap_skycls (ap)
	if (AP_PCENTER(ap) != NULL)
	    call ap_ctrcls (ap)
	if (AP_IMBUF(ap) != NULL)
	    call mfree (AP_IMBUF(ap), TY_REAL)
	if (AP_MW(ap) != NULL)
	    call mw_close (AP_MW(ap))
	call mfree (ap, TY_STRUCT)
end


# AP_YCLS -- Procedure to close up the polyphot structure and arrays.

procedure ap_ycls (ap)

pointer	ap		# pointer to the apphot structure

begin
	if (AP_POLY(ap) == NULL)
	    return
	call mfree (AP_POLY(ap), TY_STRUCT)
end
