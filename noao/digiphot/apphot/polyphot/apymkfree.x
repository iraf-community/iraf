include "../lib/apphotdef.h"

# AP_YMKFREE -- Procedure to free the polyphot structure.

procedure ap_ymkfree (ap)

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
	if (AP_IMBUF(ap) != NULL)
	    call mfree (AP_IMBUF(ap), TY_REAL)
	if (AP_MW(ap) != NULL)
	    call mw_close (AP_MW(ap))
	call mfree (ap, TY_STRUCT)
end
