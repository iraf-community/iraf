include "../lib/apphotdef.h"

# AP_YMKFREE -- Procedure to free the polyphot structure.

procedure ap_ymkfree (ap)

pointer	ap		# pointer to the apphot structure

begin
	if (ap == NULL)
	    return
	if (AP_PDISPLAY(ap) != NULL)
	    call ap_dispcls (ap)
	if (AP_POLY(ap) != NULL)
	    call ap_ycls (ap)
	call mfree (ap, TY_STRUCT)
end
