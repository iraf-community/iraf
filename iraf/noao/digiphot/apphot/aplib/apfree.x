include "../lib/apphotdef.h"

# AP_NOISECLS -- Procedure to close up the noise structure and arrays.

procedure ap_noisecls (ap)

pointer ap		# pointer to apphot structure

begin
	if (AP_NOISE(ap) == NULL)
	    return
	call mfree (AP_NOISE(ap), TY_STRUCT)
end


# AP_DISPCLS -- Procedure to close up the dislay structure and arrays.

procedure ap_dispcls (ap)

pointer		ap		# pointer to the apphot structure

begin
	if (AP_PDISPLAY(ap) != NULL)
	    call mfree (AP_PDISPLAY(ap), TY_STRUCT)
end
