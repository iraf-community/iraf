include "../lib/apphotdef.h"

# AP_FDFREE -- Procedure to free the data structure.

procedure ap_fdfree (ap)

pointer	ap		# pointer to the apphot structure

begin
	if (ap == NULL)
	    return
	if (AP_NOISE(ap) != NULL)
	    call ap_noisecls (ap)
	if (AP_FIND(ap) != NULL)
	    call ap_fdcls (ap)
	call mfree (ap, TY_STRUCT)
end


# AP_FDCLS -- Procedure to free the find data structure.

procedure ap_fdcls (ap)

pointer	ap		# pointer to the apphot structure

begin
	call mfree (AP_FIND(ap), TY_STRUCT)
end
