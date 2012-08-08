include "../lib/apphotdef.h"

# AP_FDFREE -- Free the apphot data structure.

procedure ap_fdfree (ap)

pointer	ap		# pointer to the apphot structure

begin
	if (ap == NULL)
	    return
	if (AP_NOISE(ap) != NULL)
	    call ap_noisecls (ap)
	if (AP_PFIND(ap) != NULL)
	    call ap_fdcls (ap)
	if (AP_PDISPLAY(ap) != NULL)
	    call ap_dispcls (ap)
	if (AP_IMBUF(ap) != NULL)
	    call mfree (AP_IMBUF(ap), TY_REAL)
	if (AP_MW(ap) != NULL)
	    call mw_close (AP_MW(ap))
	call mfree (ap, TY_STRUCT)
end


# AP_FDCLS -- Free the find data structure.

procedure ap_fdcls (ap)

pointer	ap		# pointer to the apphot structure

begin
	call mfree (AP_PFIND(ap), TY_STRUCT)
end
