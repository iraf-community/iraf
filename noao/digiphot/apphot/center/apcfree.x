include "../lib/apphotdef.h"
include "../lib/centerdef.h"

# APCFREE -- Procedure to free the centering data structure.

procedure apcfree (ap)

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
	if (AP_IMBUF(ap) != NULL)
	    call mfree (AP_IMBUF(ap), TY_REAL)
	if (AP_MW(ap) != NULL)
	    call mw_close (AP_MW(ap))
	call mfree (ap, TY_STRUCT)
end


# AP_CTRCLS -- Procedure to close up the centering structure arrays.

procedure ap_ctrcls (ap)

pointer	ap			# pointer to apphot structure

pointer	ctr

begin
	ctr = AP_PCENTER(ap)
	if (ctr == NULL)
	    return
	if (AP_CTRPIX(ctr) != NULL)
	    call mfree (AP_CTRPIX(ctr), TY_REAL)
	if (AP_XCTRPIX(ctr) != NULL)
	    call mfree (AP_XCTRPIX(ctr), TY_REAL)
	if (AP_YCTRPIX(ctr) != NULL)
	    call mfree (AP_YCTRPIX(ctr), TY_REAL)
	call mfree (AP_PCENTER(ap), TY_STRUCT)
end
