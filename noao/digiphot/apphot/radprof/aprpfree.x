include "../lib/apphotdef.h"
include "../lib/radprofdef.h"

# AP_RPFREE -- Procedure to free the radial profile fitting structure.

procedure ap_rpfree (ap)

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
	if (AP_RPROF(ap) != NULL)
	    call ap_rpcls (ap)
	if (AP_PSKY(ap) != NULL)
	    call ap_skycls (ap)
	if (AP_IMBUF(ap) != NULL)
	    call mfree (AP_IMBUF(ap), TY_REAL)
	if (AP_MW(ap) != NULL)
	    call mw_close (AP_MW(ap))
	call mfree (ap, TY_STRUCT)
end


# AP_RPCLS -- Procedure to closee up the radial profile fitting arrays.

procedure ap_rpcls (ap)

pointer	ap			# pointer to apphot structure

pointer	rprof

begin
	rprof = AP_RPROF(ap)
	if (rprof == NULL)
	    return
	#if (AP_RPIX(rprof) != NULL)
	    #call mfree (AP_RPIX(rprof), TY_REAL)
	if (AP_RPDIST(rprof) != NULL)
	    call mfree (AP_RPDIST(rprof), TY_REAL)
	if (AP_INTENSITY(rprof) != NULL)
	    call mfree (AP_INTENSITY(rprof), TY_REAL)
	if (AP_DINTENSITY(rprof) != NULL)
	    call mfree (AP_DINTENSITY(rprof), TY_REAL)
	if (AP_TINTENSITY(rprof) != NULL)
	    call mfree (AP_TINTENSITY(rprof), TY_REAL)
	call mfree (rprof, TY_STRUCT)
end
