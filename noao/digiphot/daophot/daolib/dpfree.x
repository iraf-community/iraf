include "../lib/daophotdef.h"
include "../lib/apseldef.h"
include "../lib/allstardef.h"

# DP_FREE - Procedure to free the the daophot structure.

procedure dp_free (dp)

pointer	dp		# pointer to the daophot structure

begin
	if (DP_MW(dp) != NULL)
	    call mw_close (DP_MW(dp))
	call mfree (dp, TY_STRUCT)
end


# DP_FITCLOSE -- Procedure to close up the psf fitting structure.

procedure dp_fitclose (dp)

pointer	dp		# pointer to the daophot structure

pointer	psffit

begin
	psffit = DP_PSFFIT(dp)
	if (DP_PSFLUT(psffit) != NULL)
	    call mfree (DP_PSFLUT(psffit), TY_REAL)
	if (DP_PSFPARS(psffit) != NULL)
	    call mfree (DP_PSFPARS(psffit), TY_REAL)
	call mfree (psffit, TY_STRUCT)
end


# DP_APCLOSE -- Procedure to close up the APSEL parameters.

procedure dp_apclose (dp)

pointer	dp		# pointer to daophot structure

pointer	apsel

begin
	apsel = DP_APSEL(dp)

	if (DP_APRESULT(apsel) != NULL)
	    call mfree (DP_APRESULT(apsel), TY_INT)
	if (DP_APID(apsel) != NULL)
	    call mfree (DP_APID(apsel), TY_INT)
	if (DP_APXCEN(apsel) != NULL)
	    call mfree (DP_APXCEN(apsel), TY_REAL)
	if (DP_APYCEN(apsel) != NULL)
	    call mfree (DP_APYCEN(apsel), TY_REAL)
	if (DP_APMAG(apsel) != NULL)
	    call mfree (DP_APMAG(apsel), TY_REAL)
	if (DP_APERR(apsel) != NULL)
	    call mfree (DP_APERR(apsel), TY_REAL)
	if (DP_APMSKY(apsel) != NULL)
	    call mfree (DP_APMSKY(apsel), TY_REAL)
	if (DP_APGROUP(apsel) != NULL)
	    call mfree (DP_APGROUP(apsel), TY_INT)
	if (DP_APNITER(apsel) != NULL)
	    call mfree (DP_APNITER(apsel), TY_INT)
	if (DP_APSHARP(apsel) != NULL)
	    call mfree (DP_APSHARP(apsel), TY_REAL)
	if (DP_APCHI(apsel) != NULL)
	    call mfree (DP_APCHI(apsel), TY_REAL)

	call mfree (apsel, TY_STRUCT)
end
