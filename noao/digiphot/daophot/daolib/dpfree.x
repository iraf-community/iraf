include "../lib/allstardef.h"
include "../lib/apsel.h"
include "../lib/daophotdef.h"
include "../lib/nstardef.h"
include "../lib/psfdef.h"
include "../lib/psf.h"

# DP_FREE - Procedure to free the the daophot structure.

procedure dp_free (dp)

pointer	dp		# pointer to the daophot structure

begin
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
	call mfree (psffit, TY_STRUCT)
end


# DP_PSFCLOSE -- Procedure to set up the PSF parameters.

procedure dp_psfclose (dp)

pointer	dp		# pointer to daophot structure

pointer	psf, psfpl

begin
	psf = DP_PSF(dp)
	if (DP_PSFMATRIX(psf) != NULL)
	    call mfree (DP_PSFMATRIX(psf), TY_REAL)

	if (DP_PLOOKUP(psf) != NULL)
	    call mfree (DP_PLOOKUP(psf), TY_REAL)
	if (DP_COLPOINT(psf) != NULL)
	    call mfree (DP_COLPOINT(psf), TY_INT)

	psfpl = DP_PSFPLOT(psf)
	call mfree (psfpl, TY_STRUCT)

	call mfree (psf, TY_STRUCT)
end


# DP_NSCLOSE -- Procedure to close up the NSTAR parameters.

procedure dp_nsclose (dp)

pointer	dp		# pointer to daophot structure

pointer	nstar

begin
	nstar = DP_NSTAR(dp)

	if (DP_NPIX (nstar) != NULL)
	    call mfree (DP_NPIX (nstar), TY_INT)
	if (DP_NUMER (nstar) != NULL)
	    call mfree (DP_NUMER (nstar), TY_REAL)
	if (DP_DENOM (nstar) != NULL)
	    call mfree (DP_DENOM (nstar), TY_REAL)
	if (DP_RPIXSQ (nstar) != NULL)
	    call mfree (DP_RPIXSQ (nstar), TY_REAL)
	if (DP_SKIP (nstar) != NULL)
	    call mfree (DP_SKIP (nstar), TY_INT)
	if (DP_XCLAMP (nstar) != NULL)
	    call mfree (DP_XCLAMP (nstar), TY_REAL)
	if (DP_XOLD (nstar) != NULL)
	    call mfree (DP_XOLD (nstar), TY_REAL)
	if (DP_X (nstar) != NULL)
	    call mfree (DP_X (nstar), TY_REAL)
	if (DP_V (nstar) != NULL)
	    call mfree (DP_V (nstar), TY_REAL)
	if (DP_SUMWT (nstar) != NULL)
	    call mfree (DP_SUMWT (nstar), TY_REAL)
	if (DP_C (nstar) != NULL)
	    call mfree (DP_C (nstar), TY_REAL)

	call mfree (nstar, TY_STRUCT)
end


# DP_ALCLOSE -- Procedure to close up the ALLSTAR parameters.

procedure dp_alclose (dp)

pointer	dp		# pointer to daophot structure

pointer	allstar

begin
	allstar = DP_ALLSTAR(dp)

	if (DP_DBUF(allstar) != NULL)
	    call mfree (DP_DBUF(allstar), TY_REAL)
	if (DP_SBUF(allstar) != NULL)
	    call mfree (DP_SBUF(allstar), TY_REAL)
	if (DP_WBUF(allstar) != NULL)
	    call mfree (DP_WBUF(allstar), TY_SHORT)

	if (DP_ANUMER1 (allstar) != NULL)
	    call mfree (DP_ANUMER1 (allstar), TY_REAL)
	if (DP_ANUMER2 (allstar) != NULL)
	    call mfree (DP_ANUMER2 (allstar), TY_REAL)
	if (DP_ADENOM1 (allstar) != NULL)
	    call mfree (DP_ADENOM1 (allstar), TY_REAL)
	if (DP_ADENOM2 (allstar) != NULL)
	    call mfree (DP_ADENOM2 (allstar), TY_REAL)
	if (DP_ARPIXSQ (allstar) != NULL)
	    call mfree (DP_ARPIXSQ (allstar), TY_REAL)
	if (DP_ASUMWT (allstar) != NULL)
	    call mfree (DP_ASUMWT (allstar), TY_REAL)
	if (DP_ASKIP (allstar) != NULL)
	    call mfree (DP_ASKIP (allstar), TY_INT)
	if (DP_ALAST (allstar) != NULL)
	    call mfree (DP_ALAST (allstar), TY_INT)
	if (DP_AXCLAMP (allstar) != NULL)
	    call mfree (DP_AXCLAMP (allstar), TY_REAL)
	if (DP_AYCLAMP (allstar) != NULL)
	    call mfree (DP_AYCLAMP (allstar), TY_REAL)
	if (DP_AXOLD (allstar) != NULL)
	    call mfree (DP_AXOLD (allstar), TY_REAL)
	if (DP_AYOLD (allstar) != NULL)
	    call mfree (DP_AYOLD (allstar), TY_REAL)
	if (DP_AX (allstar) != NULL)
	    call mfree (DP_AX (allstar), TY_REAL)
	if (DP_AV (allstar) != NULL)
	    call mfree (DP_AV (allstar), TY_REAL)
	if (DP_AC (allstar) != NULL)
	    call mfree (DP_AC (allstar), TY_REAL)
	if (DP_ANPIX (allstar) != NULL)
	    call mfree (DP_ANPIX (allstar), TY_INT)

	call fixmem (DP_SZOLDCACHE(allstar))
	call mfree (allstar, TY_STRUCT)
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
