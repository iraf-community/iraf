include "../lib/daophotdef.h"
include "../lib/psfdef.h"

# DP_MEMPSF -- Allocate memory for PSF fitting.

procedure dp_memspf (dao)

pointer	dao			# pointer to daophot structure

pointer	psffit, psf

begin
	# Allocate space for the fitted PSF.
	psffit = DP_PSFFIT(dao)
	if (DP_PSFLUT(psffit) != NULL)
	    call mfree (DP_PSFLUT(psffit), TY_REAL)
	if (DP_VARPSF(dao) == YES)
	    call malloc (DP_PSFLUT(psffit), DP_PSFSIZE(psffit) *
	        DP_PSFSIZE(psffit) * 3, TY_REAL)
	else
	    call malloc (DP_PSFLUT(psffit), DP_PSFSIZE(psffit) *
	        DP_PSFSIZE(psffit), TY_REAL)

	# Allocate space for the PSF matrix.
	psf = DP_PSF(dao)
	if (DP_PSFMATRIX(psf) != NULL)
	    call mfree (DP_PSFMATRIX(psf), TY_REAL)
	call malloc (DP_PSFMATRIX(psf), SZ_PSFMATRIX * SZ_PSFMATRIX, TY_REAL)

	# Allocate space for the lookup table.
	if (DP_PLOOKUP(psf) != NULL)
	    call mfree (DP_PLOOKUP(psf), TY_REAL)
	call malloc (DP_PLOOKUP(psf), DP_SZLOOKUP(psf) * DP_SZLOOKUP(psf),
	    TY_REAL)
end
