include "../lib/daophotdef.h"
include "../lib/psfdef.h"


# DP_PSFSETUP -- Procedure to set up the PSF parameters.

procedure dp_psfsetup (dp)

pointer	dp		# pointer to daophot structure

pointer	psf

begin
	# Allocate memory for the psf fitting structure.
	call malloc (DP_PSF(dp), LEN_PSFSTRUCT, TY_STRUCT)
	psf = DP_PSF(dp)

	# Set the size of the data box t be extracted.
	DP_LENUSERAREA(psf) = MIN_LENUSERAREA

	# Initialize the psf fitting structure.
	DP_PC(psf) = NULL
	DP_PV(psf) = NULL
	DP_PTMP(psf) = NULL
	DP_PZ(psf) = NULL
	DP_PCLAMP(psf) = NULL
	DP_POLD(psf) = NULL
	DP_PSIGANA(psf) = 0.0
	DP_PSUMANA(psf) = 0.0

	# Initialize the parameters for the current PSF star.
	DP_CUR_PSF(psf) = 0
	DP_CUR_PSFID(psf) = 0
	DP_CUR_PSFX(psf) = INDEFR
	DP_CUR_PSFY(psf) = INDEFR
	DP_CUR_PSFSKY(psf) = INDEFR
	DP_CUR_PSFMAG(psf) = INDEFR

	# Initialize the PSF star list arrays.
	DP_PNUM (psf) = 0
	DP_PXCEN(psf) = NULL
	DP_PYCEN(psf) = NULL
	DP_PMAG(psf) = NULL
	DP_PH(psf) = NULL
	DP_PWEIGHT(psf) = NULL
	DP_PSAT(psf) = NULL
	DP_PXCLAMP(psf) = NULL
	DP_PYCLAMP(psf) = NULL
	DP_PXOLD(psf) = NULL
	DP_PYOLD(psf) = NULL

	# Initialize the look-up table arrays.
	DP_PSUMN(psf) = NULL
	DP_PSUMW(psf) = NULL
	DP_PSUMSQ(psf) = NULL
	DP_PSIGMA(psf) = NULL
	DP_PCONST(psf) = NULL
	DP_POLDLUT(psf) = NULL

	# Allocate space for and initialize the plot sub-structure.
	DP_PLOTTYPE(psf) = PSF_MESHPLOT
	DP_MANGV(psf)	= 30.
	DP_MANGH(psf) = -30.
	DP_MFLOOR(psf) = 0.
	DP_MCEILING(psf) = 0.
	DP_CFLOOR(psf) = 0.0
	DP_CCEILING(psf) = 0.0
end


# DP_LMEMPSF -- Allocate memory required for fitting the list of psf stars.

procedure dp_lmempsf (dao)

pointer	dao			# pointer to daophot structure

pointer	psf

begin
	psf = DP_PSF(dao)
	if (DP_PNUM(psf) > 0) {
	    call realloc (DP_PXCEN(psf), DP_PNUM(psf), TY_REAL)
	    call realloc (DP_PYCEN(psf), DP_PNUM(psf), TY_REAL)
	    call realloc (DP_PMAG(psf), DP_PNUM(psf), TY_REAL)
	    call realloc (DP_PH(psf), DP_PNUM(psf), TY_REAL)
	    call realloc (DP_PWEIGHT(psf), DP_PNUM(psf), TY_REAL)
	    call realloc (DP_PSAT(psf), DP_PNUM(psf), TY_INT)
	    call realloc (DP_PXCLAMP(psf), DP_PNUM(psf), TY_REAL)
	    call realloc (DP_PYCLAMP(psf), DP_PNUM(psf), TY_REAL)
	    call realloc (DP_PXOLD(psf), DP_PNUM(psf), TY_REAL)
	    call realloc (DP_PYOLD(psf), DP_PNUM(psf), TY_REAL)
	}
end


# DP_AMEMPSF -- Allocate the memory required for fitting the analytic
# PSF.

procedure dp_amempsf (dao)

pointer	dao			# pointer to daophot structure

int	npars
pointer	psf, psffit

begin
	psf = DP_PSF(dao)
	psffit = DP_PSFFIT(dao)
	npars = DP_PSFNPARS(psffit)

	call realloc (DP_PC(psf), npars * npars, TY_REAL)
	call realloc (DP_PV(psf), npars, TY_REAL)
	call realloc (DP_PZ(psf), npars, TY_REAL)
	call realloc (DP_PCLAMP(psf), npars, TY_REAL)
	call realloc (DP_POLD(psf), npars, TY_REAL)
	call realloc (DP_PTMP(psf), MAX_NFCTNPARS, TY_REAL)
end


# DP_TMEMPSF -- Allocate the memory required for fitting the lookup tables.

procedure dp_tmempsf (dao)

pointer	dao			# pointer to daophot structure

int	nexp
pointer	psf, psffit

begin
	psf = DP_PSF(dao)
	psffit = DP_PSFFIT(dao)

	nexp = DP_NVLTABLE(psffit) + DP_NFEXTABLE(psffit)
	DP_PSFSIZE(psffit) = 2 * (nint (2.0 * DP_PSFRAD(dao)) + 1) + 1
	DP_PSFRAD(dao) = (real (DP_PSFSIZE(psffit) - 1) / 2.0 - 1.0) / 2.0
	DP_SPSFRAD(dao) = DP_PSFRAD(dao) * DP_SCALE(dao)
	DP_RPSFRAD(dao) = DP_PSFRAD(dao) * DP_SCALE(dao)

	call realloc (DP_PC(psf), nexp * nexp, TY_REAL)
	call realloc (DP_PV(psf), nexp, TY_REAL)
	call realloc (DP_PTMP(psf), nexp, TY_REAL)

	call realloc (DP_PSUMN(psf), DP_PSFSIZE(psffit) * DP_PSFSIZE(psffit),
	    TY_REAL)
	call realloc (DP_PSUMSQ(psf), DP_PSFSIZE(psffit) * DP_PSFSIZE(psffit),
	    TY_REAL)
	call realloc (DP_PSIGMA(psf), DP_PSFSIZE(psffit) * DP_PSFSIZE(psffit),
	    TY_REAL)
	call realloc (DP_POLDLUT(psf), nexp * DP_PSFSIZE(psffit) *
	    DP_PSFSIZE(psffit), TY_REAL)
	if (nexp > 1)
	    call realloc (DP_PCONST(psf), DP_PSFSIZE(psffit) *
	        DP_PSFSIZE(psffit), TY_REAL)

	call realloc (DP_PSFLUT(psffit), nexp * DP_PSFSIZE(psffit) *
	    DP_PSFSIZE(psffit), TY_REAL)
end


# DP_PSFCLOSE -- Procedure to set up the PSF parameters.

procedure dp_psfclose (dp)

pointer	dp		# pointer to daophot structure

pointer	psf

begin
	psf = DP_PSF(dp)

	if (DP_PC(psf) != NULL)
	    call mfree (DP_PC(psf), TY_REAL)
	if (DP_PV(psf) != NULL)
	    call mfree (DP_PV(psf), TY_REAL)
	if (DP_PTMP(psf) != NULL)
	    call mfree (DP_PTMP(psf), TY_REAL)
	if (DP_PZ(psf) != NULL)
	    call mfree (DP_PZ(psf), TY_REAL)
	if (DP_PCLAMP(psf) != NULL)
	    call mfree (DP_PCLAMP(psf), TY_REAL)
	if (DP_POLD(psf) != NULL)
	    call mfree (DP_POLD(psf), TY_REAL)

	if (DP_PXCEN(psf) != NULL)
	    call mfree (DP_PXCEN(psf), TY_REAL)
	if (DP_PYCEN(psf) != NULL)
	    call mfree (DP_PYCEN(psf), TY_REAL)
	if (DP_PMAG(psf) != NULL)
	    call mfree (DP_PMAG(psf), TY_REAL)
	if (DP_PH(psf) != NULL)
	    call mfree (DP_PH(psf), TY_REAL)
	if (DP_PWEIGHT(psf) != NULL)
	    call mfree (DP_PWEIGHT(psf), TY_REAL)
	if (DP_PSAT(psf) != NULL)
	    call mfree (DP_PSAT(psf), TY_INT)
	if (DP_PXCLAMP(psf) != NULL)
	    call mfree (DP_PXCLAMP(psf), TY_REAL)
	if (DP_PYCLAMP(psf) != NULL)
	    call mfree (DP_PYCLAMP(psf), TY_REAL)
	if (DP_PXOLD(psf) != NULL)
	    call mfree (DP_PXOLD(psf), TY_REAL)
	if (DP_PYOLD(psf) != NULL)
	    call mfree (DP_PYOLD(psf), TY_REAL)

	if (DP_PSUMN(psf) != NULL)
	    call mfree (DP_PSUMN(psf), TY_REAL)
	if (DP_PSUMSQ(psf) != NULL)
	    call mfree (DP_PSUMSQ(psf), TY_REAL)
	if (DP_PSIGMA(psf) != NULL)
	    call mfree (DP_PSIGMA(psf), TY_REAL)
	if (DP_PCONST(psf) != NULL)
	    call mfree (DP_PCONST(psf), TY_REAL)
	if (DP_POLDLUT(psf) != NULL)
	    call mfree (DP_POLDLUT(psf), TY_REAL)

	call mfree (psf, TY_STRUCT)
end
