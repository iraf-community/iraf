include	<smw.h>


# SMW_OPEN -- Open SMW structure.
# The basic MWCS pointer and a template SMW pointer or image is input
# and the SMW pointer is returned in its place.

procedure smw_open (mw, smw1, im)

pointer	mw		#U MWCS pointer input and SMW pointer output
pointer	smw1		#I Template SMW pointer
pointer	im		#I Template IMIO pointer

size_t	sz_val
int	format
long	nmw, i
size_t	nspec
pointer	sp, sys, smw
int	strdic()
pointer	mw_sctran(), mw_newcopy()
errchk	smw_daxis, smw_saxes, mw_sctran

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (sys, sz_val, TY_CHAR)

	call mw_gwattrs (mw, 0, "system", Memc[sys], SZ_FNAME)
	format = strdic (Memc[sys], Memc[sys], SZ_FNAME, SMW_FORMATS)

	sz_val = SMW_LEN(1)
	call calloc (smw, sz_val, TY_STRUCT)
	sz_val = SZ_LINE
	call malloc (SMW_APID(smw), sz_val, TY_CHAR)
	SMW_FORMAT(smw) = format
	SMW_DTYPE(smw) = INDEFI
	SMW_NMW(smw) = 1
	SMW_MW(smw,0) = mw

	switch (format) {
	case SMW_ND:
	    call smw_daxis (smw, im, INDEFI, INDEFI, INDEFI)
	    call smw_saxes (smw, smw1, im)

	case SMW_ES:
	    call smw_saxes (smw, smw1, im)

	    nspec = SMW_NSPEC(smw)
	    call calloc (SMW_APS(smw), nspec, TY_LONG)
	    call calloc (SMW_BEAMS(smw), nspec, TY_INT)
	    call calloc (SMW_APLOW(smw), 2*nspec, TY_REAL)
	    call calloc (SMW_APHIGH(smw), 2*nspec, TY_REAL)
	    call calloc (SMW_APIDS(smw), nspec, TY_POINTER)
	    if (SMW_PDIM(smw) > 1)
		SMW_CTLP(smw) = mw_sctran (mw, "logical", "physical", 2)

	case SMW_MS:
	    call smw_saxes (smw, smw1, im)

	    nspec = SMW_NSPEC(smw)
	    call calloc (SMW_APIDS(smw), nspec, TY_POINTER)
	    if (SMW_PDIM(smw) > 1)
		SMW_CTLP(smw) = mw_sctran (mw, "logical", "physical", 2)

	    nmw = 1 + (nspec - 1) / SMW_NSPLIT
	    if (nmw > 1) {
		sz_val = SMW_LEN(nmw)
		call realloc (smw, sz_val, TY_STRUCT)
		call calloc (SMW_APS(smw), nspec, TY_LONG)
	    }
	    do i = 1, nmw-1
		SMW_MW(smw,i) = mw_newcopy (mw)
	    SMW_NMW(smw) = nmw
	}

	mw = smw
	    
	call sfree (sp)
end
