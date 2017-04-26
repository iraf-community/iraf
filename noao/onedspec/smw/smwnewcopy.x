include <smw.h>


# SMW_NEWCOPY -- Make a new copy of an SMW structure.

pointer procedure smw_newcopy (smw)

pointer smw             #I SMW pointer to copy
pointer new 	        #O SMW copy

int     i, nspec
pointer mw_newcopy(), mw_sctran()

begin
	call calloc (new, SMW_LEN(SMW_NMW(smw)), TY_STRUCT)
	call amovi (Memi[smw], Memi[new], SMW_LEN(SMW_NMW(smw)))

	if (SMW_APID(smw) != NULL) {
	    call malloc (SMW_APID(new), SZ_LINE, TY_CHAR)
	    call strcpy (Memc[SMW_APID(smw)], Memc[SMW_APID(new)], SZ_LINE)
	}

	nspec = SMW_NSPEC(smw)
	if (SMW_APS(smw) != NULL) {
	    call malloc (SMW_APS(new), nspec, TY_INT)
	    call amovi (Memi[SMW_APS(smw)], Memi[SMW_APS(new)], nspec)
	}
	if (SMW_BEAMS(smw) != NULL) {
	    call malloc (SMW_BEAMS(new), nspec, TY_INT)
	    call amovi (Memi[SMW_BEAMS(smw)], Memi[SMW_BEAMS(new)], nspec)
	}
	if (SMW_APLOW(smw) != NULL) {
	    call malloc (SMW_APLOW(new), 2*nspec, TY_REAL)
	    call amovr (Memr[SMW_APLOW(smw)], Memr[SMW_APLOW(new)], 2*nspec)
	}
	if (SMW_APHIGH(smw) != NULL) {
	    call malloc (SMW_APHIGH(new), 2*nspec, TY_REAL)
	    call amovr (Memr[SMW_APHIGH(smw)], Memr[SMW_APHIGH(new)], 2*nspec)
	}
	if (SMW_APIDS(smw) != NULL) {
	    call calloc (SMW_APIDS(new), nspec, TY_POINTER)
	    do i = 0, nspec-1 {
		if (Memi[SMW_APIDS(smw)+i] != NULL) {
		    call malloc (Memi[SMW_APIDS(new)+i], SZ_LINE, TY_CHAR)
		    call strcpy (Memc[Memi[SMW_APIDS(smw)+i]],
			Memc[Memi[SMW_APIDS(new)+i]], SZ_LINE)
		}
	    }
	}

        do i = 0, SMW_NMW(smw)-1
            SMW_MW(new,i) = mw_newcopy (SMW_MW(smw,i))

	if (SMW_PDIM(smw) > 1)
	    SMW_CTLP(new) = mw_sctran (SMW_MW(new,0), "logical", "physical", 2)

	return (new)
end
