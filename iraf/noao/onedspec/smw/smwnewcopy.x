include <smw.h>


# SMW_NEWCOPY -- Make a new copy of an SMW structure.

pointer procedure smw_newcopy (smw)

pointer smw             #I SMW pointer to copy
pointer new 	        #O SMW copy

long	i
size_t	nspec, sz_val
pointer mw_newcopy(), mw_sctran()

begin
	sz_val = SMW_LEN(SMW_NMW(smw))
	call calloc (new, sz_val, TY_STRUCT)
	sz_val = SMW_LEN(SMW_NMW(smw))
	call amovp (Memp[smw], Memp[new], sz_val)

	if (SMW_APID(smw) != NULL) {
	    sz_val = SZ_LINE
	    call malloc (SMW_APID(new), sz_val, TY_CHAR)
	    call strcpy (Memc[SMW_APID(smw)], Memc[SMW_APID(new)], SZ_LINE)
	}

	nspec = SMW_NSPEC(smw)
	if (SMW_APS(smw) != NULL) {
	    call malloc (SMW_APS(new), nspec, TY_LONG)
	    call amovl (Meml[SMW_APS(smw)], Meml[SMW_APS(new)], nspec)
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
		if (Memp[SMW_APIDS(smw)+i] != NULL) {
		    sz_val = SZ_LINE
		    call malloc (Memp[SMW_APIDS(new)+i], sz_val, TY_CHAR)
		    call strcpy (Memc[Memp[SMW_APIDS(smw)+i]],
			Memc[Memp[SMW_APIDS(new)+i]], SZ_LINE)
		}
	    }
	}

        do i = 0, SMW_NMW(smw)-1
            SMW_MW(new,i) = mw_newcopy (SMW_MW(smw,i))

	if (SMW_PDIM(smw) > 1)
	    SMW_CTLP(new) = mw_sctran (SMW_MW(new,0), "logical", "physical", 2)

	return (new)
end
