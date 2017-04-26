include	<smw.h>


# SMW_CLOSE -- Close the SMW data structure.
# This includes closing the MWCS pointers.

procedure smw_close (smw)

pointer	smw		# SMW pointer

int	i
pointer	apids

begin
	if (smw == NULL)
	    return

	switch (SMW_FORMAT(smw)) {
	case SMW_ND:
	    call mfree (SMW_APID(smw), TY_CHAR)
	    call mw_close (SMW_MW(smw,0))
	case SMW_ES:
	    call mfree (SMW_APS(smw), TY_INT)
	    call mfree (SMW_BEAMS(smw), TY_INT)
	    call mfree (SMW_APLOW(smw), TY_REAL)
	    call mfree (SMW_APHIGH(smw), TY_REAL)
	    call mfree (SMW_APID(smw), TY_CHAR)
	    apids = SMW_APIDS(smw) - 1
	    do i = 1, SMW_NSPEC(smw)
		call mfree (Memi[apids+i], TY_CHAR)
	    call mfree (SMW_APIDS(smw), TY_POINTER)
	    call mw_close (SMW_MW(smw,0))
	case SMW_MS:
	    call mfree (SMW_APS(smw), TY_INT)
	    call mfree (SMW_BEAMS(smw), TY_INT)
	    call mfree (SMW_APLOW(smw), TY_REAL)
	    call mfree (SMW_APHIGH(smw), TY_REAL)
	    call mfree (SMW_APID(smw), TY_CHAR)
	    apids = SMW_APIDS(smw) - 1
	    do i = 1, SMW_NSPEC(smw)
		call mfree (Memi[apids+i], TY_CHAR)
	    do i = 0, SMW_NMW(smw)-1
		call mw_close (SMW_MW(smw,i))
	}
	call mfree (smw, TY_STRUCT)
end
