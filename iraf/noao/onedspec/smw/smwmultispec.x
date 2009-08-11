include	<smw.h>


# SMW_MULTISPEC -- Setup the MULTISPEC SMW parameters.

procedure smw_multispec (im, smw)

pointer	im			#I IMIO pointer
pointer	smw			#U MWCS pointer input SMW pointer output

size_t	sz_val
long	i, j, k, c_1
pointer	sp, key, val, mw
errchk	smw_open, smw_saxes, smw_sapid

include	<nullptr.inc>

begin
	c_1 = 1

	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (key, sz_val, TY_CHAR)
	sz_val = SZ_LINE
	call salloc (val, sz_val, TY_CHAR)

	call smw_open (smw, NULLPTR, im)
	do i = 1, SMW_NSPEC(smw) {
	    call smw_mw (smw, i, c_1, mw, j, k)
	    call sprintf (Memc[key], SZ_FNAME, "APID%d")
		call pargl (j)
	    ifnoerr (call imgstr (im, Memc[key], Memc[val], SZ_LINE))
		call smw_sapid (smw, i, c_1, Memc[val])
	}

	call sfree (sp)
end
