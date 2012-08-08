include	<smw.h>


# SMW_MULTISPEC -- Setup the MULTISPEC SMW parameters.

procedure smw_multispec (im, smw)

pointer	im			#I IMIO pointer
pointer	smw			#U MWCS pointer input SMW pointer output

int	i, j, k
pointer	sp, key, val, mw
errchk	smw_open, smw_saxes, smw_sapid

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (val, SZ_LINE, TY_CHAR)

	call smw_open (smw, NULL, im)
	do i = 1, SMW_NSPEC(smw) {
	    call smw_mw (smw, i, 1, mw, j, k)
	    call sprintf (Memc[key], SZ_FNAME, "APID%d")
		call pargi (j)
	    ifnoerr (call imgstr (im, Memc[key], Memc[val], SZ_LINE))
		call smw_sapid (smw, i, 1, Memc[val])
	}

	call sfree (sp)
end
