# XT_PROCID -- Set or ppdate PROCID keyword.

procedure xt_procid (im)

pointer	im			#I Image header

int	i, j, ver, patmake(), gpatmatch(), strlen(), ctoi()
pointer	sp, pat, str

begin
	call smark (sp)
	call salloc (pat, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Get current ID.
	iferr (call imgstr (im, "PROCID", Memc[str], SZ_LINE)) {
	    iferr (call imgstr (im, "OBSID", Memc[str], SZ_LINE)) {
		call sfree (sp)
		return
	    }
	}

	# Set new PROCID.
	ver = 0
	i = patmake ("V[0-9]*$", Memc[pat], SZ_LINE)
	if (gpatmatch (Memc[str], Memc[pat], i, j) == 0)
	    ;
	if (j > 0) {
	    j = i+1
	    if (ctoi (Memc[str], j, ver) == 0)
		ver = 0
	    i = i - 1
	} else
	    i = strlen (Memc[str])
	call sprintf (Memc[str+i], SZ_LINE, "V%d")
	    call pargi (ver+1)
	call imastr (im, "PROCID", Memc[str])
end
