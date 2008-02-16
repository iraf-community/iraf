# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# FILES -- Expand a file name template into a list of file names on the
# standard output.

procedure t_files()

pointer	list
pointer	sp, template, fname

size_t	sz_val
bool	clgetb()
int	fntgfnb(), clgeti(), btoi()
pointer	fntopnb()

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (fname, sz_val, TY_CHAR)
	sz_val = SZ_LINE
	call salloc (template, sz_val, TY_CHAR)

	if (clgeti ("$nargs") > 0)
	    call clgstr ("template", Memc[template], SZ_LINE)
	else
	    call strcpy ("*", Memc[template], SZ_LINE)

	list = fntopnb (Memc[template], btoi(clgetb("sort")))
	while (fntgfnb (list, Memc[fname], SZ_FNAME) != EOF) {
	    call printf ("%s\n")
		call pargstr (Memc[fname])
	}

	call fntclsb (list)
	call sfree (sp)
end
