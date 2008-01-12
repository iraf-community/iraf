# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# FILES -- Expand a file name template into a list of file names on the
# standard output.

procedure t_files()

int	list
pointer	sp, template, fname

bool	clgetb()
int	fntopnb(), fntgfnb(), clgeti(), btoi()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (template, SZ_LINE, TY_CHAR)

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
