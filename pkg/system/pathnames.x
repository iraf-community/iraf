# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# PATHNAMES -- Convert one or more filenames into OS pathnames.  If the input
# is redirected, filenames are read from the standard input.  Otherwise,
# a file name template is expanded and used as the list of filenames to be
# transformed.  If called without any arguments, return the pathname of the
# current working directory.

procedure t_pathnames()

int	list
pointer	sp, fname, osfname
bool	streq(), clgetb()
int	clgeti(), clpopni(), clpopnu(), clgfil(), fscan()

begin
	call smark (sp)
	call salloc (fname, SZ_LINE, TY_CHAR)
	call salloc (osfname, SZ_LINE, TY_CHAR)

	# If no arguments, return the pathame of the current
	# directory: do not prompt for the template.

	if (clgeti ("$nargs") == 0) {
	    call fpathname ("", Memc[osfname], SZ_LINE)
	    call printf ("%s\n")
		call pargstr (Memc[osfname])
	    call sfree (sp)
	    return
	}

	# Expand template, output pathname of each file therein.
	if (clgetb ("sort"))
	    list = clpopni ("template")
	else
	    list = clpopnu ("template")

	while (clgfil (list, Memc[fname], SZ_FNAME) != EOF) {
	    if (streq (Memc[fname], "STDIN")) {
		while (fscan (STDIN) != EOF) {
		    call gargstr (Memc[fname], SZ_LINE)
		    call fpathname (Memc[fname], Memc[osfname], SZ_LINE)
		    call printf ("%s\n")
			call pargstr (Memc[osfname])
		}
	    } else {
		call fpathname (Memc[fname], Memc[osfname], SZ_LINE)
		call printf ("%s\n")
		    call pargstr (Memc[osfname])
	    }
	}

	call clpcls (list)
	call sfree (sp)
end
