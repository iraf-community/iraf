# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>

# MOVEFILES -- Move a set of files to another directory.  If the destination
# is not a directory it is a fatal error.  Ambiguous directory specifications
# are resolved in favor of subdirectories, rather than logical directories.

procedure t_movefiles()

bool	verbose
int	list, root_len
pointer	sp, fname, newdir, pathname, newname, junkstr
bool	clgetb()
int	clpopni(), clgfil(), fnldir(), isdirectory()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (newdir, SZ_FNAME, TY_CHAR)
	call salloc (pathname, SZ_PATHNAME, TY_CHAR)
	call salloc (newname, SZ_PATHNAME, TY_CHAR)
	call salloc (junkstr, SZ_FNAME, TY_CHAR)

	list = clpopni ("files")
	call clgstr ("newdir", Memc[newdir], SZ_FNAME)
	verbose = clgetb ("verbose")

	if (isdirectory (Memc[newdir], Memc[pathname], SZ_PATHNAME) == 0) {
	    call strcat ("$", Memc[newdir], SZ_FNAME)
	    if (isdirectory (Memc[newdir], Memc[pathname], SZ_PATHNAME) == 0)
		call error (1, "destination is not a directory")
	}

	while (clgfil (list, Memc[fname], SZ_FNAME) != EOF) {
	    call strcpy (Memc[pathname], Memc[newname], SZ_PATHNAME)
	    root_len = fnldir (Memc[fname], Memc[junkstr], SZ_FNAME)
	    call strcat (Memc[fname+root_len], Memc[newname], SZ_PATHNAME)

	    if (verbose) {
		call eprintf ("%s -> %s\n")
		    call pargstr (Memc[fname])
		    call pargstr (Memc[newname])
	    }

	    iferr (call rename (Memc[fname], Memc[newname]))
		call erract (EA_WARN)
	}

	call clpcls (list)
	call sfree (sp)
end
