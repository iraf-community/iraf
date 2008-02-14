# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>

# MOVEFILES -- Move a set of files to another directory.  If the destination
# is not a directory it is a fatal error.  Ambiguous directory specifications
# are resolved in favor of subdirectories, rather than logical directories.

procedure t_movefiles()

bool	verbose
int	root_len
pointer	sp, fname, newdir, pathname, newname, junkstr, list
size_t	sz_val
bool	clgetb()
int	clgfil(), fnldir(), isdirectory()
pointer	clpopni()

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (fname, sz_val, TY_CHAR)
	call salloc (newdir, sz_val, TY_CHAR)
	sz_val = SZ_PATHNAME
	call salloc (pathname, sz_val, TY_CHAR)
	call salloc (newname, sz_val, TY_CHAR)
	sz_val = SZ_FNAME
	call salloc (junkstr, sz_val, TY_CHAR)

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
