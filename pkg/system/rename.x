# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>

# RENAME -- Rename a file, or rename a list of files.  In the latter case
# the ldir, root, or extn field in each of the input files is changed to
# the value of the "output file", which in this case is the value of the
# specified field of an output file name.  Thus,
# 	rename "task.*", newtask
# would rename task.x as newtask.x, task.cl as newtask.cl, and so on.

define	F_ROOT		1
define	F_EXTN		2


procedure t_rename()

pointer	sp, oldfile, newname, field, ldir, root, extn
int	list, junk, modfield
bool	streq()
int	clpopni(), clgfil(), clplen()
int	strlen(), fnldir(), fnroot(), fnextn()

begin
	call smark (sp)
	call salloc (oldfile, SZ_FNAME, TY_CHAR)
	call salloc (newname, SZ_FNAME, TY_CHAR)
	call salloc (field, SZ_FNAME, TY_CHAR)
	call salloc (ldir, SZ_FNAME, TY_CHAR)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)

	# Open the list of file to be renamed.  This is done first so that
	# the old file name is queried for before the new file name.

	list = clpopni ("files")

	# Get the output file name, or the new name for the field to be
	# changed.
	call clgstr ("newname", Memc[newname], SZ_FNAME)
	call clgstr ("field", Memc[field], SZ_FNAME)

	if (streq (Memc[field], "root")) {
	    modfield = F_ROOT
	    call strcpy (Memc[newname], Memc[root], SZ_FNAME)
	} else if (streq (Memc[field], "extn")) {
	    modfield = F_EXTN
	    call strcpy (Memc[newname], Memc[extn], SZ_FNAME)
	} else {
	    call clpcls (list)
	    call error (1, "rename: unrecognized filename field code")
	}

	# If there is only one file in the list, rename it with the newname
	# and thats that.  Otherwise, change the indicated field of each
	# filename.

	while (clgfil (list, Memc[oldfile], SZ_FNAME) != EOF) {
	    if (clplen (list) > 1) {
		if (fnldir (Memc[oldfile], Memc[newname], SZ_FNAME) > 0)
		    call strcat ("$", Memc[newname], SZ_FNAME)
		if (modfield != F_ROOT)
		    junk = fnroot (Memc[oldfile], Memc[root], SZ_FNAME)
		call strcat (Memc[root], Memc[newname], SZ_FNAME)
		if (modfield != F_EXTN)
		    junk = fnextn (Memc[oldfile], Memc[extn], SZ_FNAME)
		if (strlen (Memc[extn]) > 0) {
		    call strcat (".", Memc[newname], SZ_FNAME)
		    call strcat (Memc[extn], Memc[newname], SZ_FNAME)
		}
	    }
	    iferr (call rename (Memc[oldfile], Memc[newname]))
		call erract (EA_WARN)
	}

	call clpcls (list)
	call sfree (sp)
end
