# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>

define	F_ROOT		1
define	F_EXTN		2
define	F_ALL		3

# RENAME -- Rename a file, or rename a list of files.  In the latter case
# the ldir, root, or extn field in each of the input files is changed to
# the value of the "output file", which in this case is the value of the
# specified field of an output file name.  Thus,
#
# 	cl> rename task.* newtask
#
# would rename task.x as newtask.x, task.cl as newtask.cl, and so on.
# If 'newtask' is a logical or host directory the input files are moved to
# this directory with the same name.

procedure t_rename()

pointer	sp, oldfile, newname, field, ldir, root, extn
pointer	pathname, newfile, junkstr, fextn
int	list, len, modfield
bool	streq()
int	clpopni(), clgfil(), clplen()
int	strlen(), fnldir(), fnroot(), fnextn(), isdirectory()

begin
	call smark (sp)
	call salloc (oldfile, SZ_FNAME, TY_CHAR)
	call salloc (newname, SZ_PATHNAME, TY_CHAR)
	call salloc (pathname, SZ_PATHNAME, TY_CHAR)
	call salloc (newfile, SZ_PATHNAME, TY_CHAR)
	call salloc (junkstr, SZ_FNAME, TY_CHAR)
	call salloc (field, SZ_FNAME, TY_CHAR)
	call salloc (ldir, SZ_FNAME, TY_CHAR)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)
	call salloc (fextn, SZ_FNAME, TY_CHAR)

	# Open the list of file to be renamed.  This is done first so that
	# the old file name is queried for before the new file name.

	list = clpopni ("files")

	# Get the output file name, or the new name for the field to be
	# changed.
	call clgstr ("newname", Memc[newname], SZ_PATHNAME)
	call clgstr ("field", Memc[field], SZ_FNAME)

	if (streq (Memc[field], "root")) {
	    modfield = F_ROOT
	    call strcpy (Memc[newname], Memc[root], SZ_PATHNAME)
	} else if (streq (Memc[field], "extn")) {
	    modfield = F_EXTN
	    call strcpy (Memc[newname], Memc[extn], SZ_PATHNAME)
	} else if (streq (Memc[field], "all")) {
	    modfield = F_ALL
	} else {
	    call clpcls (list)
	    call error (1, "rename: unrecognized filename field code")
	}

	# See if the new name is actually a directory.  If so, move the input
	# files to this directory, otherwise do the renaming as we've always
	# done.

	if (isdirectory (Memc[newname], Memc[pathname], SZ_PATHNAME) != 0) {
	    # Move each of the files in the list to the destination directory.

	    while (clgfil (list, Memc[oldfile], SZ_FNAME) != EOF) {
                call strcpy (Memc[pathname], Memc[newfile], SZ_PATHNAME)
                len = fnldir (Memc[oldfile], Memc[junkstr], SZ_FNAME)
                call strcat (Memc[oldfile+len], Memc[newfile], SZ_PATHNAME)

	        iferr (call rename (Memc[oldfile], Memc[newfile]))
		    call erract (EA_WARN)
	    }
	} else {
	    # If there is only one file in the list, rename it with the newname
	    # and thats that.  Otherwise, change the indicated field of each
	    # filename.

	    while (clgfil (list, Memc[oldfile], SZ_FNAME) != EOF) {
	        if (clplen (list) > 1) {
		    if (modfield == F_ALL)
                	call error (1, "Newname is not a directory.")

		    if (fnldir (Memc[oldfile], Memc[newname], SZ_PATHNAME) > 0)
		        call strcat ("$", Memc[newname], SZ_PATHNAME)
		    if (modfield != F_ROOT) {
		        len = fnroot (Memc[oldfile], Memc[root], SZ_FNAME)
		        len = fnextn (Memc[oldfile], Memc[fextn], SZ_FNAME)
		    }
		    call strcat (Memc[root], Memc[newname], SZ_PATHNAME)
		    if (modfield != F_EXTN)
		        len = fnextn (Memc[oldfile], Memc[extn], SZ_FNAME)

		    # If the file doesn't have an extension and we are changing
		    # the extension, skip the renaming so we avoid creating a
		    # filename that may get overwritten later on.

		    if (modfield == F_EXTN && strlen (Memc[fextn]) == 0)
			next
		    else {
		        if (strlen (Memc[extn]) > 0) {
		            call strcat (".", Memc[newname], SZ_PATHNAME)
		            call strcat (Memc[extn], Memc[newname], SZ_PATHNAME)
		 	}
		    }
	        }

	        iferr (call rename (Memc[oldfile], Memc[newname]))
		    call erract (EA_WARN)
	    }
	}

	call clpcls (list)
	call sfree (sp)
end
