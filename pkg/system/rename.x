# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>

define	F_ALL		0
define	F_LDIR		1
define	F_ROOT		2
define	F_EXTN		3


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

bool	isdir
pointer	sp, oldfile, newname, field
pointer	o_ldir, o_root, o_extn, n_ldir, n_root, n_extn
pointer	pathname, newfile, junkstr
int	list, len, modfield

bool	streq()
int	clpopni(), clgfil(), clplen(), stridxs()
int	access(), fnldir(), fnroot(), fnextn(), isdirectory()
string	s_clobber "Warning: %s would overwrite existing file %s - skipping\n"

begin
	call smark (sp)
	call salloc (oldfile, SZ_FNAME, TY_CHAR)
	call salloc (newname, SZ_PATHNAME, TY_CHAR)
	call salloc (pathname, SZ_PATHNAME, TY_CHAR)
	call salloc (newfile, SZ_PATHNAME, TY_CHAR)
	call salloc (junkstr, SZ_FNAME, TY_CHAR)
	call salloc (field, SZ_FNAME, TY_CHAR)
	call salloc (o_ldir, SZ_FNAME, TY_CHAR)
	call salloc (n_ldir, SZ_FNAME, TY_CHAR)
	call salloc (o_root, SZ_FNAME, TY_CHAR)
	call salloc (n_root, SZ_FNAME, TY_CHAR)
	call salloc (o_extn, SZ_FNAME, TY_CHAR)
	call salloc (n_extn, SZ_FNAME, TY_CHAR)

	# Open the list of files to be renamed.  This is done first so that
	# the old file name is queried for before the new file name.

	list = clpopni ("files")

	# Get the output file name, or the new name for the field to be
	# changed.

	call clgstr ("newname", Memc[newname], SZ_PATHNAME)
	call clgstr ("field", Memc[field], SZ_FNAME)

	if (streq (Memc[field], "ldir"))
	    modfield = F_LDIR
	else if (streq (Memc[field], "root"))
	    modfield = F_ROOT
	else if (streq (Memc[field], "extn"))
	    modfield = F_EXTN
	else if (streq (Memc[field], "all"))
	    modfield = F_ALL
	else {
	    call clpcls (list)
	    call error (1, "rename: unrecognized filename field code")
	}

	# See if we're modifying the logical directory, If so, move the input
	# files to this directory, otherwise do the renaming as we've always
	# done.

	Memc[pathname] = EOS
	isdir = (isdirectory (Memc[newname], Memc[pathname], SZ_PATHNAME) > 0)

	if (isdir || modfield == F_LDIR) {
	    # Move each of the files in the list to the destination dir.
	    call fdirname (Memc[newname], Memc[n_ldir], SZ_FNAME)

	    while (clgfil (list, Memc[oldfile], SZ_FNAME) != EOF) {
		if (Memc[pathname] != EOS)
		    call strcpy (Memc[pathname], Memc[newfile], SZ_PATHNAME)
		else
		    call strcpy (Memc[n_ldir], Memc[newfile], SZ_PATHNAME)
		len = fnldir (Memc[oldfile], Memc[junkstr], SZ_FNAME)
		call strcat (Memc[oldfile+len], Memc[newfile], SZ_PATHNAME)

		iferr (call rename (Memc[oldfile], Memc[newfile]))
		    call erract (EA_WARN)
	    }

	} else if (modfield == F_ALL) {
	    # "newname" is the new filename.  This makes sense only if there
	    # is a single input filename.  Note that the new name may contain
	    # a new logical directory, renaming both the file and moving it		    # to a new directory.

	    if (clplen (list) > 1)
		call error (2, "rename: `newname' must be a directory")

	    # Rename the file.
	    if (clgfil (list, Memc[oldfile], SZ_FNAME) != EOF)
		iferr (call rename (Memc[oldfile], Memc[newname]))
		    call erract (EA_WARN)

	} else {
	    # We're either modifying the root or the extension.  Break out
	    # the ldir, root and extn for the input and output file names
	    # then construct the new name from these components.

	    Memc[n_root] = EOS
	    Memc[n_extn] = EOS

	    if (modfield == F_ROOT) {
	        call strcpy (Memc[newname], Memc[n_root], SZ_FNAME)
		if (stridxs ("$/", Memc[n_root]) > 0) {
		    call clpcls (list)
		    call error (3, "rename: bad replacement root field")
		}
	    }
	    if (modfield == F_EXTN) {
	        call strcpy (Memc[newname], Memc[n_extn], SZ_FNAME)
		if (stridxs ("$/", Memc[n_extn]) > 0) {
		    call clpcls (list)
		    call error (4, "rename: bad replacement extn field")
		}
	    }

	    # Process the files.
	    while (clgfil (list, Memc[oldfile], SZ_FNAME) != EOF) {

	        # Get the ldir, root and extension names of the old filename.
	        len = fnroot (Memc[oldfile], Memc[o_root], SZ_FNAME)
	        len = fnextn (Memc[oldfile], Memc[o_extn], SZ_FNAME)
	        len = fnldir (Memc[oldfile], Memc[o_ldir], SZ_PATHNAME)

		# Start by copying the ldir to the new name.
		call aclrc (Memc[newname], SZ_PATHNAME)
		call strcpy (Memc[o_ldir], Memc[newname], SZ_PATHNAME)

		# Build up the new file name.
	        if (modfield == F_ROOT) {
		    call strcat (Memc[n_root], Memc[newname], SZ_FNAME)
                    if (Memc[o_extn] != EOS) {
		        call strcat (".", Memc[newname], SZ_PATHNAME)
		        call strcat (Memc[o_extn], Memc[newname], SZ_FNAME)
		    }
	        } else if (modfield == F_EXTN) {
		    call strcat (Memc[o_root], Memc[newname], SZ_FNAME)
		    call strcat (".", Memc[newname], SZ_PATHNAME)
		    call strcat (Memc[n_extn], Memc[newname], SZ_FNAME)
	        }

		# Check to see if we're going to clobber a file.
		if (clplen (list) > 1) {
		    if (access (Memc[newname], 0, 0) == YES) {
			call eprintf (s_clobber)
			    call pargstr (Memc[oldfile])
			    call pargstr (Memc[newname])
			next
		    }
		}

		# Rename the file.
	        iferr (call rename (Memc[oldfile], Memc[newname]))
		    call erract (EA_WARN)
	    }
	}

	call clpcls (list)
	call sfree (sp)
end
