# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<fio.h>

# RENAME -- Rename a file.  First try to rename the file using the ZFRNAM
# kernel primitive, accessed by FRENAME.  If that fails try to copy the
# file and delete the original.

procedure rename (oldname, newname)

char	oldname[ARB]			# old filename
char	newname[ARB]			# new filename

int	junk, protect()
errchk	fcopy, protect

begin
	# Try a simple file rename first.
	ifnoerr (call frename (oldname, newname))
	    return

	# That failed, so copy the file to the new name.
	call fcopy (oldname, newname)

	# Now delete the original.  Transfer file protection to the new file,
	# if the old file was protected.

	if (protect (oldname, QUERY_PROTECTION) == YES) {
	    iferr (junk = protect (oldname, REMOVE_PROTECTION)) {
		call delete (newname)
		call erract (EA_ERROR)
	    }
	    call delete (oldname)
	    junk = protect (newname, SET_PROTECTION)
	} else
	    call delete (oldname)
end
