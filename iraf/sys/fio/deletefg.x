# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>

# DELETEFG -- Delete a file group, i.e., the file, all subfiles, and all
# versions.  It is an error if the file does not exist, is protected, or
# if the file simply cannot be deleted.  A subfile is a physical file which
# is logically subordinate to another file and which must be deleted if the
# main file is deleted (e.g., a pixel storage file is a subfile of an
# imagefile).

procedure deletefg (fname, versions, subfiles)

char	fname[ARB]		# file or file group to be deleted
int	versions		# delete all versions
int	subfiles		# delete any subfiles (no subsubfiles)

int	n, max_versions
errchk	delete, erract

begin
	max_versions = 1
	if (versions == YES)
	    max_versions = 30000
	    
	for (n=0;  n < max_versions;  n=n+1) {
	    # Delete the main file.
	    iferr (call delete (fname))
		if (n == 0)
		    call erract (EA_ERROR)
		else
		    break
	    # Delete any subfiles.
	    if (subfiles == YES)
		call fsfdelete (fname)
	}
end
