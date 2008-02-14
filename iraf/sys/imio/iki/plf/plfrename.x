# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	"plf.h"

# PLF_RENAME -- Rename a PLIO mask savefile (mask image).

procedure plf_rename (kernel, old_root, old_extn, new_root, new_extn, status)

int	kernel			#I IKI kernel
char	old_root[ARB]		#I old image root name
char	old_extn[ARB]		#I old image extn
char	new_root[ARB]		#I new image root name
char	new_extn[ARB]		#I old image extn
int	status			#O output status

pointer	sp, oldname, newname
errchk	rename

begin
	call smark (sp)
	call salloc (oldname, SZ_PATHNAME, TY_CHAR)
	call salloc (newname, SZ_PATHNAME, TY_CHAR)

	# Get filenames of old and new datafiles.
	call iki_mkfname (old_root, old_extn, Memc[oldname], SZ_PATHNAME)
	call iki_mkfname (new_root, PLF_EXTN, Memc[newname], SZ_PATHNAME)

	# Rename the datafile.
	iferr (call rename (Memc[oldname], Memc[newname])) {
	    call erract (EA_WARN)
	    status = ERR
	} else
	    status = OK

	call sfree (sp)
end
