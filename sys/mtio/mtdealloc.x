# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>

# MTDEALLOCATE -- Deallocate a previously allocated tape drive.  To deallocate
# a drive we try to rewind (any errors, such as drive offline, will result in a
# warning message), and then delete the lockfile.  We do not call up the OS to
# deallocate the drive; that is done at a higher level, usually XDEALLOCATE
# (in etc$xalloc.x).

procedure mtdeallocate (mtname, rewind_tape)

char	mtname[ARB]		#I magtape specification
int	rewind_tape		#I rewind before deallocating drive

pointer	sp, lockfile
errchk	mt_glock, syserrs

begin
	call smark (sp)
	call salloc (lockfile, SZ_FNAME, TY_CHAR)

	if (rewind_tape == YES)
	    iferr (call mtrewind (mtname, NO))
		call erract (EA_WARN)

	call mt_sync (OK)

	call mt_glock (mtname, Memc[lockfile], SZ_FNAME)
	iferr (call delete (Memc[lockfile]))
	    ;

	call mt_clrcache()
	call sfree (sp)
end
