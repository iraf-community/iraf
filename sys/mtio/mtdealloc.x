# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	"mtio.h"

# MT_DEALLOCATE -- Deallocate a previously allocated tape drive.  To deallocate
# a drive we try to rewind (any errors, such as drive offline, will result in a
# warning message), and then delete the lockfile.  We do not call up the OS to
# deallocate the drive; that is done at a higher level, usually XDEALLOCATE
# (in etc$xalloc.x).

procedure mt_deallocate (filespec, rewind_tape)

char	filespec[ARB]		# magtape specification
int	rewind_tape		# rewind before deallocating drive

int	junk
pointer	sp, lockfile, drive
errchk	mt_parse, mt_lockname

begin
	call smark (sp)
	call salloc (drive, SZ_FNAME, TY_CHAR)
	call salloc (lockfile, SZ_FNAME, TY_CHAR)

	call mt_parse (filespec, Memc[drive], SZ_FNAME, junk, junk, junk)
	call mt_lockname (Memc[drive], Memc[lockfile], SZ_FNAME)

	if (rewind_tape == YES)
	    iferr (call mtrewind (filespec))
		call erract (EA_WARN)

	iferr (call delete (Memc[lockfile]))
	    ;

	call sfree (sp)
end
