# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	"mtio.h"

# MTSTATUS -- Print the status of an allocated magtape device, i.e., print the
# lock file as a text file.  Called by the DEVSTATUS task.

procedure mtstatus (out, filespec)

int	out			# output file
char	filespec[ARB]		# magtape specification

int	in, junk
pointer	sp, lockfile, drive
int	open(), access()
errchk	mt_parse, mt_lockname, open, fcopyo

begin
	call smark (sp)
	call salloc (drive, SZ_FNAME, TY_CHAR)
	call salloc (lockfile, SZ_FNAME, TY_CHAR)

	call mt_parse (filespec, Memc[drive], SZ_FNAME, junk, junk, junk)
	call mt_lockname (Memc[drive], Memc[lockfile], SZ_FNAME)

	call mt_sync (OK)

	# If there is no lockfile, write one indicating that the position
	# of the tape is undefined.

	if (access (Memc[lockfile], 0, 0) == NO)
	    call mt_allocate (filespec)

	# Print the lockfile.

	in = open (Memc[lockfile], READ_ONLY, TEXT_FILE)
	call fcopyo (in, out)
	call close (in)
	
	call sfree (sp)
end
