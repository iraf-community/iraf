# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<time.h>
include	"mtio.h"

# MT_ALLOCATE -- "Allocate" a tape drive by writing the lock file.  We do
# not actually call the OS to physically allocate the drive; that is done
# by our caller (e.g., xallocate, in etc$xalloc.x).  The lock file is no longer
# used to gain exclusive access to the device; it is now used only to keep
# track of the tape position after process termination.

procedure mt_allocate (filespec)

char	filespec[ARB]		# "mtaXXXX", etc.
int	fd, junk
pointer	sp, lockfile, mtowner, userid, timestr, drive
int	open()
long	clktime()
errchk	syserrs, open
errchk	mt_parse

begin
	call smark (sp)
	call salloc (drive, SZ_FNAME, TY_CHAR)
	call salloc (lockfile, SZ_FNAME, TY_CHAR)
	call salloc (mtowner, SZ_FNAME, TY_CHAR)
	call salloc (userid, SZ_FNAME, TY_CHAR)
	call salloc (timestr, SZ_TIME, TY_CHAR)

	call mt_parse (filespec, Memc[drive], SZ_FNAME, junk, junk, junk)
	call mt_lockname (Memc[drive], Memc[lockfile], SZ_FNAME)
	call getuid (Memc[userid], SZ_FNAME)
	call cnvtime (clktime (long(0)), Memc[timestr], SZ_TIME)

	# Open lock file and write out unit, owner, time allocated, etc.
	# Overwrite any existing lockfile.  We are called only after
	# physically allocating the device at the host system level
	# (and are not called if the device is already allocated), so this
	# is safe.

	iferr (call delete (Memc[lockfile]))
	    ;
	fd = open (Memc[lockfile], NEW_FILE, TEXT_FILE)

	call fprintf (fd, "# Magtape unit `%s' allocated to `%s' %s\n")
	    call pargstr (filespec)
	    call pargstr (Memc[userid])
	    call pargstr (Memc[timestr])

	# Assume initially that the tape position is undefined.  This will
	# cause the tape to be rewound on the first open, and thereafter
	# the position will be defined unless an i/o error occurs.

	call fprintf (fd, "file = -1\n")
	call fprintf (fd, "record = -1\n")
	call fprintf (fd, "unit just allocated: no i/o has yet occurred\n")

	call close (fd)
	call sfree (sp)
end
