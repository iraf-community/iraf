# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<time.h>
include	"mtio.h"

# MTALLOCATE -- "Allocate" a tape drive by writing the lock file.  We do
# not actually call the OS to physically allocate the drive; that is done
# by our caller (e.g., xallocate, in etc$xalloc.x).  The lock file is no longer
# used to gain exclusive access to the device; it is now used only to keep
# track of the tape position after process termination.

procedure mtallocate (mtname)

char	mtname[ARB]			#I device name

int	fd, junk
pointer	sp, lockfile, mtowner, userid, timestr, device
errchk	open, mtparse
long	clktime()
int	open()

begin
	call smark (sp)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (lockfile, SZ_FNAME, TY_CHAR)
	call salloc (mtowner, SZ_FNAME, TY_CHAR)
	call salloc (userid, SZ_FNAME, TY_CHAR)
	call salloc (timestr, SZ_TIME, TY_CHAR)

	# Get name of lockfile used by the given device.
	call mtparse (mtname, Memc[device], SZ_FNAME, junk, junk, junk, 0)
	call mt_glock (mtname, Memc[lockfile], SZ_FNAME)

	# Open lock file and write out unit, owner, time allocated, etc.
	# Overwrite any existing lockfile.  We are called only after
	# physically allocating the device at the host system level
	# (and are not called if the device is already allocated), so this
	# is safe.

	iferr (call delete (Memc[lockfile]))
	    ;
	fd = open (Memc[lockfile], NEW_FILE, TEXT_FILE)

	call cnvtime (clktime(long(0)), Memc[timestr], SZ_TIME)
	call getuid (Memc[userid], SZ_FNAME)

	call fprintf (fd, "# Magtape unit %s status %s user %s\n")
	    call pargstr (Memc[device])
	    call pargstr (Memc[timestr])
	    call pargstr (Memc[userid])

	# Assume initially that the tape position is undefined.  This will
	# cause the tape to be rewound on the first open, and thereafter
	# the position will be defined unless an i/o error occurs.

	call fprintf (fd, "file = -1\n")
	call fprintf (fd, "record = -1\n")
	call fprintf (fd, "nfiles = 0\n")
	call fprintf (fd, "tapeused = 0 Kb\n")
	call fprintf (fd, "pflags = 0\n")

	call close (fd)
	call sfree (sp)
end
