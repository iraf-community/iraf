# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	"mtio.h"

# MTREWIND -- Rewind the named magtape device.  This is a synchronous
# rewind.  Rewind not only rewinds the device, it also initializes the
# MTIO view of what is on the tape (number of files, total bytes used).
# Hence, if the drive is left allocated but the tape is changed, or if
# the position cache becomes inaccurate for any reason, a rewind will
# initialize things without having to deallocate and reallocate the drive.

procedure mtrewind (mtname, initcache)

char	mtname[ARB]		#I device to be rewound
int	initcache		#I discard positional information?

pointer	sp, fname
int	fd, mtopen()
errchk	mtfname

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Init position cache.
	if (initcache == YES) {
	    call mt_glock (mtname, Memc[fname], SZ_FNAME)
	    iferr (call delete (Memc[fname]))
		;
	}

	# Rewind device.
	call mtfname (mtname, 1, Memc[fname], SZ_FNAME)
	iferr (fd = mtopen (Memc[fname], READ_ONLY, 0))
	    call erract (EA_WARN)
	else
	    call close (fd)

	call sfree (sp)
end
