# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include "mtio.h"

# MT_GLOCK -- Return the lockfile name for the given magtape device
# specification.  There can be many logical devices, defined in the tapecap
# file, which refer to the same physical device.  All entries for the same
# physical device share the same i/o device file and lock file.  If the
# physical device is on a remote node the tapecap file on that node should
# be accessed, and the node name must be included in the lockfile file name,
# although the lock file is always written on the local node.

procedure mt_glock (mtname, lockfile, maxch)

char	mtname[ARB]			#I full magtape device spec
char	lockfile[ARB]			#O receives lockfile name
int	maxch				#I max chars out

int	filno, recno
pointer	sp, lkname, device, devcap, gty
errchk	mt_gtyopen, syserrs
int	gtygets()
pointer	mt_gtyopen()

begin
	call smark (sp)
	call salloc (lkname, SZ_FNAME, TY_CHAR)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (devcap, SZ_DEVCAP, TY_CHAR)

	call mtparse (mtname,
	    Memc[device], SZ_FNAME, filno, recno, Memc[devcap], SZ_DEVCAP)

	# The "lk" capability specifies the lock file root name.
	gty = mt_gtyopen (Memc[device], Memc[devcap])
	if (gtygets (gty, "lk", Memc[lkname], SZ_FNAME) <= 0) {
	    call eprintf ("missing `lk' parameter in tapecap entry for %s\n")
		call pargstr (mtname)
	    call syserrs (SYS_MTTAPECAP, mtname)
	}

	call ki_xnode (Memc[device], Memc[lkname], SZ_FNAME)
	call mt_lockname (Memc[lkname], lockfile, maxch)

	call sfree (sp)
end
