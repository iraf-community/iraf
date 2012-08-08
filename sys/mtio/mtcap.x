# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "mtio.h"

# MTCAP -- Return the tapecap descriptor for the given magtape device.  The
# device is specified by the full specification (as passed to mtopen), hence
# there may be tapecap attributes in the device specification which override
# those in the tapecap file.

pointer procedure mtcap (mtname)

char	mtname[ARB]			#I magtape device specification

int	fileno, recno
pointer	sp, device, devcap, cache_gty, gty
pointer	mt_gtyopen(), gtycaps(), gtyopen()
errchk	mtparse, mt_gtyopen

begin
	call smark (sp)
	call salloc (device, SZ_DEVICE, TY_CHAR)
	call salloc (devcap, SZ_DEVCAP, TY_CHAR)

	call mtparse (mtname, Memc[device], SZ_DEVICE, fileno, recno,
	    Memc[devcap], SZ_DEVCAP)

	# Do not return the cached MTIO device entry, as we do not want the
	# application to close this with gtyclose.  Open a new GTY descriptor
	# using the capabilities in the cached entry.

	cache_gty = mt_gtyopen (Memc[device], Memc[devcap])
	gty = gtyopen ("", "", Memc[gtycaps(cache_gty)])

	call sfree (sp)
	return (gty)
end
