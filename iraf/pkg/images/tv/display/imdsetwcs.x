# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <knet.h>
include <mach.h>
include "iis.h"

# IMD_SETWCS -- Pass the WCS for the indicated reference frame to a display
# server.  The frame buffer configuration is also passed.

procedure imd_setwcs (chan, wcstext)

int	chan			#I display channel code (frame)
char	wcstext[ARB]		#I wcs text

size_t	sz_val
pointer	sp, pkwcs
int	status, count
int	strlen(), iisflu()
include "iis.com"

begin
	count = strlen (wcstext) + 1

	call smark (sp)
	sz_val = count
	call salloc (pkwcs, sz_val, TY_CHAR)
	call strpak (wcstext, Memc[pkwcs], sz_val)

	call iishdr (IWRITE+PACKED, count, WCS, iis_version, 0, iisflu(chan),
	    max(0,iis_config-1))
	call iisio (Memc[pkwcs], count, status)

	call sfree (sp)
end
