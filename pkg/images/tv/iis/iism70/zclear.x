# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include "../lib/ids.h"
include "iis.h"

# ZCLEAR -- Erase IIS frame.

procedure zclear (frame, bitplane, flag)

short	frame[ARB]			# frame array
short	bitplane[ARB]			# bitplane array
bool	flag				# true if image plane

int	z, t
short	erase
int	and(), andi()
short	iispack()

begin
	if (flag) {
	    z = iispack (frame)
	    z = and (z, ALLCHAN)
	} else
	    z = GRCHAN

	t = iispack (bitplane)
	erase = andi (ERASE, 177777B)

	call iishdr (IWRITE+BYPASSIFM+BLOCKXFER, 1, FEEDBACK,
	    ADVXONTC, ADVYONXOV, z, t)
	call iisio (erase, SZB_CHAR)
end
