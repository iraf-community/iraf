# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<knet.h>
include "iis.h"

# IISIO -- Read/Write to IIS.

procedure iisio (buf, nbytes)

short	buf[ARB]
int	nbytes

int	nbites
int	and()

include	"iis.com"

begin
	call iiswt (iischan, nbites)
	if (nbites == ERR)
	    return

	call zawrgd (iischan, hdr, SZB_IISHDR, 0)
	call iiswt (iischan, nbites)
	if (nbites == ERR)
	    return

	if (and (int(XFERID(hdr)), IREAD) != 0)
	    call zardgd (iischan, buf, nbytes, 0)
	else
	    call zawrgd (iischan, buf, nbytes, 0)

	call iiswt (iischan, nbites)
end
