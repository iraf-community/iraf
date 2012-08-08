# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include <knet.h>
include "zdisplay.h"
include "iis.h"

# IISIO -- Synchronous i/o to the IIS.

procedure iisio (buf, nbytes, status)

short	buf[ARB]
int	nbytes
int	status

int	xferid
int	and()
include	"iis.com"

begin
	call iiswt (iischan, status)
	xferid = XFERID(hdr)

	if (swap_bytes)
	    call bswap2 (hdr, 1, hdr, 1, SZB_IISHDR)
	call zawrgd (iischan, hdr, SZB_IISHDR, 0)
	call iiswt (iischan, status)

	if (and (xferid, IREAD) != 0) {
	    call zardgd (iischan, buf, nbytes, 0) 
	    call iiswt (iischan, status)
	    if (swap_bytes && and(xferid,PACKED) == 0)
		call bswap2 (buf, 1, buf, 1, nbytes)
	} else {
	    if (swap_bytes && and(xferid,PACKED) == 0)
		call bswap2 (buf, 1, buf, 1, nbytes)
	    call zawrgd (iischan, buf, nbytes, 0)
	    call iiswt (iischan, status)
	}

	if (status <= 0)
	    status = EOF
end
