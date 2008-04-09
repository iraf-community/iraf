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

size_t	sz_val
size_t	c_1
long	lval, lstatus
int	xferid
int	and()
include	"iis.com"

begin
	c_1 = 1

	call iiswt (iischan, lstatus)
	xferid = XFERID(hdr)

	if (swap_bytes) {
	    sz_val = SZB_IISHDR
	    call bswap2 (hdr, c_1, hdr, c_1, sz_val)
	}
	sz_val = SZB_IISHDR
	lval = 0
	call zawrgd (iischan, hdr, sz_val, lval)
	call iiswt (iischan, lstatus)

	if (and (xferid, IREAD) != 0) {
	    sz_val = nbytes
	    lval = 0
	    call zardgd (iischan, buf, sz_val, lval)
	    call iiswt (iischan, lstatus)
	    if (swap_bytes && and(xferid,PACKED) == 0) {
		sz_val = nbytes
		call bswap2 (buf, c_1, buf, c_1, sz_val)
	    }
	} else {
	    if (swap_bytes && and(xferid,PACKED) == 0) {
		sz_val = nbytes
		call bswap2 (buf, c_1, buf, c_1, sz_val)
	    }
	    sz_val = nbytes
	    lval = 0
	    call zawrgd (iischan, buf, sz_val, lval)
	    call iiswt (iischan, lstatus)
	}

	if (lstatus <= 0) {
	    status = EOF
	} else {
	    status = lstatus
	}
end
