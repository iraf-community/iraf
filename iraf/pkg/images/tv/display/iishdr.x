# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"zdisplay.h"
include	"iis.h"
# IISHDR -- Form IIS header.

procedure iishdr (id, count, subunit, x, y, z, t)

int	id, count, subunit, x, y, z, t

size_t	sz_val
int	i, sum
include	"iis.com"

begin
	sz_val = 1
	call achtiu (id, XFERID(hdr), sz_val)
	call achtiu (count, THINGCT(hdr), sz_val) 
	call achtiu (subunit, SUBUNIT(hdr), sz_val)
	call achtiu (x, XREG(hdr), sz_val)
	call achtiu (y, YREG(hdr), sz_val)
	call achtiu (z, ZREG(hdr), sz_val)
	call achtiu (t, TREG(hdr), sz_val)
	CHECKSUM(hdr) = 1
	 
	if (THINGCT(hdr) > 0)
	    THINGCT(hdr) = -THINGCT(hdr)
	sum = 0
	for (i = 1; i <= LEN_IISHDR; i = i + 1) {
	    sum = sum + hdr[i]
	}
	sz_val = 1
	call achtiu (-sum, CHECKSUM(hdr), sz_val)
end
