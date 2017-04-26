# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"zdisplay.h"
include	"iis.h"
# IISHDR -- Form IIS header.

procedure iishdr (id, count, subunit, x, y, z, t)

int	id, count, subunit, x, y, z, t
int	i, sum
include	"iis.com"

begin
	call achtiu (id, XFERID(hdr), 1)
	call achtiu (count, THINGCT(hdr), 1) 
	call achtiu (subunit, SUBUNIT(hdr), 1)
	call achtiu (x, XREG(hdr), 1)
	call achtiu (y, YREG(hdr), 1)
	call achtiu (z, ZREG(hdr), 1)
	call achtiu (t, TREG(hdr), 1)
	CHECKSUM(hdr) = 1
	 
	if (THINGCT(hdr) > 0)
	    THINGCT(hdr) = -THINGCT(hdr)
	sum = 0
	for (i = 1; i <= LEN_IISHDR; i = i + 1)
	    sum = sum + hdr[i]
	call achtiu (-sum, CHECKSUM(hdr), 1)
end
