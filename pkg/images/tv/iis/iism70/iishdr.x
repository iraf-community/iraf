# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include "iis.h"

# IISHDR -- Form IIS header.

procedure iishdr (id, count, subunit, x, y, z, t)

int	id, count, subunit, x, y, z, t
int	i, sum
include	"iis.com"

begin
	XFERID(hdr) = id
	THINGCT(hdr) = count
	SUBUNIT(hdr) = subunit
	XREG(hdr) = x
	YREG(hdr) = y
	ZREG(hdr) = z
	TREG(hdr) = t
	CHECKSUM(hdr) = 1

	if (THINGCT(hdr) > 0)
	    THINGCT(hdr) = -THINGCT(hdr)

	sum = 0
	for (i = 1; i <= LEN_IISHDR; i = i + 1)
	    sum = sum + hdr[i]
	CHECKSUM(hdr) = -sum
end
