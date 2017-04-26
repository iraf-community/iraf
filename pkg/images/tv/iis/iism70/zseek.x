# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<fset.h>
include "../lib/ids.h"
include "iis.h"

# ZSEEK -- Seek for an image frame

procedure zseek (fd, x, y)

int	fd				# file to write
int	x, y				# device coordinates

long	offset

begin
	offset = max (1, 1 + (x + y * IIS_XDIM) * SZ_SHORT)

	call seek (fd, offset)
end
