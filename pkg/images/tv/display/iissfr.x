# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "iis.h"

# IIS_SETFRAME -- Set the frame number for IISOPN.  This is a kludge to pass
# this number to IISOPN via the iis common.

procedure iis_setframe (frame)

int	frame
include	"iis.com"

begin
	iisframe = frame
end
