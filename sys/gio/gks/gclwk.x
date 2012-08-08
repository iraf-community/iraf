# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"gks.h"

# GCLWK -- Close workstation.

procedure gclwk (wkid)

int	wkid		# Workstation identifier
include	"gks.com"

begin
	call gclose (gp[wkid])
end
