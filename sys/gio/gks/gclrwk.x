# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"gks.h"

# GCLRWK -- Clear workstation.

procedure gclrwk (wkid, cofl)

int	wkid		# Workstation identifier
int	cofl		# Control flags (GCONDI, GALWAY)
include	"gks.com"

begin
	# Clear the screen or advance film on the specified workstation.  GKS
	# allows this to be done conditionally, dependent on whether or not
	# something has been drawn.

	call gclear (gp[wkid])
end
