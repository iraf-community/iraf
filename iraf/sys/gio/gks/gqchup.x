# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gks.h"

# GQCHUP -- Inquire character up vector.

procedure gqchup (ierror, chupx, chupy)

int	ierror			# Error code; ierror = 0 for no error 
real	chupx, chupy		# Character up vector x and y components

int	angle
real	txup
int	gstati()
include	"gks.com"

begin
	if (gk_std == NULL) {
	    # GKS not in proper state; no active workstations
	    ierror = 7
	    chupx = 0.0
	    chupy = 0.0
	    return
	} else
	    ierror = 0

	iferr {
	    angle = gstati (gp[gk_std], G_TXUP) 

	    txup = real (angle) * 3.1415926 / 180.
	    chupx = cos (txup)
	    chupy = sin (txup)
	} then {
	    ierror = 1
	    chupx = 0.0
	    chupy = 0.0
	}
end
