# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"ccp.h"

# Calcomp pen colors
define	BLACK		1
define	WHITE		2
define	RED		3
define	GREEN		4
define	BLUE		5

# CCP_COLOR  set pen color

procedure ccp_color(index)

int	index		# index for color switch statement
include	"ccp.com"

begin
	if (g_lcover) # CL param lcover, line color override is on; noop
	    return

	switch (index) {

	case WHITE:
	    call newpen (WHITE)
	case RED:
	    call newpen (RED)
	case GREEN:
	    call newpen (GREEN)
	case BLUE:
	    call newpen (BLUE)
	default:
	    call newpen (BLACK)
	}
end
