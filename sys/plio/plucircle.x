# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <plset.h>
include <plio.h>
include "plcircle.h"


# PL_UCIRCLE -- Regionrop ufcn for a circle (circular region), clipped at
# the borders of the mask.

bool procedure pl_ucircle (ufd, y, rl_reg, xs, npix)

pointer	ufd			#I user function descriptor
int	y			#I mask line number
int	rl_reg[3,ARB]		#O output range list for line Y
int	xs			#O first pixel to be edited
int	npix			#O number of pixels affected

pointer	pl
real	radius, dx, dy
int	rn, axlen, x1, x1_clipped, x2, x2_clipped

begin
	pl = C_PL(ufd)
	rn = RL_FIRST
	axlen = PL_AXLEN(pl,1)
	radius = C_RADIUS(ufd)

	dy = abs (C_YCEN(ufd) - y)
	if (dy <= radius) {
	    dx = sqrt (radius**2 - dy**2)
	    x1 = C_XCEN(ufd) - int(dx)
	    x2 = C_XCEN(ufd) + int(dx)
	    x1_clipped = max(1, min(axlen, x1))
	    x2_clipped = max(1, min(axlen, x2))

	    xs = x1_clipped
	    npix = x2_clipped - x1_clipped + 1

	    RL_X(rl_reg,rn) = 1
	    RL_N(rl_reg,rn) = npix
	    RL_V(rl_reg,rn) = C_PV(ufd)
	    rn = rn + 1

	} else {
	    npix = 0
	    xs = 1
	}

	RL_LEN(rl_reg) = rn - 1
	RL_AXLEN(rl_reg) = npix

	return (true)
end
