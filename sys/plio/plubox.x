# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <plset.h>
include <plio.h>
include "plbox.h"


# PL_UBOX -- Regionrop ufcn for a box (rectangular) region.

bool procedure pl_ubox (ufd, y, rl_reg, xs, npix)

pointer	ufd			#I user function descriptor
int	y			#I mask line number
int	rl_reg[3,ARB]		#O output range list for line Y
int	xs			#O start of edit region in dst mask
int	npix			#O number of pixels affected

int	rn
bool	rl_new

begin
	rl_new = true
	rn = RL_FIRST

	if (y >= B_Y1(ufd) && y <= B_Y2(ufd)) {
	    xs = B_X1(ufd)
	    npix = B_X2(ufd) - B_X1(ufd) + 1

	    RL_X(rl_reg,rn) = 1
	    RL_N(rl_reg,rn) = npix
	    RL_V(rl_reg,rn) = B_PV(ufd)

	    rl_new = (y == B_Y1(ufd))
	    rn = rn + 1

	} else {
	    npix = 0
	    xs = 1
	}

	RL_LEN(rl_reg) = rn - 1
	RL_AXLEN(rl_reg) = npix

	return (rl_new)
end
