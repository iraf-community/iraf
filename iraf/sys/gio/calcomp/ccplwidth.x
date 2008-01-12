# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"ccp.h"

# Calcomp pen widths
define	SINGLE	1	# ***** site dependence!!	[MACHDEP]
define	DOUBLE	2	# 

# CCP_LWIDTH  set pen width; see ccp_color, which also sets pens.
# We should only be called if task param "lwtype" was explicitly set to
# "p" for pen method; normally bold lines are handled by ntracing.

procedure ccp_lwidth (index)

int	index		# index for width switch statement
include	"ccp.com"

begin
	if (g_lwover)	# CL param lwover, line width override is on; noop
	    return

	# ***** site dependence; add other pen numbers here; if pen numbers
	# for multiple widths are monotonic, make single call to newpen(index).

	switch (index) {

	case DOUBLE:
	    call newpen (DOUBLE)
	default:
	    call newpen (SINGLE)
	}
end
