# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <plset.h>
include <plio.h>

define	LEN_CIRCLEDES	5
define	C_PL		Memi[$1]	# reference mask
define	C_XCEN		Memr[$1+1]	# X1 coord of circle
define	C_YCEN		Memr[$1+2]	# Y1 coord of circle
define	C_RADIUS	Memr[$1+3]	# X2 coord of circle
define	C_PV		Memi[$1+4]	# pixel value

# PL_CIRCLE -- Rasterop between a circular region as source, and an existing
# mask as destination.  It is not necessary for the center of the circle to
# be inside the mask; if it is outside, the boundary of the circle will be
# clipped to the boundary of the mask.  This is a 2-dim operator.  If the
# image dimensionality is greater than two the pl_setplane procedure should
# be called first to specify the plane to be modified.

procedure pl_circle (pl, x, y, radius, rop)

pointer	pl			#I mask descriptor
int	x,y			#I center coords of circle
int	radius			#I radius of circle
int	rop			#I rasterop

int	y1, y2
pointer	sp, ufd
extern	pl_ucircle()

begin
	call plvalid (pl)
	call smark (sp)
	call salloc (ufd, LEN_CIRCLEDES, TY_STRUCT)

	y1 = max ( 1, min (PL_AXLEN(pl,2), y - radius))
	y2 = max (y1, min (PL_AXLEN(pl,2), y + radius))

	C_PL(ufd) = pl
	C_XCEN(ufd) = x
	C_YCEN(ufd) = y
	C_RADIUS(ufd) = radius
	C_PV(ufd) = 1

	call pl_regionrop (pl, pl_ucircle, ufd, y1, y2, rop)

	call sfree (sp)
end


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
