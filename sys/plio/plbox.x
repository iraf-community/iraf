# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <plset.h>
include <plio.h>

define	LEN_BOXDES	6
define	B_PL		Memi[$1]	# reference mask
define	B_X1		Memi[$1+1]	# X1 coord of box
define	B_Y1		Memi[$1+2]	# Y1 coord of box
define	B_X2		Memi[$1+3]	# X2 coord of box
define	B_Y2		Memi[$1+4]	# Y2 coord of box
define	B_PV		Memi[$1+5]	# pixel value

# PL_BOX -- Rasterop between a box as source, and an existing mask as dest.
# This is a 2-dim operator.  The pl_setplane procedure is used to specify
# the plane to be modified.

procedure pl_box (pl, x1,y1, x2,y2, rop)

pointer	pl			#I mask descriptor
int	x1,y1			#I lower left corner of box
int	x2,y2			#I upper right corner of box
int	rop			#I rasterop

pointer	sp, ufd
bool	pl_ubox()
extern	pl_ubox()

begin
	call plvalid (pl)
	call smark (sp)
	call salloc (ufd, LEN_BOXDES, TY_STRUCT)

	B_PL(ufd) = pl
	B_X1(ufd) = max(1, min(PL_AXLEN(pl,1), x1))
	B_Y1(ufd) = max(1, min(PL_AXLEN(pl,2), y1))
	B_X2(ufd) = max(1, min(PL_AXLEN(pl,1), x2))
	B_Y2(ufd) = max(1, min(PL_AXLEN(pl,2), y2))
	B_PV(ufd) = 1

	call pl_regionrop (pl, pl_ubox, ufd, y1, y2, rop)

	call sfree (sp)
end


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
