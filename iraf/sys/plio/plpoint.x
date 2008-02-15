# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <plset.h>
include <plio.h>

# PL_POINT -- Perform a rasterop operation on a single point in a line of a
# 2-dimensional plane of a mask.  If the dimensionality of the mask exceeds 2,
# the pl_setplane() procedure should be called first to define the plane of
# the mask to be modified.

procedure pl_point (pl, x, y, rop)

pointer	pl			#I mask descriptor
int	x			#I pixel to be modified
int	y			#I line to be modified
int	rop			#I rasterop defining operation

long	v[PL_MAXDIM]
int	npix, ll_len
pointer	sp, ll_out, ll_reg, rl_out, ll_dst, op
errchk	plvalid, pl_access, pl_linerop, pl_update
pointer	pl_access()
int	pl_r2li()

begin
	call plvalid (pl)
	call amovl (PL_PLANE(pl,1), v, PL_MAXDIM)
	v[2] = y

	call smark (sp)
	call salloc (ll_out, LL_MAXLEN(pl), TY_SHORT)
	call salloc (ll_reg, LL_CURHDRLEN + 6, TY_SHORT)
	call salloc (rl_out, RL_FIRST * 3, TY_INT)

	# Access the destination line in the mask.
	ll_dst = pl_access (pl, v)

	# Construct the edit-region list.
	npix = 1
	RLI_AXLEN(rl_out) = npix
	RLI_LEN(rl_out) = RL_FIRST

	op = rl_out + (RL_FIRST - 1) * 3
	Memi[op+RL_XOFF] = 1
	Memi[op+RL_NOFF] = npix
	Memi[op+RL_VOFF] = 1

	ll_len = pl_r2li (Memi[rl_out], 1, Mems[ll_reg], npix)

	# Edit the affected line.
	call pl_linerop (Mems[ll_reg], 1, 1, Mems[ll_dst], x, PL_MAXVAL(pl),
	    Mems[ll_out], npix, rop)

	# Update the edited line in the mask.
	call pl_update (pl, v, Mems[ll_out])

	# Compress the mask if excessive free space has accumulated.
	if (PL_NEEDCOMPRESS(pl))
	    call pl_compress (pl)

	call sfree (sp)
end
