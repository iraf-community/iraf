# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <plset.h>
include <plio.h>

# PL_REGIONROP -- Perform a rasterop operation on an irregular region in a
# 2-dimensional plane of a mask.  The boundary of the region is defined by
# a user supplied function which, given the line number (Y) returns a range
# list defining the region for that line.
#
#	rl_new = ufcn (ufd, y, rl_out, xs, npix)
#
# where
#	rl_new	= true if range list for line Y is different than for Y-1
#	ufd	= user region descriptor (parameters defining region)
#	y	= input y value
#	rl_out 	= output range list
#	xs	= first pixel to be edited in dst mask
#	npix	= number of pixels in edit region
#
# If the dimensionality of the mask exceeds 2, the pl_setplane() procedure
# should be called first to define the plane of the mask to be modified.

procedure pl_regionrop (pl, ufcn, ufd, y1, y2, rop)

pointer	pl			#I mask descriptor
extern	ufcn()			#I user supplied region tracing procedure
pointer	ufd			#I user region descriptor
long	y1			#I first mask line to be modified
long	y2			#I last mask line to be modified
int	rop			#I rasterop defining operation

size_t	sz_val
bool	rl_new
long	v[PL_MAXDIM]
int	ll_len
long	xs, lval
size_t	npix
pointer	sp, ll_out, ll_reg, ll_dst, ol_dst, rl_out
pointer	pl_access()
int	pl_r2li()
bool	ufcn()
errchk	plvalid

begin
	call plvalid (pl)

	call smark (sp)
	call salloc (ll_out, LL_MAXLEN(pl), TY_SHORT)
	call salloc (ll_reg, LL_MAXLEN(pl), TY_SHORT)
	call salloc (rl_out, RL_MAXLEN(pl), TY_INT)

	sz_val = PL_MAXDIM
	call amovl (PL_PLANE(pl,1), v, sz_val)
	ol_dst = 1

	for (v[2]=y1;  v[2] <= y2;  v[2]=v[2]+1) {
	    ll_dst = pl_access (pl, v)
	    rl_new = ufcn (ufd, v[2], Memi[rl_out], xs, npix)
	    if (rl_new) {
		lval = 1
		ll_len = pl_r2li (Memi[rl_out], lval, Mems[ll_reg], npix)
	    }

	    if (ll_dst != ol_dst || rl_new) {
		lval = 1
		call pl_linestencil (Mems[ll_reg], lval, 1,
				     Mems[ll_dst], xs, PL_MAXVAL(pl),
				     Mems[ll_reg], lval,
				     Mems[ll_out], npix, rop)
		ol_dst = ll_dst
	    }

	    # Update the affected line of the destination mask.
	    call pl_update (pl, v, Mems[ll_out])
	}

	# Compress the mask if excessive free space has accumulated.
	if (PL_NEEDCOMPRESS(pl))
	    call pl_compress (pl)

	call sfree (sp)
end
