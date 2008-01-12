# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include <plset.h>
include	<plio.h>

# PL_STENCIL -- Perform a rasterop operation from the source mask to the
# destination mask at the given offsets, but only within the regions set to
# one in the stencil mask.

procedure pl_stencil (pl_src, vs_src, pl_dst, vs_dst, pl_stn, vs_stn, vn, rop)

pointer	pl_src			#I source mask or NULL
long	vs_src[PL_MAXDIM]	#I start vector in source mask
pointer	pl_dst			#I destination mask (required)
long	vs_dst[PL_MAXDIM]	#I start vector in destination mask
pointer	pl_stn			#I stencil mask (required)
long	vs_stn[PL_MAXDIM]	#I start vector in stencil mask
long	vn[PL_MAXDIM]		#I vector giving subregion size
long	rop			#I rasterop

bool	need_src
pointer	sp, ll_out, ll_src, ll_dst, ll_stn, ol_src, ol_dst, ol_stn
long	v_src[PL_MAXDIM], v_dst[PL_MAXDIM], v_stn[PL_MAXDIM]
long	ve_src[PL_MAXDIM], ve_dst[PL_MAXDIM], ve_stn[PL_MAXDIM]

int	plloop()
pointer	pl_access()
errchk	syserr, plvalid, plsslv, pl_access

begin
	call plvalid (pl_dst)
	call plvalid (pl_stn)
	need_src = R_NEED_SRC(rop)
	if (need_src && pl_src == NULL)
	    call syserr (SYS_PLNULLSRC)

	call smark (sp)
	call salloc (ll_out, LL_MAXLEN(pl_dst), TY_SHORT)

	# Initialize the N-dimensional loop counters.
	call plsslv (pl_dst, vs_dst, vn, v_dst, ve_dst)
	call plsslv (pl_stn, vs_stn, vn, v_stn, ve_stn)
	if (need_src)
	    call plsslv (pl_src, vs_src, vn, v_src, ve_src)
	else
	    ll_src = ll_out		# any valid pointer will do

	# Perform the operation.
	ol_dst = -1
	repeat {
	    # Get a line from each mask.
	    ll_dst = pl_access (pl_dst, v_dst)
	    ll_stn = pl_access (pl_stn, v_stn)
	    if (need_src)
		ll_src = pl_access (pl_src, v_src)

	    # Perform the rasterop operation upon one line of the mask.
	    # Note that if successive mask lines point to the same encoded
	    # line list, we only have to compute the result once.

	    if (ll_src != ol_src || ll_dst != ol_dst || ll_stn != ol_stn) {
		call pl_linestencil (Mems[ll_src], vs_src[1], PL_MAXVAL(pl_src),
				     Mems[ll_dst], vs_dst[1], PL_MAXVAL(pl_dst),
				     Mems[ll_stn], vs_stn[1],
				     Mems[ll_out], vn[1], rop)

		ol_src = ll_src
		ol_dst = ll_dst
		ol_stn = ll_stn
	    }

	    # Update the affected line of the destination mask.
	    call pl_update (pl_dst, v_dst, Mems[ll_out])

	    # If the end of the input mask or stencil is reached,
	    # rewind it and go again.

	    if (plloop (v_stn,vs_stn,ve_stn,PL_NAXES(pl_stn)) == LOOP_DONE)
		call amovi (vs_stn, v_stn, PL_NAXES(pl_stn))
	    if (need_src)
		if (plloop (v_src,vs_src,ve_src,PL_NAXES(pl_src)) == LOOP_DONE)
		    call amovi (vs_src, v_src, PL_NAXES(pl_src))

	} until (plloop (v_dst, vs_dst, ve_dst, PL_NAXES(pl_dst)) == LOOP_DONE)

	# Compress the mask if excessive free space has accumulated.
	if (PL_NEEDCOMPRESS(pl_dst))
	    call pl_compress (pl_dst)

	call sfree (sp)
end
