# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plset.h>
include "pllseg.h"
include	<plio.h>

# PL_LINEROP -- Rasterop operation between source and destination line lists.
# The indicated rasterop operation is performed upon the source and destination
# line lists, writing the result to LL_OUT, which is a copy of LL_DST except
# for the region affected by the rasterop operation (note that the destination
# line list cannot be edited in place since it may change size).

procedure pl_linerop (ll_src, xs, src_maxval,
		      ll_dst, ds, dst_maxval, ll_out, npix, rop)

short	ll_src[ARB]		#I source line list
int	xs			#I starting pixel index in src line list
int	src_maxval		#I maximum pixel value, source mask
short	ll_dst[ARB]		#I destination line list
int	ds			#I starting pixel index in dst line list
int	dst_maxval		#I maximum pixel value, dst mask
short	ll_out[ARB]		#O output list (edited version of ll_dst)
int	npix			#I number of pixels to convert
int	rop			#I rasterop

int	segsize, v_src, v_dst, pv
bool	need_src, need_dst, rop_enable
int	o_op, o_iz, o_pv, o_np, o_hi, src_value
int	opcode, data, nz, iz, x1, hi, dv, v, np, op, n, i
int	d_src[LEN_PLLDES], d_dst[LEN_PLLDES]
define	done_ 91

begin
	need_src = R_NEED_SRC(rop)
	need_dst = R_NEED_DST(rop)
	opcode   = R_OPCODE(rop)
	data     = R_DATA(rop)

	# Pixel value to be used if input mask is boolean.
	if (src_maxval == 1) {
	    src_value = data
	    if (src_value <= 0)
		src_value = dst_maxval
	}

	# Advance to the desired position in the source list, discarding
	# the instructions once read.  The point XS may lie within the range
	# of an instruction.

	if (need_src) {
	    x1 = 1
	    pll_init (ll_src, d_src)
	    do i = 1, ARB {
		np = min (pll_nleft(d_src), xs - x1)
		pll_getseg (ll_src, d_src, np, v_src)
		x1 = x1 + np
		if (x1 >= xs || np == 0)
		    break
	    }
	}

	# Copy DST to OUT, applying the rasterop in the region of NPIX pixels
	# beginning at DS.  To simplify things (avoid pathological complexity
	# in this case) we suffer some unnecessary unpacking and repacking
	# of encoded line list instructions in the regions of the DST list
	# which are simply copied.  This avoids the need for special treatment
	# at the edges of the region to which the ROP applies.  ROP_ENABLE is
	# false initially, true in the ROP region, and false again to the
	# right.  The number of pixels in each region is given by SEGSIZE.

	o_pv = -1
	op = LL_CURHDRLEN + 1
	segsize = ds - 1
	rop_enable = false
	x1 = 1;  iz = 1;  hi = 1
	pll_init (ll_dst, d_dst)

	do i = 1, ARB {
	    # Set up for the next segment (before, in, and after the region to
	    # which the ROP applies), when the current segment is exhausted.

	    if (segsize <= 0)
		if (!rop_enable) {
		    # Begin processing central region.
		    segsize = npix
		    rop_enable = true
		    if (segsize <= 0)
			next
		} else {
		    # Begin processing final region.
		    segsize = ARB
		    rop_enable = false
		}

	    # Determine the length of the next output segment.  This is the
	    # largest segment of constant value formed by the intersection of
	    # the two lists.  If bounds checking has been properly performed
	    # then it should not be possible to see nleft=zero on either input
	    # list.  Note that zeroed regions are valid data here.

	    np = min (segsize, pll_nleft(d_dst))
	    if (need_src && rop_enable && pll_nleft(d_src) > 0)
		np = min (np, pll_nleft(d_src))
	    if (np <= 0)
		break

	    # Get the segment value and advance the line pointers.  We always
	    # have to read the DST list in order to copy the unmodified regions
	    # to the output.  We read the SRC list and apply the rasterop only
	    # in the region to which the ROP applies.

	    pll_getseg (ll_dst, d_dst, np, v_dst)
	    if (rop_enable) {
		# Get v_src.
		if (need_src) {
		    v_src = 0
		    if (pll_nleft (d_src) > 0)
			pll_getseg (ll_src, d_src, np, v_src)

		    if (R_NOTSRC(rop)) {
			v_src = not (v_src)
			if (src_maxval != 0)
			    v_src = and (v_src, src_maxval)
		    }

		    if (v_src != 0 && src_maxval == 1)
			v_src = src_value
		}

		# Get v_dst.
		if (need_dst) {
		    if (R_NOTDST(rop)) {
			v_dst = not (v_dst)
			if (dst_maxval != 0)
			    v_dst = and (v_dst, dst_maxval)
		    }
		}

		# Apply the rasterop.
		switch (opcode) {
		case PIX_CLR:
		    pv = 0
		case PIX_SET:
		    pv = data
		case PIX_SRC, PIX_NOTSRC:
		    pv = v_src
		case PIX_DST, PIX_NOTDST:
		    pv = v_dst
		case PIX_SRC_AND_DST, PIX_SRC_AND_NOTDST, PIX_NOTSRC_AND_DST:
		    pv = and (v_src, v_dst)
		case PIX_SRC_OR_DST, PIX_SRC_OR_NOTDST, PIX_NOTSRC_OR_DST:
		    pv = or (v_src, v_dst)
		case PIX_SRC_XOR_DST:
		    pv = xor (v_src, v_dst)
		case PIX_NOT_SRC_AND_DST:
		    pv = not (and (v_src, v_dst))
		case PIX_NOT_SRC_OR_DST:
		    pv = not (or (v_src, v_dst))
		case PIX_NOT_SRC_XOR_DST:
		    pv = not (xor (v_src, v_dst))
		}

		# Mask the high bits to prevent negative values, or map int
		# to bool for the case of a boolean output mask.

		if (dst_maxval == 1 && pv != 0)
		    pv = 1
		else if (dst_maxval > 1)
		    pv = and (dst_maxval, pv)

	    } else
		pv = v_dst


	    if (pv == 0) {
		if (pll_nleft (d_dst) <= 0) {
		    # Output zeros at end of list.
		    x1 = x1 + np
		} else {
		    # Keep going until we get a nonzero range.
		    o_pv = 0
		    x1 = x1 + np
		    segsize = segsize - np
		    next
		}
	    } else if (pv == o_pv) {
		# Combine with previous range.
		iz = o_iz
		hi = o_hi
		op = o_op
		x1 = x1 - o_np
		segsize = segsize + o_np
		np = np + o_np
		o_np = np
	    } else {
		# Save current range parameters.
		o_op = op
		o_np = np
		o_iz = iz
		o_hi = hi
		o_pv = pv
	    }

	    # Encode an instruction to regenerate the current range of NP data
	    # values of nonzero level PV, starting at X1.  In the most complex
	    # case we must update the high value and output a range of zeros,
	    # followed by a range of NP high values.  If NP is 1, we can
	    # probably use a PN or [ID]S instruction to save space.

	    nz = x1 - iz

	    # Change the high value?
	    if (pv > 0) {
		dv = pv - hi
		if (dv != 0) {
		    # Output IH or DH instruction?
		    hi = pv
		    if (abs(dv) > I_DATAMAX) {
			ll_out[op] = M_SH + and (pv, I_DATAMAX)
			op = op + 1
			ll_out[op] = pv / I_SHIFT
			op = op + 1
		    } else {
			if (dv < 0)
			    ll_out[op] = M_DH + (-dv)
			else
			    ll_out[op] = M_IH + dv
			op = op + 1

			# Convert to IS or DS if range is a single pixel.
			if (np == 1 && nz == 0) {
			    v = ll_out[op-1]
			    ll_out[op-1] = or (v, M_MOVE)
			    goto done_
			}
		    }
		}
	    }

	    # Output range of zeros to catch up to current range?
	    if (nz > 0) {
		# Output the ZN instruction.
		for (n=nz;  n > 0;  n = n - I_DATAMAX) {
		    ll_out[op] = M_ZN + min(I_DATAMAX,n)
		    op = op + 1
		}
		# Convert to PN if range is a single pixel.
		if (np == 1 && pv > 0) {
		    ll_out[op-1] = ll_out[op-1] + M_PN + 1
		    goto done_
		}
		# At end of list.
		if (pv == 0)
		    goto done_
	    }

	    # The only thing left is the HN instruction if we get here.
	    for (n=np;  n > 0;  n = n - I_DATAMAX) {
		ll_out[op] = M_HN + min(I_DATAMAX,n)
		op = op + 1
	    }
done_
	    segsize = segsize - np
	    x1 = x1 + np
	    iz = x1
	}

	# Update the line list header.
	call amovs (ll_dst, ll_out, LL_CURHDRLEN)
	LL_SETLEN(ll_out, op - 1)
end
