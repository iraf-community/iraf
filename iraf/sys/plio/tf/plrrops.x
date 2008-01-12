# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plset.h>
include	<plio.h>
include "../plrseg.h"

# PL_RANGEROP -- Rasterop operation between source and destination range lists.
# The indicated rasterop operation is performed upon the source and destination
# range lists, writing the result to RL_OUT, which is a copy of RL_DST except
# for the region affected by the rasterop operation (note that the destination
# range list cannot be edited in place since it may change size).

procedure pl_rangerops (rl_src, xs, src_maxval,
			 rl_dst, ds, dst_maxval, rl_out, npix, rop)

short	rl_src[3,ARB]		#I source range list
int	xs			#I starting pixel index in src range list
int	src_maxval		#I max pixel value in src mask
short	rl_dst[3,ARB]		#I destination range list
int	ds			#I starting pixel index in dst range list
int	dst_maxval		#I max pixel value in dst mask
short	rl_out[3,ARB]		#O output list (edited version of rl_dst)
int	npix			#I number of pixels to convert
int	rop			#I rasterop

bool	need_src, need_dst, rop_enable
short	data, src_value, v_src, v_dst, pv
int	segsize, opcode, x, i, np, rn_o, p
int	d_src[LEN_PLRDES], d_dst[LEN_PLRDES]

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
	# the unused ranges.  The point XS may lie within a range or in a
	# zero area of the input line.

	if (need_src) {
	    x = 1
	    plr_init (rl_src, d_src)
	    do i = 1, ARB {
		np = min (plr_nleft(d_src), xs - x)
		plr_getseg (rl_src, d_src, np, v_src)
		x = x + np
		if (x >= xs || np == 0)
		    break
	    }
	}

	# Advance through both the source and destination lists, extracting
	# line segments which have a constant value in each input list; the
	# values for the two lists may differ.  Apply the given rasterop to
	# the source and destination pixel values and write each line segment
	# as a range to the output list.  If the ranges in the two input lists
	# differ (randomly overlap) then the output list will generally be
	# more fragmented, i.e., have more ranges of constant value.  As each
	# output range is generated compare it with the previous range to see
	# if they can be joined, as applying a rasterop may cause two different
	# ranges to have the same value.

	x = 1
	rn_o = RL_FIRST
	segsize = ds - 1
	rop_enable = false
	plr_init (rl_dst, d_dst)

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

	    np = min (segsize, plr_nleft(d_dst))
	    if (need_src && rop_enable && plr_nleft(d_src) > 0)
		np = min (np, plr_nleft(d_src))
	    if (np <= 0)
		break

	    # Get the segment value and advance the line pointers.  We must
	    # read the DST list whether or not we will use the data, since
	    # the list pointer must be advanced NPIX pixels so that we may
	    # copy the remainder of the list after the loop.

	    plr_getseg (rl_dst, d_dst, np, v_dst)
	    if (rop_enable) {
		# Get v_src.
		if (need_src) {
		    v_src = 0
		    if (plr_nleft (d_src) > 0)
			plr_getseg (rl_src, d_src, np, v_src)

		    if (R_NOTSRC(rop)) {
			v_src = not (v_src)
			if (src_maxval != 0)
			    v_src = and (int(v_src), src_maxval)
		    }

		    if (v_src != 0 && src_maxval == 1)
			v_src = src_value
		}

		# Get v_dst.
		if (need_dst) {
		    if (R_NOTDST(rop)) {
			v_dst = not (v_dst)
			if (dst_maxval != 0)
			    v_dst = and (int(v_dst), dst_maxval)
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
		    pv = and (int(pv), dst_maxval)

	    } else
		pv = v_dst

	    # Output a nonzero range.
	    if (pv > 0) {
		p = rn_o - 1
		if (p >= RL_FIRST &&
		    pv == rl_out[3,p] && x == rl_out[1,p] + rl_out[2,p]) {
		    # Merge new range with previous one.
		    rl_out[2,p] = rl_out[2,p] + np
		} else {
		    rl_out[1,rn_o] = x
		    rl_out[2,rn_o] = np
		    rl_out[3,rn_o] = pv
		    rn_o = rn_o + 1
		}
	    }

	    x = x + np
	    segsize = segsize - np
	}

	# Update the range list header.
	call amovs (rl_dst, rl_out, (RL_FIRST - 1) * 3)
	RL_LEN(rl_out) = rn_o - 1
end
