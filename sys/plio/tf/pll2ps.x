# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plio.h>

# PL_L2P -- Convert a line list to a pixel array.  The number of pixels output
# (always npix) is returned as the function value.

int procedure pl_l2ps (ll_src, xs, px_dst, npix)

short	ll_src[ARB]		#I input line list
int	xs			#I starting index in ll_src
short	px_dst[ARB]		#O output pixel array
int	npix			#I number of pixels to convert

short	pv
bool	skipword
int	opcode, data, ll_len, ll_first
int	x1, x2, i1, i2, xe, np, ip, op, otop, i
define	putpix_ 91
 
begin
	# Support old format line lists.
	if (LL_OLDFORMAT(ll_src)) {
	    ll_len = OLL_LEN(ll_src)
	    ll_first = OLL_FIRST
	} else {
	    ll_len = LL_LEN(ll_src)
	    ll_first = LL_FIRST(ll_src)
	}

	# No pixels?
	if (npix <= 0 || ll_len <= 0)
	    return (0)
 
	xe = xs + npix - 1
	skipword = false
	op = 1
	x1 = 1
	pv = 1

	do ip = ll_first, ll_len {
	    if (skipword) {
		skipword = false
		next
	    }

	    opcode = I_OPCODE(ll_src[ip])
	    data   = I_DATA(ll_src[ip])

	    switch (opcode) {
	    case I_ZN, I_HN, I_PN:
		# Determine inbounds region of segment.
		x2 = x1 + data - 1
		i1 = max (x1, xs)
		i2 = min (x2, xe)

		# Process segment if any region is inbounds.
		np = i2 - i1 + 1
		if (np > 0) {
		    otop = op + np - 1
		    if (opcode == I_HN) {
			do i = op, otop
			    px_dst[i] = pv
		    } else {
			do i = op, otop
			    px_dst[i] = 0
			if (opcode == I_PN && i2 == x2)
			    px_dst[otop] = pv
		    }
		    op = otop + 1
		}

		# Advance the line index.
		x1 = x2 + 1

	    case I_SH:
		pv = (int(ll_src[ip+1]) * I_SHIFT) + data
		skipword = true
	    case I_IH:
		pv = pv + data
	    case I_DH:
		pv = pv - data
	    case I_IS:
		pv = pv + data
		goto putpix_
	    case I_DS:
		pv = pv - data
putpix_
		if (x1 >= xs && x1 <= xe) {
		    px_dst[op] = pv
		    op = op + 1
		}
		x1 = x1 + 1
	    }

	    if (x1 > xe)
		break
	}

	# Zero any remaining output range.
	do i = op, npix
	    px_dst[i] = 0

	return (npix)
end
