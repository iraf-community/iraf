# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plio.h>

# PL_P2L -- Convert a pixel array to a line list.  The length of the list is
# returned as the function value.

int procedure pl_p2ls (px_src, xs, ll_dst, npix)

short	px_src[ARB]		#I input pixel array
int	xs			#I starting index in pixbuf
short	ll_dst[ARB]		#O destination line list
int	npix			#I number of pixels to convert

short	hi, pv, nv, zero
int	xe, x1, iz, ip, op, np, nz, dv, v
define	done_ 91

begin
	# No input pixels?
	if (npix <= 0)
	    return (0)

	# Initialize the linelist header.
	LL_VERSION(ll_dst) = LL_CURVERSION
	LL_HDRLEN(ll_dst) = LL_CURHDRLEN
	LL_NREFS(ll_dst) = 0
	LL_SETBLEN(ll_dst,0)

	xe = xs + npix - 1
	op = LL_CURHDRLEN + 1

	# Pack the pixel array into a line list.  This is done by scanning
	# the pixel list for successive ranges of pixels of constant nonzero
	# value, where each range is described as follows:

	zero = 0
	pv = max (zero, px_src[xs])	# pixel value of current range
	x1 = xs			# start index of current range
	iz = xs			# start index of range of zeros
	hi = 1			# current high value

	# Process the data array.
	do ip = xs, xe {
	    if (ip < xe) {
		# Get the next pixel value, loop again if same as previous one.
		nv = max (zero, px_src[ip+1])
		if (nv == pv)
		    next

		# If current range is zero, loop again to get nonzero range.
		if (pv == 0) {
		    pv = nv
		    x1 = ip + 1
		    next
		}
	    } else if (pv == 0)
		x1 = xe + 1

	    # Encode an instruction to regenerate the current range I0-IP
	    # of N data values of nonzero level PV.  In the most complex case
	    # we must update the high value and output a range of zeros,
	    # followed by a range of NP high values.  If NP is 1, we can
	    # probably use a PN or [ID]S instruction to save space.

	    np = ip - x1 + 1
	    nz = x1 - iz

	    # Change the high value?
	    if (pv > 0) {
		dv = pv - hi
		if (dv != 0) {
		    # Output IH or DH instruction?
		    hi = pv
		    if (abs(dv) > I_DATAMAX) {
			ll_dst[op] = M_SH + and (int(pv), I_DATAMAX)
			op = op + 1
			ll_dst[op] = pv / I_SHIFT
			op = op + 1
		    } else {
			if (dv < 0)
			    ll_dst[op] = M_DH + (-dv)
			else
			    ll_dst[op] = M_IH + dv
			op = op + 1

			# Convert to IS or DS if range is a single pixel.
			if (np == 1 && nz == 0) {
			    v = ll_dst[op-1]
			    ll_dst[op-1] = or (v, M_MOVE)
			    goto done_
			}
		    }
		}
	    }

	    # Output range of zeros to catch up to current range?
	    # The I_DATAMAX-1 limit is to allow adding M_PN+1 without
	    # overflowing the range of the data segment.
	    if (nz > 0) {
		# Output the ZN instruction.
		for (;  nz > 0;  nz = nz - (I_DATAMAX-1)) {
		    ll_dst[op] = M_ZN + min(I_DATAMAX-1,nz)
		    op = op + 1
		}
		# Convert to PN if range is a single pixel.
		if (np == 1 && pv > 0) {
		    ll_dst[op-1] = ll_dst[op-1] + M_PN + 1
		    goto done_
		}
	    }

	    # The only thing left is the HN instruction if we get here.
	    for (;  np > 0;  np = np - I_DATAMAX) {
		ll_dst[op] = M_HN + min(I_DATAMAX,np)
		op = op + 1
	    }
done_
	    x1 = ip + 1
	    iz = x1
	    pv = nv
	}

	LL_SETLEN(ll_dst, op - 1)
	return (op - 1)
end
