# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plset.h>
include	<plio.h>

# PL_P2R -- Convert a pixel array to a range list.  The length of the output
# range list is returned as the function value.

int procedure pl_p2rl (px_src, xs, rl, npix)

long	px_src[ARB]		#I input pixel array
int	xs			#I starting index in pixbuf
long	rl[3,ARB]		#O destination range list
int	npix			#I number of pixels to convert

long	hi, pv, zero
int	xe, x1, np, rn, nv, ip
define	done_ 91

begin
	# No input pixels?
	if (npix <= 0)
	    return (0)

	xe = xs + npix - 1
	rn = RL_FIRST

	# Pack the pixel array into a range list.  This is done by scanning
	# the pixel list for successive ranges of pixels of constant nonzero
	# value, where each range is described as follows:

	zero = 0
	pv = max (zero, px_src[xs])	# pixel value of current range
	x1 = xs			# start index of current range
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
	    }

	    np = ip - x1 + 1

	    # Output the new range.
	    if (pv > 0) {
		rl[1,rn] = x1
		rl[2,rn] = np
		rl[3,rn] = pv
		rn = rn + 1
	    }

	    x1 = ip + 1
	    pv = nv
	}

	RL_LEN(rl) = rn - 1
	RL_AXLEN(rl) = npix

	return (rn - 1)
end
