include <imhdr.h>
include <pkg/dttext.h>
include "iralign.h"

# IR_M1MATCH -- Procedure to match images in the direction of observation
# direction. 

procedure ir_m1match (ir, im, ranges, ic1, ic2, il1, il2, deltax, deltay,
	deltai)

pointer	ir			# pointer to the ir strucuture
pointer	im			# pointer to the input image
int	ranges[ARB]		# array elements to be skipped
int	ic1[ARB]		# input beginning column limits
int	ic2[ARB]		# output beginning column limits
int	il1[ARB]		# input beginning line limits
int	il2[ARB]		# output beginning line limits
real	deltax[ARB]		# x shifts
real	deltay[ARB]		# y shifts
real	deltai[ARB]		# intensity shifts

int	num, nmod, turn_corner
int	pc1, pc2, pl1, pl2, c1, c2, l1, l2
int	pideltax, pideltay, ideltax, ideltay
int	oc1, oc2, ol1, ol2, clim1, clim2, llim1, llim2
pointer	buf
real	pmedian, median, dif

int	ir_overlap()
pointer	imgs2r()
real	amedr()

begin
	# Initialize the intensity subraster.
	call ir_vecinit (deltai, IR_NXSUB(ir) * IR_NYSUB(ir), ranges) 

	if (IR_ORDER(ir) == IR_ROW)
	    nmod = IR_NXSUB(ir)
	else
	    nmod = IR_NYSUB(ir)

	# Loop over the subrasters to be matched.
	for (num = 1; num <= IR_NXSUB(ir) * IR_NYSUB(ir); num = num + 1) {

	    if (num == 1) {

		# Get the position and shift for the first subraster.
		pideltax = nint (deltax[num])
		pideltay = nint (deltay[num])
		pc1 = ic1[num]
		pc2 = ic2[num]
		pl1 = il1[num]
		pl2 = il2[num]
		num = num + 1
		dif = 0.0
		turn_corner = NO

	    } else if ((IR_RASTER(ir)) == NO && (mod (num, nmod) == 1)) {

		# Get the position and shift for the first subraster.
		pideltax = nint (deltax[num-nmod])
		pideltay = nint (deltay[num-nmod])
		pc1 = ic1[num-nmod]
		pc2 = ic2[num-nmod]
		pl1 = il1[num-nmod]
		pl2 = il2[num-nmod]
		dif = -deltai[num-nmod]
		turn_corner = YES

	    } else {

		# Reset the coordinates of the previous subraster.
		pc1 = c1
		pc2 = c2
		pl1 = l1
		pl2 = l2
		pideltax = ideltax
		pideltay = ideltay
		turn_corner = NO
	    }

	    # Get the positions and shifts of the next subraster.
	    ideltax = nint (deltax[num])
	    ideltay = nint (deltay[num])
	    c1 = ic1[num]
	    c2 = ic2[num]
	    l1 = il1[num]
	    l2 = il2[num]

	    # Compute the overlap region.
	    if (ir_overlap (pc1 + pideltax, pc2 + pideltax, pl1 + pideltay,
		pl2 + pideltay, c1 + ideltax, c2 + ideltax, l1 + ideltay,
		l2 + ideltay, oc1, oc2, ol1, ol2) == YES) {

		clim1 = max (pc1, min (oc1 - pideltax, pc2))
		clim2 = min (pc2, max (oc2 - pideltax, pc1))
		llim1 = max (pl1, min (ol1 - pideltay, pl2))
		llim2 = min (pl2, max (ol2 - pideltay, pl1))
		buf = imgs2r (im, clim1, clim2, llim1, llim2)
		pmedian = amedr (Memr[buf], (clim2 - clim1 + 1) * (llim2 -
		    llim1 + 1))

		clim1 = max (c1, min (oc1 - ideltax, c2))
		clim2 = min (c2, max (oc2 - ideltax, c1))
		llim1 = max (l1, min (ol1 - ideltay, l2))
		llim2 = min (l2, max (ol2 - ideltay, l1))
		buf = imgs2r (im, clim1, clim2, llim1, llim2)
		median = amedr (Memr[buf], (clim2 - clim1 + 1) * (llim2 -
		    llim1 + 1))

		dif = dif + median - pmedian
		if (turn_corner == YES) {
		    if (! IS_INDEFR (deltai[num]))
		        deltai[num] = deltai[num-nmod] - median + pmedian
		} else {
		    if (! IS_INDEFR (deltai[num]))
		        deltai[num] = deltai[num] -  dif
		}
	    }

	}
end
