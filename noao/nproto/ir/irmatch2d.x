include <imhdr.h>
include "iralign.h"

# IR_M2MATCH -- Compute the intensity matching parameters.

procedure ir_m2match (ir, im, ranges, ic1, ic2, il1, il2, deltax, deltay,
	deltai)

pointer	ir			# pointer to the ir structure
pointer	im			# pointer to the input image
int	ranges[ARB]		# ranges of data to align
int	ic1[ARB]		# array of input begin columns
int	ic2[ARB]		# array of input end columns
int	il1[ARB]		# array of input begin lines
int	il2[ARB]		# array of input end lines
real	deltax[ARB]		# array of x shifts
real	deltay[ARB]		# array of y shifts
real	deltai[ARB]		# array of i shifts

begin
	# Initialize the intensity subraster.
	call ir_vecinit (deltai, IR_NXSUB(ir) * IR_NYSUB(ir), ranges)
	if (ranges[1] == NULL)
	    return

	# Match the intensities in the direction of observation.
	call ir_omatch (ir, im, ic1, ic2, il1, il2, deltax, deltay, deltai)

	# Match the intensities in the other direction.
	call ir_nmatch (ir, im, ic1, ic2, il1, il2, deltax, deltay, deltai)
end


# IR_OMATCH -- Procedure to match images in the direction of observation
# direction. 

procedure ir_omatch (ir, im, ic1, ic2, il1, il2, deltax, deltay, deltai)

pointer	ir			# pointer to the ir structure
pointer	im			# pointer to the input image
int	ic1[ARB]		# beginning column limits
int	ic2[ARB]		# ending column limits
int	il1[ARB]		# beginning line limits
int	il2[ARB]		# ending line limits
real	deltax[ARB]		# array of x shifts
real	deltay[ARB]		# array of y shifts
real	deltai[ARB]		# array of intensity shifts

int	num, nimages, nrasters
int	pc1, pc2, pl1, pl2, c1, c2, l1, l2
int	pideltax, pideltay, ideltax, ideltay
int	oc1, oc2, ol1, ol2, clim1, clim2, llim1, llim2
pointer	buf
real	pmedian, median, dif

int	ir_overlap()
pointer	imgs2r()
real	amedr()

begin
	# Compute the do loop parameters.
	nimages = IR_NXSUB(ir) * IR_NYSUB(ir)
	if (IR_ORDER(ir) == IR_ROW)
	    nrasters = IR_NXSUB(ir)
	else
	    nrasters = IR_NYSUB(ir)

	# Loop over the subrasters to be matched.
	for (num = 1; num <= nimages; num = num + 1) {

	    if (mod (num, nrasters) == 1) {

		# Get the position and shift for the first subraster in
		# the column.
		pideltax = nint (deltax[num])
		pideltay = nint (deltay[num])
		pc1 = ic1[num]
		pc2 = ic2[num]
		pl1 = il1[num]
		pl2 = il2[num]
		num = num + 1
		dif = 0.0

		# Get the the position and shift for the next subraster in
		# the column.to be
		ideltax = nint (deltax[num])
		ideltay = nint (deltay[num])
		c1 = ic1[num]
		c2 = ic2[num]
		l1 = il1[num]
		l2 = il2[num]

	    } else {

		# Reset the coordinates of the previous subraster.
		pc1 = c1
		pc2 = c2
		pl1 = l1
		pl2 = l2
		pideltax = ideltax
		pideltay = ideltay

		# Get the positions and shifts of the next subraster.
		ideltax = nint (deltax[num])
		ideltay = nint (deltay[num])
		c1 = ic1[num]
		c2 = ic2[num]
		l1 = il1[num]
		l2 = il2[num]

	    }

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
		if (! IS_INDEFR (deltai[num]))
		    deltai[num] = deltai[num] -  dif
	    }
	}
end


# IR_NMATCH -- Procedure to match images in the other direction. 

procedure ir_nmatch (ir, im, ic1, ic2, il1, il2, deltax, deltay, deltai)

pointer	ir		# pointer to the ir structure
pointer	im		# pointer to the input image
int	ic1[ARB]	# array of beginning columns
int	ic2[ARB]	# array of ending columns
int	il1[ARB]	# array of beginning lines
int	il2[ARB]	# array of ending lines
real	deltax[ARB]	# array of x shifts
real	deltay[ARB]	# array of y shifts
real	deltai[ARB]	# array of intensity shifts

int	num, nrasters, fac, nimages, count
int	pc1, pc2, pl1, pl2, c1, c2, l1, l2
int	pideltax, pideltay, ideltax, ideltay
int	oc1, oc2, ol1, ol2, clim1, clim2, llim1, llim2
pointer	buf
real	pmedian, median, pdif, dif, tdif

int	ir_overlap()
pointer	imgs2r()
real	amedr()

begin
	# Compute the do loop parameters.
	nimages = IR_NXSUB(ir) * IR_NYSUB(ir)
	if (IR_ORDER(ir) == IR_ROW)
	    nrasters = IR_NXSUB(ir)
	else
	    nrasters = IR_NYSUB(ir)
	fac = 2 * nrasters

	# Loop over the subrasters to be matched.
	num = 1
	count = 1
	repeat {

	    # Get the position and shift for the first subraster.
	    if (num <= nrasters) {

		pideltax = nint (deltax[num])
		pideltay = nint (deltay[num])
		pc1 = ic1[num]
		pc2 = ic2[num]
		pl1 = il1[num]
		pl2 = il2[num]
		if (IS_INDEFR(deltai[num]))
		    pdif = 0.0
		else
		    pdif = deltai[num]
		tdif = 0.0
		if (IR_RASTER(ir) == YES) {
		    num = fac - num + 1
		    fac = fac + fac
		} else
		    num = num + nrasters

		# Get the the position and shift for the next.
		ideltax = nint (deltax[num])
		ideltay = nint (deltay[num])
		c1 = ic1[num]
		c2 = ic2[num]
		l1 = il1[num]
		l2 = il2[num]
		if (IS_INDEFR(deltai[num]))
		    dif = 0.0
		else
		    dif = deltai[num]

	    } else {

		# Reset the coordinates of the previous subraster.
		pc1 = c1
		pc2 = c2
		pl1 = l1
		pl2 = l2
		pideltax = ideltax
		pideltay = ideltay
		pdif = dif

		# Get the positions and shifts of the subraster to be adjusted.
		ideltax = nint (deltax[num])
		ideltay = nint (deltay[num])
		c1 = ic1[num]
		c2 = ic2[num]
		l1 = il1[num]
		l2 = il2[num]
		if (IS_INDEFR(deltai[num]))
		    dif = 0.0
		else
		    dif = deltai[num]

	    }

	    # Compute the overlap region.
	    if (ir_overlap (pc1 + pideltax, pc2 + pideltax, pl1 + pideltay,
		pl2 + pideltay, c1 + ideltax, c2 + ideltax, l1 + ideltay,
		l2 + ideltay, oc1, oc2, ol1, ol2) == YES) {

		clim1 = max (pc1, oc1 - pideltax)
		clim2 = min (pc2, oc2 - pideltax)
		llim1 = max (pl1, ol1 - pideltay)
		llim2 = min (pl2, ol2 - pideltay)
		buf = imgs2r (im, clim1, clim2, llim1, llim2)
		pmedian = amedr (Memr[buf], (clim2 - clim1 + 1) * (llim2 -
		    llim1 + 1))

		clim1 = max (c1, oc1 - ideltax)
		clim2 = min (c2, oc2 - ideltax)
		llim1 = max (l1, ol1 - ideltay)
		llim2 = min (l2, ol2 - ideltay)
		buf = imgs2r (im, clim1, clim2, llim1, llim2)
		median = amedr (Memr[buf], (clim2 - clim1 + 1) * (llim2 -
		    llim1 + 1))

		tdif = tdif + median + dif - pmedian - pdif
		if (! IS_INDEFR (deltai[num]))
		    deltai[num] = deltai[num] - tdif
	    }

	    if (IR_RASTER(ir) == YES) {
		num = fac - num + 1
		fac = fac + fac
	    } else
	        num = num + nrasters
	    if (num > nimages) {
		count = count + 1
		num = count
		fac = 2 * nrasters
	    }

	} until (count > nrasters)
end
