include <imhdr.h>
include <pkg/dttext.h>
include "iralign.h"

# IR_M1SUBALIGN -- Align all the subrasters.

procedure ir_m1subalign (im, outim, section, dt, ranges, xrshifts, yrshifts,
	xcshifts, ycshifts, nxsub, nysub, nxrsub, nyrsub, ncols, nrows,
	nxoverlap, nyoverlap, order, raster, oval, interp, verbose)

pointer	im			# pointer to the input image
pointer	outim			# pointer to the output image
char	section[ARB]		# output reference section
pointer	dt			# pointer to the database file
int	ranges[ARB]		# list of subrasters to be intensity matched
real	xrshifts[nxsub,ARB]	# x row shifts
real	yrshifts[nxsub,ARB]	# y row shifts
real	xcshifts[nxsub,ARB]	# x column shifts
real	ycshifts[nxsub,ARB]	# y column shifts
int	nxsub			# number of subrasters in x direction
int	nysub			# number of subrasters in y direction
int	nxrsub			# index of x reference subraster
int	nyrsub			# index of y reference subraster
int	ncols			# number of columns in subraster
int	nrows			# number of rows in subraster
int	nxoverlap		# number of columns of overlap
int	nyoverlap		# number of rows of overlap
int	order			# row or column order
int	raster			# raster scan
real	oval			# undefined pixel region
int	interp			# type of interpolant
int	verbose			# print messages

int	i, nxsize, nysize, nimcols, nimlines, xsubindex, ysubindex
int	c1ref, c2ref, l1ref, l2ref, c1in, c2in, l1in, l2in, c1out, c2out
int	l1out, l2out, pc1out, pc2out, pl1out, pl2out,  mc1out, mc2out
int	ml1out, ml2out, ideltax, ideltay, norows, nocols, ncount, next_match
pointer	sp, input, inbuf, outbuf, obuf, x, y, msi
real	deltax, deltay, ytemp, iscale

int	ir_decode_section(), get_next_number(), fscan(), nscan()
pointer	imps2r(), imgs2r()
real	ir_m1overlap()

begin
	# Allocate temporary space.
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (x, ncols, TY_REAL)
	call salloc (y, ncols, TY_REAL)

	# Find the position in the output image of the reference subraster.
	nxsize = ncols - nxoverlap
	nysize = nrows - nyoverlap
	if (section[1] == EOS) {
	    c1ref = (nxrsub - 1) * nxsize + 1
	    c2ref = c1ref + ncols - 1
	    l1ref = (nyrsub - 1) * nysize + 1
	    l2ref = l1ref + nrows - 1
	} else {
	    if (ir_decode_section (section, c1ref, c2ref, l1ref, l2ref) == ERR)
		call error (0, "Illegal reference subraster section.")
	}
	if ((c2ref - c1ref + 1) != ncols || (l2ref - l1ref + 1) != nrows)
	    call error (0, "Reference subsection is the wrong size.")

	# Fill in the output image with the undefined value.
	nimcols = IM_LEN(outim,1)
	nimlines = IM_LEN(outim,2)
	call ir_imzero (outim, nimcols, nimlines, oval)

	# Initialize the list of rasters to be intensity matched.
	ncount = 1
	next_match = 0
	if (get_next_number (ranges, next_match) == EOF)
	    next_match = nxsub * nysub + 1

	# Initialize the interpolant.
	call msiinit (msi, interp)

	# Extract the subrasters one by one.
	while (fscan (DT(dt)) != EOF && (ncount <= nxsub * nysub)) {

	    # Get the image section.
	    call gargwrd (Memc[input], SZ_FNAME)
	    call gargwrd (Memc[input], SZ_FNAME)
	    if (nscan () != 2)
		next

	    # Decode the input image section.
	    if (ir_decode_section (Memc[input], c1in, c2in, l1in, l2in) == ERR)
		call error (0, "Error decoding input image section.")
	    if ((c2in - c1in + 1) != ncols || (l2in - l1in + 1) != nrows)
		call error (0, "Input subraster is the wrong size")

	    # Compute the shift relative to the reference subraster.
	    xsubindex = c1in / nxsize + 1
	    ysubindex = l1in / nysize + 1
	    call ir_mkshift (xrshifts, yrshifts, xcshifts, ycshifts, nxsub,
	        nysub, xsubindex, ysubindex, nxrsub, nyrsub, order, deltax,
		deltay)
	    ideltax = nint (deltax)
	    ideltay = nint (deltay)

	    # Get the output buffer.
	    c1out = max (1, min (nimcols, c1ref + (xsubindex - nxrsub) * ncols +
	        ideltax))
	    c2out = min (nimcols, max (1, c2ref + (xsubindex - nxrsub) * ncols +
	        ideltax))
	    l1out = max (1, min (nimlines, l1ref + (ysubindex - nyrsub) *
	        nrows + ideltay))
	    l2out = min (nimlines, max (1, l2ref + (ysubindex - nyrsub) *
	        nrows + ideltay))
	    nocols = c2out - c1out + 1
	    norows = l2out - l1out + 1
	    outbuf = imps2r (outim, c1out, c2out, l1out, l2out)
	    if (outbuf == NULL)
		call error (0, "Error writing output image.")

	    # Fetch the input subraster and fit the interpolant.
	    inbuf = imgs2r (im, c1in, c2in, l1in, l2in)
	    if (inbuf == NULL)
		call error (0, "Error reading input image.")
	    call msifit (msi, Memr[inbuf], ncols, nrows, ncols)

	    # Match intensities.
	    if ((ncount < next_match) || ncount == 1) {
		iscale = INDEF
	    } else {
	        if (get_next_number (ranges, next_match) == EOF)
	    	    next_match = nxsub * nysub + 1
		iscale = ir_m1overlap (outim, Memr[inbuf], ncols, nrows, pc1out,
		    pc2out, pl1out, pl2out, c1out, c2out, l1out, l2out)
	    }


	    # Write the output image.
	    do i = 1, nocols
		Memr[x+i-1] = max (1.0, min (real (nocols), real (i + deltax -
		    ideltax)))
	    obuf = outbuf
	    do i = 1, norows {
		ytemp = max (1.0, min (real (norows), real (i + deltay -
		    ideltay)))
		call amovkr (ytemp, Memr[y], nocols)
		call msivector (msi, Memr[x], Memr[y], Memr[obuf], nocols)
		if (! IS_INDEFR(iscale))
		    call aaddkr (Memr[obuf], iscale, Memr[obuf], nocols)
		obuf = obuf + nocols
	    }

	    # Print a message.
	    if (verbose == YES) {
		call printf ("imcopy %s[%d:%d,%d:%d] to %s[%d:%d,%d:%d]  scale: %g\n")
		    call pargstr (IM_HDRFILE(im))
		    call pargi (c1in)
		    call pargi (c2in)
		    call pargi (l1in)
		    call pargi (l2in)
		    call pargstr (IM_HDRFILE(outim))
		    call pargi (c1out)
		    call pargi (c2out)
		    call pargi (l1out)
		    call pargi (l2out)
		    call pargr (iscale)
	    }

	    # Store the coordinates of the previous image.
	    call ir_overset (mc1out, mc2out, ml1out, ml2out, pc1out, pc2out,
	        pl1out, pl2out, c1out, c2out, l1out, l2out, ncount, nxsub,
		nysub, order, raster)

	    # Update.
	    ncount = ncount + 1
	}

	call msifree (msi)
	call sfree (sp)
end


## IR_OVERSET -- Procedure to set up the coordinates for the next overlap
# region.

procedure ir_overset (mc1out, mc2out, ml1out, ml2out, pc1out, pc2out,
	pl1out, pl2out, c1out, c2out, l1out, l2out, ncount, nxsub, nysub,
	order, raster)

int	mc1out, mc2out	# marker subraster limits
int	ml1out, ml2out	# marker subraster column limits
int	pc1out, pc2out	# previous subraster column limits
int	pl1out, pl2out	# previous subraster line limita
int	c1out, c2out	# current subraster column limits
int	l1out, l2out	# current subraster line limits
int	ncount		# the index of the current subraster
int	nxsub, nysub	# the number of subrasters in x and y
int	order		# column or row order
int	raster		# raster scanning

begin
	if (raster == YES) {
	    pc1out = c1out
	    pc2out = c2out
	    pl1out = l1out
	    pl2out = l2out
	} else if (order == IR_COLUMN) {
	    if (mod (ncount, nysub) == 1) {
	        mc1out = c1out
	        mc2out = c2out
	        ml1out = l1out
		ml2out = l2out
	        pc1out = c1out
	        pc2out = c2out
	        pl1out = l1out
		pl2out = l2out
	    } else if (mod (ncount, nysub) == 0) {
	        pc1out = mc1out
	        pc2out = mc2out
	        pl1out = ml1out
		pl2out = ml2out
	    } else {
	        pc1out = c1out
	        pc2out = c2out
	        pl1out = l1out
		pl2out = l2out
	    }
	} else if (order == IR_ROW) {
	    if (mod (ncount, nxsub) == 1) {
	        mc1out = c1out
	        mc2out = c2out
	        ml1out = l1out
		ml2out = l2out
	        pc1out = c1out
	        pc2out = c2out
	        pl1out = l1out
		pl2out = l2out
	    } else if (mod (ncount, nxsub) == 0) {
	        pc1out = mc1out
	        pc2out = mc2out
	        pl1out = ml1out
		pl2out = ml2out
	    } else {
	        pc1out = c1out
	        pc2out = c2out
	        pl1out = l1out
		pl2out = l2out
	    }
	}
end


## IR_M1OVERLAP -- Procedure to compute the overlap between two rectangles.

real procedure ir_m1overlap (outim, input, ncols, nrows, pc1out, pc2out, pl1out,
	pl2out, c1out, c2out, l1out, l2out)

pointer	outim			# pointer to the output image
real	input[ncols,ARB]	# buffer of input data
int	ncols, nrows		# dimensions of the input array
int	pc1out, pc2out		# previous subraster column limits
int	pl1out, pl2out		# previous subraster line limits
int	c1out, c2out		# current subraster column limits
int	l1out, l2out		# current subraster line limits

int	i, oc1out, oc2out, ol1out, ol2out, nincols, ninlines, xindex, yindex
pointer	sp, inbuf, ibuf, outbuf
real	inmedian, outmedian
pointer	imgs2r()
real	amedr()

begin
	call smark (sp)

	# Check for the case where no intersection is present.
	if (c1out > pc2out || c2out < pc1out || l1out > pl2out ||
	    l2out < pl1out)
	    return (INDEFR)

	# Compute the column overlap limits.
	if (pc1out <= c1out)
	    oc1out = c1out
	else
	    oc1out = pc1out
	if (pc2out <= c2out)
	    oc2out = pc2out
	else
	    oc2out = c2out

	# Compute the line overlap limits.
	if (pl1out <= l1out)
	    ol1out = l1out
	else
	    ol1out = pl1out
	if (pl2out <= l2out)
	    ol2out = pl2out
	else
	    ol2out = l2out
	nincols = oc2out - oc1out + 1
	ninlines = ol2out - ol1out + 1

	# Find the median of the overlap region in the output image.
	outbuf = imgs2r (outim, oc1out, oc2out, ol1out, ol2out)
	if (outbuf == NULL)
	    call error (0, "Error reading output image.")
	outmedian = amedr (Memr[outbuf], nincols * ninlines)

	# Find the median of the input image overlap region.
	call salloc (inbuf, nincols * ninlines, TY_REAL)
	ibuf = inbuf
	xindex = oc1out - c1out + 1
	do i = 1, ninlines {
	    yindex = ol1out - l1out + i
	    call amovr (input[xindex,yindex], Memr[ibuf], nincols)
	    ibuf = ibuf + nincols
	}
	inmedian = amedr (Memr[inbuf], nincols * ninlines)

	# Cleanup.
	call sfree (sp)

	return (outmedian - inmedian)
end

