include <imhdr.h>
include "iralign.h"

# IR_M2SUBALIGN -- Align all the subrasters.

procedure ir_m2subalign (im, outim, section, xrshifts, yrshifts, xcshifts,
	ycshifts, ishifts, nxsub, nysub, nxrsub, nyrsub, ncols, nrows,
	nxoverlap, nyoverlap, corner, raster, order, interp, verbose)

pointer	im			# pointer to the input image
pointer	outim			# pointer to the output image
char	section[ARB]		# output reference section
real	xrshifts[nxsub,ARB]	# x row shifts
real	yrshifts[nxsub,ARB]	# y row shifts
real	xcshifts[nxsub,ARB]	# x column shifts
real	ycshifts[nxsub,ARB]	# y column shifts
real	ishifts[nxsub,ARB]	# intensity shifts
int	nxsub			# number of subrasters in x direction
int	nysub			# number of subrasters in y direction
int	nxrsub			# index of x reference subraster
int	nyrsub			# index of y reference subraster
int	ncols			# number of columns in subraster
int	nrows			# number of rows in subraster
int	nxoverlap		# number of columns of overlap
int	nyoverlap		# number of rows of overlap
int	corner			# starting corner
int	raster			# raster scan
int	order			# row or column order
int	interp			# type of interpolant
int	verbose			# print messages

int	i, nxsize, nysize, nimcols, nimlines, xsubindex, ysubindex
int	c1ref, c2ref, l1ref, l2ref, c1in, c2in, l1in, l2in, c1out, c2out
int	l1out, l2out, ideltax, ideltay, norows, nocols, nrasters, num
pointer	sp, input, inbuf, outbuf, obuf, x, y, msi
real	deltax, deltay, ytemp

int	ir_decode_section()
pointer	imps2r(), imgs2r()

begin
	# Definitions.
	nimcols = IM_LEN(outim,1)
	nimlines = IM_LEN(outim,2)
	nxsize = ncols - nxoverlap
	nysize = nrows - nyoverlap

	# Allocate temporary space.
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (x, ncols, TY_REAL)
	call salloc (y, ncols, TY_REAL)

	# Find the position in the output image of the reference subraster.
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

	# Initialize the interpolant.
	call msiinit (msi, interp)

	# Extract the subrasters one by one.
	nrasters = nxsub * nysub
	do num = 1, nrasters {

	    # Get the correct index.
	    call ir_indices (num, xsubindex, ysubindex, nxsub, nysub, 
		corner, raster, order)

	    # Compute the shift relative to the reference subraster.
	    call ir_mkshift (xrshifts, yrshifts, xcshifts, ycshifts, nxsub,
	        nysub, xsubindex, ysubindex, nxrsub, nyrsub, order, deltax,
		deltay)
	    ideltax = nint (deltax)
	    ideltay = nint (deltay)

	    # Get the output buffer.
	    c1out = max (1, min (nimcols, c1ref + (xsubindex - nxrsub) *
		ncols + ideltax))
	    c2out = min (nimcols, max (1, c2ref + (xsubindex - nxrsub) *
		ncols + ideltax))
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
	    c1in = (xsubindex - 1) * nxsize + 1
	    c2in = c1in + ncols - 1
	    l1in = (ysubindex - 1) * nysize + 1
	    l2in = l1in + nrows - 1
	    inbuf = imgs2r (im, c1in, c2in, l1in, l2in)
	    if (inbuf == NULL)
		call error (0, "Error reading input image.")
	    call msifit (msi, Memr[inbuf], ncols, nrows, ncols)

	    # Match intensities.

	    # Set up the column limits.
	    do i = 1, nocols
		Memr[x+i-1] = max (1.0, min (real (nocols), real (i +
		    deltax - ideltax)))

	    # Write the output image.
	    obuf = outbuf
	    do i = 1, norows {
		ytemp = max (1.0, min (real (norows), real (i + deltay -
		    ideltay)))
		call amovkr (ytemp, Memr[y], nocols)
		call msivector (msi, Memr[x], Memr[y], Memr[obuf], nocols)
		if (! IS_INDEFR(ishifts[xsubindex,ysubindex]))
		    call aaddkr (Memr[obuf], ishifts[xsubindex,ysubindex],
		        Memr[obuf], nocols)
		obuf = obuf + nocols
	    }

	    # Print a message.
	    if (verbose == YES) {
		call printf ("imcopy %s[%d:%d,%d:%d]  %s[%d:%d,%d:%d]  %g\n")
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
		    call pargr (ishifts[xsubindex,ysubindex])
	    }

	}

	call msifree (msi)
	call sfree (sp)
end

# IR_MATCH -- Compute the intensity matching parameters.

procedure ir_match (im, ranges, xrshifts, yrshifts, xcshifts, ycshifts,
	ishifts, nxsub, nysub, nxrsub, nyrsub, ncols, nrows, nxoverlap,
	nyoverlap, corner, raster, order)

pointer	im			# pointer to the input image
int	ranges[ARB]		# ranges of data to align
real	xrshifts[nxsub,ARB]	# x row shifts
real	yrshifts[nxsub,ARB]	# y row shifts
real	xcshifts[nxsub,ARB]	# x column shifts
real	ycshifts[nxsub,ARB]	# y column shifts
real	ishifts[nxsub,ARB]	# intensity shifts
int	nxsub			# number of subrasters in x direction
int	nysub			# number of subrasters in y direction
int	nxrsub			# index of x reference subraster
int	nyrsub			# index of y reference subraster
int	ncols			# number of columns in subraster
int	nrows			# number of rows in subraster
int	nxoverlap		# number of columns of overlap
int	nyoverlap		# number of rows of overlap
int	corner			# initial corner
int	raster			# raster scan
int	order			# row or column order

begin
	# Initialize the intensity subraster.
	call ir_matinit (ishifts, nxsub, nysub, ranges, corner, raster, order)
	if (ranges[1] == NULL)
	    return

	# Match the intensities in the direction of observation.
	call ir_omatch (im, xrshifts, yrshifts, xcshifts, ycshifts, 
	    ishifts, nxsub, nysub, nxrsub, nyrsub, ncols, nrows,
	    nxoverlap, nyoverlap, corner, raster, order)

	# Match the instensities in the other direction.
	call ir_nmatch (im, xrshifts, yrshifts, xcshifts, ycshifts,
	    ishifts, nxsub, nysub, nxrsub, nyrsub, ncols, nrows,
	    nxoverlap, nyoverlap, corner, raster, order)
end


# IR_OMATCH -- Procedure to match images in the direction of observation
# direction. 

procedure ir_omatch (im, xrshifts, yrshifts, xcshifts, ycshifts, 
	ishifts, nxsub, nysub, nxrsub, nyrsub, ncols, nrows,
	nxoverlap, nyoverlap, corner, raster, order)

pointer	im			# pointer to the input image
real	xrshifts[nxsub,ARB]	# x row shifts
real	yrshifts[nxsub,ARB]	# y row shifts
real	xcshifts[nxsub,ARB]	# x column shifts
real	ycshifts[nxsub,ARB]	# y column shifts
real	ishifts[nxsub,ARB]	# intensity shifts
int	nxsub			# number of subrasters in x direction
int	nysub			# number of subrasters in y direction
int	nxrsub			# index of x reference subraster
int	nyrsub			# index of y reference subraster
int	ncols			# number of columns in subraster
int	nrows			# number of rows in subraster
int	nxoverlap		# number of columns of overlap
int	nyoverlap		# number of rows of overlap
int	corner			# initial corner
int	raster			# raster scan
int	order			# row or column order

int	i, j, k, num, ntotal, nrasters, ntimes, nxsize, nysize
int	pc1, pc2, pl1, pl2, c1, c2, l1, l2, pdeltax, pdeltay, deltax, deltay
int	oc1, oc2, ol1, ol2, clim1, clim2, llim1, llim2
pointer	buf
real	xshift, yshift, pmedian, median, dif

int	ir_m2overlap()
pointer	imgs2r()
real	amedr()

begin
	# Compute the do loop parameters.
	ntotal = nxsub * nysub
	if (order == IR_ROW) {
	    nrasters = nxsub
	    ntimes = nysub
	} else {
	    nrasters = nysub
	    ntimes = nxsub
	}

	nxsize = ncols - nxoverlap
	nysize = nrows - nyoverlap

	# Loop over the subrasters to be matched.
	num = 1
	do i = 1, ntotal {

	    if (mod (num, nrasters) == 1) {

		# Get the position and shift for the first subraster at the
		# beginning of the column.
		call ir_indices (num, j, k, nxsub, nysub, corner, raster, order)
		call ir_mkshift (xrshifts, yrshifts, xcshifts, ycshifts, nxsub,
		    nysub, j, k, nxrsub, nyrsub, order, xshift, yshift)
		pdeltax = nint (xshift)
		pdeltay = nint (yshift)
		pc1 = (j - 1) * nxsize + 1
		pc2 = pc1 + ncols - 1
		pl1 = (k - 1) * nysize + 1
		pl2 = pl1 + nrows - 1
		num = num + 1
		dif = 0.0

		# Get the the position and shift for the subraster to be
		#  adjusted.
		call ir_indices (num, j, k, nxsub, nysub, corner, raster, order)
		call ir_mkshift (xrshifts, yrshifts, xcshifts, ycshifts, nxsub,
		    nysub, j, k, nxrsub, nyrsub, order, xshift, yshift)
		deltax = nint (xshift)
		deltay = nint (yshift)
		c1 = (j - 1) * nxsize + 1
		c2 = c1 + ncols - 1
		l1 = (k - 1) * nysize + 1
		l2 = l1 + nrows - 1

	    } else {

		# Reset the coordinates of the previous subraster.
		pc1 = c1
		pc2 = c2
		pl1 = l1
		pl2 = l2
		pdeltax = deltax
		pdeltay = deltay

		# Get the positions and shifts of the subraster to be adjusted.
		call ir_indices (num, j, k, nxsub, nysub, corner, raster, order)
		call ir_mkshift (xrshifts, yrshifts, xcshifts, ycshifts, nxsub,
		    nysub, j, k, nxrsub, nyrsub, order, xshift, yshift)
		deltax = nint (xshift)
		deltay = nint (yshift)
		c1 = (j - 1) * nxsize + 1
		c2 = c1 + ncols - 1
		l1 = (k - 1) * nysize + 1
		l2 = l1 + nrows - 1

	    }

	    # Compute the overlap region.
	    if (ir_m2overlap (pc1 + pdeltax, pc2 + pdeltax, pl1 + pdeltay,
		pl2 + pdeltay, c1 + deltax, c2 + deltax, l1 + deltay,
		l2 + deltay, oc1, oc2, ol1, ol2) == YES) {

		clim1 = max (pc1, oc1 - pdeltax)
		clim2 = min (pc2, oc2 - pdeltax)
		llim1 = max (pl1, ol1 - pdeltay)
		llim2 = min (pl2, ol2 - pdeltay)
		buf = imgs2r (im, clim1, clim2, llim1, llim2)
		pmedian = amedr (Memr[buf], (clim2 - clim1 + 1) * (llim2 -
		    llim1 + 1))

		clim1 = max (c1, oc1 - deltax)
		clim2 = min (c2, oc2 - deltax)
		llim1 = max (l1, ol1 - deltay)
		llim2 = min (l2, ol2 - deltay)
		buf = imgs2r (im, clim1, clim2, llim1, llim2)
		median = amedr (Memr[buf], (clim2 - clim1 + 1) * (llim2 -
		    llim1 + 1))

		dif = dif + median - pmedian
		if (! IS_INDEFR (ishifts[j,k]))
		    ishifts[j,k] = ishifts[j,k] -  dif
	    }

	    num = num + 1
	    if (num > ntotal)
		break
	}
end


# IR_NMATCH -- Procedure to match images in the other direction. 

procedure ir_nmatch (im, xrshifts, yrshifts, xcshifts, ycshifts, 
	ishifts, nxsub, nysub, nxrsub, nyrsub, ncols, nrows,
	nxoverlap, nyoverlap, corner, raster, order)

pointer	im			# pointer to the input image
real	xrshifts[nxsub,ARB]	# x row shifts
real	yrshifts[nxsub,ARB]	# y row shifts
real	xcshifts[nxsub,ARB]	# x column shifts
real	ycshifts[nxsub,ARB]	# y column shifts
real	ishifts[nxsub,ARB]	# intensity shifts
int	nxsub			# number of subrasters in x direction
int	nysub			# number of subrasters in y direction
int	nxrsub			# index of x reference subraster
int	nyrsub			# index of y reference subraster
int	ncols			# number of columns in subraster
int	nrows			# number of rows in subraster
int	nxoverlap		# number of columns of overlap
int	nyoverlap		# number of rows of overlap
int	corner			# initial corner
int	raster			# raster scan
int	order			# row or column order

int	i, j, k, num, ntotal, nrasters, fac, nxsize, nysize, count
int	pc1, pc2, pl1, pl2, c1, c2, l1, l2, pdeltax, pdeltay, deltax, deltay
int	oc1, oc2, ol1, ol2, clim1, clim2, llim1, llim2
pointer	buf
real	xshift, yshift, pmedian, median, pdif, dif, tdif

int	ir_m2overlap()
pointer	imgs2r()
real	amedr()

begin
	# Compute the do loop parameters.
	ntotal = nxsub * nysub
	if (order == IR_ROW) {
	    nrasters = nxsub
	    fac = 2 * nrasters
	} else {
	    nrasters = nysub
	    fac = 2 * nrasters
	}

	nxsize = ncols - nxoverlap
	nysize = nrows - nyoverlap

	# Loop over the subrasters to be matched.
	num = 1
	count = 1
	do i = 1, ntotal {

	    # Get the position and shift for the first subraster at the
	    # beginning of the column.

	    if (num <= nrasters) {

		call ir_indices (num, j, k, nxsub, nysub, corner, raster, order)
		call ir_mkshift (xrshifts, yrshifts, xcshifts, ycshifts, nxsub,
		    nysub, j, k, nxrsub, nyrsub, order, xshift, yshift)
		pdeltax = nint (xshift)
		pdeltay = nint (yshift)
		pc1 = (j - 1) * nxsize + 1
		pc2 = pc1 + ncols - 1
		pl1 = (k - 1) * nysize + 1
		pl2 = pl1 + nrows - 1
		if (IS_INDEFR(ishifts[j,k]))
		    pdif = 0.0
		else
		    pdif = ishifts[j,k]
		tdif = 0.0
		if (raster == YES) {
		    num = fac - num + 1
		    fac = fac + fac
		} else
		    num = num + nrasters

		# Get the the position and shift for the subraster to be
		#  adjusted.
		call ir_indices (num, j, k, nxsub, nysub, corner, raster, order)
		call ir_mkshift (xrshifts, yrshifts, xcshifts, ycshifts, nxsub,
		    nysub, j, k, nxrsub, nyrsub, order, xshift, yshift)
		deltax = nint (xshift)
		deltay = nint (yshift)
		c1 = (j - 1) * nxsize + 1
		c2 = c1 + ncols - 1
		l1 = (k - 1) * nysize + 1
		l2 = l1 + nrows - 1
		if (IS_INDEFR(ishifts[j,k]))
		    dif = 0.0
		else
		    dif = ishifts[j,k]

	    } else {

		# Reset the coordinates of the previous subraster.
		pc1 = c1
		pc2 = c2
		pl1 = l1
		pl2 = l2
		pdeltax = deltax
		pdeltay = deltay
		pdif = dif

		# Get the positions and shifts of the subraster to be adjusted.
		call ir_indices (num, j, k, nxsub, nysub, corner, raster, order)
		call ir_mkshift (xrshifts, yrshifts, xcshifts, ycshifts, nxsub,
		    nysub, j, k, nxrsub, nyrsub, order, xshift, yshift)
		deltax = nint (xshift)
		deltay = nint (yshift)
		c1 = (j - 1) * nxsize + 1
		c2 = c1 + ncols - 1
		l1 = (k - 1) * nysize + 1
		l2 = l1 + nrows - 1
		if (IS_INDEFR(ishifts[j,k]))
		    dif = 0.0
		else
		    dif = ishifts[j,k]

	    }

	    # Compute the overlap region.
	    if (ir_m2overlap (pc1 + pdeltax, pc2 + pdeltax, pl1 + pdeltay,
		pl2 + pdeltay, c1 + deltax, c2 + deltax, l1 + deltay,
		l2 + deltay, oc1, oc2, ol1, ol2) == YES) {

		clim1 = max (pc1, oc1 - pdeltax)
		clim2 = min (pc2, oc2 - pdeltax)
		llim1 = max (pl1, ol1 - pdeltay)
		llim2 = min (pl2, ol2 - pdeltay)
		buf = imgs2r (im, clim1, clim2, llim1, llim2)
		pmedian = amedr (Memr[buf], (clim2 - clim1 + 1) * (llim2 -
		    llim1 + 1))

		clim1 = max (c1, oc1 - deltax)
		clim2 = min (c2, oc2 - deltax)
		llim1 = max (l1, ol1 - deltay)
		llim2 = min (l2, ol2 - deltay)
		buf = imgs2r (im, clim1, clim2, llim1, llim2)
		median = amedr (Memr[buf], (clim2 - clim1 + 1) * (llim2 -
		    llim1 + 1))

		tdif = tdif + median + dif - pmedian - pdif
		if (! IS_INDEFR (ishifts[j,k]))
		    ishifts[j,k] = ishifts[j,k] - tdif
	    }

	    if (raster == YES) {
		num = fac - num + 1
		fac = fac + fac
	    } else
	        num = num + nrasters
	    if (num > ntotal) {
		count = count + 1
		num = count
		fac = 2 * nrasters
	    }
	    if (count > nrasters)
		break
		
	}
end


# IR_MATINIT -- Procedure to initialize the intensity matching algorithm.
# If the ranges are undefined and no matching is to take place the
# ishifts are set to INDEFR and the routine returns. Otherwise the shifts
# are all initialized to zero and shifts for the missing subrasters are
# set to INDEFR.

procedure ir_matinit (ishifts, nxsub, nysub, ranges, corner, raster, order)

real	ishifts[nxsub, ARB]		# intensity shifts
int	nxsub				# number of subrasters in x direction
int	nysub				# number of subrasters in y direction
int	ranges[ARB]			# ranges of missing subrasters
int	corner				# initial corner
int	raster				# raster order
int	order				# row or column order

int	i, j, num, nsubrasters
int	get_next_number()

begin
	# Initialize the shifts to INDEFR.
	do j = 1, nysub {
	    do i = 1, nxsub
		ishifts[i,j] = INDEFR
	}

	if (ranges[1] == NULL)
	    return

	num = 0
	nsubrasters = nxsub * nysub
	while (get_next_number (ranges, num) != EOF) {
	    if (num  > nsubrasters)
		break
	    call ir_indices (num, i, j, nxsub, nysub, corner, raster, order)
	    ishifts[i,j] = 0.0
	}
end


# IR_INDICES -- Given the number in the list for a missing subraster and
# information about how the subrasters were written return the i and j
# indices of the specified subrasters.

procedure ir_indices (num, i, j, nxsub, nysub, corner, raster, order)

int	num		# number of the subraster
int	i,j		# indices of the subraster
int	nxsub,nysub	# number of subrasters in x and y
int	corner		# starting corner
int	raster		# raster order
int	order		# column or row order

begin
	switch (corner) {
	case IR_LL:
	    if (order == IR_ROW) {
		if (mod (num, nxsub) == 0) {
		    j = num / nxsub
		    if (raster == YES && mod (j,2) == 0)
			i = 1
		    else
		        i = nxsub
		} else {
		    j = num / nxsub + 1
		    if (raster == YES && mod (j,2) == 0)
			i = nxsub - mod (num, nxsub) + 1
		    else
		        i = mod (num, nxsub)
		}
	    } else if (order == IR_COLUMN) {
		if (mod (num, nysub) == 0) {
		    i = num / nysub
		    if (raster == YES && mod (i,2) == 0)
			j = 1
		    else
		        j = nysub
		} else {
		    i = num / nysub + 1
		    if (raster == YES && mod (i,2) == 0)
			j = nysub - mod (num, nysub) + 1
		    else
		        j = mod (num, nysub)
		}
	    }
	case IR_LR:
	    if (order == IR_ROW) {
		if (mod (num, nxsub) == 0) {
		    j = num / nxsub
		    if (raster == YES && mod (j,2) == 0)
			i = nxsub
		    else
			i = 1
		} else {
		    j = num / nxsub + 1
		    if (raster == YES && mod (j,2) == 0)
			i = mod (num, nxsub)
		    else
			i = nxsub - mod (num, nxsub) + 1
		}
	    } else if (order == IR_COLUMN) {
		if (mod (num, nysub) == 0) {
		    i = nxsub - num / nysub + 1
		    if (raster == YES && mod (i,2) != 0)
			j = 1
		    else
		        j = nysub
		} else {
		    i = nxsub - num / nysub
		    if (raster == YES && mod (i,2) != 0)
			j = nysub - mod (num, nysub) + 1
		    else
		        j = mod (num, nysub)
		}
	    }
	case IR_UL:
	    if (order == IR_ROW) {
		if (mod (num, nxsub) == 0) {
		    j = nysub - num / nxsub + 1
		    if (raster == YES && mod (j,2) != 0)
			i = 1
		    else
		        i = nxsub
		} else {
		    j = nysub - num / nxsub
		    if (raster == YES && mod (j,2) != 0)
			i = nxsub - mod (num, nxsub) + 1
		    else
		        i = mod (num, nxsub)
		}
	    } else if (order == IR_COLUMN) {
		if (mod (num, nysub) == 0) {
		    i = num / nysub
		    if (raster == YES && mod (i,2) == 0)
			j = nysub
		    else
			j = 1
		} else {
		    i = num / nysub + 1
		    if (raster == YES && mod (i,2) == 0)
			j = mod (num, nysub)
		    else
			j = nysub - mod (num, nysub) + 1
		}
	    }
	case IR_UR:
	    if (order == IR_ROW) {
		if (mod (num, nxsub) == 0) {
		    j = nysub - num / nxsub + 1
		    if (raster == YES && mod (j,2) != 0)
			i = nxsub
		    else
			i = 1
		} else {
		    j = nysub - num / nxsub
		    if (raster == YES && mod (j,2) != 0)
			i = mod (num, nxsub)
		    else
			i = nxsub - mod (num, nxsub) + 1
		}
	    } else if (order == IR_COLUMN) {
		if (mod (num, nysub) == 0) {
		    i = nxsub - num / nysub + 1
		    if (raster == YES && mod (i,2) != 0)
			j = nysub
		    else
			j = 1
		} else {
		    i = nxsub - num / nysub
		    if (raster == YES && mod (i,2) != 0)
			j = mod (num, nysub)
		    else
			j = nysub - mod (num, nysub) + 1
		}
	    }
	}
end


# IR_M2OVERLAP -- Procedure to compute the overlap between two rectangles.

int procedure ir_m2overlap (pc1out, pc2out, pl1out, pl2out, c1out, c2out,
	l1out, l2out, oc1out, oc2out, ol1out, ol2out)

int	pc1out, pc2out		# previous subraster column limits
int	pl1out, pl2out		# previous subraster line limits
int	c1out, c2out		# current subraster column limits
int	l1out, l2out		# current subraster line limits
int	oc1out, oc2out		# overlap column limits
int	ol1out, ol2out		# overlap line limits

begin
	# Check for the case where no intersection is present.
	if (c1out > pc2out || c2out < pc1out || l1out > pl2out ||
	    l2out < pl1out)
	    return (NO)

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

	return (YES)
end
