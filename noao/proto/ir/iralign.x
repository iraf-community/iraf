include <imhdr.h>
include <pkg/dttext.h>

# IR_SUBALIGN -- Align all the subrasters.

procedure ir_subalign (im, outim, section, dt, xrshifts, yrshifts, xcshifts,
    ycshifts, nxsub, nysub, nxrsub, nyrsub, ncols, nrows, nxoverlap,
    nyoverlap, order, oval, interp, verbose)

pointer	im			# pointer to the input image
pointer	outim			# pointer to the output image
char	section[ARB]		# output reference section
pointer	dt			# pointer to the database file
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
real	oval			# undefined pixel region
int	interp			# type of interpolant
int	verbose			# print messages

int	i, nxsize, nysize, nimcols, nimlines, xsubindex, ysubindex
int	c1ref, c2ref, l1ref, l2ref, c1in, c2in, l1in, l2in, c1out, c2out
int	l1out, l2out, ideltax, ideltay, norows, nocols
pointer	sp, input, lbuf, inbuf, outbuf, obuf, x, y, msi
real	deltax, deltay, ytemp
int	ir_decode_section(), fscan(), nscan()
pointer	impl2r(), imps2r(), imgs2r()

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

	# Check the reference subraster is the correct size.
	if ((c2ref - c1ref + 1) != ncols || (l2ref - l1ref + 1) != nrows)
	    call error (0, "Reference subsection is the wrong size.")

	# Fill in the output image with the undefined value.
	nimcols = IM_LEN(outim,1)
	nimlines = IM_LEN(outim,2)
	do i = 1, nimlines {
	    lbuf = impl2r (outim, i)
	    if (lbuf == NULL)
		call error (0, "Error writing output image")
	    call amovkr (oval, Memr[lbuf], nimcols)
	}

	# Initialize the interpolant.
	call msiinit (msi, interp)

	# Extract the subrasters one by one.
	while (fscan (DT(dt)) != EOF) {

	    # Get the input image section.
	    call gargwrd (Memc[input], SZ_FNAME)
	    call gargwrd (Memc[input], SZ_FNAME)
	    if (nscan () != 2)
		next

	    # Decode the input image section.
	    if (ir_decode_section (Memc[input], c1in, c2in, l1in, l2in) == ERR)
		call error (0, "Error decoding input image section.")
	    if ((c2in - c1in + 1) != ncols || (l2in - l1in + 1) != nrows)
		call error (0, "Input subraster is the wrong size")

	    # Compute the shift relative to the input subraster.
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

	    # Get the input buffer.
	    inbuf = imgs2r (im, c1in, c2in, l1in, l2in)
	    if (inbuf == NULL)
		call error (0, "Error reading input image.")
	    call msifit (msi, Memr[inbuf], ncols, nrows, ncols)

	    # Compute the x interpolant coordinates.
	    do i = 1, nocols
		Memr[x+i-1] = max (1.0, min (real (nocols), real (i + deltax -
		    ideltax)))

	    # Write the output image.
	    obuf = outbuf
	    do i = 1, norows {
		ytemp = max (1.0, min (real (norows), real (i + deltay -
		    ideltay)))
		call amovkr (ytemp, Memr[y], nocols)
		call msivector (msi, Memr[x], Memr[y], Memr[obuf], nocols)
		obuf = obuf + nocols
	    }

	    # Print a message.
	    if (verbose == YES) {
		call printf ("imcopy %s[%d:%d,%d:%d]  %s[%d:%d,%d:%d]\n")
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
	    }
	}

	call msifree (msi)
	call sfree (sp)
end
