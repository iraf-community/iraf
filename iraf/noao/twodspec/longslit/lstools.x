include	<imhdr.h>

# LS_AIMSUM -- Get a one dimensional image vector summed over lines
# or columns.

procedure ls_aimsum (im, axis, col1, col2, line1, line2, x, y, npts)

pointer	im			# IMIO pointer
int	axis			# Axis of vector
int	col1, col2		# Range of columns
int	line1, line2		# Range of lines
pointer	x			# Vector ordinates
pointer	y			# Vector abscissa
int	npts			# Number of points in vector

int	i, line, ncols, nlines

real	asumr()
pointer	imgs2r()

begin
	ncols = col2 - col1 + 1
	nlines = line2 - line1 + 1

	switch (axis) {
	case 1:
	    npts = ncols
	    call malloc (x, ncols, TY_REAL)
	    call calloc (y, ncols, TY_REAL)

	    do i = 1, ncols
	        Memr[x+i-1] = col1 + i - 1

	    do i = 1, nlines {
		line = line1 + i - 1
	        call aaddr (Memr[imgs2r (im, col1, col2, line, line)], Memr[y],
		    Memr[y], ncols)
	    }
	case 2:
	    npts = nlines
	    call malloc (x, nlines, TY_REAL)
	    call malloc (y, nlines, TY_REAL)

	    do i = 1, nlines {
		line = line1 + i - 1
	        Memr[x+i-1] = line
	        Memr[y+i-1] = asumr (Memr[imgs2r (im, col1, col2, line, line)],
		    ncols)
	    }
	}
end


# LS_AIMAVG -- Get a one dimensional image vector averaged over lines
# or columns.

procedure ls_aimavg (im, axis, col1, col2, line1, line2, x, y, npts)

pointer	im			# IMIO pointer
int	axis			# Axis of vector
int	col1, col2		# Range of columns
int	line1, line2		# Range of lines
pointer	x			# Vector ordinates
pointer	y			# Vector abscissa
int	npts			# Number of points in vector

begin
	call ls_aimsum (im, axis, col1, col2, line1, line2, x, y, npts)

	switch (axis) {
	case 1:
	    call adivkr (Memr[y], real (line2-line1+1), Memr[y], npts)
	case 2:
	    call adivkr (Memr[y], real (col2-col1+1), Memr[y], npts)
	}
end


# LS_IMMAP -- Map images for response and illumination calibrations

procedure ls_immap (input, output, in, out)

char	input[ARB]		# Input image
char	output[ARB]		# Output image
pointer	in			# Input IMIO pointer
pointer	out			# Output IMIO pointer

pointer	sp, root, sect, line, data

int	impnlr()
pointer	immap()

begin
	# Get the root name and section of the input image.

	call smark (sp)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (sect, SZ_FNAME, TY_CHAR)

	call get_root (input, Memc[root], SZ_FNAME)
	call get_section (input, Memc[sect], SZ_FNAME)

	# If the output image is not accessible then create it as a new copy
	# of the full input image and initialize the output to unit response.

	iferr (out = immap (output, READ_WRITE, 0)) {
	    in = immap (Memc[root], READ_ONLY, 0)
	    out = immap (output, NEW_COPY, in)
	    IM_PIXTYPE(out) = TY_REAL

	    call salloc (line, IM_MAXDIM, TY_LONG)
	    call amovkl (long (1), Meml[line], IM_MAXDIM)

	    while (impnlr (out, data, Meml[line]) != EOF)
	        call amovkr (1., Memr[data], IM_LEN(out, 1))

	    call imunmap (in)
	}
	call imunmap (out)

	# Map the input and output images.

	in = immap (input, READ_ONLY, 0)

	call sprintf (Memc[root], SZ_FNAME, "%s%s")
	    call pargstr (output)
	    call pargstr (Memc[sect])
	out = immap (Memc[root], READ_WRITE, 0)

	call sfree (sp)
end
