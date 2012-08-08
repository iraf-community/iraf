# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.


# Simples IMIO test routines.

task	mkimage		= t_mkimage,
	mktest		= t_mktest,
	cube		= t_cube,
	maxmin		= t_maxmin,
	gsubras		= t_gsubras,
	dump		= t_dump


include	<imhdr.h>
include	<printf.h>
include	<ctype.h>
include	<mach.h>


define	NTYPES		7

# MKIMAGE -- Make a new two dimensional image of a specified size
# and datatype.  The image pixels are all set to zero.

procedure t_mkimage()

int	dtype
real	pixval
int	ncols, nlines
char	imname[SZ_FNAME]
char	title[SZ_LINE]
short	ty_code[NTYPES]

real	clgetr()
char	clgetc(), ch
int	clgeti(), stridx()

string	types "usilrdx"			# Supported pixfile datatypes
data	ty_code /TY_USHORT, TY_SHORT, TY_INT, TY_LONG, TY_REAL,
	TY_DOUBLE, TY_COMPLEX/
begin
	call clgstr ("image", imname, SZ_FNAME)
	ncols  = clgeti ("ncols")
	nlines = clgeti ("nlines")
	ch = clgetc ("datatype")
	dtype  = ty_code[stridx(ch,types)]
	pixval = clgetr ("pixval")
	call clgstr ("title", title, SZ_LINE)

	call immake2 (imname, ncols, nlines, dtype, pixval, title)
end


# IMMAKE2 -- Make a two dimensional image of datatype [usilr] with all pixels
# set to the given value.

procedure immake2 (imname, ncols, nlines, dtype, pixval, title)

char	imname[ARB]		# name of new image
int	ncols, nlines		# image size
int	dtype			# datatype
real	pixval			# constant pixel value
char	title[ARB]		# image title

int	i
pointer	im, buf
pointer	immap(), impl2r()

begin
	im = immap (imname, NEW_IMAGE, 0)

	IM_PIXTYPE(im) = dtype
	IM_LEN(im,1)   = ncols
	IM_LEN(im,2)   = nlines
	call strcpy (title, IM_TITLE(im), SZ_IMTITLE)

	# Write out the lines.

	do i = 1, nlines {
	    buf = impl2r (im, i)
	    call amovkr (pixval, Memr[buf], ncols)
	}

	call imunmap (im)
end


# MKTEST -- Make a test image.

procedure t_mktest()

char	imname[SZ_FNAME]
int	ndim, dim[IM_MAXDIM]
int	i, j, k, scalar
long	offset
int	clgeti(), nscan(), clscan(), stridx()
pointer	buf, im, immap(), impl3l()

int	dtype
string	types "usilrdx"			# Supported pixfile datatypes
char	ty_code[7], clgetc()
data	ty_code /TY_USHORT, TY_SHORT, TY_INT, TY_LONG, TY_REAL,
	TY_DOUBLE, TY_COMPLEX, EOS/

begin
	call clgstr ("image_name", imname, SZ_FNAME)
	dtype = ty_code[stridx (clgetc ("datatype"), types)]
	ndim = clgeti ("ndim")

	call amovki (1, dim, 3)
	if (clscan ("axis_lengths") != EOF) {
	    do i = 1, ndim
		call gargi (dim[i])
	    if (nscan() < ndim)
		call error (1, "Insufficient dimensions")
	}

	im = immap (imname, NEW_IMAGE, 0)

	IM_PIXTYPE(im) = dtype
	do i = 1, ndim
	    IM_LEN(im,i) = dim[i]

	do k = 1, dim[3]
	    do j = 1, dim[2] {
		buf = impl3l (im, j, k)

		# Pixel value eq pixel coords.
		offset = 1
		if (ndim > 1) {
		    if (dim[1] < 100)
			scalar = 100
		    else
			scalar = 1000
		    offset = offset + j * scalar
		}

		if (ndim > 2)
		    offset = offset + k * (scalar ** 2)

		# Avoid integer overflow if large type short image.
		if (IM_PIXTYPE(im) == TY_SHORT)
		    offset = min (MAX_SHORT, offset - dim[1])

		# Initialize line of pixels.
		do i = 0, dim[1]-1
		    Meml[buf+i] = offset + i
	    }

	call imunmap (im)
end


# CUBE -- Get a subraster from an image, and print out the pixel values
# on the standard output.

define	MAXDIM		3

procedure t_cube()

char	imname[SZ_FNAME], fmt
int	i, nx, ny, nz, ndim
int	vs[IM_MAXDIM], ve[IM_MAXDIM]
pointer	im, ras, imgs3r(), immap()
int	clscan(), nscan()
char	clgetc()

begin
	call clgstr ("image_name", imname, SZ_FNAME)
	fmt = clgetc ("numeric_format")

	im = immap (imname, READ_ONLY, 0)

	# Get the coordinates of the subraster to be extracted.  Determine
	# dimensionality of subraster.

	if (clscan ("subraster_coordinates") != EOF) {
	    for (ndim=1;  ndim <= MAXDIM;  ndim=ndim+1) {
		switch (fmt) {
		case FMT_DECIMAL:
		    call gargi (vs[ndim])
		    call gargi (ve[ndim])
		case FMT_OCTAL:
		    call gargrad (vs[ndim], 8)
		    call gargrad (ve[ndim], 8)
		case FMT_HEX:
		    call gargrad (vs[ndim], 16)
		    call gargrad (ve[ndim], 16)
		}

		if (nscan() < ndim * 2) {
		    ndim = nscan() / 2
		    break
		}
	    }
	}

	if (ndim == 0)
	    return

	for (i=ndim+1;  i <= MAXDIM;  i=i+1) {
	    vs[i] = 1
	    ve[i] = 1
	}

	# Extract subraster from image.  Print table on the standard
	# output.

	ras = imgs3r (im, vs[1], ve[1], vs[2], ve[2], vs[3], ve[3])
	call imbln3 (im, nx, ny, nz)

	call print_cube (STDOUT, Memr[ras], nx, ny, nz, vs, ve, fmt)
	call imunmap (im)
end


# PRINT_CUBE -- Print a cube of pixels of type REAL on a file.

procedure print_cube (fd, cube, nx, ny, nz, vs, ve, fmt)

char	fmt
int	fd, nx, ny, nz
real	cube[nx,ny,nz]
int	vs[MAXDIM], ve[MAXDIM], vinc[MAXDIM]
int	i, j, k
errchk	fprintf, pargi, pargr

begin
	do i = 1, MAXDIM				# loop increments
	    if (vs[i] <= ve[i])
		vinc[i] = 1
	    else
		vinc[i] = -1

	# Print table of pixel values on the standard output.  Label bands,
	# lines, and columns.

	do k = 1, nz {
	    call fprintf (fd, "Band %0.0*:\n")
		call pargc (fmt)
		call pargi (vs[MAXDIM] + (k-1) * vinc[MAXDIM])

	    call fprintf (fd, "%9w")
	    do i = 1, nx {				# label columns
		call fprintf (fd, "%9*   ")
		    call pargc (fmt)
		    call pargi (vs[1] + (i-1) * vinc[1])
	    }
	    call fprintf (fd, "\n")

	    do j = 1, ny {
		call fprintf (fd, "%5*  ")
		    call pargc (fmt)
		    call pargi (vs[2] + (j-1) * vinc[2])
		do i = 1, nx {				# print pixels
		    call fprintf (fd, "%12*")
			call pargc (fmt)
			call pargr (cube[i,j,k])
		}
		call fprintf (fd, "\n")
	    }
	    call fprintf (fd, "\n")
	}
end


# MAXMIN -- Compute the minimum and maximum pixel values of an image.
# Works for images of any dimensionality, size, or datatype.

procedure t_maxmin()

char	imname[SZ_FNAME]
real	minval, maxval
long	v[IM_MAXDIM], clktime()
pointer	im, buf, immap(), imgnlr()

begin
	call clgstr ("imname", imname, SZ_FNAME)
	call amovkl (long(1), v, IM_MAXDIM)		# start vector

	im = immap (imname, READ_WRITE, 0)

	# Only calculate minimum, maximum pixel values if the current
	# values are unknown, or if the image was modified since the
	# old values were computed.

	if (IM_LIMTIME(im) < IM_MTIME(im)) {
	    IM_MIN(im) = MAX_REAL
	    IM_MAX(im) = -MAX_REAL

	    while (imgnlr (im, buf, v) != EOF) {
		call alimr (Memr[buf], IM_LEN(im,1), minval, maxval)
		IM_MIN(im) = min (IM_MIN(im), minval)
		IM_MAX(im) = max (IM_MAX(im), maxval)
	    }

	    IM_LIMTIME(im) = clktime (long(0))
	}

	call clputr ("minval", IM_MIN(im))
	call clputr ("maxval", IM_MAX(im))

	call imunmap (im)
end


define	MAXDIM		3

# GSUBRAS -- Get a type short subraster from an image, and print out the
# minimum and maximum pixel values on the standard output.

procedure t_gsubras()

char	imname[SZ_FNAME], fmt
int	i, nx, ny, nz, ndim
int	vs[IM_MAXDIM], ve[IM_MAXDIM]
short	minval, maxval
pointer	im, ras
pointer	imgs1s(), imgs2s(), imgs3s(), immap()
int	clscan(), nscan()
char	clgetc()

begin
	call clgstr ("image_name", imname, SZ_FNAME)
	fmt = clgetc ("numeric_format")

	im = immap (imname, READ_ONLY, 0)

	# Get the coordinates of the subraster to be extracted.  Determine
	# dimensionality of subraster.

	if (clscan ("subraster_coordinates") != EOF) {
	    for (ndim=1;  ndim <= MAXDIM;  ndim=ndim+1) {
		switch (fmt) {
		case FMT_DECIMAL:
		    call gargi (vs[ndim])
		    call gargi (ve[ndim])
		case FMT_OCTAL:
		    call gargrad (vs[ndim], 8)
		    call gargrad (ve[ndim], 8)
		case FMT_HEX:
		    call gargrad (vs[ndim], 16)
		    call gargrad (ve[ndim], 16)
		}

		if (nscan() < ndim * 2) {
		    ndim = nscan() / 2
		    break
		}
	    }
	    ndim = min (MAXDIM, ndim)
	}

	if (ndim == 0)
	    return

	for (i=ndim+1;  i <= MAXDIM;  i=i+1) {
	    vs[i] = 1
	    ve[i] = 1
	}

	# Extract subraster from image.  Print table on the standard
	# output.

	switch (ndim) {
	case 1:
	    ras = imgs1s (im, vs[1], ve[1])
	    call imbln1 (im, nx)
	    ny = 1
	    nz = 1
	case 2:
	    ras = imgs2s (im, vs[1], ve[1], vs[2], ve[2])
	    call imbln2 (im, nx, ny)
	    nz = 1
	case 3:
	    ras = imgs3s (im, vs[1], ve[1], vs[2], ve[2], vs[3], ve[3])
	    call imbln3 (im, nx, ny, nz)
	}

	minval = MAX_SHORT
	maxval = -MAX_SHORT
	call alims (Mems[ras], nx * ny * nz, minval, maxval)

	call printf ("min = %0.0*, max = %0.0*\n")
	    call pargc (fmt)
	    call pargs (minval)
	    call pargc (fmt)
	    call pargs (maxval)

	call imunmap (im)
end


# DUMP -- Dump the user area of an image header for diagnostic purposes.
# Blanks are rendered into underscores to make them visible.  This is a
# throwaway task.

procedure t_dump()

char	image[SZ_FNAME]
int	i
pointer	ip, im
pointer	immap()

begin
	call clgstr ("image", image, SZ_FNAME)
	im = immap (image, READ_ONLY, 0)

	# Print ruler.
	do i = 1, 80
	    if (mod(i,10) == 0)
		call putci (STDOUT, TO_DIGIT(i/10))
	    else
		call putci (STDOUT, ' ')
	call putci (STDOUT, '\n')

	do i = 1, 80
	    call putci (STDOUT, TO_DIGIT(mod(i,10)))
	call putci (STDOUT, '\n')

	# Map blanks into underscores.
	for (ip = IM_USERAREA(im);  Memc[ip] != EOS;  ip=ip+1)
	    if (Memc[ip] == ' ')
		Memc[ip] = '_'

	# Dump user area.
	call putline (STDOUT, Memc[IM_USERAREA(im)])
	call imunmap (im)
end

