# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<printf.h>

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
