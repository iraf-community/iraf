# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<imhdr.h>
include	<printf.h>

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
