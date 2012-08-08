include	<imhdr.h>

# EPIX -- Edit the value of a pixel in a two dimensional array.  Fetch
# subraster surrouding pixel, print on standard output.  Compute median
# value and set default value of new pixel value parameter, then prompt
# for actual value and edit image.

procedure t_epix()

char	image_name[SZ_FNAME]
int	xcoord, ycoord
int	x1, x2, y1, y2, m, n
int	npix, ncols, nlines, boxsize, half_size, sample_size
real	median_value, ksigma, mean, sigma
pointer	ahdr, a

bool	clgetb()
int	clgeti(), aravr()
real	clgetr()
pointer	immap(), imgs2r(), imps2r()

begin
	# Get image name and map image.
	call clgstr ("image_name", image_name, SZ_FNAME)
	ahdr = immap (image_name, READ_WRITE, 0)

	ncols  = IM_LEN(ahdr,1)
	nlines = IM_LEN(ahdr,2)

	# Get pixel coordinates, size of subraster.

	xcoord	= clgeti ("xcoord")
	ycoord	= clgeti ("ycoord")
	boxsize	= clgeti ("boxsize")
	ksigma	= clgetr ("ksigma")

	# Fetch subraster surrounding pixel.

	half_size = max (1, boxsize / 2)
	x1 = max (1,      xcoord - half_size)
	x2 = min (ncols,  xcoord + half_size)
	y1 = max (1,      ycoord - half_size)
	y2 = min (nlines, ycoord + half_size)

	a = imgs2r (ahdr, x1, x2, y1, y2)

	# Print subraster on standard output.

	if (clgetb ("verbose")) {
	    m = x2 - x1 + 1
	    n = y2 - y1 + 1
	    call print_subraster (Memr[a], m, n, x1, x2, y1, y2)

	    # Compute and print the median pixel value, and the mean value
	    # excluding the central pixel.

	    npix = m * n
	    call asrtr (Memr[a], Memr[a], npix)
	    median_value = Memr[a + (npix+1)/2 - 1]
	    sample_size = aravr (Memr[a], npix, mean, sigma, ksigma)

	    call printf ("median %g, mean %g, sigma %g, sample %d pixels\n")
		call pargr (median_value)
		call pargr (mean)
		call pargr (sigma)
		call pargi (sample_size)
	}

	if (clgetb ("edit_image")) {
	    # Edit the image.
	    a = imps2r (ahdr, xcoord, xcoord, ycoord, ycoord)
	    Memr[a] = clgetr ("new_value")
	}

	call imunmap (ahdr)
end


# PRINT_SUBRASTER -- Print the values of the pixels in a subraster on the
# standard output.

procedure print_subraster (a, m, n, x1, x2, y1, y2)

real	a[m,n]
int	m, n
int	x1, x2, y1, y2
int	column, line

begin
	# Print column labels.

	call printf ("%7w")
	do column = x1, x2 {
	    call printf ("%8d ")
		call pargi (column)
	}
	call printf ("\n")

	# Print line labels and pixel values.

	do line = y1, y2 {
	    call printf ("%8d ")
		call pargi (line)
	    do column = x1, x2 {
		call printf ("%8.6g ")
		    call pargr (a[column-x1+1, line-y1+1])
	    }
	    call printf ("\n")
	}
end
