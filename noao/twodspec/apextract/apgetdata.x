include	<imhdr.h>

# AP_GETDATA -- Get the summed dispersion line.
# Return the IMIO pointer, pointer to image data, the aperture axis and title.
# The pointers must be freed by the calling program.  Note that the value of
# line may be changed.

procedure ap_getdata (image, line, nsum, im, imdata, npts, apaxis, title)

char	image[SZ_FNAME]		# Image name
int	line			# Dispersion line to graph
int	nsum			# Number of dispersion lines to sum
pointer	im			# IMIO pointer
pointer	imdata			# Pointer to image data
int	npts			# Number of pixels 
int	apaxis			# Aperture axis
pointer	title			# Title for image data

int	i, j, k, dispaxis
pointer	buf

int	imgeti()
real	asumr()
pointer	ap_immap(), imgs2r()

errchk	ap_immap, imgeti

begin
	# Map the image
	im = ap_immap (image)
	call strcpy (image, IM_HDRFILE(im), SZ_IMHDRFILE)

	# Determine the dispersion and aperture axes.
	dispaxis = imgeti (im, "dispaxis")
	apaxis = mod (dispaxis, 2) + 1
	if (IS_INDEFI (line))
	    line = IM_LEN(im, dispaxis) / 2
	else
	    line = max (1, min (IM_LEN(im, dispaxis), line))

	# Allocate memory for the image line and title.
	npts = IM_LEN(im, apaxis)
	call calloc (imdata, npts, TY_REAL)
	call malloc (title, SZ_LINE, TY_CHAR)

	# Sum the specified number of dispersion lines.
	switch (apaxis) {
	case 1:
	    i = max (1, line - nsum / 2)
	    j = min (IM_LEN(im, dispaxis), i + nsum - 1)
	    i = max (1, j - nsum + 1)
	    buf = imgs2r (im, 1, npts, i, j)
	    j = j - i + 1
	    do k = 1, j
		call aaddr (Memr[buf+(k-1)*npts], Memr[imdata], Memr[imdata],
		    npts)

	    call sprintf (Memc[title], SZ_LINE, "Image=%s, Sum of lines %d-%d")
	        call pargstr (image)
	        call pargi (i)
	        call pargi (i+j-1)

	case 2:
	    i = max (1, line - nsum / 2)
	    j = min (IM_LEN(im, dispaxis), i + nsum - 1)
	    i = max (1, j - nsum + 1)
	    buf = imgs2r (im, i, j, 1, npts)
	    j = j - i + 1
	    do k = 1, npts
		Memr[imdata+k-1] = asumr (Memr[buf+(k-1)*j], j)

	    call sprintf (Memc[title], SZ_LINE,
		"Image=%s, Sum of columns %d-%d")
	        call pargstr (IM_HDRFILE (im))
	        call pargi (i)
	        call pargi (i+j-1)
	}
end
