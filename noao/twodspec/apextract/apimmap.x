include	<imhdr.h>

# AP_IMMAP -- Map an input image for the APEXTRACT package.

pointer procedure ap_immap (image)

char	image[ARB]	# Image to map

pointer	im, immap()
errchk	immap

begin
	im = immap (image, READ_ONLY, 0)
	if (IM_NDIM(im) != 2) {
	    call imunmap (im)
	    call error (0, "Image must be two dimensional")
	}

	return (im)
end
