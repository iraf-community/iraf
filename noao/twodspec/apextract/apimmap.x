include	<imhdr.h>

# AP_IMMAP -- Map an input image for the APEXTRACT package.

pointer procedure ap_immap (image, apaxis, dispaxis)

char	image[ARB]	# Image to map
int	apaxis		# Aperture axis
int	dispaxis	# Dispersion axis

pointer	im, immap()
int	imgeti(), apgeti()
errchk	immap

begin
	im = immap (image, READ_ONLY, 0)
	if (IM_NDIM(im) != 2) {
	    call imunmap (im)
	    call error (0, "Image must be two dimensional")
	}
	iferr (dispaxis = imgeti (im, "dispaxis"))
	    dispaxis = apgeti ("dispaxis")
	apaxis = mod (dispaxis, 2) + 1

	return (im)
end
