include	<imhdr.h>

# AP_IMMAP -- Map an input image for the APEXTRACT package.

pointer procedure ap_immap (image, apaxis, dispaxis)

char	image[ARB]	# Image to map
int	apaxis		# Aperture axis
int	dispaxis	# Dispersion axis

pointer	im, immap()
int	i, imgeti(), clgeti()
errchk	immap

data	i/0/

begin
	im = immap (image, READ_ONLY, 0)
	if (IM_NDIM(im) == 1) {
	    call imunmap (im)
	    call error (0, "Image must be two dimensional")
	} else if (IM_NDIM(im) > 2) {
	    if (i == 0)
	    call eprintf (
    "Warning: Image(s) are not two dimensional (ignoring higher dimensions)\n")
	    i = i + 1
	}
	iferr (dispaxis = imgeti (im, "dispaxis"))
	    dispaxis = clgeti ("dispaxis")
	apaxis = mod (dispaxis, 2) + 1

	return (im)
end
