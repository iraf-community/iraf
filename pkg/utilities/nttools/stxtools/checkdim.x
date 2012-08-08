include	<imhdr.h>

#* HISTORY*
#* B.Simon	04-Mar-93	original

# CHECKDIM -- Get the real dimension of an image

int procedure checkdim (im)

pointer	im		# i: image descriptor
#--
int	idim, jdim

begin
	# Ignore higher dimesions that only have length one

	jdim = 1
	do idim = 1, IM_NDIM(im) {
	    if (IM_LEN(im,idim) > 1)
		jdim = idim
	}

	return (jdim)
end
