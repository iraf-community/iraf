include <imset.h>

# AP_IMSET -- Setup image boundary extension charactersitics.

procedure ap_imset (im, boundary, npix, constant)

pointer	im		# pointer to the image
int	boundary	# type of boundary condition
int	npix		# number of pixels of boundary entension
real	constant	# constant for constant boundary extension

begin
	call imseti (im, IM_TYBNDRY, boundary)
	call imseti (im, IM_NBNDRYPIX, npix)
	if (boundary == BT_CONSTANT)
	    call imsetr (im, IM_BNDRYPIXVAL, constant)
end
