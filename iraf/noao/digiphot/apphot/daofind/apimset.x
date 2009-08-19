include <imset.h>

# AP_IMSET -- Setup image boundary extension charactersitics.

procedure ap_imset (im, boundary, npix, constant)

pointer	im		# pointer to the image
int	boundary	# type of boundary condition
size_t	npix		# number of pixels of boundary entension
real	constant	# constant for constant boundary extension

long	l_val

begin
	call imseti (im, IM_TYBNDRY, boundary)
	l_val = npix
	call imsetl (im, IM_NBNDRYPIX, l_val)
	if (boundary == BT_CONSTANT)
	    call imsetr (im, IM_BNDRYPIXVAL, constant)
end
