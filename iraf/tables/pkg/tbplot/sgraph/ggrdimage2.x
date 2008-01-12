include <imhdr.h>

# GG_RDIMAGE2 -- Read an image section and compute the projection about
# one dimension, producing x and y vectors as output.

int procedure gg_rdimage2 (imsect, x, y, size, axis)

char	imsect[SZ_FNAME]	# Image section to be plotted
pointer	x, y, size		# Pointer to x, y and size vector
int	axis			# Axis about which the projection is to be taken

int	npix, i
pointer	im
pointer	immap()

#errchk	immap, im_projection, malloc
errchk	immap

begin
	im = immap (imsect, READ_ONLY, 0)

	if (axis < 1 || axis > IM_NDIM(im))
	    call error (2, "Attempt to take projection over nonexistent axis")
	npix = IM_LEN(im,axis)

	call malloc (y, npix, TY_REAL)
	call im_projection (im, Memr[y], npix, axis)

#	iferr {
	    call malloc (x, npix, TY_REAL)
	    call malloc (size, npix, TY_REAL)
#	} then
#	    call erract (EA_FATAL)

	do i = 1, npix
	    Memr[x+i-1] = i

	call imunmap (im)
	return (npix)
end
