include	<imhdr.h>

# T_IMREP -- Replace pixels in a window with a constant.

procedure t_imrep ()

char	imtlist[SZ_LINE]		# Images to be editted
real	lower				# Lower limit of window
real	upper				# Upper limit of window
real	value				# Replacement value
real	radius				# Radius
real	img				# Imaginary part for complex

int	list
char	image[SZ_FNAME]
pointer	im

int	imtopen(), imtgetim()
real	clgetr()
pointer	immap()

begin
	# Get image template list.

	call clgstr ("images", imtlist, SZ_LINE)
	list = imtopen (imtlist)

	# Get the parameters.

	value = clgetr ("value")
	img = clgetr ("imaginary")
	lower = clgetr ("lower")
	upper = clgetr ("upper")
	radius = max (0., clgetr ("radius"))

	# Replace the pixels in each image.  Optimize IMIO.

	while (imtgetim (list, image, SZ_FNAME) != EOF) {

	    im = immap (image, READ_WRITE, 0)

	    if (radius < 1.) {
		switch (IM_PIXTYPE (im)) {
		case TY_SHORT:
		    call imreps (im, lower, upper, value, img)
		case TY_INT:
		    call imrepi (im, lower, upper, value, img)
		case TY_USHORT, TY_LONG:
		    call imrepl (im, lower, upper, value, img)
		case TY_REAL:
		    call imrepr (im, lower, upper, value, img)
		case TY_DOUBLE:
		    call imrepd (im, lower, upper, value, img)
		case TY_COMPLEX:
		    call imrepx (im, lower, upper, value, img)
		default:
		    call error (0, "Unsupported image pixel datatype")
		}

	    } else {
		switch (IM_PIXTYPE (im)) {
		case TY_SHORT:
		    call imrreps (im, lower, upper, radius, value, img)
		case TY_INT:
		    call imrrepi (im, lower, upper, radius, value, img)
		case TY_USHORT, TY_LONG:
		    call imrrepl (im, lower, upper, radius, value, img)
		case TY_REAL:
		    call imrrepr (im, lower, upper, radius, value, img)
		case TY_DOUBLE:
		    call imrrepd (im, lower, upper, radius, value, img)
		case TY_COMPLEX:
		    call imrrepx (im, lower, upper, radius, value, img)
		default:
		    call error (0, "Unsupported image pixel datatype")
		}
	    }

	    call imunmap (im)
	}

	call imtclose (list)
end
