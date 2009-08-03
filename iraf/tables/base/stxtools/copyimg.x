include	<imhdr.h>

# COPYIMG -- Copy one image to another
#
# B.Simon	02-Mar-92	Original
# B.Simon	16-Mar-94	Delete fast copy, check for existing image

procedure copyimg (old, new)

char	old[ARB]	# i: old image
char	new[ARB]	# i: new image
#--
size_t	npix
long	junk
pointer	buf1, buf2, im1, im2
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
long	l_val
size_t	sz_val

int	imaccess()
long	imgnls(), imgnli(), imgnll(), imgnlr(), imgnld(), imgnlx()
long	impnls(), impnli(), impnll(), impnlr(), impnld(), impnlx()
pointer	immap()

include	<nullptr.inc>

errchk	immap, imunmap

begin
	# Map the input and output images
	# Code adapted from iraf's imcopy task

	im1 = immap (old, READ_ONLY, NULLPTR)
	if (imaccess (new, READ_WRITE) == NO) {
	    im2 = immap (new, NEW_COPY, im1)
	} else {
	    im2 = immap (new, READ_WRITE, NULLPTR)
	}

	# Setup start vector for sequential reads and writes

	l_val = 1
	sz_val = IM_MAXDIM
	call amovkl (l_val, v1, sz_val)
	call amovkl (l_val, v2, sz_val)

	# Copy the image.

	npix = IM_LEN(im1, 1)
	switch (IM_PIXTYPE(im1)) {
	case TY_SHORT:
	    while (imgnls (im1, buf1, v1) != EOF) {
		junk = impnls (im2, buf2, v2)
		call amovs (Mems[buf1], Mems[buf2], npix)
	    }
	case TY_USHORT, TY_INT:
	    while (imgnli (im1, buf1, v1) != EOF) {
		junk = impnli (im2, buf2, v2)
		call amovi (Memi[buf1], Memi[buf2], npix)
	    }
	case TY_LONG:
	    while (imgnll (im1, buf1, v1) != EOF) {
		junk = impnll (im2, buf2, v2)
		call amovl (Meml[buf1], Meml[buf2], npix)
	    }
	case TY_REAL:
	    while (imgnlr (im1, buf1, v1) != EOF) {
		junk = impnlr (im2, buf2, v2)
		call amovr (Memr[buf1], Memr[buf2], npix)
	    }
	case TY_DOUBLE:
	    while (imgnld (im1, buf1, v1) != EOF) {
		junk = impnld (im2, buf2, v2)
		call amovd (Memd[buf1], Memd[buf2], npix)
	    }
	case TY_COMPLEX:
	    while (imgnlx (im1, buf1, v1) != EOF) {
	        junk = impnlx (im2, buf2, v2)
		call amovx (Memx[buf1], Memx[buf2], npix)
	    }
	default:
	    call error (1, "unknown pixel datatype")
	}

	# Unmap the images

	call imunmap (im2)
	call imunmap (im1)

end
