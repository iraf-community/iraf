# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# IMG_IMCOPY -- Copy an image.  Use sequential routines to permit copying
# images of any dimension.  Perform pixel i/o in the datatype of the image,
# to avoid unnecessary type conversion.

procedure img_imcopy (image1, image2, verbose)

char	image1[ARB]			# Input image
char	image2[ARB]			# Output image
bool	verbose				# Print the operation

int	npix, junk
pointer	buf1, buf2, im1, im2
pointer	sp, imtemp, section
long	v1[IM_MAXDIM], v2[IM_MAXDIM]

int	imgnls(), imgnll(), imgnlr(), imgnld(), imgnlx()
int	impnls(), impnll(), impnlr(), impnld(), impnlx()
pointer	immap()

begin
	call smark (sp)
	call salloc (imtemp, SZ_PATHNAME, TY_CHAR)
	call salloc (section, SZ_FNAME, TY_CHAR)

	# If verbose print the operation.
	if (verbose) {
	    call printf ("%s -> %s\n")
		call pargstr (image1)
		call pargstr (image2)
	    call flush (STDOUT)
	}

	# Map the input image.
	im1 = immap (image1, READ_ONLY, 0)

	# If the output has a section appended we are writing to a
	# section of an existing image.  Otherwise get a temporary
	# output image name and map it as a copy of the input image.
	# Copy the input image to the temporary output image and unmap
	# the images.  Release the temporary image name.

	call imgsection (image2, Memc[section], SZ_FNAME)
	if (Memc[section] != EOS) {
	    call strcpy (image2, Memc[imtemp], SZ_PATHNAME)
	    im2 = immap (image2, READ_WRITE, 0)
	} else {
	    call xt_mkimtemp (image1, image2, Memc[imtemp], SZ_PATHNAME)
	    im2 = immap (image2, NEW_COPY, im1)
	}

	# Setup start vector for sequential reads and writes.

	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	# Copy the image.

	npix = IM_LEN(im1, 1)
	switch (IM_PIXTYPE(im1)) {
	case TY_SHORT:
	    while (imgnls (im1, buf1, v1) != EOF) {
		junk = impnls (im2, buf2, v2)
		call amovs (Mems[buf1], Mems[buf2], npix)
	    }
	case TY_USHORT, TY_INT, TY_LONG:
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

	# Unmap the images.

	call imunmap (im2)
	call imunmap (im1)
	call xt_delimtemp (image2, Memc[imtemp])
end
