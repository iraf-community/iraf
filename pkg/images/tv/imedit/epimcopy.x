include	<imhdr.h>
 
# EP_IMCOPY -- Copy an image.  Use sequential routines to permit copying
# images of any dimension.  Perform pixel i/o in the datatype of the image,
# to avoid unnecessary type conversion.
 
procedure ep_imcopy (image1, image2)
 
char	image1[ARB]			# Input image
char	image2[ARB]			# Output image
 
int	npix, junk
pointer	buf1, buf2, im1, im2
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
 
int	imgnls(), imgnli(), imgnll(), imgnlr(), imgnld(), imgnlx()
int	impnls(), impnli(), impnll(), impnlr(), impnld(), impnlx()
pointer	immap()
errchk	immap
errchk	imgnls, imgnli, imgnll, imgnlr, imgnld, imgnlx
errchk	impnls, impnli, impnll, impnlr, impnld, impnlx
 
begin
	# Map images.
	im1 = immap (image1, READ_ONLY, 0)
	im2 = immap (image2, NEW_COPY, im1)
 
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
 
	# Unmap the images.
	call imunmap (im2)
	call imunmap (im1)
end
