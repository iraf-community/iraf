# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<error.h>

# T_IMTRANSPOSE -- Transpose images.
#
# The input and output images are given by image template lists.  The
# number of output images must match the number of input images.  Image
# sections are allowed in the input images and are ignored in the output
# images.  If the input and output image names are the same then the transpose
# is performed to a temporary file which then replaces the input image.

procedure t_imtranspose ()

char	imtlist1[SZ_LINE]			# Input image list
char	imtlist2[SZ_LINE]			# Output image list
int	len_blk					# 1D length of transpose block

char	image1[SZ_FNAME]			# Input image name
char	image2[SZ_FNAME]			# Output image name
char	imtemp[SZ_FNAME]			# Temporary file

int	list1, list2
pointer	im1, im2, mw
real	ltv[2], ltm[2,2]

bool	envgetb()
int	clgeti(), imtopen(), imtgetim(), imtlen()
pointer	immap(), mw_openim()

begin
	# Get input and output image template lists and the size of
	# the transpose block.

	call clgstr ("input", imtlist1, SZ_LINE)
	call clgstr ("output", imtlist2, SZ_LINE)
	len_blk = clgeti ("len_blk")

	# Expand the input and output image lists.

	list1 = imtopen (imtlist1)
	list2 = imtopen (imtlist2)
	if (imtlen (list1) != imtlen (list2)) {
	    call imtclose (list1)
	    call imtclose (list2)
	    call error (0, "Number of input and output images not the same")
	}

	# Do each set of input/output images.

	while ((imtgetim (list1, image1, SZ_FNAME) != EOF) &&
	    (imtgetim (list2, image2, SZ_FNAME) != EOF)) {

	    call xt_mkimtemp (image1, image2, imtemp, SZ_FNAME)

	    im1 = immap (image1, READ_ONLY, 0)
	    im2 = immap (image2, NEW_COPY, im1)

	    # Do the transpose.
	    call imtranspose (im1, im2, len_blk)

	    # Update the image WCS to reflect the shift.
	    if (!envgetb ("nomwcs")) {
		mw = mw_openim (im1)
		ltv[1] = 0.0; ltv[2] = 0.0
		ltm[1,1] = 0.0; ltm[2,1] = 1.0; ltm[1,2] = 1.0; ltm[2,2] = 0.0
		call mw_sltermr (mw, ltm, ltv, 2)
		call mw_saveim (mw, im2)
		call mw_close (mw)
	    }

	    # Unmap the input and output images.
	    call imunmap (im1)
	    call imunmap (im2)

	    call xt_delimtemp (image2, imtemp)
	}

	call imtclose (list1)
	call imtclose (list2)
end


# IMTRANSPOSE -- Transpose an image.
#
# Divide the image into square blocks of size len_blk by len_blk.
# Transpose each block with a generic array transpose operator.

procedure imtranspose (im_in, im_out, len_blk)

pointer	im_in				# Input image descriptor
pointer	im_out				# Output image descriptor
int	len_blk				# 1D length of transpose block

int	x1, x2, nx
int	y1, y2, ny
pointer	buf_in, buf_out

pointer	imgs2s(), imps2s(), imgs2i(), imps2i(), imgs2l(), imps2l()
pointer	imgs2r(), imps2r(), imgs2d(), imps2d(), imgs2x(), imps2x()

begin
	# Output image is a copy of input image with dims transposed.

	IM_LEN (im_out, 1) = IM_LEN (im_in, 2)
	IM_LEN (im_out, 2) = IM_LEN (im_in, 1)

	# Break the input image into blocks of at most len_blk by len_blk.

	do x1 = 1, IM_LEN (im_in, 1), len_blk {
	    x2 = x1 + len_blk - 1
	    if (x2 > IM_LEN(im_in, 1))
	       x2 = IM_LEN(im_in, 1)
	    nx = x2 - x1 + 1

	    do y1 = 1, IM_LEN (im_in, 2), len_blk {
	        y2 = y1 + len_blk - 1
	        if (y2 > IM_LEN(im_in, 2))
		   y2 = IM_LEN(im_in, 2)
		ny = y2 - y1 + 1

		# Switch on the pixel type to optimize IMIO.

		switch (IM_PIXTYPE (im_in)) {
		case TY_SHORT:
		    buf_in = imgs2s (im_in, x1, x2, y1, y2)
		    buf_out = imps2s (im_out, y1, y2, x1, x2)
		    call imtr2s (Mems[buf_in], Mems[buf_out], nx, ny)
		case TY_INT:
		    buf_in = imgs2i (im_in, x1, x2, y1, y2)
		    buf_out = imps2i (im_out, y1, y2, x1, x2)
		    call imtr2i (Memi[buf_in], Memi[buf_out], nx, ny)
		case TY_LONG:
		    buf_in = imgs2l (im_in, x1, x2, y1, y2)
		    buf_out = imps2l (im_out, y1, y2, x1, x2)
		    call imtr2l (Meml[buf_in], Meml[buf_out], nx, ny)
		case TY_REAL:
		    buf_in = imgs2r (im_in, x1, x2, y1, y2)
		    buf_out = imps2r (im_out, y1, y2, x1, x2)
		    call imtr2r (Memr[buf_in], Memr[buf_out], nx, ny)
		case TY_DOUBLE:
		    buf_in = imgs2d (im_in, x1, x2, y1, y2)
		    buf_out = imps2d (im_out, y1, y2, x1, x2)
		    call imtr2d (Memd[buf_in], Memd[buf_out], nx, ny)
		case TY_COMPLEX:
		    buf_in = imgs2x (im_in, x1, x2, y1, y2)
		    buf_out = imps2x (im_out, y1, y2, x1, x2)
		    call imtr2x (Memx[buf_in], Memx[buf_out], nx, ny)
		default:
		    call error (0, "unknown pixel type")
		}
	    }
	}
end
