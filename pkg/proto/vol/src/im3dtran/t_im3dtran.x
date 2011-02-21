include	<imhdr.h>
include	<error.h>

define	XYZ	1	# xyz -> xyz (identity)
define	XZY	2	# xyz -> xzy
define	YXZ	3	# xyz -> yxz
define	YZX	4	# xyz -> yzx
define	ZXY	5	# xyz -> zxy
define	ZYX	6	# xyz -> zyx


# T_IM3DTRAN -- Transpose 3d images.
#
# The input and output images are given by image template lists.  The
# number of output images must match the number of input images.  Image
# sections are allowed in the input images and are ignored in the output
# images.  If the input and output image names are the same then the transpose
# is performed to a temporary file which then replaces the input image.

procedure t_im3dtran ()

char	imtlist1[SZ_LINE]			# Input image list
char	imtlist2[SZ_LINE]			# Output image list
int	len_blk					# 1D length of transpose block

char	image1[SZ_FNAME]			# Input image name
char	image2[SZ_FNAME]			# Output image name
char	imtemp[SZ_FNAME]			# Temporary file

int	list1, list2, new_ax[3], which3d
pointer	im1, im2

int	clgeti(), imtopen(), imtgetim(), imtlen(), whichtran()
pointer	immap()

begin
	# Get input and output image template lists, the size of the transpose
	# block, and the transpose mapping.

	call clgstr ("input", imtlist1, SZ_LINE)
	call clgstr ("output", imtlist2, SZ_LINE)
	len_blk = clgeti ("len_blk")
	new_ax[1] = clgeti ("new_x")
	new_ax[2] = clgeti ("new_y")
	new_ax[3] = clgeti ("new_z")

	# Determine the type of 3d transpose.
	which3d = whichtran (new_ax)
	if (which3d <= 0)
	    call error (0, "Invalid mapping of new_x, new_y, new_z")

	# Expand the input and output image lists.

	list1 = imtopen (imtlist1)
	list2 = imtopen (imtlist2)
	if (imtlen (list1) != imtlen (list2)) {
	    call imtclose (list1)
	    call imtclose (list2)
	    call error (1, "Number of input and output images not the same")
	}

	# Do each set of input/output images.

	while ((imtgetim (list1, image1, SZ_FNAME) != EOF) &&
	    (imtgetim (list2, image2, SZ_FNAME) != EOF)) {

	    call xt_mkimtemp (image1, image2, imtemp, SZ_FNAME)

	    im1 = immap (image1, READ_ONLY, 0)
	    im2 = immap (image2, NEW_COPY, im1)

	    # Do the transpose.
	    call im3dtranspose (im1, im2, len_blk, which3d, new_ax)

	    # Unmap the input and output images.
	    call imunmap (im1)
	    call imunmap (im2)

	    call xt_delimtemp (image2, imtemp)
	}

	call imtclose (list1)
	call imtclose (list2)
end


# IM3DTRANSPOSE -- Transpose an image. 
#
# Divide the image into square blocks of size len_blk by len_blk.
# Transpose each block with a generic array transpose operator.

procedure im3dtranspose (im_in, im_out, len_blk, which3d, new_ax)

pointer	im_in				# Input image descriptor
pointer	im_out				# Output image descriptor
int	len_blk				# 1D length of transpose block
int	which3d				# Parameterized transpose order
int	new_ax[3]			# Map old axis[index] to new value

int	x1, x2, nx
int	y1, y2, ny
int	z1, z2, nz
pointer	buf_in, buf_out

pointer	imgs3s(), imps3s(), imgs3i(), imps3i(), imgs3l(), imps3l()
pointer	imgs3r(), imps3r(), imgs3d(), imps3d(), imgs3x(), imps3x()

begin
	# Output image is a copy of input image with dims transposed.

	IM_LEN (im_out, 1) = IM_LEN (im_in, new_ax[1])
	IM_LEN (im_out, 2) = IM_LEN (im_in, new_ax[2])
	IM_LEN (im_out, 3) = IM_LEN (im_in, new_ax[3])

	# Break the input image into blocks of at most (len_blk)**3 .

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

		do z1 = 1, IM_LEN (im_in, 3), len_blk {
		    z2 = z1 + len_blk - 1
		    if (z2 > IM_LEN(im_in, 3))
			z2 = IM_LEN(im_in, 3)
		    nz = z2 - z1 + 1

		    # Switch on the pixel type to optimize IMIO.

		    switch (IM_PIXTYPE (im_in)) {
		    case TY_SHORT:
			buf_in = imgs3s (im_in, x1, x2, y1, y2, z1, z2)
			switch (which3d) {
			case XYZ:
			    buf_out = imps3s (im_out, x1, x2, y1, y2, z1, z2)
			    call txyz3s (Mems[buf_in], Mems[buf_out], nx,ny,nz)
			case XZY:
			    buf_out = imps3s (im_out, x1, x2, z1, z2, y1, y2)
			    call txzy3s (Mems[buf_in], Mems[buf_out], nx,ny,nz)
			case YXZ:
			    buf_out = imps3s (im_out, y1, y2, x1, x2, z1, z2)
			    call tyxz3s (Mems[buf_in], Mems[buf_out], nx,ny,nz)
			case YZX:
			    buf_out = imps3s (im_out, y1, y2, z1, z2, x1, x2)
			    call tyzx3s (Mems[buf_in], Mems[buf_out], nx,ny,nz)
			case ZXY:
			    buf_out = imps3s (im_out, z1, z2, x1, x2, y1, y2)
			    call tzxy3s (Mems[buf_in], Mems[buf_out], nx,ny,nz)
			case ZYX:
			    buf_out = imps3s (im_out, z1, z2, y1, y2, x1, x2)
			    call tzyx3s (Mems[buf_in], Mems[buf_out], nx,ny,nz)
			}
		    case TY_INT:
			buf_in = imgs3i (im_in, x1, x2, y1, y2, z1, z2)
			switch (which3d) {
			case XYZ:
			    buf_out = imps3i (im_out, x1, x2, y1, y2, z1, z2)
			    call txyz3i (Memi[buf_in], Memi[buf_out], nx,ny,nz)
			case XZY:
			    buf_out = imps3i (im_out, x1, x2, z1, z2, y1, y2)
			    call txzy3i (Memi[buf_in], Memi[buf_out], nx,ny,nz)
			case YXZ:
			    buf_out = imps3i (im_out, y1, y2, x1, x2, z1, z2)
			    call tyxz3i (Memi[buf_in], Memi[buf_out], nx,ny,nz)
			case YZX:
			    buf_out = imps3i (im_out, y1, y2, z1, z2, x1, x2)
			    call tyzx3i (Memi[buf_in], Memi[buf_out], nx,ny,nz)
			case ZXY:
			    buf_out = imps3i (im_out, z1, z2, x1, x2, y1, y2)
			    call tzxy3i (Memi[buf_in], Memi[buf_out], nx,ny,nz)
			case ZYX:
			    buf_out = imps3i (im_out, z1, z2, y1, y2, x1, x2)
			    call tzyx3i (Memi[buf_in], Memi[buf_out], nx,ny,nz)
			}
		    case TY_LONG:
			buf_in = imgs3l (im_in, x1, x2, y1, y2, z1, z2)
			switch (which3d) {
			case XYZ:
			    buf_out = imps3l (im_out, x1, x2, y1, y2, z1, z2)
			    call txyz3l (Meml[buf_in], Meml[buf_out], nx,ny,nz)
			case XZY:
			    buf_out = imps3l (im_out, x1, x2, z1, z2, y1, y2)
			    call txzy3l (Meml[buf_in], Meml[buf_out], nx,ny,nz)
			case YXZ:
			    buf_out = imps3l (im_out, y1, y2, x1, x2, z1, z2)
			    call tyxz3l (Meml[buf_in], Meml[buf_out], nx,ny,nz)
			case YZX:
			    buf_out = imps3l (im_out, y1, y2, z1, z2, x1, x2)
			    call tyzx3l (Meml[buf_in], Meml[buf_out], nx,ny,nz)
			case ZXY:
			    buf_out = imps3l (im_out, z1, z2, x1, x2, y1, y2)
			    call tzxy3l (Meml[buf_in], Meml[buf_out], nx,ny,nz)
			case ZYX:
			    buf_out = imps3l (im_out, z1, z2, y1, y2, x1, x2)
			    call tzyx3l (Meml[buf_in], Meml[buf_out], nx,ny,nz)
			}
		    case TY_REAL:
			buf_in = imgs3r (im_in, x1, x2, y1, y2, z1, z2)
			switch (which3d) {
			case XYZ:
			    buf_out = imps3r (im_out, x1, x2, y1, y2, z1, z2)
			    call txyz3r (Memr[buf_in], Memr[buf_out], nx,ny,nz)
			case XZY:
			    buf_out = imps3r (im_out, x1, x2, z1, z2, y1, y2)
			    call txzy3r (Memr[buf_in], Memr[buf_out], nx,ny,nz)
			case YXZ:
			    buf_out = imps3r (im_out, y1, y2, x1, x2, z1, z2)
			    call tyxz3r (Memr[buf_in], Memr[buf_out], nx,ny,nz)
			case YZX:
			    buf_out = imps3r (im_out, y1, y2, z1, z2, x1, x2)
			    call tyzx3r (Memr[buf_in], Memr[buf_out], nx,ny,nz)
			case ZXY:
			    buf_out = imps3r (im_out, z1, z2, x1, x2, y1, y2)
			    call tzxy3r (Memr[buf_in], Memr[buf_out], nx,ny,nz)
			case ZYX:
			    buf_out = imps3r (im_out, z1, z2, y1, y2, x1, x2)
			    call tzyx3r (Memr[buf_in], Memr[buf_out], nx,ny,nz)
			}
		    case TY_DOUBLE:
			buf_in = imgs3d (im_in, x1, x2, y1, y2, z1, z2)
			switch (which3d) {
			case XYZ:
			    buf_out = imps3d (im_out, x1, x2, y1, y2, z1, z2)
			    call txyz3d (Memd[buf_in], Memd[buf_out], nx,ny,nz)
			case XZY:
			    buf_out = imps3d (im_out, x1, x2, z1, z2, y1, y2)
			    call txzy3d (Memd[buf_in], Memd[buf_out], nx,ny,nz)
			case YXZ:
			    buf_out = imps3d (im_out, y1, y2, x1, x2, z1, z2)
			    call tyxz3d (Memd[buf_in], Memd[buf_out], nx,ny,nz)
			case YZX:
			    buf_out = imps3d (im_out, y1, y2, z1, z2, x1, x2)
			    call tyzx3d (Memd[buf_in], Memd[buf_out], nx,ny,nz)
			case ZXY:
			    buf_out = imps3d (im_out, z1, z2, x1, x2, y1, y2)
			    call tzxy3d (Memd[buf_in], Memd[buf_out], nx,ny,nz)
			case ZYX:
			    buf_out = imps3d (im_out, z1, z2, y1, y2, x1, x2)
			    call tzyx3d (Memd[buf_in], Memd[buf_out], nx,ny,nz)
			}
		    case TY_COMPLEX:
			buf_in = imgs3x (im_in, x1, x2, y1, y2, z1, z2)
			switch (which3d) {
			case XYZ:
			    buf_out = imps3x (im_out, x1, x2, y1, y2, z1, z2)
			    call txyz3x (Memx[buf_in], Memx[buf_out], nx,ny,nz)
			case XZY:
			    buf_out = imps3x (im_out, x1, x2, z1, z2, y1, y2)
			    call txzy3x (Memx[buf_in], Memx[buf_out], nx,ny,nz)
			case YXZ:
			    buf_out = imps3x (im_out, y1, y2, x1, x2, z1, z2)
			    call tyxz3x (Memx[buf_in], Memx[buf_out], nx,ny,nz)
			case YZX:
			    buf_out = imps3x (im_out, y1, y2, z1, z2, x1, x2)
			    call tyzx3x (Memx[buf_in], Memx[buf_out], nx,ny,nz)
			case ZXY:
			    buf_out = imps3x (im_out, z1, z2, x1, x2, y1, y2)
			    call tzxy3x (Memx[buf_in], Memx[buf_out], nx,ny,nz)
			case ZYX:
			    buf_out = imps3x (im_out, z1, z2, y1, y2, x1, x2)
			    call tzyx3x (Memx[buf_in], Memx[buf_out], nx,ny,nz)
			}
		    default:
			call error (3, "unknown pixel type")
		    }
		}
	    }
	}
end


# WHICHTRAN -- Return transpose type.

int procedure whichtran (new_ax)
int	new_ax[3]

int	which

begin
	which = 0

	if (new_ax[1] == 1) {
	    if (new_ax[2] == 2)
		which = XYZ
	    else if (new_ax[2] == 3)
		which = XZY
	} else if (new_ax[1] == 2) {
	    if (new_ax[2] == 1)
		which = YXZ
	    else if (new_ax[2] == 3)
		which = YZX
	} else if (new_ax[1] == 3) {
	    if (new_ax[2] == 1)
		which = ZXY
	    else if (new_ax[2] == 2)
		which = ZYX
	}
	
	return (which)
end
