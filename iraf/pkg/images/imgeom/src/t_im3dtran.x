include	<imhdr.h>
include	<error.h>
include <mwset.h>


# Define all possible tranpose operations.
define	XYZ	1	# xyz -> xyz
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

bool	verbose
int	list1, list2, len_blk, new_ax[3], which3d
pointer	sp, imtlist1, imtlist2, image1, image2, imtemp, im1, im2, mw

bool	clgetb(), envgetb()
int	clgeti(), imtopen(), imtgetim(), imtlen(), whichtran()
pointer	immap(), mw_openim()
errchk	im3dtranpose(), mw_openim(), mw_saveim(), mw_close(), im3dtrmw()

begin
	# Get some working space.
	call smark (sp)
	call salloc (imtlist1, SZ_LINE, TY_CHAR)
	call salloc (imtlist2, SZ_LINE, TY_CHAR)
	call salloc (image1, SZ_FNAME, TY_CHAR)
	call salloc (image2, SZ_FNAME, TY_CHAR)
	call salloc (imtemp, SZ_FNAME, TY_CHAR)

	# Get input and output image template lists, the size of the transpose
	# block, and the transpose mapping.
	call clgstr ("input", Memc[imtlist1], SZ_LINE)
	call clgstr ("output", Memc[imtlist2], SZ_LINE)
	new_ax[1] = clgeti ("new_x")
	new_ax[2] = clgeti ("new_y")
	new_ax[3] = clgeti ("new_z")
	len_blk = clgeti ("len_blk")
	verbose = clgetb ("verbose")

	# Determine the type of 3d transpose.
	which3d = whichtran (new_ax)
	if (which3d <= 0)
	    call error (0, "Invalid mapping of new_x, new_y, or new_z")

	# Expand the input and output image lists.

	list1 = imtopen (Memc[imtlist1])
	list2 = imtopen (Memc[imtlist2])
	if (imtlen (list1) != imtlen (list2)) {
	    call imtclose (list1)
	    call imtclose (list2)
	    call error (1, "Number of input and output images not the same")
	}

	# Do each set of input/output images.
	while ((imtgetim (list1, Memc[image1], SZ_FNAME) != EOF) &&
	    (imtgetim (list2, Memc[image2], SZ_FNAME) != EOF)) {


	    if (verbose) {
		call printf (
		"Image: %s axes: [123] -> Image: %s axes: [%d%d%d]\n")
		    call pargstr (Memc[image1])
		    call pargstr (Memc[image2])
		    call pargi (new_ax[1])
		    call pargi (new_ax[2])
		    call pargi (new_ax[3])
		call flush (STDOUT)
	    }

	    call xt_mkimtemp (Memc[image1], Memc[image2], Memc[imtemp],
	        SZ_FNAME)
	    im1 = immap (Memc[image1], READ_ONLY, 0)
	    im2 = immap (Memc[image2], NEW_COPY, im1)

	    iferr {

	        # Do the transpose.
	        call im3dtranspose (im1, im2, len_blk, which3d, new_ax)

	        # Update the image WCS to reflect the transpose.
	        if (!envgetb ("nomwcs")) {
		    mw = mw_openim (im1)
		    call im3dtrmw (mw, which3d)
		    call mw_saveim (mw, im2)
		    call mw_close (mw)
	        }

	    } then {

                call eprintf ("Error transposing image: %s\n")
                    call pargstr (Memc[image1])
                call erract (EA_WARN)
                call imunmap (im2)
                call imunmap (im1)
                call imdelete (Memc[image2])

	    } else {

	        # Finish up
	        call imunmap (im2)
	        call imunmap (im1)
	        call xt_delimtemp (Memc[image2], Memc[imtemp])
	    }
	}

	call imtclose (list1)
	call imtclose (list2)

	call sfree (sp)
end


# IM3DTRANSPOSE -- Transpose a 3D image. 
#
# Divide the image into square blocks of size len_blk by len_blk.
# Transpose each block with a generic array transpose operator.

procedure im3dtranspose (im_in, im_out, len_blk, which3d, new_ax)

pointer	im_in			#I Input image descriptor
pointer	im_out			#I Output image descriptor
int	len_blk			#I 1D length of transpose block
int	which3d			#I Parameterized transpose order
int	new_ax[3]		#I Map old axis[index] to new value

int	x1, x2, nx, y1, y2, ny, z1, z2, nz
pointer	buf_in, buf_out
pointer	imgs3s(), imps3s(), imgs3i(), imps3i(), imgs3l(), imps3l()
pointer	imgs3r(), imps3r(), imgs3d(), imps3d(), imgs3x(), imps3x()

begin
	# Check that the image is 3D.
	if (IM_NDIM(im_in) != 3)
	    call error (1, "image is not 3D.")

	# Output image is a copy of input image with dimensions transposed.

	IM_LEN (im_out, 1) = IM_LEN (im_in, new_ax[1])
	IM_LEN (im_out, 2) = IM_LEN (im_in, new_ax[2])
	IM_LEN (im_out, 3) = IM_LEN (im_in, new_ax[3])

	# Break the input image into blocks of at most (len_blk) ** 3.

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

		    case TY_LONG, TY_USHORT:
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


# WHICHTRAN -- Return the transpose type given the axes transpose list.

int procedure whichtran (new_ax)

int	new_ax[3]		#I the input axes transpose list.

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


define	LTM	Memd[ltr+(($2)-1)*pdim+($1)-1]
define	NCD	Memd[ncd+(($2)-1)*pdim+($1)-1]
define	swap    {temp=$1;$1=$2;$2=temp}

# IM3DTRMW -- Perform a transpose operation on the image WCS.

procedure im3dtrmw (mw, which3d)

pointer	mw			#I pointer to the mwcs structure
int	which3d			#I type of 3D transpose

int	i, axes[IM_MAXDIM], axval[IM_MAXDIM]
int	naxes, pdim, nelem, axmap, ax1, ax2, ax3, szatstr
pointer	sp, ltr, ltm, ltv, cd, r, w, ncd, nr
pointer	attribute1, attribute2, attribute3, atstr1, atstr2, atstr3, mwtmp
double	temp
int	mw_stati(), itoc(), strlen()
pointer	mw_open()
errchk	mw_gwattrs(), mw_newsystem()

begin
	# Convert axis bitflags to the axis lists.
	call mw_gaxlist (mw, 07B, axes, naxes)
	if (naxes < 2)
	    return

	# Get the dimensions of the wcs and turn off axis mapping.
	pdim = mw_stati (mw, MW_NPHYSDIM) 
	nelem = pdim * pdim
	axmap = mw_stati (mw, MW_USEAXMAP)
	call mw_seti (mw, MW_USEAXMAP, NO)
	szatstr = SZ_LINE

	# Allocate working space.
	call smark (sp)
	call salloc (ltr, nelem, TY_DOUBLE)
	call salloc (cd, nelem, TY_DOUBLE)
	call salloc (r, pdim, TY_DOUBLE)
	call salloc (w, pdim, TY_DOUBLE)
	call salloc (ltm, nelem, TY_DOUBLE) 
	call salloc (ltv, pdim, TY_DOUBLE)
	call salloc (ncd, nelem, TY_DOUBLE)
	call salloc (nr, pdim, TY_DOUBLE)
	call salloc (attribute1, SZ_FNAME, TY_CHAR)
	call salloc (attribute2, SZ_FNAME, TY_CHAR)
	call salloc (attribute3, SZ_FNAME, TY_CHAR)

	# Get the wterm which corresponds to the original logical to
	# world transformation.
	call mw_gwtermd (mw, Memd[r], Memd[w], Memd[cd], pdim) 
	call mw_gltermd (mw, Memd[ltm], Memd[ltv], pdim) 
	call mwvmuld (Memd[ltm], Memd[r], Memd[nr], pdim)
	call aaddd (Memd[nr], Memd[ltv], Memd[nr], pdim)
	call mwinvertd (Memd[ltm], Memd[ltr], pdim)
	call mwmmuld (Memd[cd], Memd[ltr], Memd[ncd], pdim)

	# Define which physical axes the logical axes correspond to. 
	# and recompute the above wterm to take into account the transpose.
	ax1 = axes[1]
	ax2 = axes[2]
	ax3 = axes[3]

	switch (which3d) {
	case XYZ:
	    # do nothing

	case XZY:
	    # switch axes 3 and 2
	    call amovd (Memd[ncd], Memd[ltr], nelem)
	    NCD(ax1,ax1) = LTM(ax1,ax1)
	    NCD(ax2,ax1) = LTM(ax3,ax1)
	    NCD(ax3,ax1) = LTM(ax2,ax1)
	    NCD(ax1,ax2) = LTM(ax1,ax3)
	    NCD(ax2,ax2) = LTM(ax3,ax3)
	    NCD(ax3,ax2) = LTM(ax2,ax3)
	    NCD(ax1,ax3) = LTM(ax1,ax2)
	    NCD(ax2,ax3) = LTM(ax3,ax2)
	    NCD(ax3,ax3) = LTM(ax2,ax2)
	    swap (Memd[w+ax3-1], Memd[w+ax2-1])
	    swap (Memd[nr+ax3-1], Memd[nr+ax2-1])

	case YXZ:
	    # switch axes 1 and 2
	    call amovd (Memd[ncd], Memd[ltr], nelem)
	    NCD(ax1,ax1) = LTM(ax2,ax2)
	    NCD(ax2,ax1) = LTM(ax1,ax2)
	    NCD(ax3,ax1) = LTM(ax3,ax2)
	    NCD(ax1,ax2) = LTM(ax2,ax1)
	    NCD(ax2,ax2) = LTM(ax1,ax1)
	    NCD(ax3,ax2) = LTM(ax3,ax1)
	    NCD(ax1,ax3) = LTM(ax2,ax3)
	    NCD(ax2,ax3) = LTM(ax1,ax3)
	    NCD(ax3,ax3) = LTM(ax3,ax3)
	    swap (Memd[w+ax1-1], Memd[w+ax2-1])
	    swap (Memd[nr+ax1-1], Memd[nr+ax2-1])

	case YZX:
	    # map axes 123 to 231
	    call amovd (Memd[ncd], Memd[ltr], nelem)
	    NCD(ax1,ax1) = LTM(ax2,ax2)
	    NCD(ax2,ax1) = LTM(ax3,ax2)
	    NCD(ax3,ax1) = LTM(ax1,ax2)
	    NCD(ax1,ax2) = LTM(ax2,ax3)
	    NCD(ax2,ax2) = LTM(ax3,ax3)
	    NCD(ax3,ax2) = LTM(ax1,ax3)
	    NCD(ax1,ax3) = LTM(ax2,ax1)
	    NCD(ax2,ax3) = LTM(ax3,ax1)
	    NCD(ax3,ax3) = LTM(ax1,ax1)
	    call amovd (Memd[w], Memd[ltv], pdim)
	    Memd[w+ax1-1] = Memd[ltv+ax2-1] 
	    Memd[w+ax2-1] = Memd[ltv+ax3-1] 
	    Memd[w+ax3-1] = Memd[ltv+ax1-1] 
	    call amovd (Memd[nr], Memd[ltv], pdim)
	    Memd[nr+ax1-1] = Memd[ltv+ax2-1] 
	    Memd[nr+ax2-1] = Memd[ltv+ax3-1] 
	    Memd[nr+ax3-1] = Memd[ltv+ax1-1] 

	case ZXY:
	    # map axes 123 to 312
	    call amovd (Memd[ncd], Memd[ltr], nelem)
	    NCD(ax1,ax1) = LTM(ax3,ax3)
	    NCD(ax2,ax1) = LTM(ax1,ax3)
	    NCD(ax3,ax1) = LTM(ax2,ax3)
	    NCD(ax1,ax2) = LTM(ax3,ax1)
	    NCD(ax2,ax2) = LTM(ax1,ax1)
	    NCD(ax3,ax2) = LTM(ax2,ax1)
	    NCD(ax1,ax3) = LTM(ax3,ax2)
	    NCD(ax2,ax3) = LTM(ax1,ax2)
	    NCD(ax3,ax3) = LTM(ax2,ax2)
	    call amovd (Memd[w], Memd[ltv], pdim)
	    Memd[w+ax1-1] = Memd[ltv+ax3-1] 
	    Memd[w+ax2-1] = Memd[ltv+ax1-1] 
	    Memd[w+ax3-1] = Memd[ltv+ax2-1] 
	    call amovd (Memd[nr], Memd[ltv], pdim)
	    Memd[nr+ax1-1] = Memd[ltv+ax3-1] 
	    Memd[nr+ax2-1] = Memd[ltv+ax1-1] 
	    Memd[nr+ax3-1] = Memd[ltv+ax2-1] 

	case ZYX:
	    # switch axes 3 and 1
	    call amovd (Memd[ncd], Memd[ltr], nelem)
	    NCD(ax1,ax1) = LTM(ax3,ax3)
	    NCD(ax2,ax1) = LTM(ax2,ax3)
	    NCD(ax3,ax1) = LTM(ax1,ax3)
	    NCD(ax1,ax2) = LTM(ax3,ax2)
	    NCD(ax2,ax2) = LTM(ax2,ax2)
	    NCD(ax3,ax2) = LTM(ax1,ax2)
	    NCD(ax1,ax3) = LTM(ax3,ax1)
	    NCD(ax2,ax3) = LTM(ax2,ax1)
	    NCD(ax3,ax3) = LTM(ax1,ax1)
	    swap (Memd[w+ax1-1], Memd[w+ax3-1])
	    swap (Memd[nr+ax1-1], Memd[nr+ax3-1])
	}

	# Perform the transpose of the lterm.
	call mw_mkidmd (Memd[ltr], pdim)
	switch (which3d) {

	case XYZ:
	    # do nothing

	case XZY:
	    # switch axes 3 and 2
	    LTM(ax2,ax2) = 0.0d0
	    LTM(ax3,ax2) = 1.0d0
	    LTM(ax2,ax3) = 1.0d0
	    LTM(ax3,ax3) = 0.0d0

	case YXZ:
	    # switch axes 1 and 2
	    LTM(ax1,ax1) = 0.0d0
	    LTM(ax1,ax2) = 1.0d0
	    LTM(ax2,ax1) = 1.0d0
	    LTM(ax2,ax2) = 0.0d0

	case YZX:
	    # map axes 123 to 231
	    LTM(ax1,ax1) = 0.0d0
	    LTM(ax1,ax2) = 1.0d0
	    LTM(ax1,ax3) = 0.0d0
	    LTM(ax2,ax1) = 0.0d0
	    LTM(ax2,ax2) = 0.0d0
	    LTM(ax2,ax3) = 1.0d0
	    LTM(ax3,ax1) = 1.0d0
	    LTM(ax3,ax2) = 0.0d0
	    LTM(ax3,ax3) = 0.0d0

	case ZXY:
	    # map axes 123 to 312
	    LTM(ax1,ax1) = 0.0d0
	    LTM(ax1,ax2) = 0.0d0
	    LTM(ax1,ax3) = 1.0d0
	    LTM(ax2,ax1) = 1.0d0
	    LTM(ax2,ax2) = 0.0d0
	    LTM(ax2,ax3) = 0.0d0
	    LTM(ax3,ax1) = 0.0d0
	    LTM(ax3,ax2) = 1.0d0
	    LTM(ax3,ax3) = 0.0d0

	case ZYX:
	    # switch axes 3 and 1
	    LTM(ax3,ax3) = 0.0d0
	    LTM(ax3,ax1) = 1.0d0
	    LTM(ax1,ax3) = 1.0d0
	    LTM(ax1,ax1) = 0.0d0

	}
	call aclrd (Memd[ltv], pdim)
	call aclrd (Memd[r], pdim)
	call mw_translated (mw, Memd[ltv], Memd[ltr], Memd[r], pdim)

	# Get the new lterm, recompute the wterm, and store it.
	call mw_gltermd (mw, Memd[ltm], Memd[ltv], pdim) 
	call mwmmuld (Memd[ncd], Memd[ltm], Memd[cd], pdim)
	call mwinvertd (Memd[ltm], Memd[ltr], pdim)
	call asubd (Memd[nr], Memd[ltv], Memd[r], pdim)
	call mwvmuld (Memd[ltr], Memd[r], Memd[nr], pdim)
	call mw_swtermd (mw, Memd[nr], Memd[w], Memd[cd], pdim)

	# Make a new temporary wcs and set the system name.
	mwtmp = mw_open (NULL, pdim)
	call mw_gsystem (mw, Memc[attribute1], SZ_FNAME)
	iferr (call mw_newsystem (mwtmp, Memc[attribute1], pdim))
	    call mw_ssystem (mwtmp, Memc[attribute1])

	# Copy the wterm and the lterm to it.
	call mw_gwtermd (mw, Memd[r], Memd[w], Memd[ltr], pdim)
	call mw_swtermd (mwtmp, Memd[r], Memd[w], Memd[ltr], pdim)
	call mw_gltermd (mw, Memd[ltr], Memd[r], pdim)
	call mw_sltermd (mwtmp, Memd[ltr], Memd[r], pdim)

	# Set the axis map and the axis types.
	call mw_gaxmap (mw, axes, axval, pdim)
	call mw_saxmap (mwtmp, axes, axval, pdim)
	iferr (call mw_gwattrs (mw, ax1, "wtype", Memc[attribute1], SZ_FNAME))
	    call strcpy ("linear", Memc[attribute1], SZ_FNAME)
	iferr (call mw_gwattrs (mw, ax2, "wtype", Memc[attribute2], SZ_FNAME))
	    call strcpy ("linear", Memc[attribute2], SZ_FNAME)
	iferr (call mw_gwattrs (mw, ax3, "wtype", Memc[attribute3], SZ_FNAME))
	    call strcpy ("linear", Memc[attribute3], SZ_FNAME)

	switch (which3d) {
	case XYZ:
	    call mw_swtype (mwtmp, ax1, 1, Memc[attribute1], "")
	    call mw_swtype (mwtmp, ax2, 1, Memc[attribute2], "")
	    call mw_swtype (mwtmp, ax3, 1, Memc[attribute3], "")
	case XZY:
	    call mw_swtype (mwtmp, ax1, 1, Memc[attribute1], "")
	    call mw_swtype (mwtmp, ax2, 1, Memc[attribute3], "")
	    call mw_swtype (mwtmp, ax3, 1, Memc[attribute2], "")
	case YXZ:
	    call mw_swtype (mwtmp, ax1, 1, Memc[attribute2], "")
	    call mw_swtype (mwtmp, ax2, 1, Memc[attribute1], "")
	    call mw_swtype (mwtmp, ax3, 1, Memc[attribute3], "")
	case YZX:
	    call mw_swtype (mwtmp, ax1, 1, Memc[attribute2], "")
	    call mw_swtype (mwtmp, ax2, 1, Memc[attribute3], "")
	    call mw_swtype (mwtmp, ax3, 1, Memc[attribute1], "")
	case ZXY:
	    call mw_swtype (mwtmp, ax1, 1, Memc[attribute3], "")
	    call mw_swtype (mwtmp, ax2, 1, Memc[attribute1], "")
	    call mw_swtype (mwtmp, ax3, 1, Memc[attribute2], "")
	case ZYX:
	    call mw_swtype (mwtmp, ax1, 1, Memc[attribute3], "")
	    call mw_swtype (mwtmp, ax2, 1, Memc[attribute2], "")
	    call mw_swtype (mwtmp, ax3, 1, Memc[attribute1], "")
	}

	# Copy the axis attributes.
	call malloc (atstr1, szatstr, TY_CHAR)
	call malloc (atstr2, szatstr, TY_CHAR)
	call malloc (atstr3, szatstr, TY_CHAR)

	for (i =  1; ; i = i + 1) {

	    if (itoc (i, Memc[attribute1], SZ_FNAME) <= 0)
		Memc[attribute1] = EOS
	    if (itoc (i, Memc[attribute2], SZ_FNAME) <= 0)
		Memc[attribute2] = EOS
	    if (itoc (i, Memc[attribute3], SZ_FNAME) <= 0)
		Memc[attribute3] = EOS

	    repeat {
		iferr (call mw_gwattrs (mw, ax1, Memc[attribute1],
		    Memc[atstr1], szatstr))
		    Memc[atstr1] = EOS
		iferr (call mw_gwattrs (mw, ax2, Memc[attribute2],
		    Memc[atstr2], szatstr))
		    Memc[atstr2] = EOS
		iferr (call mw_gwattrs (mw, ax3, Memc[attribute3],
		    Memc[atstr3], szatstr))
		    Memc[atstr3] = EOS
		if ((strlen (Memc[atstr1]) < szatstr) &&
		    (strlen (Memc[atstr2]) < szatstr) &&
		    (strlen (Memc[atstr3]) < szatstr))
		    break
		szatstr = szatstr + SZ_LINE
		call realloc (atstr1, szatstr, TY_CHAR)
		call realloc (atstr2, szatstr, TY_CHAR)
		call realloc (atstr3, szatstr, TY_CHAR)
	    }
	    if ((Memc[atstr1] == EOS) && (Memc[atstr2] == EOS) &&
		(Memc[atstr3] == EOS))
	        break

	    switch (which3d) {
	    case XYZ:
	        if (Memc[atstr1] != EOS)
	            call mw_swattrs (mwtmp, ax1, Memc[attribute1], Memc[atstr1])
	        if (Memc[atstr2] != EOS)
	            call mw_swattrs (mwtmp, ax2, Memc[attribute2], Memc[atstr2])
	        if (Memc[atstr3] != EOS)
	            call mw_swattrs (mwtmp, ax3, Memc[attribute3], Memc[atstr3])
	    case XZY:
	        if (Memc[atstr1] != EOS)
	            call mw_swattrs (mwtmp, ax1, Memc[attribute1], Memc[atstr1])
	        if (Memc[atstr3] != EOS)
	            call mw_swattrs (mwtmp, ax2, Memc[attribute3], Memc[atstr3])
	        if (Memc[atstr2] != EOS)
	            call mw_swattrs (mwtmp, ax3, Memc[attribute2], Memc[atstr2])
	    case YXZ:
	        if (Memc[atstr2] != EOS)
	            call mw_swattrs (mwtmp, ax1, Memc[attribute2], Memc[atstr2])
	        if (Memc[atstr1] != EOS)
	            call mw_swattrs (mwtmp, ax2, Memc[attribute1], Memc[atstr1])
	        if (Memc[atstr3] != EOS)
	            call mw_swattrs (mwtmp, ax3, Memc[attribute3], Memc[atstr3])
	    case YZX:
	        if (Memc[atstr2] != EOS)
	            call mw_swattrs (mwtmp, ax1, Memc[attribute2], Memc[atstr2])
	        if (Memc[atstr3] != EOS)
	            call mw_swattrs (mwtmp, ax2, Memc[attribute3], Memc[atstr3])
	        if (Memc[atstr1] != EOS)
	            call mw_swattrs (mwtmp, ax3, Memc[attribute1], Memc[atstr1])
	    case ZXY:
	        if (Memc[atstr3] != EOS)
	            call mw_swattrs (mwtmp, ax1, Memc[attribute3], Memc[atstr3])
	        if (Memc[atstr1] != EOS)
	            call mw_swattrs (mwtmp, ax2, Memc[attribute1], Memc[atstr1])
	        if (Memc[atstr2] != EOS)
	            call mw_swattrs (mwtmp, ax3, Memc[attribute2], Memc[atstr2])
	    case ZYX:
	        if (Memc[atstr3] != EOS)
	            call mw_swattrs (mwtmp, ax1, Memc[attribute3], Memc[atstr3])
	        if (Memc[atstr2] != EOS)
	            call mw_swattrs (mwtmp, ax2, Memc[attribute2], Memc[atstr2])
	        if (Memc[atstr1] != EOS)
	            call mw_swattrs (mwtmp, ax3, Memc[attribute1], Memc[atstr1])
	    }

	}
	call mfree (atstr1, TY_CHAR)
	call mfree (atstr2, TY_CHAR)
	call mfree (atstr3, TY_CHAR)
	call mw_close (mw)

	# Delete the old wcs and set equal to the new one.
	call sfree (sp)
	mw = mwtmp
	call mw_seti (mw, MW_USEAXMAP, axmap)
end
