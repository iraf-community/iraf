# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# T_BLKREP -- Block replicate n-dimensional images.
#
# The input and output images are given by image template lists.  The
# number of output images must match the number of input images.  Image
# sections are allowed in the input images and are ignored in the output
# images.  If the input and output image names are the same then the
# replication operation is performed to a temporary file which then replaces
# the input image.

procedure t_blkrep()

int	i, list1, list2
pointer	sp, image1, image2, image3, blkfac, im1, im2, mw
real	shifts[IM_MAXDIM], mags[IM_MAXDIM]

bool	envgetb()
int	imtopenp(), imtgetim(), imtlen(), clgeti()
pointer	immap(), mw_openim()
string	blk_param	"bX"

begin
	# Allocate memory.
	call smark (sp)
	call salloc (image1, SZ_LINE, TY_CHAR)
	call salloc (image2, SZ_LINE, TY_CHAR)
	call salloc (image3, SZ_LINE, TY_CHAR)
	call salloc (blkfac, IM_MAXDIM, TY_INT)

	# Expand the input and output image lists.

	list1 = imtopenp ("input")
	list2 = imtopenp ("output")

	if (imtlen (list1) != imtlen (list2)) {
	    call imtclose (list1)
	    call imtclose (list2)
	    call error (1, "Number of input and output images not the same")
	}

	# Do each set of input/output images.

	call amovki (INDEFI, Memi[blkfac], IM_MAXDIM)
	while ((imtgetim (list1, Memc[image1], SZ_LINE) != EOF) &&
	    (imtgetim (list2, Memc[image2], SZ_LINE) != EOF)) {

	    call xt_mkimtemp (Memc[image1], Memc[image2], Memc[image3], SZ_LINE)

	    im1 = immap (Memc[image1], READ_ONLY, 0)
	    im2 = immap (Memc[image2], NEW_COPY, im1)

	    do i = 1, IM_NDIM(im1) {
		if (IS_INDEFI(Memi[blkfac+i-1])) {
		    call sprintf (blk_param[2], SZ_CHAR, "%1d")
			call pargi (i)
		    Memi[blkfac+i-1] = clgeti (blk_param)
		}
	    }

	    # Perform the block operation.
	    switch (IM_PIXTYPE (im1)) {
	    case TY_SHORT:
		call blkrps (im1, im2, Memi[blkfac])
	    case TY_INT, TY_LONG:
		call blkrpl (im1, im2, Memi[blkfac])
	    case TY_DOUBLE:
		call blkrpd (im1, im2, Memi[blkfac])
	    default:
		call blkrpr (im1, im2, Memi[blkfac])
	    }

	    if (!envgetb ("nomwcs")) {
		mw = mw_openim (im1)
		do i = 1, IM_NDIM(im1)
		    mags[i] = double (Memi[blkfac+i-1])
		call mw_scale (mw, mags, (2 ** IM_NDIM(im1) - 1))
		do i = 1, IM_NDIM(im1)
		    shifts[i] = 0.5d0 - double (Memi[blkfac+i-1]) / 2.0d0
		call mw_shift (mw, shifts, (2 ** IM_NDIM(im1) - 1))
		call mw_saveim (mw, im2)
		call mw_close (mw)
	    }

	    call imunmap (im2)
	    call imunmap (im1)

	    call xt_delimtemp (Memc[image2], Memc[image3])
	}

	call imtclose (list1)
	call imtclose (list2)
	call sfree (sp)
end
