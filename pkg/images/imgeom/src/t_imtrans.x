# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<error.h>
include <mwset.h>

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
		call imtrmw (mw)
		call mw_saveim (mw, im2)
		call mw_close (mw)
	    }

	    # Unmap the input and output images.
	    call imunmap (im2)
	    call imunmap (im1)

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
		case TY_USHORT, TY_LONG:
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

define	LTM	Memd[ltr+(($2)-1)*pdim+($1)-1]
define	NCD	Memd[ncd+(($2)-1)*pdim+($1)-1]
define	swap    {temp=$1;$1=$2;$2=temp}


# IMTRMW -- Perform a transpose operation on the image WCS.

procedure imtrmw (mw)

pointer	mw			# pointer to the mwcs structure

int	i, axes[IM_MAXDIM], axval[IM_MAXDIM]
int	naxes, pdim, nelem, axmap, ax1, ax2, szatstr
pointer	sp, ltr, ltm, ltv, cd, r, w, ncd, nr
pointer	attribute1, attribute2, atstr1, atstr2, mwtmp
double	temp
int	mw_stati(), itoc(), strlen()
pointer	mw_open()
errchk	mw_gwattrs(), mw_newsystem()

begin
	# Convert axis bitflags to the axis lists.
	call mw_gaxlist (mw, 03B, axes, naxes)
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
	swap (NCD(ax1,ax1), NCD(ax2,ax2))
	swap (NCD(ax1,ax2), NCD(ax2,ax1))
	swap (Memd[w+ax1-1], Memd[w+ax2-1])
	swap (Memd[nr+ax1-1], Memd[nr+ax2-1])

	# Perform the transpose of the lterm.
	call mw_mkidmd (Memd[ltr], pdim)
	LTM(ax1,ax1) = 0.0d0
	LTM(ax1,ax2) = 1.0d0
	LTM(ax2,ax1) = 1.0d0
	LTM(ax2,ax2) = 0.0d0
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
	call mw_swtype (mwtmp, ax1, 1, Memc[attribute2], "")
	call mw_swtype (mwtmp, ax2, 1, Memc[attribute1], "")

	# Copy the axis attributes.
	call malloc (atstr1, szatstr, TY_CHAR)
	call malloc (atstr2, szatstr, TY_CHAR)
	for (i =  1; ; i = i + 1) {

	    if (itoc (i, Memc[attribute1], SZ_FNAME) <= 0)
		Memc[attribute1] = EOS
	    if (itoc (i, Memc[attribute2], SZ_FNAME) <= 0)
		Memc[attribute2] = EOS

	    repeat {
		iferr (call mw_gwattrs (mw, ax1, Memc[attribute1],
		    Memc[atstr1], szatstr))
		    Memc[atstr1] = EOS
		iferr (call mw_gwattrs (mw, ax2, Memc[attribute2],
		    Memc[atstr2], szatstr))
		    Memc[atstr2] = EOS
		if ((strlen (Memc[atstr1]) < szatstr) &&
		    (strlen (Memc[atstr2]) < szatstr))
		    break
		szatstr = szatstr + SZ_LINE
		call realloc (atstr1, szatstr, TY_CHAR)
		call realloc (atstr2, szatstr, TY_CHAR)
	    }
	    if ((Memc[atstr1] == EOS) && (Memc[atstr2] == EOS))
	        break

	    if (Memc[atstr2] != EOS)
	        call mw_swattrs (mwtmp, ax1, Memc[attribute2], Memc[atstr2])
	    if (Memc[atstr1] != EOS)
	        call mw_swattrs (mwtmp, ax2, Memc[attribute1], Memc[atstr1])
	}
	call mfree (atstr1, TY_CHAR)
	call mfree (atstr2, TY_CHAR)
	call mw_close (mw)

	# Delete the old wcs and set equal to the new one.
	call sfree (sp)
	mw = mwtmp
	call mw_seti (mw, MW_USEAXMAP, axmap)
end
