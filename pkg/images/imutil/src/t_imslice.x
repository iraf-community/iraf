# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <imhdr.h>
include <ctype.h>
include <mwset.h>

# T_IMSLICE -- Slice an input image into a list of output images equal in
# length  to the length of the dimension to be sliced. The remaining
# dimensions are unchanged. For a 1 dimensionsal image this task is a null
# operation.

procedure t_imslice()

pointer imtlist1                # Input image list
pointer imtlist2                # Output image list
pointer image1                  # Input image
pointer image2                  # Output image
int	sdim			# Dimension to be sliced
int	verbose			# Verbose mode

pointer sp
int     list1, list2

bool    clgetb()
int     imtopen(), imtgetim(), imtlen(), btoi(), clgeti()
errchk	sl_slice

begin
        call smark (sp)
        call salloc (imtlist1, SZ_FNAME, TY_CHAR)
        call salloc (imtlist2, SZ_FNAME, TY_CHAR)
        call salloc (image1, SZ_FNAME, TY_CHAR)
        call salloc (image2, SZ_FNAME, TY_CHAR)

        # Get task parameters.
        call clgstr ("input", Memc[imtlist1], SZ_FNAME)
        call clgstr ("output", Memc[imtlist2], SZ_FNAME)
	sdim = clgeti ("slice_dimension")
        verbose = btoi (clgetb ("verbose"))

        list1 = imtopen (Memc[imtlist1])
        list2 = imtopen (Memc[imtlist2])
        if (imtlen (list1) != imtlen (list2)) {
            call imtclose (list1)
            call imtclose (list2)
            call error (0, "Number of input and output images not the same.")
        }

        # Loop over the set of input and output images
        while ((imtgetim (list1, Memc[image1], SZ_FNAME) != EOF) &&
            (imtgetim (list2, Memc[image2], SZ_FNAME) != EOF))
	    call sl_imslice (Memc[image1], Memc[image2], sdim, verbose)

        call imtclose (list1)
        call imtclose (list2)

        call sfree (sp)
end


# SL_IMSLICE -- Procedure to slice an n-dimensional image into a set
# of images with one fewer dimensions. A number is appendend to the
# output image name indicating which element of the n-th dimension the
# new image originated from.

procedure sl_imslice (image1, image2, sdim, verbose)

char	image1[ARB]		# input image
char	image2[ARB]		# output image
int	sdim			# slice dimension
int	verbose			# verbose mode

int	i, j, ndim, fdim, ncols, nlout, nimout, pdim
int	axno[IM_MAXDIM], axval[IM_MAXDIM]
pointer	sp, inname, outname, outsect, im1, im2, buf1, buf2, vim1, vim2
pointer	mw, vs, ve
real	shifts[IM_MAXDIM]

pointer	immap(), mw_openim()
int	mw_stati()
int	imgnls(), imgnli(), imgnll(), imgnlr(), imgnld(), imgnlx()
int	imggss(), imggsi(), imggsl(), imggsr(), imggsd(), imggsx()
int	impnls(), impnli(), impnll(), impnlr(), impnld(), impnlx()
bool	envgetb()

errchk	imgnls(), imgnli(), imgnll(), imgnlr(), imgnld(), imgnlx()
errchk	imggss(), imggsi(), imggsl(), imggsr(), imggsd(), imggsx()
errchk	impnls(), impnli(), impnll(), impnlr(), impnld(), impnlx()

begin
	iferr (im1 = immap (image1, READ_ONLY, 0)) {
	    call erract (EA_WARN)
	    return
	}

	ndim = IM_NDIM(im1)

	# Check that sdim is in range.
	if (sdim > ndim) {
	    call printf ("Image %s has fewer than %d dimensions.\n")
		call pargstr (image1)
		call pargi (sdim)
	    call imunmap (im1)
	    return
	}

	# Cannot slice 1D images.
	if (ndim == 1) {
	    call printf ("Image %s is 1 dimensional.\n")
		call pargstr (image1)
	    call imunmap (im1)
	    return
	}

	# Cannot slice an image which is degnerate in slice dimension.
	#if (IM_LEN(im1,sdim) == 1) {
	    #call printf ("Image %s is degenerate in the %d dimension.\n")
		#call pargstr (image1)
		#call pargi (sdim)
	    #call imunmap (im1)
	    #return
	#}

	call smark (sp)
	call salloc (inname, SZ_LINE, TY_CHAR)
	call salloc (outname, SZ_FNAME, TY_CHAR)
	call salloc (outsect, SZ_LINE, TY_CHAR)

	call salloc (vs, IM_MAXDIM, TY_LONG)
	call salloc (ve, IM_MAXDIM, TY_LONG)
	call salloc (vim1, IM_MAXDIM, TY_LONG)
	call salloc (vim2, IM_MAXDIM, TY_LONG)

	# Compute the number of output images. and the number of columns
	nimout = IM_LEN(im1, sdim)

	# Compute the number of lines and columns in the output image.
	if (sdim == 1) {
	    fdim = 2
	    ncols = IM_LEN(im1,2)
	} else {
	    fdim = 1
	    ncols = IM_LEN(im1,1)
	}
	nlout = 1
	do i = 1, sdim - 1
	    nlout = nlout * IM_LEN(im1,i)
	do i = sdim + 1, ndim
	    nlout = nlout * IM_LEN(im1,i)
	nlout = nlout / ncols 

	call amovkl (long(1), Meml[vim1], IM_MAXDIM)
	do i = 1, nimout {

	    # Construct the output image name.
	    call sprintf (Memc[outname], SZ_FNAME, "%s%03d")
		call pargstr (image2)
		call pargi (i)

	    # Open the output image.
	    iferr (im2 = immap (Memc[outname], NEW_COPY, im1)) {
		call erract (EA_WARN)
		call imunmap (im1)
		call sfree (sp)
		return
	    } else {
	        IM_NDIM(im2) = ndim - 1
		do j = 1, sdim - 1
		    IM_LEN(im2,j) = IM_LEN(im1,j)
		do j = sdim + 1, IM_NDIM(im1)
		    IM_LEN(im2,j-1) = IM_LEN(im1,j)
	    }

	    # Print messages on the screen.
	    if (verbose == YES) {
		call sl_einsection (im1, i, sdim, Memc[inname], SZ_LINE)
		call sl_esection (im2, Memc[outsect], SZ_LINE)
	        call printf ("Copied image %s %s -> %s %s\n")
		    call pargstr (image1)
		    call pargstr (Memc[inname])
		    call pargstr (Memc[outname])
		    call pargstr (Memc[outsect])
		call flush (STDOUT)
	    }

	    # Initialize the v vectors for each new image.
	    if (sdim != ndim) {
	        do j = 1, ndim {
		    if (j == sdim) {
		        Meml[vs+j-1] = i
		        Meml[ve+j-1] = i
		    } else if (j == fdim) {
		        Meml[vs+j-1] = 1
		        Meml[ve+j-1] = IM_LEN(im1,j)
		    } else {
		        Meml[vs+j-1] = 1
		        Meml[ve+j-1] = 1
		    }
	        }
	    }

	    # Loop over the appropriate range of lines.
	    call amovkl (long(1), Meml[vim2], IM_MAXDIM)
	    switch (IM_PIXTYPE(im1)) {
	    case TY_SHORT:
		if (sdim == ndim) {
		    do j = 1, nlout {
		        if (impnls (im2, buf2, Meml[vim2]) == EOF)
			    call error (0, "Error writing output image.")
		        if (imgnls (im1, buf1, Meml[vim1]) == EOF)
			    call error (0, "Error reading input image.")
		       call amovs (Mems[buf1], Mems[buf2], ncols) 
		    }
		} else {
		    do j = 1, nlout {
		        if (impnls (im2, buf2, Meml[vim2]) == EOF)
			    call error (0, "Error writing output image.")
			buf1 = imggss (im1, Meml[vs], Meml[ve], IM_NDIM(im1))
			if (buf1 == EOF)
			    call error (0, "Error reading input image.")
		       call amovs (Mems[buf1], Mems[buf2], ncols) 
		       call sl_loop (Meml[vs], Meml[ve], IM_LEN(im1,1), fdim,
		           sdim, ndim)
		    }
		}
	    case TY_USHORT, TY_INT:
		if (sdim == ndim) {
		    do j = 1, nlout {
		        if (impnli (im2, buf2, Meml[vim2]) == EOF)
			    call error (0, "Error writing output image.")
		        if (imgnli (im1, buf1, Meml[vim1]) == EOF)
			    call error (0, "Error reading input image.")
		       call amovi (Memi[buf1], Memi[buf2], ncols) 
		    }
		} else {
		    do j = 1, nlout {
		        if (impnli (im2, buf2, Meml[vim2]) == EOF)
			    call error (0, "Error writing output image.")
			buf1= imggsi (im1, Meml[vs], Meml[ve], IM_NDIM(im1))
		        if (buf1 == EOF)
			    call error (0, "Error reading input image.")
		       call amovi (Memi[buf1], Memi[buf2], ncols) 
		       call sl_loop (Meml[vs], Meml[ve], IM_LEN(im1,1), fdim,
		           sdim, ndim)
		    }
		}
	    case TY_LONG:
		if (sdim == ndim) {
		    do j = 1, nlout {
		        if (impnll (im2, buf2, Meml[vim2]) == EOF)
			    call error (0, "Error writing output image.")
		        if (imgnll (im1, buf1, Meml[vim1]) == EOF)
			    call error (0, "Error reading input image.")
		       call amovl (Meml[buf1], Meml[buf2], ncols) 
		    }
		} else {
		    do j = 1, nlout {
		        if (impnll (im2, buf2, Meml[vim2]) == EOF)
			    call error (0, "Error writing output image.")
			buf1 = imggsl (im1, Meml[vs], Meml[ve], IM_NDIM(im1))
		        if (buf1 == EOF)
			    call error (0, "Error reading input image.")
		       call amovl (Meml[buf1], Meml[buf2], ncols) 
		       call sl_loop (Meml[vs], Meml[ve], IM_LEN(im1,1), fdim,
		           sdim, ndim)
		    }
		}
	    case TY_REAL:
		if (sdim == ndim) {
		    do j = 1, nlout {
		        if (impnlr (im2, buf2, Meml[vim2]) == EOF)
			    call error (0, "Error writing output image.")
		        if (imgnlr (im1, buf1, Meml[vim1]) == EOF)
			    call error (0, "Error reading input image.")
		       call amovr (Memr[buf1], Memr[buf2], ncols) 
		    }
		} else {
		    do j = 1, nlout {
		        if (impnlr (im2, buf2, Meml[vim2]) == EOF)
			    call error (0, "Error writing output image.")
			buf1 = imggsr (im1, Meml[vs], Meml[ve], IM_NDIM(im1))
		        if (buf1 == EOF)
			    call error (0, "Error reading input image.")
		       call amovr (Memr[buf1], Memr[buf2], ncols) 
		       call sl_loop (Meml[vs], Meml[ve], IM_LEN(im1,1),
		           fdim, sdim, ndim)
		    }
		}
	    case TY_DOUBLE:
		if (sdim == ndim) {
		    do j = 1, nlout {
		        if (impnld (im2, buf2, Meml[vim2]) == EOF)
			    call error (0, "Error writing output image.")
		        if (imgnld (im1, buf1, Meml[vim1]) == EOF)
			    call error (0, "Error reading input image.")
		       call amovd (Memd[buf1], Memd[buf2], ncols) 
		    }
		} else {
		    do j = 1, nlout {
		        if (impnld (im2, buf2, Meml[vim2]) == EOF)
			    call error (0, "Error writing output image.")
			buf1 = imggsd (im1, Meml[vs], Meml[ve], IM_NDIM(im1))
		        if (buf1 == EOF)
			    call error (0, "Error reading input image.")
		       call amovd (Memd[buf1], Memd[buf2], ncols) 
		       call sl_loop (Meml[vs], Meml[ve], IM_LEN(im1,1), fdim,
		           sdim, ndim)
		    }
		}
	    case TY_COMPLEX:
		if (sdim == ndim) {
		    do j = 1, nlout {
		        if (impnlx (im2, buf2, Meml[vim2]) == EOF)
			    call error (0, "Error writing output image.")
		        if (imgnlx (im1, buf1, Meml[vim1]) == EOF)
			    call error (0, "Error reading input image.")
		       call amovx (Memx[buf1], Memx[buf2], ncols) 
		    }
		} else {
		    do j = 1, nlout {
		        if (impnlx (im2, buf2, Meml[vim2]) == EOF)
			    call error (0, "Error writing output image.")
			buf1 = imggsx (im1, Meml[vs], Meml[ve], IM_NDIM(im1))
		        if (buf1 == EOF)
			    call error (0, "Error reading input image.")
		       call amovx (Memx[buf1], Memx[buf2], ncols) 
		       call sl_loop (Meml[vs], Meml[ve], IM_LEN(im1,1), fdim,
		           sdim, ndim)
		    }
		}
	    }

	    # Update the wcs.
	    if (! envgetb ("nowcs")) {

		# Open and shift the wcs.
	        mw = mw_openim (im1)
	        call aclrr (shifts, ndim)
		shifts[sdim] = -(i - 1)
	        call mw_shift (mw, shifts, (2 ** ndim - 1))

		# Get and reset the axis map.
		pdim = mw_stati (mw, MW_NPHYSDIM)
		call mw_gaxmap (mw, axno, axval, pdim)
		do j = 1, pdim {
		    if (axno[j] < sdim) {
			next
		    } else if (axno[j] > sdim) {
			axno[j] = axno[j] - 1
		    } else {
		        axno[j] = 0
		        axval[j] = i - 1
		    }
		}
		call mw_saxmap (mw, axno, axval, pdim)

	        call mw_savim (mw, im2)
	        call mw_close (mw)
	    }

	    call imunmap (im2)
	}


	call imunmap (im1)
	call sfree (sp)
end


# SL_LOOP -- Increment the vector V from VS to VE (nested do loops cannot
# be used because of the variable number of dimensions).

procedure sl_loop (vs, ve, ldim, fdim, sdim, ndim)

long	vs[ndim]		# vector of starting points
long	ve[ndim]		# vector of ending points
long	ldim[ndim]		# vector of dimension lengths
int	fdim			# first dimension
int	sdim			# slice dimension
int	ndim			# number of dimensions

int	dim

begin
	for (dim = fdim+1;  dim <= ndim;  dim = dim + 1) {
	    if (dim == sdim)
		next
	    vs[dim] = vs[dim] + 1
	    ve[dim] = vs[dim]
	    if (vs[dim] - ldim[dim] == 1) {
		if (dim < ndim) {
		    vs[dim] = 1
		    ve[dim] = 1
		} else
		    break
	    } else
		break
	}
end


# SL_EINSECTION -- Encode the dimensions of an image where the element of
# the slice dimension is fixed in section notation.

procedure  sl_einsection (im, el, sdim, section, maxch)

pointer	im		# pointer to the image
int	el		# element of last dimension
int	sdim		# slice dimension
char	section[ARB]	# output section
int	maxch		# maximum number of characters in output section

int	i, op
int	ltoc(), gstrcat()

begin
	op = 1
	section[1] = '['
	op = op + 1

	# Encode dimensions up to the slice dimension.
	for (i = 1; i <= sdim - 1 && op <= maxch; i = i + 1) {
	    op = op + ltoc (long(1), section[op], maxch)
	    op = op + gstrcat (":", section[op], maxch)
	    op = op + ltoc (IM_LEN(im,i), section[op], maxch)
	    op = op + gstrcat (",", section[op], maxch)
	}

	# Encode the slice dimension.
	op = op + ltoc (el, section[op], maxch)
	op = op + gstrcat (",", section[op], maxch)

	# Encode dimensions above the slice dimension.
	for (i = sdim + 1; i <= IM_NDIM(im); i = i + 1) {
	    op = op + ltoc (long(1), section[op], maxch)
	    op = op + gstrcat (":", section[op], maxch)
	    op = op + ltoc (IM_LEN(im,i), section[op], maxch)
	    op = op + gstrcat (",", section[op], maxch)
	}

	section[op-1] = ']'
	section[op] = EOS
end


# SL_ESECTION -- Encode the dimensions of an image in section notation.

procedure  sl_esection (im, section, maxch)

pointer	im		# pointer to the image
char	section[ARB]	# output section
int	maxch		# maximum number of characters in output section

int	i, op
int	ltoc(), gstrcat()

begin
	op = 1
	section[1] = '['
	op = op + 1

	for (i = 1; i <= IM_NDIM(im); i = i + 1) {
	    op = op + ltoc (long(1), section[op], maxch)
	    op = op + gstrcat (":", section[op], maxch)
	    op = op + ltoc (IM_LEN(im,i), section[op], maxch)
	    op = op + gstrcat (",", section[op], maxch)
	}

	section[op-1] = ']'
	section[op] = EOS
end
