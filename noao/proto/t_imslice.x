include <error.h>
include <imhdr.h>
include <fset.h>

# T_IMSLICE -- Slice an input image into a list of output images equal in
# length  to the length of the highest dimension of the input image. The
# remaining dimensions are unchanged. For a 1 dimensionsal image this
# task is a null operation.

procedure t_imslice()

pointer imtlist1                # Input image list
pointer imtlist2                # Output image list

pointer image1                  # Input image
pointer image2                  # Output image

int     list1, list2, verbose
pointer sp
bool    clgetb()
int     imtopen(), imtgetim(), imtlen(), btoi()

errchk	sl_slice()

begin
        call fseti (STDOUT, F_FLUSHNL, YES)

        # Allocate temporary space.
        call smark (sp)
        call salloc (imtlist1, SZ_FNAME, TY_CHAR)
        call salloc (imtlist2, SZ_FNAME, TY_CHAR)
        call salloc (image1, SZ_FNAME, TY_CHAR)
        call salloc (image2, SZ_FNAME, TY_CHAR)

        # Get task parameters.
        call clgstr ("input", Memc[imtlist1], SZ_FNAME)
        call clgstr ("output", Memc[imtlist2], SZ_FNAME)
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
		call sl_imslice (Memc[image1], Memc[image2], verbose)

        call imtclose (list1)
        call imtclose (list2)

        call sfree (sp)
end


# SL_IMSLICE -- Procedure to slice an n-dimensional image into a set
# of images with one fewer dimensions. A number is appendend to the
# output image name indicating which element of the n-th dimension the
# new image originated from.

procedure sl_imslice (image1, image2, verbose)

char	image1[ARB]		# input image
char	image2[ARB]		# output image
int	verbose			# verbose mode

int	i, j, ndim, ncols, nlout, nimout
pointer	sp, imname, insect, outsect, im1, im2, buf1, buf2, vim1, vim2
int	imgnls(), imgnli(), imgnll(), imgnlr(), imgnld(), imgnlx()
int	impnls(), impnli(), impnll(), impnlr(), impnld(), impnlx()
pointer	immap()

errchk	imgnls(), imgnli(), imgnll(), imgnlr(), imgnld(), imgnlx()
errchk	impnls(), impnli(), impnll(), impnlr(), impnld(), impnlx()

begin
	iferr (im1 = immap (image1, READ_ONLY, 0)) {
	    call erract (EA_WARN)
	    return
	}

	ndim = IM_NDIM(im1)
	if (ndim == 1) {
	    call printf ("Cannot slice 1D image: %s\n")
		call pargstr (image1)
	    call imunmap (im1)
	    return
	}

	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (insect, SZ_LINE, TY_CHAR)
	call salloc (outsect, SZ_LINE, TY_CHAR)
	call salloc (vim1, IM_MAXDIM, TY_LONG)
	call salloc (vim2, IM_MAXDIM, TY_LONG)

	# Compute the number of output images and the number of columns
	# and lines in the output image.
	nimout = IM_LEN(im1, ndim)
	ncols = IM_LEN(im1,1)
	nlout = 1
	do i = 2, ndim
	    nlout = nlout * IM_LEN(im1,i)
	nlout = nlout / nimout
		

	call imgcluster (image2, image2, SZ_FNAME)
	call amovkl (long(1), Meml[vim1], IM_MAXDIM)
	do i = 1, nimout {

	    # Construct the output image name.
	    call sprintf (Memc[imname], SZ_FNAME, "%s%03d")
		call pargstr (image2)
		call pargi (i)

	    # Open the output image.
	    iferr (im2 = immap (Memc[imname], NEW_COPY, im1)) {
		call erract (EA_WARN)
		call imunmap (im1)
		call sfree (sp)
		return
	    }
	    IM_NDIM(im2) = ndim - 1

	    # Print messages on the screen.
	    if (verbose == YES) {
		call sl_em1section (im1, i, Memc[insect], SZ_LINE)
		call sl_esection (im2, Memc[outsect], SZ_LINE)
	        call printf ("Copied image %s %s -> %s %s\n")
		    call pargstr (image1)
		    call pargstr (Memc[insect])
		    call pargstr (Memc[imname])
		    call pargstr (Memc[outsect])
	    }

	    # Loop over the appropriate range of lines.
	    call amovkl (long(1), Meml[vim2], IM_MAXDIM)
	    switch (IM_PIXTYPE(im1)) {
	    case TY_SHORT:
		do j = 1, nlout {
		    if (impnls (im2, buf2, Meml[vim2]) != ncols)
			call error (0, "Error writing output image.")
		    if (imgnls (im1, buf1, Meml[vim1]) != ncols)
			call error (0, "Error reading input image.")
		   call amovs (Mems[buf1], Mems[buf2], ncols) 
		}
	    case TY_USHORT, TY_INT:
		do j = 1, nlout {
		    if (impnli (im2, buf2, Meml[vim2]) != ncols)
			call error (0, "Error writing output image.")
		    if (imgnli (im1, buf1, Meml[vim1]) != ncols)
			call error (0, "Error reading input image.")
		   call amovi (Memi[buf1], Memi[buf2], ncols) 
		}
	    case TY_LONG:
		do j = 1, nlout {
		    if (impnll (im2, buf2, Meml[vim2]) != ncols)
			call error (0, "Error writing output image.")
		    if (imgnll (im1, buf1, Meml[vim1]) != ncols)
			call error (0, "Error reading input image.")
		   call amovl (Meml[buf1], Meml[buf2], ncols) 
		}
	    case TY_REAL:
		do j = 1, nlout {
		    if (impnlr (im2, buf2, Meml[vim2]) != ncols)
			call error (0, "Error writing output image.")
		    if (imgnlr (im1, buf1, Meml[vim1]) != ncols)
			call error (0, "Error reading input image.")
		   call amovr (Memr[buf1], Memr[buf2], ncols) 
		}
	    case TY_DOUBLE:
		do j = 1, nlout {
		    if (impnld (im2, buf2, Meml[vim2]) != ncols)
			call error (0, "Error writing output image.")
		    if (imgnld (im1, buf1, Meml[vim1]) != ncols)
			call error (0, "Error reading input image.")
		   call amovd (Memd[buf1], Memd[buf2], ncols) 
		}
	    case TY_COMPLEX:
		do j = 1, nlout {
		    if (impnlx (im2, buf2, Meml[vim2]) != ncols)
			call error (0, "Error writing output image.")
		    if (imgnlx (im1, buf1, Meml[vim1]) != ncols)
			call error (0, "Error reading input image.")
		   call amovx (Memx[buf1], Memx[buf2], ncols) 
		}
	    }

	    call imunmap (im2)
	}

	call imunmap (im1)
	call sfree (sp)
end


# SL_EM1SECTION -- Encode the dimensions of an image where the element of
# the last dimension is fixed in section notation.

procedure  sl_em1section (im, el, section, maxch)

pointer	im		# pointer to the image
int	el		# element of last dimension
char	section[ARB]	# output section
int	maxch		# maximum number of characters in output section

int	i, ip
int	ltoc()

begin
	ip = 1
	section[1] = '['
	ip = ip + 1

	for (i = 1; i <= IM_NDIM(im) - 1; i = i + 1) {
	    ip = ip + ltoc (long(1), section[ip], maxch)
	    section[ip] = ':'
	    ip = ip + 1
	    ip = ip + ltoc (IM_LEN(im,i), section[ip], maxch)
	    section[ip] = ','
	    ip = ip + 1
	}

	ip = ip + ltoc (el, section[ip], maxch)
	section[ip] = ']'
	section [ip+1] = EOS
end


# SL_ESECTION -- Encode the dimensions of an image in section notation.

procedure  sl_esection (im, section, maxch)

pointer	im		# pointer to the image
char	section[ARB]	# output section
int	maxch		# maximum number of characters in output section

int	i, ip
int	ltoc()

begin
	ip = 1
	section[1] = '['
	ip = ip + 1

	for (i = 1; i <= IM_NDIM(im); i = i + 1) {
	    ip = ip + ltoc (long(1), section[ip], maxch)
	    section[ip] = ':'
	    ip = ip + 1
	    ip = ip + ltoc (IM_LEN(im,i), section[ip], maxch)
	    section[ip] = ','
	    ip = ip + 1
	}

	section[ip-1] = ']'
	section [ip] = EOS

end
