# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imhdr.h>

# PLT_GETDATA -- Decrease resolution of image by either subsampling
# or block averaging.  A pointer to the data values to be plotted is
# returned, as well as the number of columns and lines in the data matrix.

procedure plt_getdata (im, subsample, imsect, x_sample, y_sample, subras,
	ncols, nlines, preserve)

pointer	im			# Pointer to image header
int	subsample		# Subsample versus block average (yes/no)?
char	imsect[SZ_FNAME]	# Image section being plotted
int	x_sample, y_sample	# x and y reduction factors
pointer	subras			# Pointer to data values to be plotted
int	ncols			# Number of rows in data matrix
int	nlines			# Number of lines in data matrix
bool	preserve		# Preserve aspect ratio (yes/no)?

pointer	si
int	nsx, nsy, i, si_xfact, si_yfact
int	x_factor, y_factor
pointer	immap(), imgs2r(), sigl2_setup(), sigl2r()
errchk	immap, imunmap, sigl2_setup, sigl2r, sigl2_free

begin
	# Subsampling or block averaging factors are depend on whether or
	# not the image aspect ratio is to be preserved

	if (preserve) {
	    x_factor = max (x_sample, y_sample)
	    y_factor = max (x_sample, y_sample)
	} else {
	    x_factor = x_sample
	    y_factor = y_sample
	}

	# Image can be either subsampled or block averaged
	if (subsample == YES) {
            call imunmap (im)

	    call plt_mergsect (imsect, x_factor, y_factor, imsect)
            im = immap (imsect, READ_ONLY, 0)

            ncols = IM_LEN(im, 1)
            nlines = IM_LEN(im, 2)
            call eprintf ("Subsampled image section being plotted: %s\n")
	        call pargstr (imsect)
            subras = imgs2r (im, 1, ncols, 1, nlines)

        } else {
	    # Block average - later, this will be an integer factor.  For
	    # now, just make block averaging factor equal in x and y.

	    nsx = (ncols  + x_factor - 1) / x_factor
	    nsy = (nlines + y_factor - 1) / y_factor
	    si_xfact = INDEFI
	    si_yfact = INDEFI

	    si = sigl2_setup (im, 1., real (ncols), nsx, si_xfact, 1., 
	        real (nlines), nsy, si_yfact)

	    call eprintf ("Image %s block averaged by %d in x and %d in y\n")
	        call pargstr (imsect)
	        call pargi (si_xfact)
	        call pargi (si_yfact)
	    call malloc (subras, nsx * nsy, TY_REAL)

	    do i = 1, nsy 
                call amovr (Memr[sigl2r (si, i)], Memr[subras + (i-1)*nsx], nsx)

	    ncols = nsx
	    nlines = nsy
	    call sigl2_free (si)
	}
end


#  PL_MERGSECT -- Merge image section specifications.
#  Build a new image section from a specification on an image name and
#  include additional subsampling. 

procedure plt_mergsect (oimage, xsub, ysub, nimage)

char	oimage[SZ_PATHNAME]	# Input image name
int	xsub, ysub		# Additional subsampling
char	nimage[SZ_PATHNAME]	# New image name

pointer	sp, file, ins, news, grp, ker
int	grpind, grpsize
int	x1[2], x2[2], step[2]
int	ip, i

int	stridxs()

begin
	call smark (sp)
	call salloc (file, SZ_PATHNAME, TY_CHAR)
	call salloc (ins,  SZ_PATHNAME, TY_CHAR)
	call salloc (ker,  SZ_PATHNAME, TY_CHAR)
	call salloc (news, SZ_PATHNAME, TY_CHAR)
	call salloc (grp,  SZ_PATHNAME, TY_CHAR)

	if (stridxs ("[", oimage) == 0) {
	    # No image section
	    if (xsub > 1 || ysub > 1) {
		# Subsampling
		call sprintf (Memc[ins], SZ_PATHNAME, "%s[*:%d,*:%d]")
		    call pargstr (oimage)
		    call pargi (xsub)
		    call pargi (ysub)
		call strcpy (Memc[ins], nimage, SZ_PATHNAME)
	    } else
		# No subsampling
		call strcpy (oimage, nimage, SZ_PATHNAME)

	    call sfree (sp)
	    return
	}

	# Parse the full image specification into its component parts.
	call imparse (oimage, Memc[file], SZ_PATHNAME, Memc[ker], SZ_PATHNAME,
	    Memc[ins], SZ_PATHNAME, grpind, grpsize)

	ip = 2
	# Decode the section, yielding the start, end, and step vectors
	for (i = 1;  i <=2 && Memc[ins+ip-1] != ']';  i = i + 1)
	    call im_decode_subscript (Memc[ins], ip, x1[i], x2[i], step[i])

	# Merge subsampling
	step[1] = step[1] * xsub
	step[2] = step[2] * ysub

	if (grpind > 0) {
	    # Group member
	    if (grpsize == 0) {
		# No group size
		call sprintf (Memc[grp], SZ_PATHNAME, "[%d]")
		    call pargi (grpind)
	    } else {
		call sprintf (Memc[grp], SZ_PATHNAME, "[%d/%d]")
		    call pargi (grpind)
		    call pargi (grpsize)
	    }

	    call strcat (Memc[grp], Memc[file], SZ_PATHNAME)
	}

	# Build the new image section
	call sprintf (Memc[news], SZ_PATHNAME, "[%d:%d:%d,%d:%d:%d]")
	    call pargi (x1[1])
	    call pargi (x2[1])
	    call pargi (step[1])
	    call pargi (x1[2])
	    call pargi (x2[2])
	    call pargi (step[2])
	call strcat (Memc[news], Memc[file], SZ_PATHNAME)

	call strcpy (Memc[file], nimage, SZ_PATHNAME]
	call sfree (sp)
end

