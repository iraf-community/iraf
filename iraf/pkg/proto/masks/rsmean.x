include <imhdr.h>
include "rskysub.h"

# RS_MSUB -- Perform a running mean sky subtraction on a list of images
# with no masking or rejection.

procedure rs_msub (inlist, outlist, rs, cache, verbose)

pointer	inlist			#I the input image list
pointer	outlist			#I the output image list
pointer	rs			#I the sky subtraction descriptor
bool	cache			#I cache temp image buffer in memory ?
bool	verbose			#I print task statistics

size_t	sz_val
real	fscale
pointer	sp, image, outimage, tmpimage, str
pointer	im, outim, tmpim
int	i, nimages, nlo, nhi, ostart, ofinish, start, finish, imno
int	first, last
size_t	oldsize, bufsize
pointer	immap()
int	imtlen(), imtrgetim(), btoi(), imaccess()
errchk	immap()
include	<nullptr.inc>

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (image, sz_val, TY_CHAR)
	call salloc (outimage, sz_val, TY_CHAR)
	call salloc (tmpimage, sz_val, TY_CHAR)
	call salloc (str, sz_val, TY_CHAR)

	# Check image status. If resubtract is yes then delete the output
	# images if they already exist. Otherwise determine whether the
	# images already exist and if so whether or not they need to be
	# sky subtracted again.

	nimages = imtlen (inlist)
	if (RS_RESUBTRACT(rs) == NO) {
	    first = 0
	    last = 0
	    do i = 1, nimages {
	        if (imtrgetim (outlist, i, Memc[outimage], SZ_FNAME) == EOF)
		    break
		if (imaccess (Memc[outimage], 0) == NO) {
		    if (first == 0) {
			first = i
			last = i
		    } else
			last = i
		} else {
		    outim = immap (Memc[outimage], READ_ONLY, NULLPTR)
		    iferr (call imgstr (outim, RS_KYSKYSUB(rs), Memc[str],
		        SZ_FNAME)) {
			if (first == 0) {
			    first = i
			    last = i
			} else
			    last = i
		    }
		    call imunmap (outim)
		}
	    }
	} else {
	    first = 1
	    last = nimages
	    do i = 1, nimages {
	        if (imtrgetim (outlist, i, Memc[outimage], SZ_FNAME) == EOF)
		    break
		if (imaccess (Memc[outimage], 0) == YES)
		    call imdelete (Memc[outimage])
	    }
	}

	# Check the sky subtraction status.
	if (first <= 0 && last <= 0) {
	    if (verbose) {
		call printf (
		    "The output images have already been sky subtracted \n")
	    }
	    call sfree (sp)
	    return
	}

	# Create the temporary image.
	call mktemp ("_rsum", Memc[tmpimage], SZ_FNAME)
	tmpim = immap (Memc[tmpimage], NEW_IMAGE, NULLPTR)

	# Compute the sliding mean parameters.
	nlo = RS_NCOMBINE(rs) / 2
	nhi = RS_NCOMBINE(rs) - nlo

	# Loop over the images.
	ostart = 0
	ofinish = 0
	do imno = 1, nimages {

	    # Skip over beginning and ending images that have already been
	    # sky subtracted.

	    if (imno < first || imno > last) {
		if (verbose) {
	            if (imtrgetim (outlist, imno, Memc[outimage],
		        SZ_FNAME) == EOF) {
			call printf (
			    "The sky subtracted image %s already exists\n")
			    call pargstr (Memc[outimage])
		    }
		}
		next
	    }

	    # Determine which images will contribute to the sky image.
	    # Start and finish set the endpoints of the sequence. Imno
	    # is the current image which is to be sky subtracted.

	    if ((imno - nlo) < 1) {
	        start = 1
	        finish = min (nimages, max (2 * imno - 1, RS_NMIN(rs) + 1))
	    } else if ((imno + nhi) > nimages) {
	        start = max (1, min (2 * imno - nimages, nimages - RS_NMIN(rs)))
	        finish = nimages
	    } else {
	        start = imno - nlo
	        finish = imno + nhi
	    }

	    # Check that the minimum number of images exists.
	    if ((finish - start) < RS_NMIN(rs)) {
	        call eprintf ("There are too few images for sky subtraction\n")
		break
	    }

	    # Open the current input image.
	    if (imtrgetim (inlist, imno, Memc[image], SZ_FNAME) == EOF) {
	        call eprintf ("Error reading input image list\n")
		break
	    }
	    iferr (im = immap (Memc[image], READ_ONLY, NULLPTR)) {
	        call eprintf ("Error opening input image %s\n")
		    call pargstr (Memc[image])
		break
	    }

	    # Open the output image.
	    if (imtrgetim (outlist, imno, Memc[outimage], SZ_FNAME) == EOF) {
	        call eprintf ("Error reading output image list\n")
		call imunmap (im)
		break
	    }
	    iferr (outim = immap (Memc[outimage], NEW_COPY, im)) {
	        call eprintf ("Error opening output image %s\n")
		    call pargstr (Memc[outimage])
		call imunmap (im)
		break
	    }

	    if (verbose) {
		call printf ("Sky subtracting image %s and writing to %s\n")
		    call pargstr (Memc[image])
		    call pargstr (Memc[outimage])
		call flush (STDOUT)
	    }

	    # Accumulate the running mean. The first time through the loop
	    # the number of dimensions, size, and pixel type of the temporary
	    # storage image are set and the first set of images are
	    # accumulated into the temporary image.

	    if (imno == first) {
		IM_NDIM(tmpim) = IM_NDIM(im)
		sz_val = IM_MAXDIM
		call amovl (IM_LEN(im,1), IM_LEN(tmpim,1), sz_val)
		IM_PIXTYPE(tmpim) = TY_REAL
		call rs_cachen (btoi(cache), 1, tmpim, oldsize)
		call rs_minit (inlist, tmpim, start, finish, RS_KYFSCALE(rs))
	    } else if ((ostart > 0 && start > ostart) || (ofinish > 0 &&
	        finish > ofinish)) {
		call rs_maddsub (inlist, tmpim, start, finish, ostart, ofinish,
		    RS_KYFSCALE(rs))
	    }

	    # Attempt to cache the input and output images. Try to cache
	    # the output image first since it will be accessed more
	    # frequently.
	    call rs_cachen (btoi(cache), 2, outim, bufsize)
	    call rs_cachen (btoi(cache), 3, im, bufsize)

	    # Compute the new normalization factor.
	    call rs_mnorm (rs, im, tmpim, outim, finish - start + 1, fscale)

	    # Write the output image.
	    call rs_mout (im, tmpim, outim, finish - start + 1, fscale,
	        RS_KYFSCALE(rs), RS_KYSKYSUB(rs))

	    # Close up images.
	    call imunmap (outim)
	    call imunmap (im)

	    ostart = start
	    ofinish = finish

	}

	# Close and delete temporary image.
	call imunmap (tmpim)
	call imdelete (Memc[tmpimage])

	call fixmem (oldsize)

	call sfree (sp)
end


# RS_RRMSUB -- Perform a running mean sky subtraction on a list of images
# with no masking but with minmax rejection opening and closing the input
# images for each set.

procedure rs_rrmsub (inlist, outlist, rs, cache, verbose)

pointer	inlist			#I the input image list
pointer	outlist			#I the output image list
pointer	rs			#I the sky subtraction descriptor
bool	cache			#I cache temp image buffer in memory ?
bool	verbose			#I print task statistics

size_t	sz_val
real	fscale
pointer	sp, image, outimage, tmpimage, imptrs, imids, str
pointer	im, tmpim, outim
int	i, imno, nlo, nhi, ostart, ofinish, start, finish, nimages
int	first, last
size_t	oldsize, bufsize
pointer	immap()
int	imtlen(), imtrgetim(), btoi(), imaccess()
errchk	immap(), imgstr()
include	<nullptr.inc>

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (image, sz_val, TY_CHAR)
	call salloc (outimage, sz_val, TY_CHAR)
	call salloc (tmpimage, sz_val, TY_CHAR)
	sz_val = RS_NCOMBINE(rs) + 1
	call salloc (imptrs, sz_val, TY_POINTER)
	call salloc (imids, sz_val, TY_INT)
	sz_val = SZ_FNAME
	call salloc (str, sz_val, TY_CHAR)

	# Check image status. If resubtract is yes then delete the output
	# images if they already exist. Otherwise determine whether the
	# images already exist and if so whether or not they need to be
	# sky subtracted again.

	nimages = imtlen (inlist)
	if (RS_RESUBTRACT(rs) == NO) {
	    first = 0
	    last = 0
	    do i = 1, nimages {
	        if (imtrgetim (outlist, i, Memc[outimage], SZ_FNAME) == EOF)
		    break
		if (imaccess (Memc[outimage], 0) == NO) {
		    if (first == 0) {
			first = i
			last = i
		    } else
			last = i
		} else {
		    outim = immap (Memc[outimage], READ_ONLY, NULLPTR)
		    iferr (call imgstr (outim, RS_KYSKYSUB(rs), Memc[str],
		        SZ_FNAME)) {
			if (first == 0) {
			    first = i
			    last = i
			} else
			    last = i
		    }
		    call imunmap (outim)
		}
	    }
	} else {
	    first = 1
	    last = nimages
	    do i = 1, nimages {
	        if (imtrgetim (outlist, i, Memc[outimage], SZ_FNAME) == EOF)
		    break
		if (imaccess (Memc[outimage], 0) == YES)
		    call imdelete (Memc[outimage])
	    }
	}

	# Check the sky subtraction status.
	if (first <= 0 && last <= 0) {
	    if (verbose) {
		call printf (
		    "The output images have already been sky subtracted \n")
	    }
	    call sfree (sp)
	    return
	}

	# Create the temporary image.
	call mktemp ("_rsum", Memc[tmpimage], SZ_FNAME)
	tmpim = immap (Memc[tmpimage], NEW_IMAGE, NULLPTR)

	# Compute the sliding mean parameters.
	nlo = RS_NCOMBINE(rs) / 2
	nhi = RS_NCOMBINE(rs) - nlo

	# Loop over the images.
	ostart = 0
	ofinish = 0
	do imno = 1, nimages {

	    # Skip over beginning and ending images that have already been
	    # sky subtracted.

	    if (imno < first || imno > last) {
		if (verbose) {
	            if (imtrgetim (outlist, imno, Memc[outimage],
		        SZ_FNAME) == EOF) {
			call printf (
			    "The sky subtracted image %s already exists\n")
			    call pargstr (Memc[outimage])
		    }
		}
		next
	    }

	    # Determine which images will contribute to the sky image.
	    # Start and finish set the endpoints of the sequence. Imno
	    # is the current image which is to be sky subtracted.

	    if ((imno - nlo) < 1) {
	        start = 1
	        finish = min (nimages, max (2 * imno - 1, RS_NMIN(rs) + 1))
	    } else if ((imno + nhi) > nimages) {
	        start = max (1, min (2 * imno - nimages, nimages - RS_NMIN(rs)))
	        finish = nimages
	    } else {
	        start = imno - nlo
	        finish = imno + nhi
	    }

	    # Check that the minimum number of images exists.
	    if ((finish - start) < RS_NMIN(rs)) {
	        call eprintf ("There are too few images for sky subtraction\n")
		break
	    }

	    # Get the input image name.
	    if (imtrgetim (inlist, imno, Memc[image], SZ_FNAME) == EOF) {
	        call eprintf ("Error reading input image list\n")
		break
	    }

	    # Open the output image name.
	    if (imtrgetim (outlist, imno, Memc[outimage], SZ_FNAME) == EOF) {
	        call eprintf ("Error reading output image list\n")
		break
	    }

	    if (verbose) {
		call printf ("Sky subtracting image %s and writing to %s\n")
		    call pargstr (Memc[image])
		    call pargstr (Memc[outimage])
		call flush (STDOUT)
	    }

	    # Determine which images are to be open at any given time.

	    if (imno == first) {
		call rs_iptrs (inlist, Memp[imptrs], Memi[imids], start,
		    finish, cache, oldsize)
		IM_NDIM(tmpim) = IM_NDIM(Memp[imptrs])
		sz_val = IM_MAXDIM
		call amovl (IM_LEN(Memp[imptrs],1), IM_LEN(tmpim,1), sz_val)
		IM_PIXTYPE(tmpim) = TY_REAL
		call rs_cachen (btoi(cache), finish - start + 2, tmpim,
		    bufsize)
	    } else {
		call rs_asptrs (inlist, Memp[imptrs], Memi[imids],
		    start, finish, ostart, ofinish, cache)
	    }

	    # Determine which image is the input image.
	    im = NULL
	    do i = 1, finish - start + 1 {
		if (Memi[imids+i-1] != imno)
		    next
		im = Memp[imptrs+i-1]
		break
	    }

	    # Open the output image and cache it.
	    iferr {
	        outim = immap (Memc[outimage], NEW_COPY, im)
	    } then {
	        call eprintf ("Error opening output image %s\n")
		    call pargstr (Memc[outimage])
	    } else {

		# Cache the output image.
		call rs_cachen (btoi(cache), finish - start + 3, outim, bufsize)

	        # Combine images with rejection.
	        if (RS_COMBINE(rs) == RS_MEAN)
	            call rs_asumr (Memp[imptrs], Memi[imids], tmpim, start,
		        finish, imno, RS_NLOREJ(rs), RS_NHIREJ(rs),
			RS_KYFSCALE(rs))
	        else
	            call rs_asumr (Memp[imptrs], Memi[imids], tmpim, start,
		        finish, imno, INDEFI, INDEFI, RS_KYFSCALE(rs))

	        # Compute the normalization factor.
	        call rs_rmnorm (rs, im, tmpim, outim, fscale)

	        # Write output image.
	        call rs_rmout (im, tmpim, outim, fscale, RS_KYSKYSUB(rs))

	        # Close up images.
	        call imunmap (outim)
	    }

	    # Unmap the remaining image pointers.
	    if (imno == last) {
		do i = 1, finish - start + 1
		    call imunmap (Memp[imptrs+i-1])
	    }

	    ostart = start
	    ofinish = finish
	}

	# Close and delete temporary image.
	call imunmap (tmpim)
	call imdelete (Memc[tmpimage])

	call fixmem (oldsize)

	call sfree (sp)
end


# RS_RMSUB -- Perform a running mean sky subtraction on a list of images
# with no masking but with rejection.

procedure rs_rmsub (inlist, outlist, rs, cache, verbose)

pointer	inlist			#I the input image list
pointer	outlist			#I the output image list
pointer	rs			#I the sky subtraction descriptor
bool	cache			#I cache temp image buffer in memory ?
bool	verbose			#I print task statistics

size_t	sz_val
real	fscale
pointer	sp, image, outimage, tmpimage, str
pointer	im, outim, tmpim
int	i, nimages, nlo, nhi, ostart, ofinish, start, finish, imno
int	first, last
size_t	oldsize, newsize
pointer	immap()
int	imtlen(), imtrgetim(), btoi(), imaccess()
errchk	immap()
include	<nullptr.inc>

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (image, sz_val, TY_CHAR)
	call salloc (outimage, sz_val, TY_CHAR)
	call salloc (tmpimage, sz_val, TY_CHAR)
	call salloc (str, sz_val, TY_CHAR)

	# Check image status. If resubtract is yes then delete the output
	# images if they already exist. Otherwise determine whether the
	# images already exist and if so whether or not they need to be
	# sky subtracted again.

	nimages = imtlen (inlist)
	if (RS_RESUBTRACT(rs) == NO) {
	    first = 0
	    last = 0
	    do i = 1, nimages {
	        if (imtrgetim (outlist, i, Memc[outimage], SZ_FNAME) == EOF)
		    break
		if (imaccess (Memc[outimage], 0) == NO) {
		    if (first == 0) {
			first = i
			last = i
		    } else
			last = i
		} else {
		    outim = immap (Memc[outimage], READ_ONLY, NULLPTR)
		    iferr (call imgstr (outim, RS_KYSKYSUB(rs), Memc[str],
		        SZ_FNAME)) {
			if (first == 0) {
			    first = i
			    last = i
			} else
			    last = i
		    }
		    call imunmap (outim)
		}
	    }
	} else {
	    first = 1
	    last = nimages
	    do i = 1, nimages {
	        if (imtrgetim (outlist, i, Memc[outimage], SZ_FNAME) == EOF)
		    break
		if (imaccess (Memc[outimage], 0) == YES)
		    call imdelete (Memc[outimage])
	    }
	}

	# Check the sky subtraction status.
	if (first <= 0 && last <= 0) {
	    if (verbose) {
		call printf (
		    "The output images have already been sky subtracted \n")
	    }
	    call sfree (sp)
	    return
	}

	# Create the temporary image.
	call mktemp ("_rsum", Memc[tmpimage], SZ_FNAME)
	tmpim = immap (Memc[tmpimage], NEW_IMAGE, NULLPTR)

	# Compute the sliding mean parameters.
	nlo = RS_NCOMBINE(rs) / 2
	nhi = RS_NCOMBINE(rs) - nlo

	# Loop over the images.
	ostart = 0
	ofinish = 0
	do imno = 1, nimages {

	    # Skip over beginning and ending images that have already been
	    # sky subtracted.

	    if (imno < first || imno > last) {
		if (verbose) {
	            if (imtrgetim (outlist, imno, Memc[outimage],
		        SZ_FNAME) == EOF) {
			call printf (
			    "The sky subtracted image %s already exists\n")
			    call pargstr (Memc[outimage])
		    }
		}
		next
	    }

	    # Determine which images will contribute to the sky image.
	    # Start and finish set the endpoints of the sequence. Imno
	    # is the current image which is to be sky subtracted.

	    if ((imno - nlo) < 1) {
	        start = 1
	        finish = min (nimages, max (2 * imno - 1, RS_NMIN(rs) + 1))
	    } else if ((imno + nhi) > nimages) {
	        start = max (1, min (2 * imno - nimages, nimages - RS_NMIN(rs)))
	        finish = nimages
	    } else {
	        start = imno - nlo
	        finish = imno + nhi
	    }

	    # Check that the minimum number of images exists.
	    if ((finish - start) < RS_NMIN(rs)) {
	        call eprintf ("There are too few images for sky subtraction\n")
		break
	    }

	    # Open the current input image.
	    if (imtrgetim (inlist, imno, Memc[image], SZ_FNAME) == EOF) {
	        call eprintf ("Error reading input image list\n")
		break
	    }
	    iferr (im = immap (Memc[image], READ_ONLY, NULLPTR)) {
	        call eprintf ("Error opening input image %s\n")
		    call pargstr (Memc[image])
		break
	    }

	    # Open the output image. 

	    if (imtrgetim (outlist, imno, Memc[outimage], SZ_FNAME) == EOF) {
	        call eprintf ("Error reading output image list\n")
		call imunmap (im)
		break
	    }
	    iferr (outim = immap (Memc[outimage], NEW_COPY, im)) {
	        call eprintf ("Error opening output image %s\n")
		    call pargstr (Memc[outimage])
		call imunmap (im)
		break
	    }

	    if (verbose) {
		call printf ("Sky subtracting image %s and writing to %s\n")
		    call pargstr (Memc[image])
		    call pargstr (Memc[outimage])
		call flush (STDOUT)
	    }

	    # Set the size of the temporary image. 
	    if (imno == first) {
		IM_NDIM(tmpim) = IM_NDIM(im)
		sz_val = IM_MAXDIM
		call amovl (IM_LEN(im,1), IM_LEN(tmpim,1), sz_val)
		IM_PIXTYPE(tmpim) = TY_REAL
		call rs_cachen (btoi(cache), 1, tmpim, oldsize)
	    }

	    # Accumulate the temporary image.
	    if (RS_COMBINE(rs) == RS_MEAN)
	        call rs_sumr (inlist, tmpim, start, finish, imno, RS_NLOREJ(rs),
		    RS_NHIREJ(rs), RS_KYFSCALE(rs))
	    else
	        call rs_sumr (inlist, tmpim, start, finish, imno, INDEFI,
		    INDEFI, RS_KYFSCALE(rs))

	    # Cache the output image.
	    call rs_cachen (btoi(cache), 2, outim, newsize)

	    # Compute the normalization factor.
	    call rs_rmnorm (rs, im, tmpim, outim, fscale)

	    # Write the output image.
	    call rs_rmout (im, tmpim, outim, fscale, RS_KYSKYSUB(rs))

	    # Close up images.
	    call imunmap (outim)
	    call imunmap (im)

	    ostart = start
	    ofinish = finish

	}

	# Close and delete temporary image.
	call imunmap (tmpim)
	call imdelete (Memc[tmpimage])

	call fixmem (oldsize)

	call sfree (sp)
end


# RS_MINIT -- Initialize the accumulation buffer for the running median
# in the case of no masks.

procedure rs_minit (inlist, tmpim, start, finish, skyscale)

pointer	inlist			#I the input image list
pointer	tmpim			#I the output storage image
int	start			#I the starting image in the list
int	finish			#I the ending image in the list
#real	normsum			#U the normalization accumulator 
long	l_val
size_t	sz_val
char	skyscale[ARB]		#I the scaling factor keyword 

pointer	sp, image, imptrs, imnorm, vin, vout, obuf, ibuf
int	i, j, nin
size_t	npix
real	imgetr()
pointer	immap()
int	imtrgetim()
long	impnlr(), imgnlr()
errchk	imgetr()
include	<nullptr.inc>

begin
	nin = finish - start + 1
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (image, sz_val, TY_CHAR) 
	sz_val = nin
	call salloc (imptrs, sz_val, TY_POINTER) 
	call salloc (imnorm, sz_val, TY_REAL) 
	sz_val = IM_MAXDIM
	call salloc (vout, sz_val, TY_LONG) 
	sz_val = nin * IM_MAXDIM
	call salloc (vin, sz_val, TY_LONG) 

	# Open the input images
	j = 1
	#normsum = 0.0
	do i = start, finish {
	    if (imtrgetim (inlist, i, Memc[image], SZ_FNAME) == EOF)
		;
	    Memp[imptrs+j-1] = immap (Memc[image], READ_ONLY, NULLPTR)
	    iferr (Memr[imnorm+j-1] = imgetr (Memp[imptrs+j-1], skyscale))
		Memr[imnorm+j-1] = 1.0
		#normsum = normsum + 1.0
	    #else
		#normsum = normsum + Memr[imnorm+j-1]
	    j = j + 1
	}

	l_val = 1
	sz_val = IM_MAXDIM * nin
	call amovkl (l_val, Meml[vin], sz_val)
	sz_val = IM_MAXDIM
	call amovkl (l_val, Meml[vout], sz_val)
	npix = IM_LEN(tmpim,1)
	while (impnlr (tmpim, obuf, Meml[vout]) != EOF) {
	    call amovkr (0.0, Memr[obuf], npix)
	    do j = 1, nin {
		if (imgnlr (Memp[imptrs+j-1], ibuf,
		    Meml[vin+(j-1)*IM_MAXDIM]) == EOF)
		    ;
		call amulkr (Memr[ibuf], Memr[imnorm+j-1], Memr[ibuf], npix) 
		call aaddr (Memr[ibuf], Memr[obuf], Memr[obuf], npix)
	    }
	}

	# Close the input images.
	do j = 1, nin
	    call imunmap (Memp[imptrs+j-1])

	call sfree (sp)
end


# RS_MNORM -- Compute the normalization factor for the new output image.

procedure rs_mnorm (rs, im, tmpim, outim, nin, fscale)

pointer	rs			#I the sky subtraction descriptor
pointer	im			#I the input image descriptor
pointer	tmpim			#I the storage image descriptor
pointer	outim			#I the output image descriptor
int	nin			#I the number of images
real	fscale			#I the scaling factor


long	l_val
size_t	sz_val
real	norm1, normf, rmin, rmax
pointer	sp, vin, vout, vtmp, obuf, ibuf, tbuf
long	i, npix	
real	imgetr()
long	impnlr(), imgnlr()
errchk	imgetr()

begin

	call smark (sp)
	sz_val = IM_MAXDIM
	call salloc (vin, sz_val, TY_LONG)
	call salloc (vout, sz_val, TY_LONG)
	call salloc (vtmp, sz_val, TY_LONG)

	l_val = 1
	sz_val = IM_MAXDIM
	call amovkl (l_val, Meml[vout], sz_val)
	call amovkl (l_val, Meml[vin], sz_val)
	call amovkl (l_val, Meml[vtmp], sz_val)

	iferr (norm1 = imgetr (im, RS_KYFSCALE(rs)))
	    norm1 = 1.0
	normf = 1.0 / (nin - 1)
	npix = IM_LEN(im,1)

	# Compute the normalized image.
	while (impnlr (outim, obuf, Meml[vout]) != EOF && imgnlr (im, ibuf,
	    Meml[vin]) != EOF && imgnlr (tmpim, tbuf, Meml[vtmp]) != EOF) {
	    do i = 1, npix {
		Memr[obuf+i-1] = normf * (Memr[tbuf+i-1] - norm1 *
		    Memr[ibuf+i-1])
		if (Memr[obuf+i-1] == 0.0)
		    Memr[obuf+i-1] = Memr[ibuf+i-1]
		else
		    Memr[obuf+i-1] = Memr[ibuf+i-1] / Memr[obuf+i-1]
	    }
	}

	# Compute the statistic.
	rmin = RS_LOWER(rs)
	rmax = RS_UPPER(rs)
	RS_LOWER(rs) = INDEFR
	RS_UPPER(rs) = INDEFR
	call rs_med (outim, rs, fscale)
	RS_LOWER(rs) = rmin
	RS_UPPER(rs) = rmax

	call sfree (sp)

end


# RS_MOUT -- Write the output image. Subtract the normalized  input
# image from the accumulation buffer before computing the final average.

procedure rs_mout (im, tmpim, outim, nin, fscale, skyscale, skysub)

pointer	im			#I the input image descriptor
pointer	tmpim			#I the storage image descriptor
pointer	outim			#I the output image descriptor
int	nin			#I the number of images
real	fscale			#I the normalization factor 
char	skyscale[ARB]		#I the sky scaling keyword
char	skysub[ARB]		#I the sky subtraction keyword

long	l_val
size_t	sz_val
real	norm1, normf
pointer	sp, vin, vout, vtmp, str, obuf, ibuf, tbuf
long	i, npix
real	imgetr()
long	impnlr(), imgnlr()
errchk	imgetr()

begin
	call smark (sp)
	sz_val = IM_MAXDIM
	call salloc (vin, sz_val, TY_LONG)
	call salloc (vout, sz_val, TY_LONG)
	call salloc (vtmp, sz_val, TY_LONG)
	sz_val = SZ_FNAME
	call salloc (str, sz_val, TY_CHAR)

	# Write a sky subtraction flag to the output image.
	call sprintf (Memc[str], SZ_FNAME,
	    "Sky subtracting with scale factor %g")
	    call pargr (fscale)
	call imastr (outim, skysub, Memc[str])

	# Get and set the normalization factors
	iferr (norm1 = imgetr (im, skyscale))
	    norm1 = 1.0
	normf = fscale / (nin - 1)
	norm1 = 1.0 + normf * norm1

	l_val = 1
	sz_val = IM_MAXDIM
	call amovkl (l_val, Meml[vout], sz_val)
	call amovkl (l_val, Meml[vin], sz_val)
	call amovkl (l_val, Meml[vtmp], sz_val)
	npix = IM_LEN(im,1)
	while (impnlr (outim, obuf, Meml[vout]) != EOF && imgnlr (im, ibuf,
	    Meml[vin]) != EOF && imgnlr (tmpim, tbuf, Meml[vtmp]) != EOF) {
	    do i = 1, npix
	        Memr[obuf+i-1] = norm1 * Memr[ibuf+i-1] - normf * Memr[tbuf+i-1] 
	}

	call sfree (sp)
end


# RS_MADDSUB -- Add images to and subtract images from the accumulation 
# buffer.

procedure rs_maddsub (inlist, tmpim, start, finish, ostart, ofinish, skyscale)

pointer	inlist			#I the input image list
pointer	tmpim			#I the storage image descriptor
int	start			#I the current starting image
int	finish			#I the current ending image
int	ostart			#I the previous starting image
int	ofinish			#I the previous ending image
#real	normsum			#I the norm factor accumulator
long	l_val
size_t	sz_val
char	skyscale		#I the sky scaling keyword

pointer	sp, image, vin, vsub, vadd, vout, imsub, imadd, norma, norms
pointer	ibuf, obuf, sbuf, abuf
int	i, j, nsub, nadd, doadd, dosub
size_t	npix
real	imgetr()
pointer	immap()
int	imtrgetim()
long	impnlr(), imgnlr()
errchk	imgetr()
include	<nullptr.inc>

begin
	if (start == ostart && finish == ofinish)
	    return
	nsub = start - ostart
	nadd = finish - ofinish

	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (image, sz_val, TY_CHAR)
	sz_val = IM_MAXDIM
	call salloc (vin, sz_val, TY_LONG)
	call salloc (vout, sz_val, TY_LONG)
	sz_val = nsub
	call salloc (imsub, sz_val, TY_POINTER)
	call salloc (norms, sz_val, TY_REAL)
	sz_val = nsub * IM_MAXDIM
	call salloc (vsub, sz_val, TY_LONG)
	sz_val = nadd
	call salloc (imadd, sz_val, TY_POINTER)
	sz_val = nadd * IM_MAXDIM
	call salloc (vadd, sz_val, TY_LONG)
	sz_val = nadd
	call salloc (norma, sz_val, TY_REAL)

	# Open the images to be subtracted. In most cases there will be
	# one such image.
	if (ostart < start) {
	    dosub = YES
	    j = 1
	    do i = ostart, start - 1 {
	        if (imtrgetim (inlist, i, Memc[image], SZ_FNAME) != EOF) {
	    	    Memp[imsub+j-1] = immap (Memc[image], READ_ONLY, NULLPTR)
	    	    iferr (Memr[norms+j-1] = imgetr (Memp[imsub+j-1], skyscale))
			Memr[norms+j-1] = 1.0
	    	    #normsum = normsum - Memr[norms+j-1]
	        }
		j = j + 1
	    }
	} else
	    dosub = NO

	# Open the images to be added. In most cases there will be one
	# such image.
	if (finish > ofinish) {
	    doadd = YES
	    j = 1
	    do i = ofinish + 1, finish {
	        if (imtrgetim (inlist, i, Memc[image], SZ_FNAME) != EOF) {
	            Memp[imadd+j-1] = immap (Memc[image], READ_ONLY, NULLPTR)
	            iferr (Memr[norma+j-1] = imgetr (Memp[imadd+j-1], skyscale))
			Memr[norma+j-1] = 1.0
	            #normsum = normsum + Memr[norma+j-1]
		}
		j = j + 1
	    }
	} else
	    doadd = NO

	# Make the vector operators in-line code later if necessary.
	l_val = 1
	sz_val = IM_MAXDIM
	call amovkl (l_val, Meml[vin], sz_val)
	sz_val = nsub * IM_MAXDIM
	call amovkl (l_val, Meml[vsub], sz_val)
	sz_val = nadd * IM_MAXDIM
	call amovkl (l_val, Meml[vadd], sz_val)
	sz_val = IM_MAXDIM
	call amovkl (l_val, Meml[vout], sz_val)
	npix = IM_LEN(tmpim,1)
	while (impnlr (tmpim, obuf, Meml[vout]) != EOF &&
	    imgnlr (tmpim, ibuf, Meml[vin]) != EOF) {
	    if (dosub == YES && doadd == YES) {
		do i = 1, nsub {
		    if (imgnlr (Memp[imsub+i-1], sbuf,
		        Meml[vsub+(i-1)*nsub]) != EOF) {
		        call amulkr (Memr[sbuf], Memr[norms+i-1], Memr[sbuf],
			    npix)
			if (i == 1)
		            call asubr (Memr[ibuf], Memr[sbuf], Memr[obuf],
			        npix)
			else
		            call asubr (Memr[obuf], Memr[sbuf], Memr[obuf],
			        npix)
		    }
		}
		do i = 1, nadd {
		    if (imgnlr (Memp[imadd+i-1], abuf,
		        Meml[vadd+(i-1)*nadd]) != EOF) {
		        call amulkr (Memr[abuf], Memr[norma+i-1], Memr[abuf],
			    npix)
		        call aaddr (Memr[obuf], Memr[abuf], Memr[obuf], npix)
		    }
		}
	    } else if (dosub == YES) {
		do i = 1, nsub {
		    if (imgnlr (Memp[imsub+i-1], sbuf,
		        Meml[vsub+(i-1)*nsub]) != EOF) {
		        call amulkr (Memr[sbuf], Memr[norms+i-1], Memr[sbuf],
			    npix)
			if (i == 1)
		            call asubr (Memr[ibuf], Memr[sbuf], Memr[obuf],
			        npix)
			else
		            call asubr (Memr[obuf], Memr[sbuf], Memr[obuf],
			        npix)
		    }
		}
	    } else if (doadd == YES) {
		do i = 1, nadd {
		    if (imgnlr (Memp[imadd+i-1], abuf,
		        Meml[vadd+(i-1)*nadd]) != EOF) {
		        call amulkr (Memr[abuf], Memr[norma+i-1], Memr[abuf],
			    npix)
			if ( i == 1)
		            call aaddr (Memr[ibuf], Memr[abuf], Memr[obuf],
			        npix)
			else
		            call aaddr (Memr[obuf], Memr[abuf], Memr[obuf],
			        npix)
		    }
		}
	    }
	}

	# Close the images to be added or subtracted.
	do i = 1, nsub {
	    call imunmap (Memp[imsub+i-1])
	}
	do i = 1, nadd {
	    call imunmap (Memp[imadd+i-1])
	}

	call sfree (sp)
end


# RS_IPTRS -- Get the initial set of image points.

procedure rs_iptrs (inlist, imptrs, imids, start, finish, cache, oldsize)

pointer	inlist				#I the input image list
pointer	imptrs[ARB]			#O the input image pointers
int	imids[ARB]			#O the input image ids
int	start				#I the starting image in the series
int	finish				#I the ending image in the serious
bool	cache				#I cache the image in memory ?
size_t	oldsize				#O the original working set size

size_t	sz_val
pointer	sp, image
int	n, i
size_t	bufsize
pointer	immap()
int	imtrgetim(), btoi()
include	<nullptr.inc>

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (image, sz_val, TY_CHAR)

	n = 1
	do i = start, finish {
	    if (imtrgetim (inlist, i, Memc[image], SZ_FNAME) != EOF) {
		imids[n] = i
		imptrs[n] = immap (Memc[image], READ_ONLY, NULLPTR)
		call rs_cachen (btoi(cache), n, imptrs[n], bufsize)
		if (n == 1)
		    oldsize = bufsize
		n = n + 1
	    }
	}

	call sfree (sp)
end


# RS_ASPTRS -- Advance the image pointer and id buffers for the next
# current image.

procedure rs_asptrs (inlist, imptrs, imids, start, finish, ostart, ofinish,
	cache)

pointer	inlist				#I the input image list
pointer	imptrs[ARB]			#U the input image pointers
int	imids[ARB]			#U the input image ids
int	start				#I the starting image in the series
int	finish				#I the ending image in the serious
int	ostart				#I the old starting image in the series
int	ofinish				#I the old ending image in the serious
bool	cache				#I cache image buffers ?

size_t	sz_val
pointer	sp, image
int	i, n, nold, nsub, nadd
size_t	bufsize
pointer	immap()
int	imtrgetim(), btoi()
include	<nullptr.inc>

begin
	# No new images are added or deleted.
	if (start == ostart && finish == ofinish)
	    return

	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (image, sz_val, TY_CHAR)

	nold = ofinish - start + 1

	# Delete some images from the combine list.
	nsub = start - ostart
	if (nsub > 0) {
	    # Unmap the images to be deleted.
	    do i = 1, nsub {
		call imunmap (imptrs[i])
	    }
	    # Rotate the image pointer buffer.
	    do i = 1, nold {
		imptrs[i] = imptrs[i+nsub]
		imids[i] = imids[i+nsub]
	    }
	}

	# Add new images to the combine list. Note that the cacheing
	# mechanism must include the temporary image hence a request for
	# n + 1 cached image buffers is issued instead of a request for n.
	nadd = finish - ofinish
	if (nadd > 0) {
	    n = nold + 1
	    do i = ofinish + 1, finish {
		if (imtrgetim (inlist, i, Memc[image], SZ_FNAME) != EOF) {
		    imptrs[n] = immap (Memc[image], READ_ONLY, NULLPTR)
		    imids[n] = i
		    if ((finish - start) > (ofinish - ostart))
		        call rs_cachen (btoi(cache), n+1, imptrs[n], bufsize)
		    n = n + 1
		}
	    }
	}

	call sfree (sp)
end


# RS_RMNORM -- Compute the normalization factor for the new output image.

procedure rs_rmnorm (rs, im, tmpim, outim, fscale)

pointer	rs			#I the sky subtraction structure
pointer	im			#I the input image descriptor
pointer	tmpim			#I the storage image descriptor
pointer	outim			#I the output image descriptor
real	fscale			#O the scaling factor

long	l_val
size_t	sz_val
real	rmin, rmax
pointer	sp, vout, vin, vtmp, obuf, tmpbuf, ibuf
long	i, npix
long	impnlr(), imgnlr()

begin
	call smark (sp)
	sz_val = IM_MAXDIM
	call salloc (vout, sz_val, TY_LONG)
	call salloc (vin, sz_val, TY_LONG)
	call salloc (vtmp, sz_val, TY_LONG)

	l_val = 1
	sz_val = IM_MAXDIM
	call amovkl (l_val, Meml[vout], sz_val)
	call amovkl (l_val, Meml[vtmp], sz_val)
	call amovkl (l_val, Meml[vin], sz_val)

	# Compute the normalized input image.
	npix = IM_LEN(im,1)
	while (impnlr (outim, obuf, Meml[vout]) != EOF &&
	    imgnlr (tmpim, tmpbuf, Meml[vtmp]) != EOF &&
	    imgnlr (im, ibuf, Meml[vin]) != EOF) {
	    do i = 1, npix {
		if (Memr[tmpbuf+i-1] == 0.0)
		    Memr[obuf+i-1] = Memr[ibuf+i-1]
		else
		    Memr[obuf+i-1] = Memr[ibuf+i-1] / Memr[tmpbuf+i-1] 
	    }
	}

	# Compute the normalization factor. Set the good data limits to
	# INDEF for this case
	rmin = RS_LOWER(rs)
	rmax = RS_UPPER(rs)
	RS_LOWER(rs) = INDEFR
	RS_UPPER(rs) = INDEFR
	call rs_med (outim, rs, fscale)
	RS_LOWER(rs) = rmin
	RS_UPPER(rs) = rmax

	call sfree (sp)
end


# RS_RMOUT -- Compute the output sky subtracted image. 

procedure rs_rmout (im, tmpim, outim, fscale, skysub)

pointer	im			#I the input image descriptor
pointer	tmpim			#I the temporary image descriptor
pointer	outim			#I the output image descriptor
real	fscale			#I the scaling factor
char	skysub[ARB]		#I the skyscale keyword

long	l_val
size_t	sz_val
pointer	sp, vout, vtmp, vin, str, obuf, tmpbuf, ibuf
size_t	npix
long	imgnlr(), impnlr()

begin
	call smark (sp)
	sz_val = IM_MAXDIM
	call salloc (vout, sz_val, TY_LONG)
	call salloc (vin, sz_val, TY_LONG)
	call salloc (vtmp, sz_val, TY_LONG)
	sz_val = SZ_FNAME
	call salloc (str, sz_val, TY_CHAR)

	l_val = 1
	sz_val = IM_MAXDIM
	call amovkl (l_val, Meml[vout], sz_val)
	call amovkl (l_val, Meml[vtmp], sz_val)
	call amovkl (l_val, Meml[vin], sz_val)

	# Add keyword to image header.
	call sprintf (Memc[str], SZ_FNAME,
	    "Sky subtracted with scale factor = %g")
	    call pargr (fscale)
	call imastr (outim, skysub, Memc[str])

	# Normalize the output image.
	npix = IM_LEN(im,1)
	while (impnlr (outim, obuf, Meml[vout]) != EOF &&
	    imgnlr (tmpim, tmpbuf, Meml[vtmp]) != EOF &&
	    imgnlr (im, ibuf, Meml[vin]) != EOF) {
	    call amulkr (Memr[tmpbuf], fscale, Memr[obuf], npix)
	    call asubr (Memr[ibuf], Memr[obuf], Memr[obuf], npix)
	}

	call sfree (sp)
end


# RS_DIVERR -- Function for divide by zero error.

#real	procedure rs_diverr (rval)

#real	rval			#I input return value.

#begin
#	return (rval)
#end
