include <imhdr.h>
include <mwset.h>

# T_WCSCOPY -- Copy the world coordinate system of a reference image to
# the world coordinate system of an input image. 

procedure t_wcscopy()

bool	verbose
int	ilist, rlist
pointer	sp, image, refimage, value, str, imr, mwr, im
real	rval
double	dval
bool	clgetb()
int	imtopen(), imtlen(), imtgetim()
#int	mw_stati(), rg_samesize()
pointer	immap(), mw_openim()
real	imgetr()
double	imgetd()
errchk	mw_openim(), imgstr(), imgetr(), imgetd(), imdelf()

begin
	# Get some temporary working space.
	call smark (sp)
	call salloc (refimage, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (value, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the input image and reference image lists.
	call clgstr ("images", Memc[str], SZ_FNAME)
	ilist = imtopen (Memc[str])
	call clgstr ("refimages", Memc[str], SZ_FNAME)
	rlist = imtopen (Memc[str])
	verbose = clgetb ("verbose")

	# Check the reference image list length.
	if (imtlen (rlist) <= 0)
	    call error (0, "The reference image list is empty.")
	if (imtlen(rlist) > 1 && imtlen(rlist) != imtlen(ilist))
	    call error (0,
	        "The number of reference and input images is not the same.")

	# Initialize the reference image and coordinate list pointers.
	imr = NULL

	# Loop over the input images.
	while (imtgetim (ilist, Memc[image], SZ_FNAME) != EOF) {

	    # Open the reference image and reference coordinate file and
	    # compute the logical and world reference coordinates.
	    if (imtgetim (rlist, Memc[refimage], SZ_FNAME) != EOF) {

		# Open the reference image.
		if (imr != NULL) {
		    if (mwr != NULL)
		        call mw_close (mwr)
		    call imunmap (imr)
		}
		imr = immap (Memc[refimage], READ_ONLY, 0)

		# Open the reference image wcs.
		iferr (mwr = mw_openim (imr))
		    mwr = NULL

		# Check that the wcs dimensions are rational.
#		if (mwr != NULL) {
#		    if (mw_stati(mwr, MW_NPHYSDIM) < IM_NDIM(imr)) {
#			call mw_close (mwr)
#			mwr = NULL
#		    }
#		}
	    }

	    # Print message about progress of task
	    if (verbose) {
	        call printf ("Copying wcs from image %s to image %s\n")
	            call pargstr (Memc[refimage])
	            call pargstr (Memc[image])
	    }

	    # Remove any image section and open the input image.
	    call imgimage (Memc[image], Memc[image], SZ_FNAME)
	    iferr (im = immap (Memc[image], READ_WRITE, 0)) {
	        im = immap (Memc[image], NEW_IMAGE, 0)
		IM_NDIM(im) = 0
	    }

	    # Test for valid wcs.
	    if (mwr == NULL) {
		if (verbose) {
		    call printf (
		        "\tError: cannot read wcs for reference image %s\n")
	            call pargstr (Memc[refimage])
		}
#	    } else if (IM_NDIM(im) != IM_NDIM(imr)) {
#		if (verbose) {
#		    call printf (
#		    "\tError: %s and %s have different number of dimensions\n")
#	                call pargstr (Memc[image])
#	                call pargstr (Memc[refimage])
#		}
	    } else {
#		if (rg_samesize (imr, im) == NO) {
#		    if (verbose) {
#		        call printf (
#		        "\tWarning: images %s and %s have different sizes\n")
#	                call pargstr (Memc[image])
#	                call pargstr (Memc[refimage])
#		    }
#		}
		#mw = mw_open (NULL, mw_stati (mwr,MW_NPHYSDIM))
		#call mw_loadim (mw, imr)
		#call mw_saveim (mw, im)
		#call mw_close (mw)
		call mw_saveim (mwr, im)

		# Copy the RADECSYS keyword to the input image header.
		ifnoerr {
		    call imgstr (imr, "RADECSYS", Memc[value], SZ_FNAME)
		} then {
		    call imastr (im, "RADECSYS", Memc[value])
		} else {
		    iferr (call imdelf (im, "RADECSYS"))
			;
		}

		# Copy the EQUINOX or EPOCH keyword to the input image header
		# EQUINOX keyword.
		ifnoerr {
		    rval = imgetr (imr, "EQUINOX")
		} then {
		    call imaddr (im, "EQUINOX", rval)
		    iferr (call imdelf (im, "EPOCH"))
			;
		} else {
		    ifnoerr {
		        rval = imgetr (imr, "EPOCH")
		    } then {
		        call imaddr (im, "EQUINOX", rval)
		        iferr (call imdelf (im, "EPOCH"))
			    ;
		    } else {
		        iferr (call imdelf (im, "EQUINOX"))
			    ;
		        iferr (call imdelf (im, "EPOCH"))
			    ;
		    }
		}

		# Copy the MJD-WCSkeyword to the input image header.
		ifnoerr {
		    dval = imgetd (imr, "MJD-WCS")
		} then {
		    call imaddd (im, "MJD-WCS", dval)
		} else {
		    iferr (call imdelf (im, "MJD-WCS"))
			;
		}
	    }

	    # Close the input image.
	    call imunmap (im)

	}

	if (imr != NULL) {
	    if (mwr != NULL)
	        call mw_close (mwr)
	    call imunmap (imr)
	}

	if (ilist != NULL)
	    call imtclose (ilist)
	if (rlist != NULL)
	    call imtclose (rlist)

	call sfree (sp)
end


# RG_SAMESIZE -- Determine whether two images of the same dimension are
# the same size.

int procedure rg_samesize (im1, im2)

pointer	im1			#I the first image descriptor
pointer	im2			#I the second image descriptor

int	i, stat

begin
	stat = YES
	do i = 1, IM_NDIM(im1) {
	    if (IM_LEN(im1,i) != IM_LEN(im2,i))
		return (NO)
	}
	return (stat)
end
