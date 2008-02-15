# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <imhdr.h>
include <mach.h>
include "rmode.h"

# T_RMODE -- Ring modak filter a list of images in x and y.

procedure t_rmode()

bool	verbose
int	boundary, nxk, nyk
pointer	list1, list2, sp, imtlist1, imtlist2, image1, image2, imtemp, str
pointer	med, im1, im2, kernel
real	rinner, router, ratio, theta, constant, a1, b1, c1, f1, a2, b2, c2, f2

size_t	sz_val
bool	fp_equalr(), clgetb()
int	imtgetim(), imtlen(), clgwrd()
int	med_mkring()
pointer	imtopen(), immap()
real	clgetr()
errchk	med_ell_gauss, med_mkring, med_modring

begin
	# Allocate some working space.
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (imtlist1, sz_val, TY_CHAR)
	call salloc (imtlist2, sz_val, TY_CHAR)
	sz_val = SZ_FNAME
	call salloc (image1, sz_val, TY_CHAR)
	call salloc (image2, sz_val, TY_CHAR)
	call salloc (imtemp, sz_val, TY_CHAR)
	sz_val = SZ_LINE
	call salloc (str, sz_val, TY_CHAR)

	# Allcoate space for the rmode structure.
	sz_val = LEN_RMODE_STRUCT
	call calloc (med, sz_val, TY_STRUCT)

	# Get the task parameters.
	call clgstr ("input", Memc[imtlist1], SZ_FNAME)
	call clgstr ("output", Memc[imtlist2], SZ_FNAME)

	# Get the ring filter parameters.
	rinner = clgetr ("rinner")
	router = clgetr ("router")
	ratio = clgetr ("ratio")
	theta = clgetr ("theta")

	# Get the rejection parameters.
	RMOD_ZLOW(med) = clgetr ("zloreject")
	if (IS_INDEFR(RMOD_ZLOW(med)))
	    RMOD_ZLOW(med) = -MAX_REAL
	RMOD_ZHIGH(med) = clgetr ("zhireject")
	if (IS_INDEFR(RMOD_ZHIGH(med)))
	    RMOD_ZHIGH(med) = MAX_REAL

	# Get the boundary extension parameters.
	boundary = clgwrd ("boundary", Memc[str], SZ_LINE,
	    ",constant,nearest,reflect,wrap,")
	constant = clgetr ("constant")
	verbose = clgetb ("verbose")

	# Open the input and output image lists.
	list1 = imtopen (Memc[imtlist1])
	list2 = imtopen (Memc[imtlist2])
	if (imtlen (list1) != imtlen (list2)) {
	    call imtclose (list1)
	    call imtclose (list2)
	    call error (0, "Number of input and output images not the same.")
	}

	# Check kernel parameters.
        if (fp_equalr (real(router), 0.0))
            call error (0, "T_RMODE: Router must be greater than 0.")
	if (rinner >= router)
	    call error (0, "T_RMODE: Rinner must be less than router.")
        if (ratio < 0.0 || ratio > 1.0)
            call error (0, "T_RMODE: Ratio must be between 0 and 1.")
        if (theta < 0.0 || theta > 180.0)
            call error (0,
	    "T_RMODE: Theta must be between 0 and 180 degrees.")
        if (fp_equalr (ratio, 0.0) && ! fp_equalr (theta, 0.0) &&
            ! fp_equalr (theta, 90.0) && ! fp_equalr (theta, 180.0))
            call error (0,
	        "T_RMODE: Cannot make 1D ring filter at given theta.")

	# Median filter each set of input and output images.
	while ((imtgetim (list1, Memc[image1], SZ_FNAME) != EOF) &&
	      (imtgetim (list2, Memc[image2], SZ_FNAME) != EOF)) {
	    
	    call xt_mkimtemp (Memc[image1], Memc[image2], Memc[imtemp],
	        SZ_FNAME)

	    im1 = immap (Memc[image1], READ_ONLY, 0)
	    im2 = immap (Memc[image2], NEW_COPY, im1)

	    kernel = NULL

 	    if (verbose) {
                call printf ( "Ring rin=%0.1f rout=%0.1f ")
                    call pargr (rinner)
                    call pargr (router)
                if (ratio < 1.0) {
                    call printf ("ratio=%0.2f theta=%0.1f ")
                        call pargr (ratio)
                        call pargr (theta)
                }
                call printf ("modal filter %s to %s\n")
                    call pargstr (Memc[image1])
                    call pargstr (Memc[imtemp])
                call flush (STDOUT)
            }

	    # Modal filter the image.
	    iferr {

		switch (IM_NDIM(im1)) {
		case 1:
		    call med_ell_gauss (rinner, 0.0, 0.0, a1, b1, c1, f1,
			nxk, nyk)
		    call med_ell_gauss (router, 0.0, 0.0, a2, b2, c2, f2,
			nxk, nyk)
		case 2:
		    call med_ell_gauss (rinner, ratio, theta, a1, b1, c1, f1,
			nxk, nyk)
		    call med_ell_gauss (router, ratio, theta, a2, b2, c2, f2,
			nxk, nyk)
		default:
		    call error (0,
		    "T_RMODE: Cannot modal filter a greater than 2D image.")
		}

		sz_val = nxk * nyk
		call calloc (kernel, sz_val, TY_SHORT)
		RMOD_NRING(med) = med_mkring (Mems[kernel], nxk, nyk,
		    a1, b1, c1, f1, a2, b2, c2, f2)

	        if (verbose) {
		    if (RMOD_ZLOW(med) > -MAX_REAL || RMOD_ZHIGH(med) <
                        MAX_REAL)  {
                        call printf (
                    "    Pixels < %g or > %g excluded from the modal filter\n")
                        if (RMOD_ZLOW(med) <= -MAX_REAL)
                            call pargr (INDEFR)
                        else
                            call pargr (RMOD_ZLOW(med))
                        if (RMOD_ZHIGH(med) >= MAX_REAL)
                            call pargr (INDEFR)
                        else
                            call pargr (RMOD_ZHIGH(med))
                    }
                    call flush (STDOUT)
                }

		call med_modring (med, im1, im2, boundary, constant,
		    Mems[kernel], nxk, nyk)

	    } then {
		call eprintf ("Error modal filtering image: %s\n")
		    call pargstr (Memc[image1])
		call erract (EA_WARN)
		call imunmap (im1)
		call imunmap (im2)
		call imdelete (Memc[image2])
	    } else {
	        call imunmap (im1)
	        call imunmap (im2)
	        call xt_delimtemp (Memc[image2], Memc[imtemp])
	    }

	    if (kernel != NULL)
		call mfree (kernel, TY_SHORT)
	}

	call imtclose (list1)
	call imtclose (list2)

	call mfree (med, TY_STRUCT)

	call sfree (sp)
end
