# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <imhdr.h>
include <mach.h>
include "rmedian.h"

# T_RMEDIAN -- Ring median filter a list of images in x and y.

procedure t_rmedian()

bool	verbose
int	list1, list2, boundary, nxk, nyk
pointer	sp, imtlist1, imtlist2, image1, image2, imtemp, str
pointer	med, im1, im2, kernel
real	rinner, router, ratio, theta, constant, a1, b1, c1, f1, a2, b2, c2, f2

bool	clgetb(), fp_equalr()
int	imtopen(), imtgetim(), imtlen(), clgwrd()
int	med_mkring()
pointer	immap()
real	clgetr()
errchk	med_ell_gauss, med_mkring, med_medring

begin
	# Allocate some working space.
	call smark (sp)
	call salloc (imtlist1, SZ_LINE, TY_CHAR)
	call salloc (imtlist2, SZ_LINE, TY_CHAR)
	call salloc (image1, SZ_FNAME, TY_CHAR)
	call salloc (image2, SZ_FNAME, TY_CHAR)
	call salloc (imtemp, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Allcoate space for the rmedian structure.
	call calloc (med, LEN_RMEDIAN_STRUCT, TY_STRUCT)

	# Get the task parameters.
	call clgstr ("input", Memc[imtlist1], SZ_FNAME)
	call clgstr ("output", Memc[imtlist2], SZ_FNAME)

	# Get the ring filter parameters.
	rinner = clgetr ("rinner")
	router = clgetr ("router")
	ratio = clgetr ("ratio")
	theta = clgetr ("theta")

	# Get the rejection parameters.
	RMED_ZLOW(med) = clgetr ("zloreject")
	if (IS_INDEFR(RMED_ZLOW(med)))
	    RMED_ZLOW(med) = -MAX_REAL
	RMED_ZHIGH(med) = clgetr ("zhireject")
	if (IS_INDEFR(RMED_ZHIGH(med)))
	    RMED_ZHIGH(med) = MAX_REAL

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
            call error (0, "T_RMEDIAN: Router must be greater than 0.")
	if (rinner >= router)
	    call error (0, "T_RMEDIAN: Rinner must be less than router.")
        if (ratio < 0.0 || ratio > 1.0)
            call error (0, "T_RMEDIAN: Ratio must be between 0 and 1.")
        if (theta < 0.0 || theta > 180.0)
            call error (0,
	    "T_RMEDIAN: Theta must be between 0 and 180 degrees.")
        if (fp_equalr (ratio, 0.0) && ! fp_equalr (theta, 0.0) &&
            ! fp_equalr (theta, 90.0) && ! fp_equalr (theta, 180.0))
            call error (0,
	        "T_RMEDIAN: Cannot make 1D ring filter at given theta.")

	# Median filter each set of input and output images.
	while ((imtgetim (list1, Memc[image1], SZ_FNAME) != EOF) &&
	      (imtgetim (list2, Memc[image2], SZ_FNAME) != EOF)) {
	    
	    call xt_mkimtemp (Memc[image1], Memc[image2], Memc[imtemp],
	        SZ_FNAME)

	    im1 = immap (Memc[image1], READ_ONLY, 0)
	    im2 = immap (Memc[image2], NEW_COPY, im1)

	    if (verbose) {
                call printf ( "Ring rin=%0.1f rout=%0.1f ")
                    call pargr (rinner)
                    call pargr (router)
                if (ratio < 1.0) {
                    call printf ("ratio=%0.2f theta=%0.1f ")
                        call pargr (ratio)
                        call pargr (theta)
                }
                call printf ("median filter %s to %s\n")
                    call pargstr (Memc[image1])
                    call pargstr (Memc[imtemp])
                call flush (STDOUT)
            }

	    kernel = NULL

	    # Median filter the image.
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
		    "T_RMEDIAN: Cannot median filter a greater than 2D image.")
		}

		call calloc (kernel, nxk * nyk, TY_SHORT)
		RMED_NRING(med) = med_mkring (Mems[kernel], nxk, nyk,
		    a1, b1, c1, f1, a2, b2, c2, f2)

               if (verbose) {
	           if (RMED_ZLOW(med) > -MAX_REAL || RMED_ZHIGH(med) <
		       MAX_REAL)  {
                        call printf (
                    "    Pixels < %g or > %g excluded from the median filter\n")
                        if (RMED_ZLOW(med) <= -MAX_REAL)
                            call pargr (INDEFR)
                        else
                            call pargr (RMED_ZLOW(med))
                        if (RMED_ZHIGH(med) >= MAX_REAL)
                            call pargr (INDEFR)
                        else
                            call pargr (RMED_ZHIGH(med))
                    }
                    call flush (STDOUT)
                }

		call med_medring (med, im1, im2, boundary, constant,
		    Mems[kernel], nxk, nyk)

	    } then {
		call eprintf ("Error median filtering image: %s\n")
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
