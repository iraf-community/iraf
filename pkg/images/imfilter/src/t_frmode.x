# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <imhdr.h>
include "frmode.h"

# T_FRMODE -- Ring modal filter a list of images in x and y.

procedure t_frmode()

bool	verbose
int	list1, list2, boundary, nxk, nyk
pointer	sp, imtlist1, imtlist2, image1, image2, imtemp, str
pointer	fmd, im1, im2, kernel
real	rinner, router, ratio, theta, constant, a1, b1, c1, f1, a2, b2, c2, f2

bool	clgetb(), fp_equalr()
int	clgeti(), imtopen(), imtgetim(), imtlen(), clgwrd(), btoi()
int	med_mkring()
pointer	immap()
real	clgetr()
errchk	med_ell_gauss, med_mkring, fmd_maxmin, fmd_modring

begin
	# Allocate some working space.
	call smark (sp)
	call salloc (imtlist1, SZ_LINE, TY_CHAR)
	call salloc (imtlist2, SZ_LINE, TY_CHAR)
	call salloc (image1, SZ_FNAME, TY_CHAR)
	call salloc (image2, SZ_FNAME, TY_CHAR)
	call salloc (imtemp, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Allcoate space for the fmode structure.
	call calloc (fmd, LEN_FRMODE_STRUCT, TY_STRUCT)

	# Get the task parameters.
	call clgstr ("input", Memc[imtlist1], SZ_FNAME)
	call clgstr ("output", Memc[imtlist2], SZ_FNAME)

	# Get the ring filter parameters.
	rinner = clgetr ("rinner")
	router = clgetr ("router")
	ratio = clgetr ("ratio")
	theta = clgetr ("theta")

	# Get the quantization parameters.
	FRMOD_Z1(fmd) = clgetr ("zmin")
	FRMOD_Z2(fmd) = clgetr ("zmax")
	FRMOD_ZLOW(fmd) = clgetr ("zloreject")
	FRMOD_ZHIGH(fmd) = clgetr ("zhireject")
	FRMOD_HMIN(fmd) = clgeti ("hmin")
	FRMOD_HMAX(fmd) = clgeti ("hmax")
	FRMOD_UNMAP(fmd) = btoi (clgetb ("unmap"))

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
            call error (0, "T_FRMODE: Router must be greater than 0.")
	if (rinner >= router)
	    call error (0, "T_FRMODE: Rinner must be less than router.")
        if (ratio < 0.0 || ratio > 1.0)
            call error (0, "T_FRMODE: Ratio must be between 0 and 1.")
        if (theta < 0.0 || theta > 180.0)
            call error (0,
	    "T_FRMODE: Theta must be between 0 and 180 degrees.")
        if (fp_equalr (ratio, 0.0) && ! fp_equalr (theta, 0.0) &&
            ! fp_equalr (theta, 90.0) && ! fp_equalr (theta, 180.0))
            call error (0,
	        "T_FRMODE: Cannot make 1D ring filter at given theta.")

	# Modal filter each set of input and output images.
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
		call printf ("modal filter %s to %s\n")
                    call pargstr (Memc[image1])
                    call pargstr (Memc[imtemp])
                call flush (STDOUT)
            }

	    kernel = NULL

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
		    "T_FRMODE: Cannot modal filter a greater than 2D image.")
		}

		call calloc (kernel, nxk * nyk, TY_SHORT)
		FRMOD_NRING(fmd) = med_mkring (Mems[kernel], nxk, nyk,
		    a1, b1, c1, f1, a2, b2, c2, f2)

	        # Find input image max and min if necessary.
	        if (IS_INDEFR(FRMOD_Z1(fmd)) || IS_INDEFR(FRMOD_Z2(fmd))) 
	            call fmd_maxmin (im1, nxk, nyk, boundary, constant,
		    FRMOD_ZMIN(fmd), FRMOD_ZMAX(fmd))

               if (verbose) {
                    if (! fp_equalr (FRMOD_Z1(fmd), real(FRMOD_HMIN(fmd))) &&
                    ! fp_equalr (FRMOD_Z2(fmd), real(FRMOD_HMAX(fmd)))) {
                        call printf (
                  "    Pixels from %g to %g mapped to integers from %d to %d\n")
                        if (IS_INDEFR(FRMOD_Z1(fmd)))
                            call pargr (FRMOD_ZMIN(fmd))
                        else
                            call pargr (FRMOD_Z1(fmd))
                        if (IS_INDEFR(FRMOD_Z2(fmd)))
                            call pargr (FRMOD_ZMAX(fmd))
                        else
                            call pargr (FRMOD_Z2(fmd))
                        call pargi (FRMOD_HMIN(fmd))
                        call pargi (FRMOD_HMAX(fmd))
                    }
                    if (! IS_INDEFR(FRMOD_ZLOW(fmd)) ||
                        ! IS_INDEFR(FRMOD_ZHIGH(fmd)))  {
                        call printf (
                    "    Pixels < %g or > %g excluded from the modal filter\n")
                            call pargr (FRMOD_ZLOW(fmd))
                            call pargr (FRMOD_ZHIGH(fmd))
                    }
                    call flush (STDOUT)
                }


		call fmd_modring (fmd, im1, im2, boundary, constant,
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

	call mfree (fmd, TY_STRUCT)

	call sfree (sp)
end
