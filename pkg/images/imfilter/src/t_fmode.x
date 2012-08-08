# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include "fmode.h"

# T_FMODE -- Modal filter a list of images in x and y.

procedure t_fmode()

bool	verbose
int	list1, list2, xwindow, ywindow, boundary
pointer	sp, imtlist1, imtlist2, image1, image2, imtemp, str, fmd, im1, im2
real	constant

bool	clgetb(), fp_equalr()
int	clgeti(), imtopen(), imtgetim(), imtlen(), clgwrd(), btoi()
pointer	immap()
real	clgetr()
errchk	fmd_modbox

begin
	# Allocate some working space.
	call smark (sp)
	call salloc (imtlist1, SZ_LINE, TY_CHAR)
	call salloc (imtlist2, SZ_LINE, TY_CHAR)
	call salloc (image1, SZ_FNAME, TY_CHAR)
	call salloc (image2, SZ_FNAME, TY_CHAR)
	call salloc (imtemp, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Allocate space for the fmode structure.
	call calloc (fmd, LEN_FMODE_STRUCT, TY_STRUCT)

	# Get the task parameters.
	call clgstr ("input", Memc[imtlist1], SZ_FNAME)
	call clgstr ("output", Memc[imtlist2], SZ_FNAME)

	# Get the window sizes.
	xwindow = clgeti ("xwindow")
	ywindow = clgeti ("ywindow")

	# Get the quantization parameters.
	FMOD_Z1(fmd) = clgetr ("zmin")
	FMOD_Z2(fmd) = clgetr ("zmax")
	FMOD_ZLOW(fmd) = clgetr ("zloreject")
	FMOD_ZHIGH(fmd) = clgetr ("zhireject")
	FMOD_HMIN(fmd) = clgeti ("hmin")
	FMOD_HMAX(fmd) = clgeti ("hmax")
	FMOD_UNMAP(fmd) = btoi (clgetb ("unmap"))

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

	# Modal filter each set of input and output images.
	while ((imtgetim (list1, Memc[image1], SZ_FNAME) != EOF) &&
	      (imtgetim (list2, Memc[image2], SZ_FNAME) != EOF)) {
	    
	    call xt_mkimtemp (Memc[image1], Memc[image2], Memc[imtemp],
	        SZ_FNAME)

	    im1 = immap (Memc[image1], READ_ONLY, 0)
	    im2 = immap (Memc[image2], NEW_COPY, im1)

	    if (mod (xwindow, 2) == 0)
	        FMOD_XBOX(fmd) = xwindow + 1
	    else
	        FMOD_XBOX(fmd) = xwindow
	    if (mod (ywindow, 2) == 0)
	        FMOD_YBOX(fmd) = ywindow + 1
	    else
	        FMOD_YBOX(fmd) = ywindow

	    if (verbose) {
                call printf ("%dx%d Box modal filter %s to %s\n")
                    call pargi (FMOD_XBOX(fmd))
                    call pargi (FMOD_YBOX(fmd))
                    call pargstr (Memc[image1])
                    call pargstr (Memc[imtemp])
                call flush (STDOUT)
            }

	    # Find input image max and min if necessary.
	    if (IS_INDEFR(FMOD_Z1(fmd)) || IS_INDEFR(FMOD_Z2(fmd))) 
	        call fmd_maxmin (im1, FMOD_XBOX(fmd), FMOD_YBOX(fmd),
		    boundary, constant, FMOD_ZMIN(fmd), FMOD_ZMAX(fmd))

	    if (verbose) {
                if (! fp_equalr (FMOD_Z1(fmd), real(FMOD_HMIN(fmd))) &&
                    ! fp_equalr (FMOD_Z2(fmd), real(FMOD_HMAX(fmd)))) {
                    call printf (
                  "    Pixels from %g to %g mapped to integers from %d to %d\n")
                        if (IS_INDEFR(FMOD_Z1(fmd)))
                            call pargr (FMOD_ZMIN(fmd))
                        else
                            call pargr (FMOD_Z1(fmd))
                        if (IS_INDEFR(FMOD_Z2(fmd)))
                            call pargr (FMOD_ZMAX(fmd))
                        else
                            call pargr (FMOD_Z2(fmd))
                        call pargi (FMOD_HMIN(fmd))
                        call pargi (FMOD_HMAX(fmd))
                }
                if (! IS_INDEFR(FMOD_ZLOW(fmd)) ||
                    ! IS_INDEFR(FMOD_ZHIGH(fmd)))  {
                    call printf (
                    "    Pixels < %g or > %g excluded from the modal filter\n")
                        call pargr (FMOD_ZLOW(fmd))
                        call pargr (FMOD_ZHIGH(fmd))
                }
                call flush (STDOUT)
            }

	    # Modal filter the image.
	    iferr {
		call fmd_modbox (fmd, im1, im2, boundary, constant)
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
	}

	call imtclose (list1)
	call imtclose (list2)

	call mfree (fmd, TY_STRUCT)

	call sfree (sp)
end
