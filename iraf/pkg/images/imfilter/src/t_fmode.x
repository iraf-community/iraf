# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include "fmode.h"

# T_FMODE -- Modal filter a list of images in x and y.

procedure t_fmode()

bool	verbose
long	xwindow, ywindow
int	boundary
pointer	list1, list2
pointer	sp, imtlist1, imtlist2, image1, image2, imtemp, str, fmd, im1, im2
real	constant
size_t	sz_val
long	c_2

bool	clgetb(), fp_equalr()
long	clgetl()
int	clgeti(), imtgetim(), imtlen(), clgwrd(), btoi()
pointer	imtopen(), immap()
real	clgetr()
long	lmod()
errchk	fmd_modbox
include	<nullptr.inc>

begin
	c_2 = 2
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

	# Allocate space for the fmode structure.
	sz_val = LEN_FMODE_STRUCT
	call calloc (fmd, sz_val, TY_STRUCT)

	# Get the task parameters.
	call clgstr ("input", Memc[imtlist1], SZ_FNAME)
	call clgstr ("output", Memc[imtlist2], SZ_FNAME)

	# Get the window sizes.
	xwindow = clgetl ("xwindow")
	ywindow = clgetl ("ywindow")

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

	    im1 = immap (Memc[image1], READ_ONLY, NULLPTR)
	    im2 = immap (Memc[image2], NEW_COPY, im1)

	    if (lmod (xwindow, c_2) == 0)
	        FMOD_XBOX(fmd) = xwindow + 1
	    else
	        FMOD_XBOX(fmd) = xwindow
	    if (lmod (ywindow, c_2) == 0)
	        FMOD_YBOX(fmd) = ywindow + 1
	    else
	        FMOD_YBOX(fmd) = ywindow

	    if (verbose) {
                call printf ("%dx%d Box modal filter %s to %s\n")
                    call pargz (FMOD_XBOX(fmd))
                    call pargz (FMOD_YBOX(fmd))
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
