# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <error.h>
include "median.h"

# T_MEDIAN -- Median filter an image in x and y.

procedure t_median()

bool	verbose
long	xwindow, ywindow
int	boundary
pointer	list1, list2
pointer	sp, imtlist1, imtlist2, image1, image2, imtemp, str, im1, im2, mde
real	constant
long	c_2
size_t	sz_val

bool	clgetb()
int	imtgetim(), imtlen(), clgwrd()
long	clgetl(), lmod()
pointer	imtopen(), immap()
real	clgetr()
include	<nullptr.inc>
errchk	mde_medbox

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

	# Allocate the median fitting structure
	sz_val = LEN_MEDIAN_STRUCT
	call calloc (mde, sz_val, TY_STRUCT)

	# Get task parameters
	call clgstr ("input", Memc[imtlist1], SZ_LINE)
	call clgstr ("output", Memc[imtlist2], SZ_LINE)

	# Get algorithm parameters.
	xwindow = clgetl ("xwindow")
	ywindow = clgetl ("ywindow")
	MED_ZLOW(mde) = clgetr ("zloreject")
	if (IS_INDEFR(MED_ZLOW(mde)))
	    MED_ZLOW(mde) = -MAX_REAL
	MED_ZHIGH(mde) = clgetr ("zhireject")
	if (IS_INDEFR(MED_ZHIGH(mde)))
	    MED_ZHIGH(mde) = MAX_REAL

	# Get boundary extension parameters
	boundary = clgwrd ("boundary", Memc[str], SZ_LINE,
	    ",constant,nearest,reflect,wrap,")
	constant = clgetr ("constant")
	verbose = clgetb ("verbose")

	list1 = imtopen (Memc[imtlist1])
	list2 = imtopen (Memc[imtlist2])
	if (imtlen (list1) != imtlen (list2)) {
	    call imtclose (list1)
	    call imtclose (list2)
	    call error (0, "Number of input and output images not the same.")
	}

	# Median filter the input images
	while ((imtgetim (list1, Memc[image1], SZ_FNAME) != EOF) &&
	      (imtgetim (list2, Memc[image2], SZ_FNAME) != EOF)) {
	    
	    call xt_mkimtemp (Memc[image1], Memc[image2], Memc[imtemp],
	        SZ_FNAME)

	    im1 = immap (Memc[image1], READ_ONLY, NULLPTR)
	    im2 = immap (Memc[image2], NEW_COPY, im1)

	    if (lmod (xwindow, c_2) == 0)
	        MED_XBOX(mde) = xwindow + 1
	    else
	        MED_XBOX(mde) = xwindow
	    if (lmod (ywindow, c_2) == 0)
	        MED_YBOX(mde) = ywindow + 1
	    else
	        MED_YBOX(mde) = ywindow

	    if (verbose) {
                call printf ("%dx%d Box median filter %s to %s\n")
                    call pargz (MED_XBOX(mde))
                    call pargz (MED_YBOX(mde))
                    call pargstr (Memc[image1])
                    call pargstr (Memc[imtemp])
                if (MED_ZLOW(mde) > -MAX_REAL || MED_ZHIGH(mde) < MAX_REAL)  {
                    call printf (
                    "    Pixels < %g or > %g excluded from the median filter\n")
		    if (MED_ZLOW(mde) <= -MAX_REAL)
			call pargr (INDEFR)
		    else
                        call pargr (MED_ZLOW(mde))
		    if (MED_ZHIGH(mde) >= MAX_REAL)
			call pargr (INDEFR)
		    else
                        call pargr (MED_ZHIGH(mde))
                }
                call flush (STDOUT)
            }


	    # Median filter an image
	    iferr {
		call mde_medbox (mde, im1, im2, boundary, constant)
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
	}

	call imtclose (list1)
	call imtclose (list2)

	call mfree (mde, TY_STRUCT)

	call sfree (sp)
end
