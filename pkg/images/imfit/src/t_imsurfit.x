# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <imhdr.h>
include "imsurfit.h"

# T_IMSURFIT -- Fit a surface function to an image
#
# 1. A user selected function is fit to each surface.
# 2. Only the selected regions of the image are fit.
# 3. Deviant pixels may be rejected from the fit.
# 4. The user selects the type of output image. The choices are:
#    a. the fitted image.
#    b. the input image with deviant  pixels replaced by
#       the fitted values
#    c. the input image minus the fitted image.
#    d. the ratio of the input image and the fit where
#       pixels less than div_min are set to a ratio of 1.


procedure t_imsurfit ()

char	imtlist1[SZ_LINE]			# Input image list
char	imtlist2[SZ_LINE]			# Output image list
char	image1[SZ_FNAME]			# Input image
char	image2[SZ_FNAME]			# Output image

char	str[SZ_LINE], region_str[SZ_LINE]
int	list1, list2, region_type
pointer	im1, im2, imfit, gl, sp

bool	clgetb()
int	imtopen(), imtgetim(), imtlen(), btoi(), clgeti(), clgwrd()
pointer	immap()
real	clgetr()

begin
	# Allocate space for imfit structure.
	call smark (sp)
	call salloc (imfit, LEN_IMSFSTRUCT, TY_STRUCT)

	# Get task parameters.
	call clgstr ("input", imtlist1, SZ_FNAME)
	call clgstr ("output", imtlist2, SZ_FNAME)
	TYPE_OUTPUT(imfit) = clgwrd ("type_output", str, SZ_LINE,
		      	     ",fit,clean,residual,response,")
	DIV_MIN(imfit) = clgetr ("div_min")

	# Get surface ftting parameters.
	SURFACE_TYPE(imfit) = clgwrd ("function", str, SZ_LINE,
			      ",legendre,chebyshev,spline3,spline1,")
	XORDER(imfit) = clgeti ("xorder")
	YORDER(imfit) = clgeti ("yorder")
	CROSS_TERMS(imfit) = btoi (clgetb ("cross_terms"))

	# Get median processing parameters.
	XMEDIAN(imfit) = clgeti ("xmedian")
	YMEDIAN(imfit) = clgeti ("ymedian")
	MEDIAN_PERCENT(imfit) = clgetr ("median_percent")
	if (XMEDIAN(imfit) > 1 || YMEDIAN(imfit) > 1)
	    MEDIAN(imfit) = YES
	else
	    MEDIAN(imfit) = NO

	# Get rejection cycle parameters.
	NITER(imfit) = clgeti ("niter")
	LOWER(imfit) = clgetr ("lower")
	UPPER(imfit) = clgetr ("upper")
	NGROW(imfit) = clgeti ("ngrow")

	if (MEDIAN(IMFIT) == YES) {
	    REJECT(imfit) = NO
	    NITER(imfit) = 0
	} else if (NITER(imfit) > 0 && (LOWER(imfit) > 0. || UPPER(imfit) > 0.))
	    REJECT(imfit) = YES
	else {
	    REJECT(imfit) = NO
	    NITER(imfit) = 0
	}

	# Checking sigmas for cleaning.
	if (TYPE_OUTPUT(imfit) == CLEAN && MEDIAN(imfit) == YES)
	    call error (0,
	    "T_IMSURFIT: Clean option and median processing are exclusive.")
	if (TYPE_OUTPUT(imfit) == CLEAN && NITER(imfit) <= 0)
	    call error (0, "T_IMSURFIT: Clean option requires non-zero niter.")
	if (TYPE_OUTPUT(imfit) == CLEAN && LOWER(imfit) <= 0. &&
	    UPPER(imfit) <= 0.)
	    call error (0, "T_IMSURFIT: Clean option requires non-zero sigma.")

	# Get regions to be fit.
	gl = NULL
	region_type = clgwrd ("regions", str, SZ_LINE,
		      ",all,columns,rows,border,sections,circle,invcircle,")
	switch (region_type) {
	case ALL:
	    ;
	case BORDER:
	    call clgstr ("border", region_str, SZ_LINE)
	case SECTIONS:
	    call clgstr ("sections", region_str, SZ_LINE)
	case COLUMNS:
	    call clgstr ("columns", region_str, SZ_LINE)
	case ROWS:
	    call clgstr ("rows", region_str, SZ_LINE)
	case CIRCLE, INVCIRCLE:
	    call clgstr ("circle", region_str, SZ_LINE)
	}

	# Expand the input and output image lists.
	list1 = imtopen (imtlist1)
	list2 = imtopen (imtlist2)
	if (imtlen (list1) != imtlen (list2)) {
	    call imtclose (list1)
	    call imtclose (list2)
	    call error (0, "Number of input and output images not the same.")
	}

	# Do each set of input and output images.
	while ((imtgetim (list1, image1, SZ_FNAME) != EOF) &&
	      (imtgetim (list2, image2, SZ_FNAME) != EOF)) {
	    
	    im1 = immap (image1, READ_ONLY, 0)
	    im2 = immap (image2, NEW_COPY, im1)

	    iferr {
	        if (region_type != ALL)
		    call make_good_list (im1, gl, region_type, region_str)
	        call imsurfit (im1, im2, imfit, gl)
	    } then
		call erract (EA_WARN)

	    call imunmap (im1)
	    call imunmap (im2)
	    call prl_free (gl)
	}

	# Cleanup.
	call sfree (sp)
	call imtclose (list1)
	call imtclose (list2)
end


# MAKE_GOOD_LIST -- Procedure to make a list of good regions. The program
# returns an error message if no good regions are defined. The good
# list parameter is set to NULL if the whole image is to be fit. This routine
# uses both the ranges and pixlist package which will be replaced by image
# masking.

procedure make_good_list (im, gl, region_type, region_string)

pointer	im			# pointer to the image
pointer	gl			# good pixel list descriptor
int	region_type		# type of good region
char	region_string[ARB]	# region parameters

int	i, ip, zero, nvals, range_min, r2, xdist, max_nranges
int	x1, x2, y1, y2, temp, border, xcenter, ycenter, radius
int	columns[7]
pointer	sp, ranges, list

bool	is_in_rangelist()
int	is_next_number(), is_decode_ranges(), open(), fscan(), nscan(), ctoi()
errchk	open, close

begin
	# Determine the maximum number of images.
	max_nranges = IM_LEN(im,1)

	# Allocate working space.
	call smark (sp)
	call salloc (ranges, 3 * max_nranges + 1, TY_INT)

	# Compute the good pixel list.
	switch (region_type) {
	case ROWS:

	    # Decode the row ranges.
	    if (is_decode_ranges (region_string, Memi[ranges], max_nranges, 1,
	        int (IM_LEN(im,2)), nvals) == ERR)
		call error (0, "MAKE_GOOD_LIST: Error decoding row string.")
	    if (nvals == 0) 
		call error (0, "MAKE_GOOD_LIST: no good rows.") 
	    if (nvals == IM_LEN(im,2)) {
		call sfree (sp)
		return
	    }

	    # Intialize the good pixel list.
	    call prl_init (gl, int (IM_LEN(im,1)), int (IM_LEN(im,2)))

	    # Set column limits using the ranges format.
	    columns[1] = 1
	    columns[2] = IM_LEN(im,1)
	    columns[3] = 1
	    columns[4] = NULL

	    # Set column limits for the specied lines.
	    zero = 0
	    range_min = is_next_number (Memi[ranges], zero)
	    while (range_min != EOF) {
	        for (i = range_min; i <= IM_LEN(im,2) + 1; i = i + 1) {
		    if (!is_in_rangelist (Memi[ranges], i) ||
		        i == IM_LEN(im,2)+1) {
		        call prl_put_ranges (gl, range_min, i-1, columns)
		        break
		    }
		}
		range_min = is_next_number (Memi[ranges], i)
	    }

	case COLUMNS:

	    # Set the specified columns.
	    if (is_decode_ranges (region_string, Memi[ranges], max_nranges, 1,
	        int (IM_LEN(im,1)), nvals) == ERR)
		call error (0, "MAKE_GOOD_LIST: Error decoding column string.")
	    if (nvals == 0)
		call error (0, "MAKE_GOOD_LIST: No good columns.")
	    if (nvals == IM_LEN(im,1)) {
		call sfree (sp)
		return
	    }

	    # Make the good pixel list.
	    call prl_init (gl, int (IM_LEN(im,1)), int (IM_LEN(im,2)))
	    call prl_add_ranges (gl, 1, int (IM_LEN(im,2)), Memi[ranges])

	case CIRCLE, INVCIRCLE:
	    
	    # Get the parameters of the circle.
	    ip = 1
	    if (ctoi (region_string, ip, xcenter) <= 0)
		call error (0, "MAKE_GOOD_LIST: Error decoding xcenter.")
	    if (ctoi (region_string, ip, ycenter) <= 0)
		call error (0, "MAKE_GOOD_LIST: Error decoding ycenter.")
	    if (ctoi (region_string, ip, radius) <= 0)
		call error (0, "MAKE_GOOD_LIST: Error decoding radius.")

	    y1 = max (1, ycenter - radius)
	    y2 = min (int (IM_LEN(im,2)), ycenter + radius)
	    x1 = max (1, xcenter - radius)
	    x2 = min (int (IM_LEN(im,1)), xcenter + radius)
	    if (region_type == CIRCLE) {
	        if (y1 > IM_LEN(im,2) || y2 < 1 || x1 > IM_LEN(im,1) || x2 < 1)
		    call error (0, "MAKE_GOOD_LIST: No good regions.")
	    }

	    # Create the good pixel list.
	    call prl_init (gl, int (IM_LEN(im,1)), int (IM_LEN(im,2)))

	    r2 = radius ** 2
	    if (region_type == CIRCLE) {
	        do i = y1, y2 {
		    xdist = sqrt (real (r2 - (ycenter - i) ** 2))
		    x1 = max (1, xcenter - xdist)
		    x2 = min (IM_LEN(im,1), xcenter + xdist)
		    columns[1] = x1
		    columns[2] = x2
		    columns[3] = 1
		    columns[4] = NULL
		    call prl_put_ranges (gl, i, i, columns)
	        }
	    } else if (region_type == INVCIRCLE) {
		do i = 1, y1 - 1 {
		    columns[1] = 1
		    columns[2] = IM_LEN(im,1)
		    columns[3] = 1
		    columns[4] = NULL
		    call prl_put_ranges (gl, i, i, columns)
		}
		do i = y2 + 1, IM_LEN(im,2) {
		    columns[1] = 1
		    columns[2] = IM_LEN(im,1)
		    columns[3] = 1
		    columns[4] = NULL
		    call prl_put_ranges (gl, i, i, columns)
		}
		do i = y1, y2 {
		    xdist = sqrt (real (r2 - (ycenter - i) ** 2))
		    x1 = max (1, xcenter - xdist)
		    x2 = min (IM_LEN(im,1), xcenter + xdist)
		    if (x1 > 1) {
			columns[1] = 1
			columns[2] = x1 - 1
			columns[3] = 1
			if (x2 < IM_LEN(im,1)) {
			    columns[4] = x2 + 1
			    columns[5] = IM_LEN(im,1)
			    columns[6] = 1
			    columns[7] = NULL
			} else
			    columns[4] = NULL
		    } else if (x2 < IM_LEN(im,1)) {
			columns[1] = x2 + 1
			columns[2] = IM_LEN(im,1)
			columns[3] = 1
			columns[4] = NULL
		    } else
			columns[1] = NULL
		    call prl_put_ranges (gl, i, i, columns)
		}
	    }


	case SECTIONS:

	    # Open file of sections.
	    list = open (region_string, READ_ONLY, TEXT_FILE)
	    call prl_init (gl, int (IM_LEN(im,1)), int (IM_LEN(im,2)))

	    # Scan the list.
	    while (fscan (list) != EOF) {
		
		# Fetch parameters from list.
		call gargi (x1)
		call gargi (x2)
		call gargi (y1)
		call gargi (y2)
		if (nscan() != 4)
		    next
		
		# Check and correct for out of bounds limits.
		x1 = max (1, min (IM_LEN(im,1), x1))
		x2 = min (IM_LEN(im,1), max (1, x2))
		y1 = max (1, min (IM_LEN(im,2), y1))
		y2 = min (IM_LEN(im,2), max (1, y2))

		# Check the order.
		if (x2 < x1) {
		    temp = x1
		    x1 = x2
		    x2 = temp
		}
		if (y2 < y1) {
		    temp = y1
		    y1 = y2
		    y2 = temp
		}

		# If entire image return.
		if ((x1 == 1) && (x2 == IM_LEN(im,1)) && (y1 == 1) &&
		    (y2 == IM_LEN(im,2))) {
		    call prl_free (gl)
		    gl = NULL
		    break
		}

		# Set ranges.
		columns[1] = x1
		columns[2] = x2
		columns[3] = 1
		columns[4] = NULL
		call prl_add_ranges (gl, y1, y2, columns)
	    }

	    call close (list)

	case BORDER:

	    # Decode border parameter.
	    ip = 1
	    if (ctoi (region_string, ip, border) == ERR)
		call error (0, "MAKE_GOOD_LIST: Error decoding border string.")
	    if (border < 1)
		call error (0, "MAKE_GOOD_LIST: No border.")
	    if ((border > IM_LEN(im,1)/2) && (border > IM_LEN(im,2)/2)) {
		call sfree (sp)
		return
	    }

	    # Intialize list.
	    call prl_init (gl, int (IM_LEN(im,1)), int (IM_LEN(im,2)))
	    y1 = 1 + border - 1
	    y2 = IM_LEN(im,2) - border + 1
	    columns[1] = 1
	    columns[2] = IM_LEN(im,1)
	    columns[3] = 1
	    columns[4] = NULL

	    # Set ranges for top and bottom edges of image.
	    call prl_put_ranges (gl, 1, y1, columns)
	    call prl_put_ranges (gl, y2, int (IM_LEN(im,2)), columns)

	    columns[1] = 1
	    columns[2] = y1
	    columns[3] = 1
	    columns[4] = NULL
	    call prl_put_ranges (gl, y1 + 1, y2 - 1, columns)

	    columns[1] = IM_LEN(im,1) - border + 1
	    columns[2] = IM_LEN(im,1)
	    columns[3] = 1
	    columns[4] = NULL
	    call prl_add_ranges (gl, y1 + 1, y2 - 1, columns)
	}

	call sfree (sp)
end
