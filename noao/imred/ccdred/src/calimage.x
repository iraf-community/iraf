include	<imset.h>
include	"ccdtypes.h"

define	SZ_SUBSET	16			# Maximum size of subset string
define	IMAGE	Memc[$1+($2-1)*SZ_FNAME]	# Image string
define	SUBSET	Memc[$1+($2-1)*SZ_SUBSET]	# Subset string

# CAL_IMAGE -- Return a calibration image for a specified input image.
# CAL_OPEN  -- Open the calibration image list.
# CAL_CLOSE -- Close the calibration image list.
# CAL_LIST  -- Add images to the calibration image list.
#
# The open procedure is called first to get the calibration image
# lists and add them to an internal list.  Calibration images from the
# input list are also added so that calibration images may be specified
# either from the calibration image list parameters or in the input image list.
# Existence errors and duplicate calibration images are ignored.
# Validity checks are made when the calibration images are requested.
#
# During processing the calibration image names are requested for each input
# image.  The calibration image list is searched for a calibration image of
# the right type and subset.  If more than one is found the first one is
# returned and a warning given for the others.  The warning is only issued
# once.  If no calibration image is found then an error is returned.
#
# The calibration image list must be closed at the end of processing the
# input images.


# CAL_IMAGE -- Return a calibration image of a particular type.
# Search the calibration list for the first calibration image of the desired
# type and subset.  Print a warning if there  is more than one possible
# calibration image and return an error if there is no calibration image. 

procedure cal_image (im, ccdtype, image, maxchars)

pointer	im		# Image to be processed
int	ccdtype		# Callibration CCD image type
char	image[maxchars]	# Calibration image (returned)
int	maxchars	# Maximum number chars in image name

int	i, n
pointer	sp, subset, str
bool	strne(), ccd_cmp()

pointer	ccdtypes	# Pointer to array of calibration ccdtypes
pointer	subsets		# Pointer to array of calibration subsets
pointer	images		# Pointer to array of calibration image names
int	nimages		# Number of images
common	/calib/ ccdtypes, subsets, images, nimages

begin
	call smark (sp)
	call salloc (subset, SZ_SUBSET, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	switch (ccdtype) {
	case ZERO, DARK:
	    n = 0
	    do i = 1, nimages {
		if (Memi[ccdtypes+i-1] != ccdtype)
		    next
		n = n + 1
		if (n == 1)
		    call strcpy (IMAGE(images,i), image, maxchars)
		else {
#		    call eprintf (
#			"Warning: Extra calibration image %s ignored\n")
#		        call pargstr (IMAGE(images,i))

		    # Reset the image type to eliminate further warnings.
		    Memi[ccdtypes+i-1] = UNKNOWN
		}
	    }
	case FLAT, ILLUM, FRINGE:
	    call ccdsubset (im, Memc[subset], SZ_SUBSET)

	    n = 0
	    do i = 1, nimages {
		if (Memi[ccdtypes+i-1] != ccdtype)
		    next
		if (strne (SUBSET(subsets,i), Memc[subset]))
		    next
		n = n + 1
		if (n == 1)
		    call strcpy (IMAGE(images,i), image, maxchars)
		else {
#		    call eprintf (
#			"Warning: Extra calibration image %s ignored\n")
#		        call pargstr (IMAGE(images,i))

		    # Reset the image type to eliminate further warnings.
		    Memi[ccdtypes+i-1] = UNKNOWN
		}
	    }
	}

	# If no calibration image is found then it is an error.
	if (n == 0)
	    switch (ccdtype) {
	    case ZERO:
	        call error (0, "No zero level calibration image found")
	    case DARK:
	        call error (0, "No dark count calibration image found")
	    case FLAT:
		call sprintf (Memc[str], SZ_LINE,
	             "No flat field calibration image of subset %s found")
		     call pargstr (Memc[subset])
	        call error (0, Memc[str])
	    case ILLUM:
		call sprintf (Memc[str], SZ_LINE,
	             "No illumination calibration image of subset %s found")
		     call pargstr (Memc[subset])
	        call error (0, Memc[str])
	    case FRINGE:
		call sprintf (Memc[str], SZ_LINE,
	             "No fringe calibration image of subset %s found")
		     call pargstr (Memc[subset])
	        call error (0, Memc[str])
	    }

	# Check that the input image is not the same as the calibration image.
	call imstats (im, IM_IMAGENAME, Memc[str], SZ_LINE)
	if (ccd_cmp (Memc[str], image)) {
	    call sprintf (Memc[str], SZ_LINE,
		"Calibration image %s is the same as the input image")
		call pargstr (image)
	    call error (0, Memc[str])
	}

	call sfree (sp)
end


# CAL_OPEN -- Create a list of calibration images from the input image list
# and the calibration image lists.

procedure cal_open (list)

int	list		# List of input images
int	list1		# List of calibration images

int	imtopenp()
bool	clgetb()

pointer	ccdtypes	# Pointer to array of calibration ccdtypes
pointer	subsets		# Pointer to array of calibration subset numbers
pointer	images		# Pointer to array of calibration image names
int	nimages		# Number of images
common	/calib/ ccdtypes, subsets, images, nimages

begin
	# Add calibration images to list.
	nimages = 0
	if (clgetb ("zerocor")) {
	    list1 = imtopenp ("zero")
	    call cal_list (list1, ZERO)
	    call imtclose (list1)
	}
	if (clgetb ("darkcor")) {
	    list1 = imtopenp ("dark")
	    call cal_list (list1, DARK)
	    call imtclose (list1)
	}
	if (clgetb ("flatcor")) {
	    list1 = imtopenp ("flat")
	    call cal_list (list1, FLAT)
	    call imtclose (list1)
	}
	if (clgetb ("illumcor")) {
	    list1 = imtopenp ("illum")
	    call cal_list (list1, ILLUM)
	    call imtclose (list1)
	}
	if (clgetb ("fringecor")) {
	    list1 = imtopenp ("fringe")
	    call cal_list (list1, FRINGE)
	    call imtclose (list1)
	}
	if (list != NULL) {
	    call cal_list (list, UNKNOWN)
	    call imtrew (list)
	}
end


# CAL_CLOSE -- Free memory from the internal calibration image list.

procedure cal_close ()

pointer	ccdtypes	# Pointer to array of calibration ccdtypes
pointer	subsets		# Pointer to array of calibration subset
pointer	images		# Pointer to array of calibration image names
int	nimages		# Number of images
common	/calib/ ccdtypes, subsets, images, nimages

begin
	if (nimages > 0) {
	    call mfree (ccdtypes, TY_INT)
	    call mfree (subsets, TY_CHAR)
	    call mfree (images, TY_CHAR)
	}
end


# CAL_LIST -- Add calibration images to an internal list.
# Map each image and get the CCD image type and subset.
# If the ccdtype is given as a procedure argument this overrides the
# image header type.  For the calibration images add the type, subset,
# and image name to dynamic arrays.  Ignore duplicate names.

procedure cal_list (list, listtype)

pointer	list		# Image list
int	listtype	# CCD type of image in list.
			# Overrides header type if not UNKNOWN.

int	i, ccdtype, ccdtypei(), imtgetim()
pointer	sp, image, im, immap()
bool	streq()

pointer	ccdtypes	# Pointer to array of calibration ccdtypes
pointer	subsets		# Pointer to array of calibration subsets
pointer	images		# Pointer to array of calibration image names
int	nimages		# Number of images
common	/calib/ ccdtypes, subsets, images, nimages

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)

	while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {
	    # Open the image and ignore errors.
	    iferr (im = immap (Memc[image], READ_ONLY, 0))
		next

	    # Override image header CCD type if a list type is given.
	    if (listtype == UNKNOWN)
	        ccdtype = ccdtypei (im)
	    else
		ccdtype = listtype
		
	    switch (ccdtype) {
	    case ZERO, DARK, FLAT, ILLUM, FRINGE:
		# Check for duplication.
		for (i=1; i<=nimages; i=i+1)
		    if (streq (Memc[image], IMAGE(images,i)))
			break
		if (i <= nimages)
		    break

		# Allocate memory for a new image.
		if (i == 1) {
		    call malloc (ccdtypes, i, TY_INT)
		    call malloc (subsets, i * SZ_SUBSET, TY_CHAR)
		    call malloc (images, i * SZ_FNAME, TY_CHAR)
		} else {
		    call realloc (ccdtypes, i, TY_INT)
		    call realloc (subsets, i * SZ_FNAME, TY_CHAR)
		    call realloc (images, i * SZ_FNAME, TY_CHAR)
		}

		# Enter the ccdtype, subset, and image name.
		Memi[ccdtypes+i-1] = ccdtype
		call ccdsubset (im, SUBSET(subsets,i), SZ_SUBSET-1)
		call strcpy (Memc[image], IMAGE(images,i), SZ_FNAME-1)
		nimages = i
	    }
	    call imunmap (im)
	}
	call sfree (sp)
end
