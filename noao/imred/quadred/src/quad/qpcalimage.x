include	<error.h>
include	<imset.h>
include	"ccdtypes.h"

define	SZ_SUBSET	16			# Maximum size of subset string
define	IMAGE	Memc[$1+($2-1)*SZ_FNAME]	# Image string
define	SUBSET	Memc[$1+($2-1)*SZ_SUBSET]	# Subset string

procedure t_qpcalimage ()

pointer	im, subsets, list
int	i, j
bool	flatcor, illumcor, fringecor, found, check
char	instrument[SZ_LINE], image[SZ_FNAME], buffer[SZ_SUBSET-1]

pointer	immap(), imtopenp()
int	imtgetim()
bool	clgetb(), streq()

begin
	# Open list of images and instrument file
	list = imtopenp ("images")
	call clgstr ("instrument", instrument, SZ_LINE)
	call hdmopen (instrument)

	if (clgetb ("only_param")) {
	    call cal_open (NULL)
	} else {
	    call cal_open (list)
	}

	check = clgetb ("check")

	if (clgetb ("zerocor")) {
	    iferr (call cal_find (ZERO, "", image, SZ_FNAME)) {
		if (check) {
		    call erract (EA_WARN)
		}

	    } else {
		call printf ("%s\n")
		    call pargstr (image)
	    }
	}

	if (clgetb ("darkcor")) {
	    iferr (call cal_find (DARK, "", image, SZ_FNAME)) {
		if (check) 
		    call erract (EA_WARN)

	    } else {
		call printf ("%s\n")
		    call pargstr (image)
	    }
	}

	flatcor   = clgetb ("flatcor")
	illumcor  = clgetb ("illumcor")
	fringecor = clgetb ("fringecor")

	if (flatcor || illumcor || fringecor) {

	    i = 1
	    found = false
	    while (imtgetim (list, image, SZ_FNAME) != EOF) {
		# Open the image.  Silently skip any non-existant images
		iferr (im = immap (image, READ_ONLY, 0)) 
		    next

		call ccdsubset (im, buffer, SZ_SUBSET-1)
		call imunmap (im)

		# Check to see if we have already dealt with this subset
		do j = 1, i - 1 {
		    found = (streq (buffer, SUBSET (subsets, j)))
		    if (found)
			break
		}

		if (!found) {

		    # Add subset to list of processed subsets
		    if (i == 1) 
			call malloc  (subsets, i * SZ_SUBSET, TY_CHAR)
		    else 
			call realloc (subsets, i * SZ_SUBSET, TY_CHAR)

		    call strcpy (buffer, SUBSET(subsets, i), SZ_SUBSET-1)
		    i = i + 1

		    # Find and print names of associated calibration images
		    if (flatcor) {
			iferr (call cal_find (FLAT, buffer, image, SZ_FNAME)) {
			    if (check) 
				call erract (EA_WARN)

			} else {
			    call printf ("%s\n")
				call pargstr (image)
			}
		    }

		    if (illumcor) {
			iferr (call cal_find (ILLUM, buffer, image, SZ_FNAME)) {
			    if (check) 
				call erract (EA_WARN)

			} else {
			    call printf ("%s\n")
				call pargstr (image)
			}
		    }

		    if (fringecor) {
			iferr (call cal_find (FRINGE, buffer, image, SZ_FNAME)){
			    if (check) 
				call erract (EA_WARN)

			} else {
			    call printf ("%s\n")
				call pargstr (image)
			}
		    }
		}
	    }
	}

	call hdmclose ()
	call imtclose (list)
	call mfree (subsets, TY_CHAR)
	call cal_close ()

end

# CAL_FIND  -- Return a calibration image of the specified type and subset
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

# CAL_FIND -- Return a calibration image of a particular type and subset.
# Search the calibration list for the first calibration image of the desired
# type and subset.  Print a warning if there  is more than one possible
# calibration image and return an error if there is no calibration image. 

procedure cal_find (ccdtype, subset, image, maxchars)

int	ccdtype			#I Callibration CCD image type
char	subset[ARB]		#I Calibration image subset
char	image[maxchars]		#O Calibration image (returned)
int	maxchars		#I Maximum number chars in image name

int	i
char	errmsg[SZ_LINE]
bool	strne()

pointer	ccdtypes	# Pointer to array of calibration ccdtypes
pointer	subsets		# Pointer to array of calibration subsets
pointer	images		# Pointer to array of calibration image names
int	nimages		# Number of images
common	/calib/ ccdtypes, subsets, images, nimages

begin

	switch (ccdtype) {
	case ZERO, DARK:
	    do i = 1, nimages {
		if (Memi[ccdtypes+i-1] != ccdtype)
		    next

		call strcpy (IMAGE(images,i), image, maxchars)
		return
	    }

	case FLAT, ILLUM, FRINGE:
	    do i = 1, nimages {
		if (Memi[ccdtypes+i-1] != ccdtype)
		    next
		if (strne (SUBSET(subsets,i), subset))
		    next

		call strcpy (IMAGE(images,i), image, maxchars)
		return
	    }
	}

	# If no calibration image is found then it is an error.
	switch (ccdtype) {
	case ZERO:
	    call error (0, "No zero level calibration image found")
	case DARK:
	    call error (0, "No dark count calibration image found")
	case FLAT:
	    call sprintf (errmsg, SZ_LINE,
		 "No flat field calibration image of subset %s found")
		 call pargstr (subset)
	    call error (0, errmsg)
	case ILLUM:
	    call sprintf (errmsg, SZ_LINE,
		 "No illumination calibration image of subset %s found")
		 call pargstr (subset)
	    call error (0, errmsg)
	case FRINGE:
	    call sprintf (errmsg, SZ_LINE,
		 "No fringe calibration image of subset %s found")
		 call pargstr (subset)
	    call error (0, errmsg)
	}
end

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

pointer	sp, str
int	ccdtype, strdic(), imtopenp()
bool	clgetb()

pointer	ccdtypes	# Pointer to array of calibration ccdtypes
pointer	subsets		# Pointer to array of calibration subset numbers
pointer	images		# Pointer to array of calibration image names
int	nimages		# Number of images
common	/calib/ ccdtypes, subsets, images, nimages

errchk	cal_list

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	call clgstr ("ccdtype", Memc[str], SZ_LINE)
	call xt_stripwhite (Memc[str])
	if (Memc[str] == EOS)
	    ccdtype = NONE
	else
	    ccdtype = strdic (Memc[str], Memc[str], SZ_LINE, CCDTYPES)

	# Add calibration images to list.
	nimages = 0
	if (ccdtype != ZERO && clgetb ("zerocor")) {
	    list1 = imtopenp ("zero")
	    call cal_list (list1, ZERO)
	    call imtclose (list1)
	}
	if (ccdtype != ZERO && ccdtype != DARK && clgetb ("darkcor")) {
	    list1 = imtopenp ("dark")
	    call cal_list (list1, DARK)
	    call imtclose (list1)
	}
	if (ccdtype != ZERO && ccdtype != DARK && ccdtype != FLAT &&
	    clgetb ("flatcor")) {
	    list1 = imtopenp ("flat")
	    call cal_list (list1, FLAT)
	    call imtclose (list1)
	}
	if (ccdtype != ZERO && ccdtype != DARK && ccdtype != FLAT &&
	    ccdtype != ILLUM && clgetb ("illumcor")) {
	    list1 = imtopenp ("illum")
	    call cal_list (list1, ILLUM)
	    call imtclose (list1)
	}
	if (ccdtype != ZERO && ccdtype != DARK && ccdtype != FLAT &&
	    ccdtype != FRINGE && clgetb ("fringecor")) {
	    list1 = imtopenp ("fringe")
	    call cal_list (list1, FRINGE)
	    call imtclose (list1)
	}
	if (list != NULL) {
	    call cal_list (list, UNKNOWN)
	    call imtrew (list)
	}

	call sfree (sp)
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
	    # Open the image.  If an explicit type is given it is an
	    # error if the image can't be opened.
	    iferr (im = immap (Memc[image], READ_ONLY, 0)) {
		if (listtype == UNKNOWN)
		    next
		else
		    call erract (EA_ERROR)
	    }

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
#	call eprintf ("nimages=%d\n")
#	    call pargi (nimages)
#	do i = 1, nimages {
#	    call eprintf ("ccdtype=%d subset=%s image=%s\n")
#		call pargi (Memi[ccdtypes+i-1])
#		call pargstr (SUBSET (subsets, i))
#		call pargstr (IMAGE (images, i))
#	}

	call sfree (sp)
end

# CCD_CMP -- Compare two image names with extensions ignored.

bool procedure ccd_cmp (image1, image2)

char	image1[ARB]		# First image
char	image2[ARB]		# Second image

int	i, j, strmatch(), strlen(), strncmp()
bool	streq()

begin
	if (streq (image1, image2))
	    return (true)

	i = max (strmatch (image1, ".imh"), strmatch (image1, ".hhh"))
	if (i == 0)
	    i = strlen (image1)
	j = max (strmatch (image2, ".imh"), strmatch (image2, ".hhh"))
	if (j == 0)
	    j = strlen (image2)

	return (strncmp (image1, image2, max (i, j)) == 0)
end
