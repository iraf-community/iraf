include	<error.h>
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

procedure cal_image (im, ccdtype, nscan, image, maxchars)

pointer	im		# Image to be processed
int	ccdtype		# Callibration CCD image type desired
int	nscan		# Number of scan rows desired
char	image[maxchars]	# Calibration image (returned)
int	maxchars	# Maximum number chars in image name

int	i, m, n
pointer	sp, subset, str
bool	strne(), ccd_cmp()

pointer	ccdtypes	# Pointer to array of calibration ccdtypes
pointer	subsets		# Pointer to array of calibration subsets
pointer	nscans		# Pointer to array of calibration nscan values
pointer	images		# Pointer to array of calibration image names
int	nimages		# Number of images
common	/calib/ ccdtypes, subsets, nscans, images, nimages

begin
	call smark (sp)
	call salloc (subset, SZ_SUBSET, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	m = 0
	n = 0
	switch (ccdtype) {
	case ZERO, DARK:
	    do i = 1, nimages {
		if (Memi[ccdtypes+i-1] != ccdtype)
		    next
		n = n + 1
		if (n == 1) {
		    m = i
		} else {
		    if (Memi[nscans+i-1] == Memi[nscans+m-1]) {
#			call eprintf (
#			    "Warning: Extra calibration image %s ignored\n")
#			    call pargstr (IMAGE(images,i))

			# Reset the image type to eliminate further warnings.
			Memi[ccdtypes+i-1] = UNKNOWN
		    } else if (Memi[nscans+m-1] != nscan &&
			       (Memi[nscans+i-1] == nscan ||
			       Memi[nscans+i-1] == 1)) {
			m = i
		    }
		}
	    }
	case FLAT, ILLUM, FRINGE:
	    call ccdsubset (im, Memc[subset], SZ_SUBSET)

	    do i = 1, nimages {
		if (Memi[ccdtypes+i-1] != ccdtype)
		    next
		if (strne (SUBSET(subsets,i), Memc[subset]))
		    next
		n = n + 1
		if (n == 1) {
		    m = i
		} else {
		    if (Memi[nscans+i-1] == Memi[nscans+m-1]) {
#			call eprintf (
#			    "Warning: Extra calibration image %s ignored\n")
#			    call pargstr (IMAGE(images,i))

			# Reset the image type to eliminate further warnings.
			Memi[ccdtypes+i-1] = UNKNOWN
		    } else if (Memi[nscans+m-1] != nscan &&
			       (Memi[nscans+i-1] == nscan ||
			       Memi[nscans+i-1] == 1)) {
			m = i
		    }
		}
	    }
	}

	# If no calibration image is found then it is an error.
	if (m == 0) {
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
	}

	call strcpy (IMAGE(images,m), image, maxchars)
	if (nscan != Memi[nscans+m-1]) {
	    if (nscan != 1 && Memi[nscans+m-1] == 1)
		call cal_scan (nscan, image, maxchars)
	    else {
		call sprintf (Memc[str], SZ_LINE,
	             "Cannot find or create calibration with nscan of %d")
		     call pargi (nscan)
	        call error (0, Memc[str])
	    }
	}

	# Check that the input image is not the same as the calibration image.
	call imstats (im, IM_IMAGENAME, Memc[str], SZ_LINE)
	if (ccd_cmp (Memc[str], IMAGE(images,m))) {
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
pointer	nscans		# Pointer to array of calibration nscan values
pointer	images		# Pointer to array of calibration image names
int	nimages		# Number of images
common	/calib/ ccdtypes, subsets, nscans, images, nimages

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
pointer	nscans		# Pointer to array of calibration nscan values
pointer	images		# Pointer to array of calibration image names
int	nimages		# Number of images
common	/calib/ ccdtypes, subsets, nscans, images, nimages

begin
	if (nimages > 0) {
	    call mfree (ccdtypes, TY_INT)
	    call mfree (subsets, TY_CHAR)
	    call mfree (nscans, TY_INT)
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

int	i, ccdtype, ccdtypei(), ccdnscan(), imtgetim()
pointer	sp, image, im, immap()
bool	streq()

pointer	ccdtypes	# Pointer to array of calibration ccdtypes
pointer	subsets		# Pointer to array of calibration subsets
pointer	nscans		# Pointer to array of calibration nscan values
pointer	images		# Pointer to array of calibration image names
int	nimages		# Number of images
common	/calib/ ccdtypes, subsets, nscans, images, nimages

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
		    call malloc (nscans, i, TY_INT)
		    call malloc (images, i * SZ_FNAME, TY_CHAR)
		} else {
		    call realloc (ccdtypes, i, TY_INT)
		    call realloc (subsets, i * SZ_FNAME, TY_CHAR)
		    call realloc (nscans, i, TY_INT)
		    call realloc (images, i * SZ_FNAME, TY_CHAR)
		}

		# Enter the ccdtype, subset, and image name.
		Memi[ccdtypes+i-1] = ccdtype
		Memi[nscans+i-1] = ccdnscan (im, ccdtype)
		call ccdsubset (im, SUBSET(subsets,i), SZ_SUBSET-1)
		call strcpy (Memc[image], IMAGE(images,i), SZ_FNAME-1)
		nimages = i
	    }
	    call imunmap (im)
	}
	call sfree (sp)
end


# CAL_SCAN -- Generate name for scan corrected calibration image.

procedure cal_scan (nscan, image, maxchar)

int	nscan			#I Number of scan lines
char	image[maxchar]		#U Input root name, output scan name
int	maxchar			#I Maximum number of chars in image name

bool	clgetb()
pointer	sp, root, ext

begin
	# Check if this operation is desired.
	if (!clgetb ("scancor") || nscan == 1)
	    return

	call smark (sp)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (ext, SZ_FNAME, TY_CHAR)

	call xt_imroot (image, Memc[root], SZ_FNAME)
	call xt_imext (image, Memc[ext], SZ_FNAME)
	if (IS_INDEFI (nscan)) {
	    call sprintf (image, maxchar, "%s.1d%s")
		call pargstr (Memc[root])
		call pargstr (Memc[ext])
	} else {
	    call sprintf (image, maxchar, "%s.%d%s")
		call pargstr (Memc[root])
		call pargi (nscan)
		call pargstr (Memc[ext])
	}

	call sfree (sp)
end
