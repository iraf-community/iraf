include	<fset.h>
include	<imhdr.h>
include	<imio.h>
include	<pkg/dttext.h>
include	"identify.h"

# T_REIDENTIFY -- Reidentify features starting from reference features.

procedure t_reidentify ()

char	refimage[SZ_FNAME]	# Reference image
char	images[SZ_LINE]		# List of images
double	shft			# Initial shift between ref features and image
int	step			# Step size
int	nlost			# Maximum number of features to be lost
bool	verbose			# Verbose logfiles?

int	i, list, pfd, fd, nlogfd, nreid
char	newimage[SZ_FNAME]
pointer	id, logfile, str, logfd

int	clgeti(), clpopnu(), clgfil(), open(), btoi()
int	imtopen(), id_getim()
bool	clgetb()
real	clgetr()
double	clgetd()
errchk	id_dbread()

begin
	# Allocate the basic data structure.
	call id_init (id)

	# Get task parameters.
	call clgstr ("reference", refimage, SZ_FNAME)
 	list = imtopen (refimage)
 	i = id_getim (list, refimage, SZ_FNAME)
 	call imtclose (list)
	call clgstr ("images", images, SZ_LINE)
	list = imtopen (images)
	call clgstr ("section", Memc[ID_SECTION(id)], SZ_FNAME)
	call xt_mk1d (refimage, Memc[ID_SECTION(id)], SZ_FNAME)
	shft = clgetd ("shift")
	step = clgeti ("step")

	nlost = clgeti ("nlost")
	call clgstr ("database", Memc[ID_DATABASE(id)], SZ_FNAME)
	ID_NSUM(id) = clgeti ("nsum")
	ID_CRADIUS(id) = clgetr ("cradius")
	ID_THRESHOLD(id) = clgetr ("threshold")
	ID_LOGFILES(id) = clpopnu ("logfiles")
	ID_REFIT(id) = btoi (clgetb ("refit"))
	verbose = clgetb ("verbose")

	# Open the logfiles and write a header.
	call malloc (logfile, SZ_FNAME, TY_CHAR)
	call malloc (str, SZ_LINE, TY_CHAR)

	i = 0
	while (clgfil (ID_LOGFILES(id), Memc[logfile], SZ_FNAME) != EOF) {
	    fd = open (Memc[logfile], APPEND, TEXT_FILE)
	    call fseti (fd, F_FLUSHNL, YES)
	    call sysid (Memc[str], SZ_LINE)
	    call fprintf (fd, "\nREIDENTIFY: %s\n")
		call pargstr (Memc[str])

	    i = i + 1
	    if (i == 1)
		call malloc (logfd, i, TY_INT)
	    else
		call realloc (logfd, i, TY_INT)
	    Memi[logfd+i-1] = fd
	}
	nlogfd = i

	# Open the plotfile.
	call clgstr ("plotfile", Memc[logfile], SZ_FNAME)
	iferr (pfd = open (Memc[logfile], APPEND, BINARY_FILE))
	    pfd = NULL

	call mfree (logfile, TY_CHAR)
	call mfree (str, TY_CHAR)
	call clpcls (ID_LOGFILES(id))

	# Initialize ICFIT.
	call ic_open (ID_IC(id))

	# Get the database entry for the reference image.
	call id_dbread (id, refimage, NO)
	nreid = max (1, ID_NFEATURES(id) - nlost)

	# Set fixed parameters used in id_shift.
	ID_FWIDTH(id) = FWIDTH(id,1)
	ID_FTYPE(id) = FTYPE(id,1)
	ID_MINSEP(id) = 1.
	ID_MAXFEATURES(id) = 50

	# Expand the image template and trace features in each image.
	while (id_getim (list, newimage, SZ_FNAME) != EOF) {
	    for (i = 1; i <= nlogfd; i = i + 1) {
		fd = Memi[logfd+i-1]
		call fprintf (fd,
		"  Reference image = %s, New image = %s, Refit = %b\n")
		    call pargstr (refimage)
		    call pargstr (newimage)
		    call pargb (ID_REFIT(id) == YES)
		if (verbose) {
		    call fprintf (fd, "%20s  %7s %7s  %9s  %10s  %7s  %7s\n")
			call pargstr ("Image Sections")
			call pargstr ("Found")
			call pargstr ("Fit")
			call pargstr ("Pix Shift")
			call pargstr ("User Shift")
			call pargstr ("Z Shift")
			call pargstr ("RMS")
		}
	    }

	    call ri_reidentify (id, refimage, newimage, shft, step, nreid,
		Memi[logfd], nlogfd, pfd, verbose)
	}

	# Finish up.

	if (nlogfd > 0) {
	    do i = 1, nlogfd
	        call close (Memi[logfd+i-1])
	    call mfree (logfd, TY_INT)
	}

	if (pfd != NULL)
	    call close (pfd)

	call ic_closed (ID_IC(id))
	call dcvfree (ID_CV(id))
	call id_free (id)
	call imtclose (list)
end


# RI_REIDENTIFY -- Reidentify and trace features.

procedure ri_reidentify (id, refimage, newimage, shft, step, nreid, logfd,
	nlogfd, pfd, verbose)

pointer	id			# ID pointer
char	refimage[ARB]		# Reference 1D image
char	newimage[ARB]		# New image
double	shft			# Initial shift between ref and new
int	step			# Step size
int	nreid			# Minimum number of features reidentified
int	logfd[ARB]		# Logfiles
int	nlogfd			# Number of logfiles
int	pfd			# File descriptor for plot file
bool	verbose			# Verbose log?

char	refim[SZ_FNAME]
int	i, axis, start, imlen

bool	strne()

begin
	# Determine the 1D image vector of the reference features defined
	# by the axis along which the feature positions are measured, the
	# length of the image in the other axis, and the line or column.
	# Set the image vector in the new image to be reidentified to be
	# the same as the reference image.  Note that this may be the
	# same as the reference image.  If the new image vector and the
	# reference image vector are the same then the reidentification
	# is not performed.

	call ri_getstart (refimage, axis, start, imlen)
	call ri_setimage (newimage, axis, start, Memc[ID_IMAGE(id)], SZ_FNAME)

	if (strne (refimage, Memc[ID_IMAGE(id)])) {
	    call id_reidentify (id, refimage, shft, logfd, nlogfd, pfd, verbose)

	    if (ID_NFEATURES(id) < nreid) {
		for (i = 1; i <= nlogfd; i = i + 1)
		    call fprintf (logfd[i],
			"    ** Too many features lost **\n")
	        return
	    }
	}

	# Trace features in the new image if the step size is greater than 0.

	if (step < 1)
	    return

	call strcpy (Memc[ID_IMAGE(id)], refim, SZ_FNAME)

	# Determine the starting 1D vector.  Note that this is done again
	# because the image length of the new reference image may be different
	# than the original reference image.

	call ri_getstart (refim, axis, start, imlen)

	# Trace the features to lower lines or columns.  Note that a trace
	# uses a shift of 0. in id_reidentify even if a shift between images
	# is given.

	call id_dbread (id, refim, NO)

	i = start
	while (i > step) {
	    i = i - step
	    call ri_setimage (refim, axis, i, Memc[ID_IMAGE(id)], SZ_FNAME)
	    call id_reidentify (id, "", double (0.), logfd, nlogfd, pfd,  
		verbose)
	    if (ID_NFEATURES(id) < nreid) {
		for (i = 1; i <= nlogfd; i = i + 1)
		    call fprintf (logfd[i],
			"    ** Too many features lost **\n")
		break
	    }
	}

	# Trace to greater lines or columns.

	call id_dbread (id, refim, NO)
	i = start
	while (i <= imlen - step) {
	    i = i + step
	    call ri_setimage (refim, axis, i, Memc[ID_IMAGE(id)], SZ_FNAME)
	    call id_reidentify (id, "", double (0.), logfd, nlogfd, pfd, 
		verbose)
	    if (ID_NFEATURES(id) < nreid) {
		for (i = 1; i <= nlogfd; i = i + 1)
		    call fprintf (logfd[i],
			"    ** Too many features lost **\n")
		break
	    }
	}
end


# RI_SETIMAGE -- Set a 1D image name by taking the root of the input image
# and adding a section with the specified axis and index.

procedure ri_setimage (image1, axis, index, image2, maxchar)

char	image1[ARB]		# Input image name
int	axis			# Image axis
int	index			# Image line or column index
char	image2[maxchar]		# Output image name
int	maxchar			# Maximum size of output image name

char	section[SZ_FNAME]

begin
	call imgimage (image1, image2, maxchar)

	switch (axis) {
	case 1:
	    call sprintf (section, SZ_FNAME, "[*,%d]")
		call pargi (index)
	case 2:
	    call sprintf (section, SZ_FNAME, "[%d,*]")
		call pargi (index)
	}

	call xt_mk1d (image2, section, SZ_FNAME)
end


# RI_GETSTART -- Determine the 1D image vector in terms of the full image.
# This procedure depends on the imio structure.

procedure ri_getstart (refimage, axis, start, imlen)

char	refimage[ARB]		# Reference 1D image
int	axis			# Axis of feature positions
int	start			# Starting column or line
int	imlen			# Length of trace dimension

pointer	im, immap()

begin
	im = immap (refimage, READ_ONLY, 0)

	switch (IM_NPHYSDIM(im)) {
	case 1:
	    axis = 1
	    start = 1
	    imlen = 1
	case 2:
	    axis = IM_VMAP (im, 1)
	    if (axis == 1) {
	        start = IM_VOFF(im, 2) + 1
	        imlen = IM_SVLEN(im, 2)
	    } else {
	        start = IM_VOFF(im, 1) + 1
	        imlen = IM_SVLEN(im, 1)
	    }
	default:
	    call imunmap (im)
	    call error (0, "Image physical dimension too large")
	}

	call imunmap (im)
end
