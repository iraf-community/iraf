include	<error.h>
include	<pkg/igsfit.h>
include	<pkg/xtanswer.h>

# T_FITCOORDS -- Fit a surface to the coordinates of longslit images.
#
# This is the CL entry for this task.  All the real work is done by
# fc_fitcoords.

procedure t_fitcoords ()

int	list1			# Image list
char	fitname[SZ_FNAME]	# Database name for coordinate fit
char	database[SZ_FNAME]	# Database
int	logfiles		# List of log files
bool	combine			# Combine input data?
int	interactive		# Interactive?

char	image[SZ_FNAME], prompt[SZ_LINE]
int	list2

int	clgeti(), clpopnu(), imtopen(), fc_getim()
bool	clgetb()

begin
	# Get the task parameters.

	call clgstr ("fitname", fitname, SZ_FNAME)
	call xt_stripwhite (fitname)
	combine = clgetb ("combine")

	if (combine && (fitname[1] == EOS))
	    call error (0, "Fit name not specified")

	call clgstr ("images", prompt, SZ_LINE)
	list1 = imtopen (prompt)
	call clgstr ("database", database, SZ_FNAME)
	logfiles = clpopnu ("logfiles")
	if (clgetb ("interactive"))
	    interactive = YES
	else
	    interactive = ALWAYSNO

	# Set the initial surface in the igsfit package.

	call clgstr ("function", prompt, SZ_LINE)
	call igs_sets (IGS_FUNCTION, prompt)
	call igs_seti (IGS_XORDER, clgeti ("xorder"))
	call igs_seti (IGS_YORDER, clgeti ("yorder"))

	# For each fit ask the user whether to do the fit interactively.
	# If combining the coordinates from all the images in the
	# input list then pass the list directly to fc_fitcoords.
	# Otherwise for each image in the list create a second list
	# containing just that image.  A second list is needed because
	# fc_fitcoords expects a list.

	if (combine) {
	    call sprintf (prompt, SZ_LINE, "Fit interactively")
	    call xt_answer (prompt, interactive)
	    call fc_fitcoords (fitname, database, list1, logfiles, interactive)

	} else {
	    while (fc_getim (list1, image, SZ_FNAME) != EOF) {
	        list2 = imtopen (image)
	        call sprintf (prompt, SZ_LINE, "Fit %s interactively")
		    call pargstr (image)
	        call xt_answer (prompt, interactive)
		call sprintf (prompt, SZ_LINE, "%s%s")
		    call pargstr (fitname)
		    call pargstr (image)
	        iferr (call fc_fitcoords (prompt, database, list2, logfiles,
		    interactive))
		    call erract (EA_WARN)
	        call imtclose (list2)
	    }
	}

	# Finish up.

	call clpcls (logfiles)
	call imtclose (list1)
end
