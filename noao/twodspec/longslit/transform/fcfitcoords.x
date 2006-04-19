include	<pkg/gtools.h>
include	<pkg/igsfit.h>
include	<pkg/xtanswer.h>

# FC_FITCOORDS -- Fit a surface to the user coordinates.

procedure fc_fitcoords (fitname, database, list, logfiles, interactive)

char	fitname[SZ_FNAME]	# Fitname
char	database[SZ_FNAME]	# Database
int	list			# List of images
int	logfiles		# List of log files
int	interactive		# Interactive?

int	axis			# Axis of surface fit
pointer	sf			# Surface pointer
char	logfile[SZ_FNAME], labels[SZ_LINE, IGSPARAMS]
bool	answer
int	ncoords, logfd, axes[2]
real	xmin, xmax, ymin, ymax
pointer	gp, gplog, gt, coords, title, un

int	imtgetim(), fntgfntb(), open(), igs_geti(), scan()
real	xgseval()
pointer	gopen(), gt_init()

errchk	fc_getcoords

begin
	# Print a header to the log files giving the inputs.  This is
	# done first so that if one of the logfiles is STDOUT the user
	# will see that something is happening.

	axis = 0
	while (fntgfntb (logfiles, logfile, SZ_FNAME) != EOF) {
	    logfd = open (logfile, APPEND, TEXT_FILE)
	    call sysid (logfile, SZ_FNAME)
	    call fprintf (logfd, "\n%s\n")
		call pargstr (logfile)
	    call fprintf (logfd, "  Longslit coordinate fit name is %s.\n")
		call pargstr (fitname)
	    call fprintf (logfd, "  Longslit database is %s.\n")
		call pargstr (database)
	    call fprintf (logfd, "  Features from images:\n")
	    while (imtgetim (list, logfile, SZ_FNAME) != EOF) {
		call fprintf (logfd, "    %s\n")
		    call pargstr (logfile)
	    }
	    call imtrew (list)
	    call close (logfd)
	}
	call fntrewb (logfiles)

	# Get the coordinates for the specified images and axis.  The
	# coordinates are returned in a pointer which must be explicitly
	# freed.

	call fc_getcoords (database, list, axis, xmin, xmax, ymin, ymax,
	    coords, ncoords, labels, un)

	# Read points from the deletion list.

	switch (axis) {
	case 1:
	    call fc_dlread (Memr[coords+(Z-1)*ncoords],
		Memr[coords+(Y-1)*ncoords], Memr[coords+(W-1)*ncoords], ncoords)
	case 2:
	    call fc_dlread (Memr[coords+(Z-1)*ncoords],
		Memr[coords+(X-1)*ncoords], Memr[coords+(W-1)*ncoords], ncoords)
	}

	# Initialize the graphics.

	if ((interactive == YES) || (interactive == ALWAYSYES)) {
	    call clgstr ("graphics", logfile, SZ_FNAME)
	    gp = gopen (logfile, NEW_FILE, STDGRAPH)
	}

	# Set plot log.

	gplog = NULL
	call clgstr ("plotfile", logfile, SZ_FNAME)
	if (logfile[1] != EOS) {
	    logfd = open (logfile, APPEND, BINARY_FILE)
	    gplog = gopen ("stdplot", APPEND, logfd)
	} else
	    gplog = NULL

	gt = gt_init ()
	call malloc (title, SZ_LINE, TY_CHAR)
	call sprintf (Memc[title], SZ_LINE,
	    "Fit User Coordinates to Image Coordinates for %s")
	    call pargstr (fitname)
	call gt_sets (gt, GTTITLE, Memc[title])
	call mfree (title, TY_CHAR)

	# Fit the surface.  The surface is defined over the full range of
	# image coordinates.

	call igs_setr (IGS_XMIN, xmin)
	call igs_setr (IGS_XMAX, xmax)
	call igs_setr (IGS_YMIN, ymin)
	call igs_setr (IGS_YMAX, ymax)

	switch (axis) {
	case 1:
	    if (Memr[coords+ncoords-1] == 1) {
	        axes[1] = Y
	        axes[2] = R
	        call igs_fit2 (sf, gp, gplog, gt, axes, Memr[coords], ncoords,
		    labels, interactive)
	    } else {
	        axes[1] = X
	        axes[2] = R
	        call igs_fit1 (sf, gp, gplog, gt, axes, Memr[coords], ncoords,
		    labels, interactive)
	    }
	case 2:
	    if (Memr[coords+ncoords-1] == 1) {
	        axes[1] = X
	        axes[2] = R
	        call igs_fit3 (sf, gp, gplog, gt, axes, Memr[coords], ncoords,
		    labels, interactive)
	    } else {
	        axes[1] = Y
	        axes[2] = R
	        call igs_fit1 (sf, gp, gplog, gt, axes, Memr[coords], ncoords,
		    labels, interactive)
	    }
	}

	# Close graphics.

	if (gp != NULL)
	    call gclose (gp)
	if (gplog != NULL) {
	    call gclose (gplog)
	    call close (logfd)
	}
	call gt_free (gt)

	# Print logs.

	while (fntgfntb (logfiles, logfile, SZ_FNAME) != EOF) {
	    logfd = open (logfile, APPEND, TEXT_FILE)
	    call fprintf (logfd,
		"  Map %s coordinates for axis %d using image features:\n")
		call pargstr (labels[1, Z])
		call pargi (axis)
	    call fprintf (logfd, "  Number of feature coordnates = %d\n")
		call pargi (ncoords)
	    call igs_gets (IGS_FUNCTION, logfile, SZ_FNAME)
	    call fprintf (logfd, "  Mapping function = %s\n")
		call pargstr (logfile)
	    call fprintf (logfd, "    X order = %d\n    Y order = %d\n")
		call pargi (igs_geti (IGS_XORDER))
		call pargi (igs_geti (IGS_YORDER))
	    call fprintf (logfd,
		"  Fitted coordinates at the corners of the images:\n")
	    call fprintf (logfd, "    (%d, %d) = %g  (%d, %d) = %g\n")
		call pargr (xmin)
		call pargr (ymin)
		call pargr (xgseval (sf, xmin, ymin))
		call pargr (xmax)
		call pargr (ymin)
		call pargr (xgseval (sf, xmax, xmin))
	    call fprintf (logfd, "    (%d, %d) = %g  (%d, %d) = %g\n")
		call pargr (xmin)
		call pargr (ymax)
		call pargr (xgseval (sf, xmin, ymax))
		call pargr (xmax)
		call pargr (ymax)
		call pargr (xgseval (sf, xmax, ymax))
	    call close (logfd)
	}
	call fntrewb (logfiles)

	# Write the fit to the database.

	answer = true
	if ((interactive == YES) || (interactive == ALWAYSYES)) {
	    call printf ("Write coordinate map to the database (yes)? ")
	    call flush (STDOUT)
	    if (scan() != EOF)
	        call gargb (answer)
	}
	if (answer)
	    call fc_dbwrite (database, fitname, axis, un, sf)

	# Write list of deleted points.

	if ((interactive == YES) || (interactive == ALWAYSYES)) {
	    switch (axis) {
	    case 1:
		call fc_dlwrite (Memr[coords+(Z-1)*ncoords],
		    Memr[coords+(Y-1)*ncoords],
		    Memr[coords+(W-1)*ncoords], ncoords)
	    case 2:
		call fc_dlwrite (Memr[coords+(Z-1)*ncoords],
		    Memr[coords+(X-1)*ncoords],
		    Memr[coords+(W-1)*ncoords], ncoords)
	    }
	}

	# Free memory.

	call mfree (coords, TY_REAL)
	if (un != NULL)
	    call un_close (un)
	call xgsfree (sf)
end
