# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<gset.h>
include	<imhdr.h>
include	"imexam.h"
 
define	HELP		"iraf$lib/scr/imexamine.key"
define	PROMPT		"imexamine options"
define	SZ_IMLIST	512

 
# T_IMEXAMINE -- Examine images using image display, graphics, and text output. 
 
procedure t_imexamine ()
 
real	x, y
pointer	sp, cmd, imname, imlist, gp, ie, im
int	curtype, key, redraw, mode, nframes, nargs
 
bool	clgetb()
pointer	gopen(), ie_gimage(), imtopen()
int	imd_wcsver(), ie_gcur(), ie_getnframes()
int	btoi(), clgeti(), imtlen()

begin
	call smark (sp)
	call salloc (ie, IE_LEN, TY_STRUCT)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (imlist, SZ_IMLIST, TY_CHAR)

	# Initialize the imexamine descriptor.
	call aclri (Memi[ie], IE_LEN)

	# Determine if we will be accessing the image display, and if so,
	# the maximum number of frames to be accessed.

	IE_USEDISPLAY(ie) = btoi (clgetb ("use_display"))
	if (IE_USEDISPLAY(ie) == YES) {
	    if (imd_wcsver() == 0)
		;
	    iferr (nframes = ie_getnframes (ie)) {
		call eprintf ("cannot access display\n")
		IE_USEDISPLAY(ie) = NO
	    }
	}

	# Get the list of images to be examined, if given on the command
	# line.  If no images are explicitly listed use the display to
	# determine the images to be examined.

	nargs = clgeti ("$nargs")
	if (nargs > 0) {
	    call clgstr ("input", Memc[imlist], SZ_IMLIST)
	    IE_LIST(ie) = imtopen (Memc[imlist])
	    IE_LISTLEN(ie) = imtlen (IE_LIST(ie))
	    IE_INDEX(ie) = 1

	    if (nargs >= 1) {
		# Set user specified display frame.
		IE_DFRAME(ie) = 100 * clgeti ("frame") + 1
		IE_NEWFRAME(ie) = IE_DFRAME(ie)
		if (IE_USEDISPLAY(ie) == YES) {
		    nframes = max (IE_NEWFRAME(ie)/100, nframes)
		    IE_NFRAMES(ie) = nframes
		}
	    } else {
		# If we have to display an image and no frame was specified,
		# default to frame 1 (should use the current display frame
		# but we don't have a cursor read yet to tell us what it is).

		IE_DFRAME(ie) = 101
		IE_NEWFRAME(ie) = 101
	    }

	} else {
	    IE_INDEX(ie) = 1
	    IE_DFRAME(ie) = 101
	    IE_NEWFRAME(ie) = 101
	}
 
	# Set the wcs, logfile and graphics.
	call clgstr ("wcs", IE_WCSNAME(ie), IE_SZFNAME)
	IE_LOGFD(ie) = NULL
	call clgstr ("logfile", IE_LOGFILE(ie), IE_SZFNAME)
	if (clgetb ("keeplog"))
	    iferr (call ie_openlog (ie))
		call erract (EA_WARN)

	call clgstr ("graphics", Memc[cmd], SZ_LINE)
	gp = gopen (Memc[cmd], NEW_FILE+AW_DEFER, STDGRAPH)

	# Initialize the data structure.
	IE_IM(ie) = NULL
	IE_DS(ie) = NULL
	IE_PP(ie) = NULL
	IE_MAPFRAME(ie) = 0
	IE_NFRAMES(ie) = nframes
	IE_ALLFRAMES(ie) = btoi (clgetb ("allframes"))
	IE_GTYPE(ie) = NULL
	IE_XORIGIN(ie) = 0.
	IE_YORIGIN(ie) = 0.

	# Access the first image.  If an image list was specified and the
	# display is being used, this will set the display frame to the first
	# image listed, or display the first image if not already loaded into
	# the display.

	if (IE_LIST(ie) != NULL)
	    im = ie_gimage (ie, YES)
 
	# Enter the cursor loop.  The commands are returned by the
	# IE_GCUR procedure.
 
	x = 1.
	y = 1.
	redraw = NO
	curtype = 'i'
	mode = NEW_FILE

	while (ie_gcur (ie, curtype, x,y, key, Memc[cmd], SZ_LINE) != EOF) {
	    # Check to see if the user has changed frames on us while in
	    # examine-image-list mode.

	    if (IE_USEDISPLAY(ie) == YES && IE_LIST(ie) != NULL &&
		IE_NEWFRAME(ie)/100 != IE_MAPFRAME(ie)/100) {
		call ie_imname (IE_DS(ie), IE_NEWFRAME(ie), Memc[imname],
		    SZ_FNAME)
		call ie_addimage (ie, Memc[imname], imlist)
	    }

	    # Set workstation state.
	    switch (key) {
	    case 'a', 'b', 'd', 'm', 't', 'w', 'x', 'y', 'z', ',':
		call gdeactivate (gp, 0)
	    }
 
	    # Act on the command key.
	    switch (key) {
	    case '?':	# Print help
		call gpagefile (gp, HELP, PROMPT)
	    case ':':	# Process colon commands
		call ie_colon (ie, Memc[cmd], gp, redraw)
		if (redraw == YES) {
		    x = INDEF
		    y = INDEF
		}
	    case 'f':	# Redraw frame
		redraw = YES
		x = INDEF
		y = INDEF
	    case 'a':	# Aperture photometry
		call ie_rimexam (NULL, NULL, ie, x, y)
	    case ',':	# Aperture photometry
		call ie_qrimexam (NULL, NULL, ie, x, y)

	    case 'b':	# Print image region coordinates
		call printf ("%4d %4d %4d %4d\n")
		    call pargi (IE_IX1(ie))
		    call pargi (IE_IX2(ie))
		    call pargi (IE_IY1(ie))
		    call pargi (IE_IY2(ie))

		if (IE_LOGFD(ie) != NULL) {
		    call fprintf (IE_LOGFD(ie), "%4d %4d %4d %4d\n")
			call pargi (IE_IX1(ie))
			call pargi (IE_IX2(ie))
			call pargi (IE_IY1(ie))
			call pargi (IE_IY2(ie))
		}

	    case 'c','e','h','j','k','s','l','r','u','v','.': # Graphs
		IE_GTYPE(ie) = key
		redraw = YES

	    case 'd':	# Load the display.
		# Query the user for the frame to be loaded, the current
		# display frame being the default.

		call clgstr ("image", Memc[imname], SZ_FNAME)
		call clputi ("frame", IE_NEWFRAME(ie)/100)
		IE_DFRAME(ie) = 100 * clgeti ("frame") + 1
		IE_NEWFRAME(ie) = IE_DFRAME(ie)

		if (IE_LIST(ie) != NULL)
		    call ie_addimage (ie, Memc[imname], imlist)
		else
		    call ie_display (ie, Memc[imname], IE_DFRAME(ie)/100)

	    case 'g':	# Graphics cursor
		curtype = 'g'
	    case 'i':	# Image cursor
		curtype = 'i'
	    case 'm':	# Image statistics
		call ie_statistics (ie, x, y)

	    case 'n':	# Next frame
		if (IE_LIST(ie) != NULL) {
		    IE_INDEX(ie) = IE_INDEX(ie) + 1
		    if (IE_INDEX(ie) > IE_LISTLEN(ie))
			IE_INDEX(ie) = 1
		} else {
		    IE_NEWFRAME(ie) = 100 * (IE_NEWFRAME(ie)/100 + 1) + 1
		    if (IE_NEWFRAME(ie)/100 > IE_NFRAMES(ie))
			IE_NEWFRAME(ie) = 101
		}
		im = ie_gimage (ie, YES)

	    case 'o':	# Overplot
		mode = APPEND

	    case 'p':	# Previous frame
		if (IE_LIST(ie) != NULL) {
		    IE_INDEX(ie) = IE_INDEX(ie) - 1
		    if (IE_INDEX(ie) <= 0)
			IE_INDEX(ie) = IE_LISTLEN(ie)
		} else {
		    IE_NEWFRAME(ie) = 100 * (IE_NEWFRAME(ie)/100 - 1) + 1
		    if (IE_NEWFRAME(ie)/100 <= 0)
			IE_NEWFRAME(ie) = 100 * IE_NFRAMES(ie) + 1
		}
		im = ie_gimage (ie, YES)

	    case 'q':	# Quit
		break

	    case 't':	# Extract a section.
		call ie_timexam (ie, x, y)

	    case 'w':	# Toggle logfile
		if (IE_LOGFD(ie) == NULL) {
		    if (IE_LOGFILE(ie) == EOS)
			call printf ("No log file defined\n")
		    else {
		        iferr (call ie_openlog (ie))
			    call erract (EA_WARN)
		    }
		} else {
		    call close (IE_LOGFD(ie))
		    IE_LOGFD(ie) = NULL
		    call printf ("Logfile %s closed\n")
			call pargstr (IE_LOGFILE(ie))
		}

	    case 'x', 'y':	# Positions
		call ie_pos (ie, x, y, key)
	    case 'z':	# Print grid
		call ie_print (ie, x, y)
	    case 'I':	# Immediate interrupt
		call fatal (1, "Interrupt")
	    default:	# Unrecognized command
		call printf ("\007")
	    }

	    switch (key) {
	    case '?', 'a', 'b', 'd', 'm', 'w', 'x', 'y', 'z', ',':
		IE_LASTKEY(ie) = key
	    }

	    # Draw or overplot a graph.
	    if (redraw == YES) {
		switch (IE_GTYPE(ie)) {
		case 'c':	# column plot
		    call ie_cimexam (gp, mode, ie, x)
		case 'e':	# contour plot
		    call ie_eimexam (gp, mode, ie, x, y)
		case 'h':	# histogram plot
		    call ie_himexam (gp, mode, ie, x, y)
		case 'j':	# line plot
		    call ie_jimexam (gp, mode, ie, x, y, 1)
		case 'k':	# line plot
		    call ie_jimexam (gp, mode, ie, x, y, 2)
		case 'l':	# line plot
		    call ie_limexam (gp, mode, ie, y)
		case 'r':	# radial profile plot
		    call ie_rimexam (gp, mode, ie, x, y)
		case 's':	# surface plot
		    call ie_simexam (gp, mode, ie, x, y)
		case 'u', 'v':	# vector cut plot
		    call ie_vimexam (gp, mode, ie, x, y, IE_GTYPE(ie))
		case '.':	# radial profile plot
		    call ie_qrimexam (gp, mode, ie, x, y)
		}
		redraw = NO
		mode = NEW_FILE
	    }
	}

	# Finish up.
	call gclose (gp)
	if (IE_IM(ie) != NULL && IE_IM(ie) != IE_DS(ie))
	    call imunmap (IE_IM(ie))
	if (IE_MW(ie) != NULL)
	    call mw_close (IE_MW(ie))
	if (IE_PP(ie) != NULL)
	    call clcpset (IE_PP(ie))
	if (IE_DS(ie) != NULL)
	    call imunmap (IE_DS(ie))
	if (IE_LOGFD(ie) != NULL)
	    call close (IE_LOGFD(ie))
	if (IE_LIST(ie) != NULL)
	    call imtclose (IE_LIST(ie))
	call sfree (sp)
end


# IE_ADDIMAGE -- Add an image to the image list if not already present in
# the list, and display the image.

procedure ie_addimage (ie, image, imlist)

pointer	ie			#I imexamine descriptor
char	image[ARB]		#I image name
pointer	imlist			#I image list

int	i
bool	inlist
pointer	im, sp, lname
pointer	ie_gimage(), imtopen()
int	imtrgetim(), imtlen()
bool	streq()

begin
	call smark (sp)
	call salloc (lname, SZ_FNAME, TY_CHAR)

	# Is image already in list?
	inlist = false
	do i = 1, IE_LISTLEN(ie) {
	    if (imtrgetim (IE_LIST(ie), i, Memc[lname], SZ_FNAME) > 0)
		if (streq (Memc[lname], image)) {
		    inlist = true
		    IE_INDEX(ie) = i
		    break
		}
	}

	# Add to list if missing.
	if (!inlist) {
	    call strcat (",", Memc[imlist], SZ_IMLIST)
	    call strcat (image, Memc[imlist], SZ_IMLIST)
	    call imtclose (IE_LIST(ie))
	    IE_LIST(ie) = imtopen (Memc[imlist])
	    IE_LISTLEN(ie) = imtlen (IE_LIST(ie))
	    IE_INDEX(ie) = IE_LISTLEN(ie)
	}

	# Display the image.
	im = ie_gimage (ie, YES)
	call sfree (sp)
end
