include	<gset.h>
include	<imhdr.h>
include	<mach.h>
include	<pkg/gtools.h>
include	<pkg/xtanswer.h>
include	"apertures.h"
include	"extract.h"

define	HELP	"noao$lib/scr/apedit.key"
define	PROMPT	"apextract options"

# Sort flags
define	ID	1	# Sort by ID
define	POS	2	# Sort by position

# AP_EDIT -- Define and edit apertures.  This is the main interactive
# procedure for manipulating apertures.  The selected dispersion line
# is graphed with possible summing of neighboring lines and then
# cursor keys are used to define new apertures or edit existing apertures.
# Note that the value of line may be changed.

procedure ap_edit (image, line, nsum, edit, dbwrite, aps, naps)

char	image[SZ_FNAME]		# Image to be edited
int	line			# Dispersion line
int	nsum			# Number of dispersion lines to sum
int	edit			# Edit image?
int	dbwrite			# Write apertures to database?

pointer	aps[AP_MAXAPS]		# Aperture pointers
int	naps			# Number of apertures

char	cmd[SZ_LINE]
int	i, npts, apaxis, dispaxis, fittrace, exreview, statusline
int	current, newgraph, newim, newdata, all, wcs, key, apid, apbeam
real	center, low, high, wx, wy
pointer	im, imdata, title
pointer	sp, x, wts, apdef, gp, gt, ic_gt, cv, str, output, profiles

int	gt_gcur(), clgwrd(), ctor()
real	cveval(), ap_center()
bool	clgetb()
pointer	gt_init()
errchk	ap_getdata, ap_gopen

define	new_	10
define	beep_	99

begin
	# Query the user.
	call sprintf (cmd, SZ_LINE, "Edit apertures for %s?")
	    call pargstr (image)
	call xt_answer (cmd, edit)
	if ((edit == NO) || (edit == ALWAYSNO))
	    return

	# Set flags.
	all = NO

	# Map the image and get the image data.
new_	call ap_getdata (image, line, nsum, im, imdata, npts, apaxis, title)
	newdata = NO
	newim = NO

	# Allocate additional memory.
	call smark (sp)
	call salloc (x, npts, TY_REAL)
	call salloc (wts, npts, TY_REAL)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (profiles, SZ_FNAME, TY_CHAR)

	# Set the default aperture and delete apertures which do not have
	# the correct aperture axis.
	call ap_default (im, 1, 1, apaxis, INDEFR, real (line), apdef)
	dispaxis = mod (apaxis, 2) + 1
	for (i = naps; i > 0; i = i - 1)
	    if (AP_AXIS(aps[i]) != apaxis)
	        call ap_delete (i, aps, naps)

	# Set up the graphics.
	call ap_gopen (gp)
	gt = gt_init()
	call gt_sets (gt, GTTITLE, "Define and Edit Apertures")
	call gt_sets (gt, GTPARAMS, Memc[title])

	# Enter cursor loop.
	current = min (1, naps)
	key = 'r'
	wy = INDEF
	repeat {
	    statusline = NO

	    # For those keys affecting the nearest aperture set the current
	    # aperture to be the aperture nearest the cursor.
	    switch (key) {
	    case '.', 'b', 'c', 'd', 'e', 'g', 'i', 'j', 'k', 's', 't', 'x','y':
		# The current aperture is the one nearest the cursor.
	        call ap_nearest (current, line, aps, naps, wx)
	    }

	    # Set the current aperture values.
	    call ap_values (current, aps, line, apid, apbeam, center, low, high)

	    # Select the operation to be performed.
	    switch (key) {
	    case '?': # Print help text.
		call gpagefile (gp, HELP, PROMPT)

	    case ':': # Colon commands.
		switch (cmd[1]) {
		case '/':
		    call gt_colon (cmd, gp, gt, newgraph)
		default:
		    call ap_colon (cmd, im, gp, aps, naps, current, image,
			line, nsum, all, newgraph, newim, newdata)
		    if (newim == YES)
			break
		    if (newdata == YES) {
			call mfree (imdata, TY_REAL)
			call mfree (title, TY_CHAR)
			call imunmap (im)
			call ap_getdata (image, line, nsum, im, imdata, npts,
			    apaxis, title)
			call gt_sets (gt, GTPARAMS, Memc[title])
			call ap_free (apdef)
			call ap_default (im, 1, 1, apaxis, INDEFR,
			    real (line), apdef)
			newdata = NO
			newgraph = YES
		    }
		}

	    case '.': # Select current aperture.  This has been done already.
		;

	    case '+': # Go to next aperture.
		current = min (naps, current + 1)

	    case '-': # Go to last aperture.
		current = min (naps, max (1, current - 1))

	    case 'a':	# Toggle all flag
		if (all == NO)
		    all = YES
		else
		    all = NO

	    case 'b':	# Set background fitting parameters.
		if (current == 0)
		   goto beep_

		do i = 1, npts {
		    Memr[x+i-1] = i - center
		    Memr[wts+i-1] = 1
		}

		if (ic_gt == NULL) {
		    ic_gt = gt_init()
		    call gt_sets (ic_gt, GTTYPE, "line")
	    	    wx = max (10., high - low)
	    	    call gt_setr (ic_gt, GTXMIN, low - 2 * wx)
	    	    call gt_setr (ic_gt, GTXMAX, high + 2 * wx)
		}

		call sprintf (Memc[str], SZ_LINE,
		    "Set Background Subtraction for Aperture %d")
		    call pargi (apid)
		call gt_sets (ic_gt, GTTITLE, Memc[str])

		if (AP_IC(aps[current]) == NULL)
		    call ap_icset (apdef, 0., 0., aps[current])

		call icg_fit (AP_IC(aps[current]), gp, "apio.cursor",
		    ic_gt, cv, Memr[x], Memr[imdata], Memr[wts], npts)
		call cvfree (cv)

	        # Set background min and max based on sample regions.
	        wx = low
	        wy = high
		call ic_gstr (AP_IC(aps[current]), "sample", Memc[str], SZ_LINE)
	        for (i=str; Memc[i]!=EOS; i=i+1)
		    if (Memc[i] == ':')
		        Memc[i] = ','
	        for (i=1; Memc[str+i-1]!=EOS; i=i+1)
	            if (ctor (Memc[str], i, low) > 0) {
		        wx = min (wx, low)
		        wy = max (wy, low)
		        i = i - 1
	            }
	        if (wx > wy) {
		    wx = -npts
		    wy = npts
	        }
	        call ic_putr (AP_IC(aps[current]), "xmin", wx)
	        call ic_putr (AP_IC(aps[current]), "xmax", wy)

		if ((naps > 1) && (all == YES))
		   do i = 1, naps
		       if (i != current)
			   call ap_icset (aps[current], 0., 0., aps[i])
		newgraph = YES

	    case 'c':	# Center current aperture or all apertures.
		if (current == 0)
		    goto beep_

		if ((naps == 1) || (all == NO)) {
		    center = ap_center (center, Memr[imdata], npts)
		    if (!IS_INDEF(center))
			call ap_update (gp, aps[current], line, apid, apbeam,
			    center, low, high)
		} else {
		    do i = 1, naps {
		        call ap_values (i, aps, line, apid, apbeam, center,
			    low, high)
			center = ap_center (center, Memr[imdata], npts)
			if (!IS_INDEF(center))
			    call ap_update (gp, aps[i], line, apid, apbeam,
				center, low, high)
		    }
		}

	    case 'd':	# Delete apertures
		if (current == 0)
		    goto beep_

		call gseti (gp, G_PLTYPE, 0)
		if ((naps == 1) || (all == NO)) {
		    call ap_gmark (gp, line, aps[current], 1)
		    call ap_delete (current, aps, naps)
		    call ap_gscur (current, gp, line, aps, wy)
	            call ap_values (current, aps, line, apid, apbeam,
			center, low, high)
		} else {
		    do i = 1, naps {
			call ap_gmark (gp, line, aps[i], 1)
			call ap_free (aps[i])
		    }
		    naps = 0
		    current = 0
		}
		call gseti (gp, G_PLTYPE, 1)

	    case 'e': # Sum extraction
		if (current == 0)
		    goto beep_

		call clgstr ("apedit.output", Memc[output], SZ_FNAME)
		if (clgetb ("apsum.skyextract"))
		    call clgstr ("apedit.sky", Memc[str], SZ_LINE)
		else
		    Memc[str] = EOS
		i = clgwrd ("apsum.weights", Memc[profiles], SZ_FNAME, WEIGHTS)
		if (clgetb ("apsum.clean") || i == VARIANCE)
		    call clgstr ("apedit.profiles", Memc[profiles], SZ_FNAME)
		exreview = YES
		if (all == NO)
	            call ex_sum (image, Memc[output], Memc[str], Memc[profiles],
			exreview, aps[current], 1)
		else
	            call ex_sum (image, Memc[output], Memc[str], Memc[profiles],
			exreview, aps, naps)
		newgraph = YES

	    case 'f':		# Find apertures
		if (current == 0)
		    call ap_findnew (line, Memr[imdata], npts, apdef, aps, naps)
		else
		    call ap_findnew (line, Memr[imdata], npts, aps[current],
			aps, naps)
		call ap_gmark (gp, line, aps, naps)
		current = naps

	    case 'g': # Strip extraction
		if (current == 0)
		    goto beep_

		call clgstr ("apedit.output", Memc[output], SZ_FNAME)
		if (clgetb ("apstrip.clean") || clgetb ("apstrip.fit"))
		    call clgstr ("apedit.profiles", Memc[profiles], SZ_FNAME)
		else
		    Memc[profiles] = EOS
		if (all == NO)
	            call ex_strip (image, Memc[output], Memc[profiles],
			aps[current], 1)
		else
	            call ex_strip (image, Memc[output], Memc[profiles],
			aps, naps)
		newgraph = YES

	    case 'i':  # Print extra info about the current aperture.
		if (current == 0)
		    goto beep_

		call ap_info (aps[current], line)
		statusline = YES

	    case 'j':	# Set aperture ID
		if (current == 0)
		    goto beep_

		call ap_gapid (apid)
		AP_ID(aps[current]) = apid
		AP_BEAM(aps[current]) = apid
		call ap_sort (current, aps, naps, ID)

	    case 'k':	# Set beam number
		if (current == 0)
		    goto beep_

		call ap_gbeam (apbeam)
		if (all == NO)
		    AP_BEAM(aps[current]) = apbeam
		else {
		    do i = 1, naps
			AP_BEAM(aps[i]) = apbeam
		}

	    case 'l':	# Set the low limit.
		if (current == 0)
		    goto beep_

		wx = wx - center
		if ((naps == 1) || (all == NO))
		    call ap_update (gp, aps[current], line, apid, apbeam,
		        center, wx, high)
		else {
		    do i = 1, naps {
			call ap_values (i, aps, line, apid, apbeam, center,
			    low, high)
			call ap_update (gp, aps[i], line, apid, apbeam,
			    center, wx, high)
		    }
		}

	    case 'm', 'n':	# Define a new aperture.
		if (naps == AP_MAXAPS) {
		    call printf (
			"Maximum number of apertures already defined\n")
		    statusline = YES
		    next 
		}

		if (key == 'm')
		    wx = ap_center (wx, Memr[imdata], npts)

		if (!IS_INDEF(wx)) {
		    naps = naps + 1
		    if (naps > 1) {
			call ap_copy (aps[current], aps[naps])
		        AP_ID(aps[naps]) = AP_ID(aps[naps-1]) + 1
		    } else {
			call ap_copy (apdef, aps[naps])
		        AP_ID(aps[naps]) = 1
		    }

		    AP_BEAM(aps[naps]) = AP_ID(aps[naps])
		    AP_CEN(aps[naps], apaxis) = wx -
		    	cveval (AP_CV(aps[naps]), real (line))
		    AP_CEN(aps[naps], dispaxis) = line
		    AP_LOW(aps[naps], dispaxis) = 1 - AP_CEN(aps[naps],dispaxis)
		    AP_HIGH(aps[naps], dispaxis) = IM_LEN(im, dispaxis) -
			AP_CEN(aps[naps], dispaxis)

		    current = naps
		    call ap_values (current, aps, line, apid, apbeam, center,
			low, high)
		    call ap_gmark (gp, line, aps[current], 1)
		}

	    case 'o':	# Order the aperture and beam numbers
		if (naps == 0)
		    goto beep_

		call ap_sort (current, aps, naps, POS)
		do i = 1, naps {
		    AP_ID(aps[i]) = i
		    AP_BEAM(aps[i]) = i
		}
		newgraph = YES

	    case 'r':	# Redraw the graph.
		newgraph = YES

	    case 's':	# Shift apertures
		if (current == 0)
		    goto beep_

		if ((naps == 1) || (all == NO))
		    call ap_update (gp, aps[current], line, apid, apbeam,
			wx, low, high)
		else {
		    wx = wx - center
		    do i = 1, naps {
		        call ap_values (i, aps, line, apid, apbeam, center,
			    low, high)
			call ap_update (gp, aps[i], line, apid, apbeam,
			    center + wx, low, high)
		    }
		}

	    case 't': # Trace.
		if (current == 0)
		    goto beep_

		fittrace = YES
		if (all == NO)
		    call tr_trace (image, line, ALWAYSYES, fittrace,
			ALWAYSNO, aps[current], 1)
		else
		    call tr_trace (image, line, ALWAYSYES, fittrace,
			ALWAYSNO, aps, naps)
		newgraph = YES

	    case 'u':	# Set the upper limit.
		if (current == 0)
		    goto beep_

		wx = wx - center
		if ((naps == 1) || (all == NO))
		    call ap_update (gp, aps[current], line, apid, apbeam,
			center, low, wx)
		else {
		    do i = 1, naps {
			call ap_values (i, aps, line, apid, apbeam, center,
			    low, high)
			call ap_update (gp, aps[i], line, apid, apbeam,
			    center, low, wx)
		    }
		}

	    case 'w':	# Window the graph.
		call gt_window (gt, gp, "apio.cursor", newgraph)

	    case 'x':	# Delete background fitting.
		if (current == 0)
		    goto beep_

		if ((naps == 1) || (all == NO))
		    call ic_closer (AP_IC(aps[current]))
		else
		    do i = 1, naps
			call ic_closer (AP_IC(aps[i]))

	    case 'y':	# Set aperture limits at the y level.
		if (current == 0)
		    goto beep_

		if ((naps == 1) || (all == NO)) {
		    call ap_ylevel (Memr[imdata], npts, wy, center, low, high)
		    call ap_update (gp, aps[current], line, apid, apbeam,
			center, low, high)
		} else {
		    do i = 1, naps {
		        call ap_values (i, aps, line, apid, apbeam, center,
			    low, high)
			call ap_ylevel (Memr[imdata], npts, wy, center, low,
			    high)
			call ap_update (gp, aps[i], line, apid, apbeam, center,
			    low, high)
		    }
		}

	    case 'I': # Interrupt
		call fatal (0, "Interrupt")

	    default: # Ring bell for unrecognized commands.
beep_		call printf ("\7")
	    }

	    # Update the graph if needed.
	    if (newgraph == YES) {
		call ap_graph (gp, gt, Memr[imdata], npts, line, aps, naps)
	        newgraph = NO
	    }

	    # Set the cursor to the current aperture and print the current
	    # aperture on the status line.
	    call ap_gscur (current, gp, line, aps, wy)
	    if (statusline == NO)
	        call ap_print (current, line, all, aps)

	} until (gt_gcur ("apio.cursor", wx, wy, wcs, key, cmd, SZ_LINE) == EOF)

	# Log the editing operation.
	call sprintf (Memc[str], SZ_LINE,
	    "APEDIT  - %d apertures edited for %s.")
       	    call pargi (naps)
       	    call pargstr (image)
	call ap_log (Memc[str])

	# Free memory.
	call mfree (imdata, TY_REAL)
	call mfree (title, TY_CHAR)
	call imunmap (im)
	call gt_free (gt)
	call gt_free (ic_gt)
	call ap_free (apdef)
	call sfree (sp)

	# Write the edited apertures.
	call ap_dbwrite (image, dbwrite, aps, naps)

	# If a new image is desired loop back.
	if (newim == YES) {
	    call strcpy (cmd, image, SZ_FNAME)
	    call ap_dbread (image, aps, naps)
	    goto new_
	}
end
