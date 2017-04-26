include	<error.h>
include	<gset.h>
include	<imhdr.h>
include	<mach.h>
include	<pkg/gtools.h>
include	"apertures.h"

define	HELP	"noao$twodspec/apextract/apedit.key"
define	PROMPT	"apextract options"

# Sort flags
define	ORDER	"|increasing|decreasing|"

# AP_EDIT -- Define and edit apertures.  This is the main interactive
# procedure for manipulating apertures.  The selected dispersion line
# is graphed with possible summing of neighboring lines and then
# cursor keys are used to define new apertures or edit existing apertures.
# Note that the value of line may be changed.

procedure ap_edit (image, line, nsum, aps, naps)

char	image[SZ_FNAME]		# Image to be edited
int	line			# Dispersion line
int	nsum			# Number of dispersion lines to sum

pointer	aps			# Aperture pointers
int	naps			# Number of apertures

char	cmd[SZ_LINE]
int	i, npts, apaxis, dispaxis, statline
int	current, newgraph, newim, newdata, all, wcs, key, apid, apbeam
real	center, low, high, wx, wy
bool	peak
pointer	im, imdata, title
pointer	sp, x, wts, apdef, gp, gt, ic_gt, cv, str, output, profiles, ids

int	gt_gcur(), apgwrd(), scan(), nscan()
real	ap_cveval(), ap_center()
bool	ap_answer()
pointer	gt_init()
errchk	ap_getdata, ap_gopen, ap_default

define	new_	10
define	beep_	99

begin
	# Query user.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call sprintf (Memc[str], SZ_LINE, "Edit apertures for %s?")
    	    call pargstr (image)
	if (!ap_answer ("ansedit", Memc[str])) {
	    call sfree (sp)
	    return
	}

	# Set flags.
	all = NO

	# Get user aperture ID's
	call ap_gids (ids)

	# Map the image and get the image data.
new_	call ap_getdata (image, line, nsum, im, imdata, npts, apaxis, title)
	newdata = NO
	newim = NO

	# Allocate additional memory.
	call salloc (x, npts, TY_REAL)
	call salloc (wts, npts, TY_REAL)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (profiles, SZ_FNAME, TY_CHAR)

	# Set the default aperture and delete apertures which do not have
	# the correct aperture axis.
	call ap_default (im, INDEFI, 1, apaxis, INDEFR, real (line), apdef)
	dispaxis = mod (apaxis, 2) + 1
	for (i = naps; i > 0; i = i - 1)
	    if (AP_AXIS(Memi[aps+i-1]) != apaxis)
	        call ap_delete (i, Memi[aps], naps)

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
	    statline = NO

	    # For those keys affecting the nearest aperture set the current
	    # aperture to be the aperture nearest the cursor.
	    switch (key) {
	    case '.','b','c','d','e','g','i','j','o','t','y','z':
		# The current aperture is the one nearest the cursor.
	        call ap_nearest (current, line, Memi[aps], naps, wx)
	    }

	    # Set the current aperture values.
	    call ap_values (current, Memi[aps], line, apid,
		apbeam, center, low, high)

	    # Select the operation to be performed.
	    switch (key) {
	    case '?': # Print help text.
		call gpagefile (gp, HELP, PROMPT)

	    case ':': # Colon commands.
		if (cmd[1] == '/')
		    call gt_colon (cmd, gp, gt, newgraph)
		else {
		    call ap_colon (cmd, im, gp, apdef, aps, naps, current,
			image, line, nsum, all, newgraph, newim, newdata,
			statline)
		    if (newim == YES)
			break
		    if (newdata == YES) {
			call mfree (imdata, TY_REAL)
			call mfree (title, TY_CHAR)
			call imunmap (im)
			call ap_getdata (image, line, nsum, im, imdata, npts,
			    apaxis, title)
			call gt_sets (gt, GTPARAMS, Memc[title])
			newdata = NO
			newgraph = YES
		    }
		    call ap_free (apdef)
		    iferr (call ap_default (im, INDEFI, 1, apaxis, INDEFR,
			real (line), apdef))
			call erract (EA_WARN)
		}

	    case '.': # Select current aperture.  This has been done already.
		;

	    case '+': # Go to next aperture.
		current = min (naps, current + 1)

	    case '-': # Go to last aperture.
		current = min (naps, max (1, current - 1))

	    case 'a': # Toggle all flag
		if (all == NO)
		    all = YES
		else
		    all = NO

	    case 'b': # Set background fitting parameters.
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

		if (AP_IC(Memi[aps+current-1]) == NULL)
		    call ap_icset (apdef, Memi[aps+current-1], npts)

		call icg_fit (AP_IC(Memi[aps+current-1]), gp, "gcur",
		    ic_gt, cv, Memr[x], Memr[imdata], Memr[wts], npts)
		call cvfree (cv)

		# Set background limits
		call ap_icset (Memi[aps+current-1], Memi[aps+current-1], npts)

		if ((naps > 1) && (all == YES))
		   do i = 1, naps
		       if (i != current)
			   call ap_icset (Memi[aps+current-1],
			       Memi[aps+i-1], npts)
		newgraph = YES

	    case 'c': # Center current aperture or all apertures.
		if (current == 0)
		    goto beep_

		if ((naps == 1) || (all == NO)) {
		    center = ap_center (center, Memr[imdata], npts)
		    if (!IS_INDEF(center))
			call ap_update (gp, Memi[aps+current-1], line, apid,
			    apbeam, center, low, high)
		} else {
		    do i = 1, naps {
		        call ap_values (i, Memi[aps], line, apid,
			    apbeam, center, low, high)
			center = ap_center (center, Memr[imdata], npts)
			if (!IS_INDEF(center))
			    call ap_update (gp, Memi[aps+i-1], line, apid,
				apbeam, center, low, high)
		    }
		}

	    case 'd': # Delete apertures
		if (current == 0)
		    goto beep_

		call gseti (gp, G_PLTYPE, 0)
		if ((naps == 1) || (all == NO)) {
		    call ap_gmark (gp, line, Memi[aps+current-1], 1)
		    call ap_delete (current, Memi[aps], naps)
		    call ap_gscur (current, gp, line, Memi[aps], wy)
	            call ap_values (current, Memi[aps], line, apid,
			apbeam, center, low, high)
		} else {
		    do i = 1, naps {
			call ap_gmark (gp, line, Memi[aps+i-1], 1)
			call ap_free (Memi[aps+i-1])
		    }
		    naps = 0
		    current = 0
		}
		call gseti (gp, G_PLTYPE, 1)

	    case 'e': # Sum extraction
		if (current == 0)
		    goto beep_

		call imunmap (im)
		call apgstr ("e_output", Memc[output], SZ_FNAME)
		call apgstr ("e_profiles", Memc[profiles], SZ_FNAME)
	        call apgstr ("format", Memc[str], SZ_LINE)
		call appstr ("ansreview", "yes")
		call appstr ("ansreview1", "yes")
		call appstr ("ansclobber", "yes")
		call appstr ("ansclobber1", "yes")
		if (all == NO)
	            call ap_extract (image, Memc[output],
			Memc[str], Memc[profiles], Memi[aps+current-1], 1)
		else
	            call ap_extract (image, Memc[output],
			Memc[str], Memc[profiles], Memi[aps], naps)
		call ap_getdata (image, line, nsum, im, imdata, npts, apaxis,
		    title)
		newgraph = YES

	    case 'f': # Find apertures
		if (current == 0)
		    call ap_findnew (line, Memr[imdata], npts,
			apdef, aps, naps)
		else
		    call ap_findnew (line, Memr[imdata], npts,
			Memi[aps+current-1], aps, naps)
		call ap_gmark (gp, line, Memi[aps], naps)
		current = naps

	    case 'g': # Apply recenter algorithm.
		if (current == 0)
		    goto beep_

		call imunmap (im)
		if (all == NO) {
		    call gseti (gp, G_PLTYPE, 0)
		    call ap_gmark (gp, line, Memi[aps+current-1], 1)
		    call ap_recenter (image, line, nsum,
			Memi[aps+current-1], 1, YES)
		    call gseti (gp, G_PLTYPE, 1)
		    call ap_gmark (gp, line, Memi[aps+current-1], 1)
		    call ap_values (current, Memi[aps], line, apid,
			apbeam, center, low, high)
		} else {
		    call gseti (gp, G_PLTYPE, 0)
		    do i = 1, naps
		        call ap_gmark (gp, line, Memi[aps+i-1], 1)
		    call ap_recenter (image, line, nsum, Memi[aps], naps, YES)
		    call gseti (gp, G_PLTYPE, 1)
		    do i = 1, naps
		        call ap_gmark (gp, line, Memi[aps+i-1], 1)
		}
		call ap_getdata (image, line, nsum, im, imdata, npts, apaxis,
		    title)

	    case 'i': # Set aperture ID
		if (current == 0)
		    goto beep_

		repeat {
		    call printf ("Aperture (%d) = ")
		        call pargi (AP_ID(Memi[aps+current-1]))
		    call flush (STDOUT)
		    if (scan () != EOF) {
		        call gargi (apid)
		        if (nscan() == 1) {
			    if (apid < 1) {
				call printf (
				"Aperture numbers < 1 are not allowed: ")
			    } else {
				for (i=1; i<=naps; i=i+1)
				    if (i != current &&
					apid == AP_ID(Memi[aps+i-1]))
					break
				if (i <= naps) {
				    call printf ("Aperture %d already used: ")
					call pargi (apid)
				} else {
				    AP_ID(Memi[aps+current-1]) = apid
				    call ap_ids (Memi[aps+current-1], 1, ids)
				    break
				}
			    }
			} else
			    break
		    }
		}

	    case 'j': # Set beam number
		if (current == 0)
		    goto beep_

		repeat {
		    call printf ("Beam (%d) = ")
			call pargi (AP_BEAM(Memi[aps+current-1]))
		    call flush (STDOUT)
		    if (scan () != EOF) {
			call gargi (apbeam)
			if (nscan() == 1) {
#			    if (apbeam < 0) {
#				call printf (
#				    "Beam numbers < 0 are not allowed: ")
#			    } else {
				if (all == NO)
				    AP_BEAM(Memi[aps+current-1]) = apbeam
				else
				    do i = 1, naps
					AP_BEAM(Memi[aps+i-1]) = apbeam
				break
#			    }
			} else
			    break
		    }
		}

	    case 'l': # Set the low limit.
		if (current == 0)
		    goto beep_

		wx = wx - center
		if ((naps == 1) || (all == NO))
		    call ap_update (gp, Memi[aps+current-1], line, apid,
			apbeam, center, wx, high)
		else {
		    do i = 1, naps {
			call ap_values (i, Memi[aps], line, apid,
			    apbeam, center, low, high)
			call ap_update (gp, Memi[aps+i-1], line, apid,
			    apbeam, center, wx, high)
		    }
		}

	    case 'm', 'n': # Define a new aperture.
		if (mod (naps, 100) == 0)
		    call realloc (aps, naps+100, TY_POINTER)

		if (key == 'm')
		    wx = ap_center (wx, Memr[imdata], npts)

		if (!IS_INDEF(wx)) {
		    naps = naps + 1
		    if (naps > 1)
			call ap_copy (Memi[aps+current-1], Memi[aps+naps-1])
		    else
			call ap_copy (apdef, Memi[aps+naps-1])

		    AP_ID(Memi[aps+naps-1]) = INDEFI
		    AP_CEN(Memi[aps+naps-1], apaxis) = wx -
		    	ap_cveval (AP_CV(Memi[aps+naps-1]), real (line))
		    AP_CEN(Memi[aps+naps-1], dispaxis) = line
		    AP_LOW(Memi[aps+naps-1], dispaxis) =
			1 - AP_CEN(Memi[aps+naps-1], dispaxis)
		    AP_HIGH(Memi[aps+naps-1], dispaxis) = IM_LEN(im, dispaxis) -
			AP_CEN(Memi[aps+naps-1], dispaxis)

		    call ap_icset (Memi[aps+naps-1], Memi[aps+naps-1], npts)

		    current = naps
		    i = apgwrd ("order", cmd, SZ_LINE, ORDER)
		    call ap_sort (current, Memi[aps], naps, i)
		    call ap_ids (Memi[aps], naps, ids)
		    call ap_titles (Memi[aps+current-1], 1, ids)

		    call ap_values (current, Memi[aps], line, apid,
			apbeam, center, low, high)
		    call ap_gmark (gp, line, Memi[aps+current-1], 1)
		}

	    case 'o': # Order the aperture and beam numbers
		if (naps == 0)
		    goto beep_

		do i = 1, naps
		    if (i != current)
		        AP_ID(Memi[aps+i-1]) = INDEFI

		call printf ("Aperture (%d) = ")
		    call pargi (AP_ID(Memi[aps+current-1]))
		call flush (STDOUT)
		if (scan () != EOF) {
		    call gargi (apid)
		    if (nscan() == 1) {
			AP_ID(Memi[aps+current-1]) = apid
			AP_BEAM(Memi[aps+current-1]) = apid
		    }
		}

		i = apgwrd ("order", cmd, SZ_LINE, ORDER)
		call ap_sort (current, Memi[aps], naps, i)
		call ap_ids (Memi[aps], naps, ids)

		# Reset the titles
		do i = 1, naps
		    if (AP_TITLE(Memi[aps+i-1]) != NULL)
			call mfree (AP_TITLE(Memi[aps+i-1]), TY_CHAR)
		call ap_titles (Memi[aps], naps, ids)
			
		newgraph = YES

	    case 'r': # Redraw the graph.
		newgraph = YES

	    case 's': # Shift apertures
		if (current == 0)
		    goto beep_

		call printf ("Center aperture %d (no)? ")
		    call pargi (AP_ID(Memi[aps+current-1]))
		call flush (STDOUT)
		if (scan () != EOF) {
		    call gargb (peak)
		    if (nscan() == 1 && peak) {
		        wy = ap_center (wx, Memr[imdata], npts)
		        if (!IS_INDEF(wy))
			    wx = wy
		    }
		}

		if ((naps == 1) || (all == NO))
		    call ap_update (gp, Memi[aps+current-1], line, apid,
			apbeam, wx, low, high)
		else {
		    wx = wx - center
		    do i = 1, naps {
		        call ap_values (i, Memi[aps], line, apid,
			    apbeam, center, low, high)
			call ap_update (gp, Memi[aps+i-1], line, apid,
			    apbeam, center + wx, low, high)
		    }
		}

	    case 't': # Trace.
		if (current == 0)
		    goto beep_

		call imunmap (im)
		call appstr ("ansfittrace1", "yes")
		if (all == NO)
		    call ap_trace (image, line, Memi[aps+current-1], 1, YES)
		else
		    call ap_trace (image, line, Memi[aps], naps, YES)
		call ap_getdata (image, line, nsum, im, imdata, npts, apaxis,
		    title)
		newgraph = YES

	    case 'u': # Set the upper limit.
		if (current == 0)
		    goto beep_

		wx = wx - center
		if ((naps == 1) || (all == NO))
		    call ap_update (gp, Memi[aps+current-1], line, apid,
			apbeam, center, low, wx)
		else {
		    do i = 1, naps {
			call ap_values (i, Memi[aps], line, apid,
			    apbeam, center, low, high)
			call ap_update (gp, Memi[aps+i-1], line, apid,
			    apbeam, center, low, wx)
		    }
		}

	    case 'w': # Window the graph.
		call gt_window (gt, gp, "gcur", newgraph)

	    case 'y': # Set aperture limits at the y level.
		if (current == 0)
		    goto beep_

		if ((naps == 1) || (all == NO)) {
		    low = -npts
		    high = npts
		    call ap_ylevel (Memr[imdata], npts, wy, false, false, 0.,
			center, low, high)
		    call ap_update (gp, Memi[aps+current-1], line, apid,
			apbeam, center, low, high)
		} else {
		    do i = 1, naps {
		        call ap_values (i, Memi[aps], line, apid,
			    apbeam, center, low, high)
		        low = -npts
		        high = npts
			call ap_ylevel (Memr[imdata], npts, wy, false, false,
			    0., center, low, high)
			call ap_update (gp, Memi[aps+i-1], line, apid,
			    apbeam, center, low, high)
		    }
		}

	    case 'z': # Apply resize algorithm.
		if (current == 0)
		    goto beep_

		call imunmap (im)
		if (all == NO) {
		    call gseti (gp, G_PLTYPE, 0)
		    call ap_gmark (gp, line, Memi[aps+current-1], 1)
		    call ap_resize (image, line, nsum,
			Memi[aps+current-1], 1, YES)
		    call gseti (gp, G_PLTYPE, 1)
		    call ap_gmark (gp, line, Memi[aps+current-1], 1)
		    call ap_values (current, Memi[aps], line, apid,
			apbeam, center, low, high)
		} else {
		    call gseti (gp, G_PLTYPE, 0)
		    do i = 1, naps
		        call ap_gmark (gp, line, Memi[aps+i-1], 1)
		    call ap_resize (image, line, nsum, Memi[aps], naps, YES)
		    call gseti (gp, G_PLTYPE, 1)
		    do i = 1, naps
		        call ap_gmark (gp, line, Memi[aps+i-1], 1)
		}
		call ap_getdata (image, line, nsum, im, imdata, npts, apaxis,
		    title)

	    case 'I': # Interrupt
		call fatal (0, "Interrupt")

	    default: # Ring bell for unrecognized commands.
beep_		call printf ("Invalid or unrecognized command\007")
		statline = YES
	    }

	    # Update the graph if needed.
	    if (newgraph == YES) {
		call ap_graph (gp, gt, Memr[imdata], npts, line,
		    Memi[aps], naps)
	        newgraph = NO
	    }

	    # Set the cursor to the current aperture and print the current
	    # aperture on the status line.
	    call ap_gscur (current, gp, line, Memi[aps], wy)
	    if (statline == NO)
	        call ap_print (current, line, all, Memi[aps])

	} until (gt_gcur ("gcur", wx, wy, wcs, key, cmd, SZ_LINE) == EOF)

	# Log the editing operation.
	call sprintf (Memc[str], SZ_LINE, "EDIT - %d apertures edited for %s")
       	    call pargi (naps)
       	    call pargstr (image)
	call ap_log (Memc[str], YES, NO, NO)

	# Free memory.
	call ap_fids (ids)
	call mfree (imdata, TY_REAL)
	call mfree (title, TY_CHAR)
	call imunmap (im)
	call gt_free (gt)
	call gt_free (ic_gt)
	call ap_free (apdef)

	# If a new image is desired loop back.
	if (newim == YES) {
	    call clgstr ("database", Memc[output], SZ_LINE)
	    call sprintf (Memc[str], SZ_LINE,
	        "Write apertures for %s to %s")
	        call pargstr (image)
	        call pargstr (Memc[output])
	    if (ap_answer ("ansdbwrite", Memc[str]))
	        call ap_dbwrite (image, aps, naps)
	    call strcpy (cmd, image, SZ_FNAME)
	    call ap_dbread (image, aps, naps)
	    goto new_
	}

	call appstr ("ansdbwrite1", "yes")
	call sfree (sp)
end
