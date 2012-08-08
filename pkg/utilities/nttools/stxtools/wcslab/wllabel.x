include <gset.h>
include <math.h>
include "psiescape.h"
include "wcslab.h"
include "wcs_desc.h"


# Define the offset array.
define OFFSET Memr[$1+$2-1]

# Define the postscript kernel
define PSIKERN "psikern"

# WL_LABEL  -- Place the labels on the grids.
#
# Description
#  Format and write the labels for the grid/tick marks.  Much of this
#  is wading through conditions to decide whether a label should be
#  written or not.

procedure wl_label (wd)

pointer wd                  # I: the WCSLAB descriptor

bool	no_side_axis1, no_side_axis2, streq()
int	i, axis1_side, axis2_side
short	flag
pointer	kernel, sp, offset_ptr
real	offset

begin
	# Get some memory.
	call smark (sp)
	call salloc (offset_ptr, N_SIDES, TY_REAL)
	do i = 1, N_SIDES
	    OFFSET(offset_ptr,i) = 0.
        call salloc (kernel, SZ_LINE, TY_CHAR )

	# Decide whether any sides were specified for either axis.
	no_side_axis1 = true
	no_side_axis2 = true
	do i = 1, N_SIDES {
	    if (WL_LABEL_SIDE(wd,i,AXIS1))
	        no_side_axis1 = false
	    if (WL_LABEL_SIDE(wd,i,AXIS2))
	        no_side_axis2 = false
	}

	# If polar, then label the axis 2's next to their circles on the
	# graph and allow the Axis 1s to be labeled on all sides of the graph.

	if  (WL_GRAPH_TYPE(wd) == POLAR) {

	    call wl_polar_label (wd)

	    if (no_side_axis1) {
	        do i = 1, N_SIDES {
	          WL_LABEL_SIDE(wd,i,AXIS1) = true
	        }
	        if (IS_INDEFI (WL_AXIS_TITLE_SIDE(WD,AXIS1)))
	    	    WL_AXIS_TITLE_SIDE(WD,AXIS1) = BOTTOM
	    }

	# If we are near-polar, label the Axis 2 as if polar, and label
	# Axis1 on all sides except the side closest to the pole.

	} else if  (WL_GRAPH_TYPE(wd) == NEAR_POLAR) {

	    if (no_side_axis1) {
	        WL_LABEL_SIDE(wd,WL_BAD_LABEL_SIDE(wd),AXIS1) = true
	        if (IS_INDEFI (WL_AXIS_TITLE_SIDE(wd,AXIS1)))
	    	    WL_AXIS_TITLE_SIDE(wd,AXIS1) = WL_BAD_LABEL_SIDE(wd)
	    }

	    if (no_side_axis2) {
	        WL_LABEL_SIDE(wd,WL_POLAR_LABEL_DIRECTION(wd),AXIS2) = true
	        if (IS_INDEFI (WL_AXIS_TITLE_SIDE(wd,AXIS2)))
	        WL_AXIS_TITLE_SIDE(wd,AXIS2) = WL_POLAR_LABEL_DIRECTION(wd)
	    }

	# Final case- adjacent sides should be labelled.

	} else {

	    # Determine the best sides for labelling.
	    if  (INVERT (WL_ROTA(wd))) {
	        axis1_side = LEFT
	        axis2_side = BOTTOM
	    } else {
	        axis1_side = BOTTOM
	        axis2_side = LEFT
	    }

	    # If no sides were specified, use the calculated ones above.
	    if (no_side_axis1)
	        WL_LABEL_SIDE(wd,axis1_side,AXIS1) = true
	    if (no_side_axis2)
	        WL_LABEL_SIDE(wd,axis2_side,AXIS2) = true
	}

        # Check to see if this is a psikern printer.  If so, set text
        # so that it is mono-spaced.  The superscripting algorithm
        # doesn't work too well in a proportional-spaced system.
        call ggets (WL_GP(wd), "tn", Memc[kernel], SZ_LINE )
        if (streq (Memc[kernel], PSIKERN)) {
            flag = NO
            call gescape (WL_GP(wd), PS_VARIABLE_SPACE, flag,
                          PS_VARIABLE_SPACE_SIZE)
        }

	# Now draw the labels for axis 1.
	do i = 1, N_SIDES {

	    if (WL_LABEL_SIDE(wd,i,AXIS1)) {
	        call wl_lab_edges (wd, AXIS1, i, offset)
	        if (IS_INDEFI (WL_AXIS_TITLE_SIDE(WD,AXIS1)))
	  	    WL_AXIS_TITLE_SIDE(WD,AXIS1) = i
	    } else
	        offset = 0.

	  # Modify the bounding box for the new viewport.
	  if (abs (offset) > abs (OFFSET(offset_ptr,i)))
	    OFFSET(offset_ptr,i) = offset
	}

	# Draw the labels for axis 2.
	if (WL_GRAPH_TYPE(wd) != POLAR) 
	    do i = 1, N_SIDES {

	        if (WL_LABEL_SIDE(wd,i,AXIS2)) {
	      	    call wl_lab_edges (wd, AXIS2, i, offset)
	      	    if (IS_INDEFI (WL_AXIS_TITLE_SIDE(wd,AXIS2)))
	            WL_AXIS_TITLE_SIDE(wd,AXIS2) = i
	        } else
	    	    offset = 0.

	        # Modify the bounding box for the new viewport.
	        if (abs (offset) > abs (OFFSET(offset_ptr,i)))
	    	    OFFSET(offset_ptr,i) = offset
	    }

        # Reset to variable spacing.
        if (streq (Memc[kernel], PSIKERN)) {
            flag = YES
            call gescape (WL_GP(wd), PS_VARIABLE_SPACE, flag,
                          PS_VARIABLE_SPACE_SIZE)
        }

	# Set the bounding box.
	do i = 1, N_SIDES
	    WL_NEW_VIEW(wd,i) = WL_NEW_VIEW(wd,i) + OFFSET(offset_ptr,i)

	# Now write the graph title.
	call wl_title (WL_GP(wd), WL_AXIS_TITLE(wd,AXIS1), 
	    WL_AXIS_TITLE_SIDE(wd,AXIS1), WL_AXIS_TITLE_SIZE(wd),
	    WL_NEW_VIEW(wd,1))
	if (WL_GRAPH_TYPE(wd) != POLAR)
	    call wl_title (WL_GP(wd), WL_AXIS_TITLE(wd,AXIS2), 
	        WL_AXIS_TITLE_SIDE(wd,AXIS2), WL_AXIS_TITLE_SIZE(WD),
		WL_NEW_VIEW(wd,1))
	if (! IS_INDEFI (WL_TITLE_SIDE(wd)))
	    call wl_title (WL_GP(wd), WL_TITLE(wd), WL_TITLE_SIDE(wd),
	        WL_TITLE_SIZE(wd), WL_NEW_VIEW(wd,1))

	# Release memory.
	call sfree (sp)
end


# Define what is in the screen.

define IN (($1>WL_SCREEN_BOUNDARY(wd,LEFT))&&($1<WL_SCREEN_BOUNDARY(wd,RIGHT))&&($2>WL_SCREEN_BOUNDARY(wd,BOTTOM))&&($2<WL_SCREEN_BOUNDARY(wd,TOP)))

# WL_POLAR_LABEL -- Place Latitude labels next to Latitude circles.
#
# Description
#  Since Lines of constant Latitude on a polar graph are usually circles
#  around the pole, the lines may never cross edges.  Instead, the labels
#  are placed next to circles.  The grid-drawing routines should setup
#  the label position array such that each line has only one label point.

procedure wl_polar_label (wd)

pointer wd            # I: the WCSLAB descriptor

int	i, prec
pointer sp, label, units, label_format, units_format
real	char_height, char_width, ndc_textx, ndc_texty, old_text_size
real	textx, texty
int	wl_precision()
real	gstatr(), ggetr()

begin
	# Get some memory.
	call smark (sp)
	call salloc (label, SZ_LINE, TY_CHAR)
	call salloc (units, SZ_LINE, TY_CHAR)
	call salloc (label_format, SZ_LINE, TY_CHAR)
	call salloc (units_format, SZ_LINE, TY_CHAR)

	# Get the character height and width.  This is used to ensure that we
	# have moved the label strings off the border.

	char_height = ggetr (WL_GP(wd), "ch") * gstatr (WL_GP(wd), G_TXSIZE) /
	    2.
	char_width = ggetr (WL_GP(wd), "cw") * gstatr (WL_GP(wd), G_TXSIZE) /
	    2.

	# Get the text size and cut it in half for on the plot labelling.
	old_text_size = gstatr (WL_GP(wd), G_TXSIZE)
	call gsetr (WL_GP(wd), G_TXSIZE, old_text_size)
	call gsetr (WL_GP(wd), G_TXSIZE, old_text_size * 0.80)

	# Determine the precision of the output.
	prec = wl_precision (wd, AXIS2)

	# Place the labels.
	for  (i = 1; i <= WL_N_LABELS(wd); i = i + 1)
	    if  (WL_LABEL_AXIS(wd,i) == AXIS2) {

	        # Decode the coordinate into a text string.
	        call wl_dms (WL_LABEL_VALUE(wd,i), Memc[label], Memc[units],
	            SZ_LINE, prec, true)

	        # Convert text position from "unknown" coordinates to NDC.
	        call gctran (WL_GP(wd), real (WL_LABEL_POSITION(wd,i,AXIS1)), 
	            real (WL_LABEL_POSITION(wd,i,AXIS2)), ndc_textx, ndc_texty,
		    WL_PLOT_WCS(wd), WL_NDC_WCS(wd))

	        # Determine the text justification.
	        switch  (WL_POLAR_LABEL_DIRECTION(wd)) {
	        case BOTTOM: 
	            call strcpy ("h=c;v=t", Memc[label_format], SZ_LINE)
	            call strcpy ("h=c;v=c", Memc[units_format], SZ_LINE)
	            ndc_texty = ndc_texty - char_height
	        case TOP: 
	            call strcpy ("h=c;v=c", Memc[label_format], SZ_LINE)
	            call strcpy ("h=c;v=b", Memc[units_format], SZ_LINE)
	            ndc_texty = ndc_texty + char_height
	        case LEFT: 
	            call strcpy ("h=r;v=c", Memc[label_format], SZ_LINE)
	            call strcpy ("h=r;v=b", Memc[units_format], SZ_LINE)
	            ndc_textx = ndc_textx - char_width
	        case RIGHT: 
	            call strcpy ("h=l;v=c", Memc[label_format], SZ_LINE)
	            call strcpy ("h=l;v=b", Memc[units_format], SZ_LINE)
	            ndc_textx = ndc_textx + char_width
	        }

	        # Convert the text position from NDC back to the "unknown"
		# system.
	        call gctran (WL_GP(wd), ndc_textx, ndc_texty, textx, texty, 
	            WL_NDC_WCS(wd), WL_PLOT_WCS(wd))
	  
	        # Print the label.
	        if (IN (textx, texty)) {
	    	    call gtext (WL_GP(wd), textx, texty, Memc[label], 
	                Memc[label_format])
	    	    call gtext (WL_GP(wd), textx, texty, Memc[units], 
	                Memc[units_format])
	        }

	    }

	# Set the text size back.
	call gsetr (WL_GP(wd), G_TXSIZE, old_text_size)

	# Release memory.
	call sfree (sp)

end


# Memory management for labels

define LABEL_LIST Memi[labels+$1-1]

# WL_LAB_EDGES  -- Place labels along the edges of the window.
#
# Description
#  Place labels on the specified side of the graph.

procedure wl_lab_edges (wd, axis, side, offset)

pointer wd      	# I: the WCSLAB descriptor
int     axis    	# I: the type of axis being labeled
int     side    	# I: the side to place the labels
real    offset  	# O: offset in NDC units for titles

bool	do_full
double	angle, tangle
int	i, full_label, nlabels, old_wcs, prec
pointer sp, labels 
real	ndc_textx, ndc_texty, old_text_size, textx, texty

int	wl_full_label_position(), wl_find_side()
double	wl_string_angle(), wl_angle()
int	gstati(), wl_precision()
real	gstatr()

begin
	call smark (sp)

	# All label placement is done in NDC coordinates.
	old_wcs = gstati (WL_GP(wd), G_WCS)
	call gseti (WL_GP(wd), G_WCS, WL_NDC_WCS(wd))

	# Set text labelling size.
	old_text_size = gstatr (WL_GP(wd), G_TXSIZE)
	call gsetr (WL_GP(wd), G_TXSIZE, WL_LABEL_SIZE(wd))

	# Get the precision of the axis interval.
	prec = wl_precision (wd, axis)

	# Initialize string size.
	offset = 0.

	# Build a list of possible labels for this side. The conditions are
	# that the label should be for the current axis and that it lies on
	# the current side.

	call salloc (labels, WL_N_LABELS(wd), TY_INT)
	nlabels = 0
	for (i = 1; i <= WL_N_LABELS(wd); i = i + 1)
	    if  (WL_LABEL_AXIS(wd,i) == axis && 
	        wl_find_side (WL_LABEL_POSITION(wd,i,AXIS1),
	        WL_LABEL_POSITION(wd,i,AXIS2), 
	        WL_SCREEN_BOUNDARY(wd,1)) == side) {
	        nlabels = nlabels + 1
	        LABEL_LIST(nlabels) = i
	    }  
	
	# If no labels found, then just forget it.  If labels found, well
	# write them out.

	if (nlabels != 0) {

	    # Determine which label should be written out in full.
	    full_label = wl_full_label_position (wd, Memi[labels], nlabels,
	        axis, side, prec)

	    # Determine the angle that all the labels will be written at.
	    if ((WL_LABOUT(wd) == NO) && (WL_GRAPH_TYPE(wd) != NORMAL) &&
	        (WL_LABEL_ROTATE(wd) == YES))
		angle = INDEFR
	    else if ((WL_GRAPH_TYPE(wd) == NORMAL) && ((WL_LABEL_ROTATE(wd) ==
		YES) || ((WL_LABOUT(wd) == NO) && (WL_MAJ_GRIDON(wd) == YES))))
	        angle = wl_angle (wd, Memi[labels], nlabels)
	    else
		angle = 0.0

	    # Place the labels.
	    for  (i = 1; i <= nlabels; i = i + 1) {
	
	        # Save some pertinent information.
	        textx = real (WL_LABEL_POSITION(wd,LABEL_LIST(i),AXIS1))
	        texty = real (WL_LABEL_POSITION(wd,LABEL_LIST(i),AXIS2))
	        do_full =  ((LABEL_LIST(i) == full_label) ||
	    	    (WL_ALWAYS_FULL_LABEL(wd) == YES))
	
	        # Transform the "unknown" coordinate system to a known
		# coordinate system, NDC, for text placement.
	        call gctran (WL_GP(wd), textx, texty, ndc_textx, ndc_texty, 
	            old_wcs, WL_NDC_WCS(wd))

		# If angle is undefined, determine the angle for each label.
		if (IS_INDEFR(angle))
		    tangle = wl_string_angle (WL_LABEL_ANGLE(wd,
		        LABEL_LIST(i)), WL_LABOUT(wd))
		else
		    tangle = angle
	
	        # Format and write the label.
	        call wl_write_label (wd, WL_LABEL_VALUE(wd,LABEL_LIST(i)), 
	            side, ndc_textx, ndc_texty, tangle, axis, prec, do_full,
		    offset)
	    }
	}
	
	# Reset the graphics WCS.
	call gsetr (WL_GP(wd), G_TXSIZE, old_text_size)
	call gseti (WL_GP(wd), G_WCS, old_wcs)

	call sfree (sp)
end


# WL_TITLE - Write the title of the graph.

procedure wl_title (gp, title, side, size, viewport)

pointer gp                 # I: the graphics descriptor
char    title[ARB]         # I: the title to write
int     side               # I: which side the title will go
real    size               # I: the character size to write the title
real    viewport[N_SIDES]  # I: the viewport in NDC to keep the title out of

int	old_wcs
real	char_height, char_width, left, right, top, bottom, old_rotation
real	old_text_size, x, y
int	gstati(), strlen()
real	ggetr(), gstatr()

begin
	# Make sure there is a title to write.  If not, then punt.
	if (strlen (title) <= 0)
	  return

	# Get/Set pertinent graphics info.
	call ggview (gp, left, right, bottom, top)

	old_text_size = gstatr (gp, G_TXSIZE)
	call gsetr (gp, G_TXSIZE, size)
	old_rotation = gstatr (gp, G_TXUP)

	char_height = ggetr (gp, "ch") * size
	char_width = ggetr (gp, "cw") * size

	old_wcs = gstati (gp, G_WCS)
	call gseti (gp, G_WCS, NDC_WCS)

	# Depending on side, set text position and rotation.
	switch (side) {
	case TOP:
	    call gsetr (gp, G_TXUP, 90.)
	    x =  (right + left) / 2.
	    y = viewport[TOP] +  (2 * char_height)
	    viewport[TOP] = y +  (char_height / 2.)
	case BOTTOM:
	    call gsetr (gp, G_TXUP, 90.)
	    x =  (right + left) / 2.
	    y = viewport[BOTTOM] -  (2 * char_height)
	    viewport[BOTTOM] = y -  (char_height / 2.)
	case RIGHT:
	    call gsetr (gp, G_TXUP, 180.)
	    x = viewport[RIGHT] +  (2 * char_width)
	    y =  (top + bottom) / 2.
	    viewport[RIGHT] = x +  (char_width / 2.)
	case LEFT:
	    call gsetr (gp, G_TXUP, 180.)
	    x = viewport[LEFT] -  (2 * char_width)
	    y =  (top + bottom) / 2.
	    viewport[LEFT] = x -  (char_width / 2.)
	}

	# Write the puppy out.
	call gtext (gp, x, y, title, "h=c;v=c")

	# Set the graphics state back.
	call gseti (gp, G_WCS, old_wcs)
	call gsetr (gp, G_TXSIZE, old_text_size)
	call gsetr (gp, G_TXUP, old_rotation)
end


# WL_PRECISION -- Determine the precision of the interval.

int procedure wl_precision (wd, axis)

pointer wd    	  # I: the WCSLAB descriptor
int     axis      # I: which axis is being examined ?

int	prec 

begin
	# Handle the sky coordinates.
	if (WL_SYSTEM_TYPE(wd) == RA_DEC)

	    if (axis == AXIS1) {
	        if (WL_MAJOR_INTERVAL(wd,AXIS1) >= STTODEG (3600.0D0))
	    	    prec = HOUR
	        else if (WL_MAJOR_INTERVAL(wd,AXIS1) >= STTODEG (60.0D0))
	    	    prec = MINUTE
	        else if (WL_MAJOR_INTERVAL(wd,AXIS1) >= STTODEG (1.0D0))
	    	    prec = SECOND
	        else if (WL_MAJOR_INTERVAL(wd,AXIS1) >= STTODEG (.01D0))
	    	    prec = SUBSEC_LOW
	        else
	    	    prec = SUBSEC_HIGH
	    } else {
	        if (WL_MAJOR_INTERVAL(wd,AXIS2) >= SATODEG (3600.0D0))
	    	    prec = DEGREE
	        else if (WL_MAJOR_INTERVAL(wd,AXIS2) >= SATODEG (60.0D0))
	    	    prec = MINUTE
	        else if (WL_MAJOR_INTERVAL(wd,AXIS2) >= SATODEG (1.0D0))
	    	    prec = SECOND
	        else if (WL_MAJOR_INTERVAL(wd,AXIS2) >= SATODEG (.01D0))
	    	    prec = SUBSEC_LOW
	        else
	    	    prec = SUBSEC_HIGH
	    }

	# Handle other coordinate types.
	else
	    prec = INDEFI

	return (prec)

end


# Define some value constraints.

define LOW_ACCURACY .01
define HIGH_ACCURACY .0001

# WL_HMS -- Convert value to number in hours, minutes, and seconds.

procedure wl_hms (rarad, hms, units, maxch, precision, all)

double  rarad            # I: the value to format into a string (degrees)
char    hms[ARB]         # O: string containing formatted value
char    units[ARB]       # O: string containing formatted units
int     maxch            # I: the maximum number of characters allowed
int     precision        # I: how precise the output should be
bool    all              # I: true if all relevent fields should be formatted

double  accuracy, fraction
int	sec, h, m, s
pointer sp, temp_hms, temp_units 

begin
	# Get some memory.
	call smark (sp)
	call salloc (temp_hms, maxch, TY_CHAR)
	call salloc (temp_units, maxch, TY_CHAR)

	units[1] = EOS
	hms[1]   = EOS

	# Define how close to zero is needed.
	accuracy = LOW_ACCURACY
	if (precision == SUBSEC_HIGH)
	    accuracy = HIGH_ACCURACY

	# Seconds of time.
	fraction = double (abs(DEGTOST (rarad)))
	if (precision == SUBSEC_LOW || precision == SUBSEC_HIGH) {
	    sec = int (fraction)
	    fraction = fraction - double (sec)
	} else {
	    sec = int (fraction + 0.5)
	    fraction = 0.
	}

	# Range:  0 to 24 hours.
	if (sec < 0)
	    sec = sec + STPERDAY
	else if (sec >= STPERDAY)
	    sec = mod (sec, STPERDAY)

	# Separater fields.
	s = mod (sec, 60)
	m = mod (sec / 60, 60)
	h = sec / 3600

	# Format fields.

	# Subseconds.
	if (precision == SUBSEC_LOW || precision == SUBSEC_HIGH) {
	    fraction = s + fraction
	    if (precision == SUBSEC_LOW) {
	        call sprintf (hms, 6, "%05.2f")
	    	    call pargd (fraction)
	        call strcpy ("  s  ", units, maxch)
	    } else {
	        call sprintf (hms, 8, "%07.4f")
	    	    call pargd (fraction)
	        call strcpy ("  s    ", units, maxch)
	    }
	    if (!all)
	        all =  (fraction < accuracy)

	# Seconds
	} else if (precision == SECOND) {

	    # NOTE: The all is not part of the if statement because if
	    # SUBSEC's have been printed, then seconds have already been
	    # dealt with.  If SUBSEC's have not been dealt with, then this
	    # is the first field to be checked anyways.

	    call sprintf (hms, 3, "%02d ")
	        call pargi (s)
	    call strcpy ("  s", units, maxch)
	    if (! all) 
		all =  (s == 0)
	}

	# Minutes.
	if (precision == MINUTE ||  (precision > MINUTE && all)) {
	    if (all) {
	        call strcpy (hms, Memc[temp_hms], maxch)
	        call strcpy (units, Memc[temp_units], maxch)
	    }
	    call sprintf (hms, 3, "%02d ")
		call pargi (m)
	    call strcpy ("  m", units, maxch)
	    if (all) {
	        call strcat (Memc[temp_hms], hms, maxch)
	        call strcat (Memc[temp_units], units, maxch)
	    } else
	        all =  (m == 0)
	}

	# Non-zero hours.
	if  (precision == HOUR || all) {
	    if (all) {
	        call strcpy (hms, Memc[temp_hms], maxch)
	        call strcpy (units, Memc[temp_units], maxch)
	    }
	    call sprintf (hms, 3, "%2.2d ")
		call pargi (h)
	    call strcpy("  h", units, maxch)
	    if (all) {
		call strcat (Memc[temp_hms], hms, maxch)
	        call strcat (Memc[temp_units], units, maxch)
	    }
	}

	# Release memory
	call sfree (sp)
end


# WL_DMS - Convert value to number in degrees, minutes, and seconds.

procedure wl_dms (arcrad, dms, units, maxch, precision, all)

double  arcrad           # I: the value to format into a string (degrees)
char    dms[ARB]         # O: string containing formatted value
char    units[ARB]       # O: string containing formatted units
int     maxch            # I: the maximum number of characters allowed
int     precision        # I: how precise the output should be ?
bool    all              # I: true if all relavent fields should be formatted

double  accuracy, fraction 
int	sec, h, m, s
pointer sp, temp_dms, temp_units
int	strlen()

begin
	# Get some memory.
	call smark (sp)
	call salloc (temp_dms, maxch, TY_CHAR)
	call salloc (temp_units, maxch, TY_CHAR)

	units[1] = EOS
	dms[1]   = EOS

	# Define how close to zero is needed.
	accuracy = LOW_ACCURACY
	if (precision == SUBSEC_HIGH)
	    accuracy = HIGH_ACCURACY

	# Seconds of time.
	fraction = double (abs (DEGTOSA (arcrad)))
	if (precision == SUBSEC_LOW || precision == SUBSEC_HIGH) {
	    sec = int (fraction)
	    fraction = fraction - double (sec)
	} else {
	    sec = nint  (fraction)
	    fraction = 0.
	}

	# Separater fields.
	s = mod (abs(sec), 60)
	m = mod (abs(sec) / 60, 60)
	h = abs(sec) / 3600

	# Format fields
	#
	# Subseconds.
	if (precision == SUBSEC_LOW || precision == SUBSEC_HIGH) {

	    fraction = s + fraction
	    call strcpy (dms, Memc[temp_dms], maxch)
	    call strcpy (units, Memc[temp_units], maxch)
	    if (precision == SUBSEC_LOW) {
	        call sprintf (dms, 6, "%05.2f\"")
	    	    call pargd (fraction)
	        call strcpy ("      ", units, maxch)
	    } else {
	        call sprintf (dms, 8, "%07.4f\"")
	            call pargd (fraction)
	        call strcpy ("        ", units, maxch)
	    }
	    if (! all)
	        all =  (fraction < accuracy)
	    call strcat (Memc[temp_dms], dms, maxch)
	    call strcat (Memc[temp_units], units, maxch)

	# Seconds
	} else if (precision == SECOND) {

	    # NOTE: The all is not part of the if statement because if
	    # SUBSEC's have been printed, then seconds have already been
	    # dealt with.  If SUBSEC's have not been dealt with, then this
	    # is the first field to be checked anyways.

	    call strcpy (dms, Memc[temp_dms], maxch)
	    call strcpy (units, Memc[temp_units], maxch)
	    call sprintf (dms, 3, "%02d\"")
		call pargi (s)
	    call strcpy ("   ", units, maxch)
	    if (! all) 
	    	all =  (s == 0)
	    call strcat (Memc[temp_dms], dms, maxch)
	    call strcat (Memc[temp_units], units, maxch)
	}

	# Minutes.
	if (precision == MINUTE ||  (precision > MINUTE && all)) {
	    call strcpy (dms, Memc[temp_dms], maxch)
	    call strcpy (units, Memc[temp_units], maxch)
	    call sprintf (dms, 3, "%02d'")
		call pargi (m)
	    call strcpy ("   ", units, maxch)
	    call strcat (Memc[temp_dms], dms, maxch)
	    call strcat (Memc[temp_units], units, maxch)
	    if (! all)
	        all =  (m == 0)
	}

	# Hours.
	if (precision == DEGREE || all) {
	    call strcpy (dms, Memc[temp_dms], maxch)
	    call strcpy (units, Memc[temp_units], maxch)
	    if (sec + fraction < accuracy)
	        call strcpy (" 0 ", dms, maxch)
	    else if (arcrad < 0.) {
	        call sprintf (dms, 4, "-%d ")
	    	    call pargi (h)
	    } else {
	        call sprintf (dms, 4, "+%d ")
	    	    call pargi (h)
	    }
	    call sprintf(units, 4, "%*wo")
		call pargi (strlen (dms) - 1)
	    call strcat (Memc[temp_dms], dms, maxch)
	    call strcat (Memc[temp_units], units, maxch)
	}

	# Release memory.
	call sfree (sp)
end


# WL_FULL_LABEL_POSTION -- Find the position where the full label should be.
#
# Description
#   This routine returns the index to the label that should be printed
#   in its full form, regardless of its value.  This is so there is always
#   at least one labelled point with the full information.  This point is
#   choosen by examining which label is the closest to the passed point
#   (usually one of the four corners of the display).
#
# Returns
#   Index into the labell arrays of the label to be fully printed.
#   If the return index is 0, then there are no labels for the given
#   side.

int procedure wl_full_label_position (wd, labels, nlabels, axis, side,
	precision)

pointer wd                   # I: the WCSLAB descriptor
int     labels[nlabels]      # I: array of indexes of labels to be printed
int     nlabels              # I: the number of labels in labels
int     axis                 # I: the axis being dealt with
int     side                 # I: the side being dealt with
int	precision	     # I: precision of the label

bool	all
double	cur_dist, dist
int	i, cur_label, xside, yside
pointer	sp, temp1
double	wl_distanced()

begin
	# Allocate some working space.
	call smark (sp)
	call salloc (temp1, SZ_LINE, TY_CHAR)

	# Initialize.
	xside = INDEFI
	yside = INDEFI

	# Determine which corner will have the full label.
	if (side == TOP || side == BOTTOM) {
	    yside = side
	    if (axis == AXIS1) {
	        if (WL_LABEL_SIDE(wd,RIGHT,AXIS2))
	    	    xside = RIGHT
	        if (WL_LABEL_SIDE(wd,LEFT,AXIS2))
	    	    xside = LEFT
	    } else {
	        if (WL_LABEL_SIDE(wd,RIGHT,AXIS1))
	    	    xside = RIGHT
	        if (WL_LABEL_SIDE(wd,LEFT,AXIS1))
	    	    xside = LEFT
	    }
	    if (IS_INDEFI (xside))
	    	xside = LEFT
	} else {
	    xside = side
	    if (axis == AXIS1) {
	        if (WL_LABEL_SIDE(wd,TOP,AXIS2))
	    	    yside = TOP
	        if (WL_LABEL_SIDE(wd,BOTTOM,AXIS2))
	    	    yside = BOTTOM
	    } else {
	        if (WL_LABEL_SIDE(wd,TOP,AXIS1))
	    	    yside = TOP
	        if (WL_LABEL_SIDE(wd,BOTTOM,AXIS1))
	    	    yside = BOTTOM
	    }
	    if (IS_INDEFI (yside))
	        yside = BOTTOM
	}

	# Find the full label.
	cur_label = labels[1]
	cur_dist = wl_distanced (WL_SCREEN_BOUNDARY(wd,xside), 
	    WL_SCREEN_BOUNDARY(wd,yside),
	    WL_LABEL_POSITION(wd,cur_label,AXIS1),
	    WL_LABEL_POSITION(wd,cur_label,AXIS2))
	
	# Now go through the rest of the labels to find a closer label.
	for (i = 2; i <= nlabels; i = i + 1) {

	    # Check to see if the label would be written in full anyways.
	    all = false
	    if (WL_SYSTEM_TYPE(wd) == RA_DEC) {
		if (WL_LABEL_AXIS(wd, labels[i]) == LONGITUDE)
		    call wl_hms (WL_LABEL_VALUE(wd, labels[i]),
		        Memc[temp1], Memc[temp1], SZ_LINE, precision, all)
		else
		    call wl_dms (WL_LABEL_VALUE(wd, labels[i]),
		        Memc[temp1], Memc[temp1], SZ_LINE, precision, all)
	    }

	    # If so, don't figure out which label should be full, there
	    # will be one someplace.
	    if (all) {
		cur_label = INDEFI
		break
	    }

	    dist = wl_distanced (WL_SCREEN_BOUNDARY(wd,xside), 
	        WL_SCREEN_BOUNDARY(wd,yside), 
	        WL_LABEL_POSITION(wd,labels[i],AXIS1), 
	        WL_LABEL_POSITION(wd,labels[i],AXIS2))
	    if (dist < cur_dist) {
	        cur_dist = dist
	        cur_label = labels[i]
	    }
	}

	# Release memory.
	call sfree (sp)

	# Return the label index.
	return (cur_label)
end


# WL_WRITE_LABEL - Write the label in the format specified by the WCS type.

procedure wl_write_label (wd, value, side, x, y, angle, axis, precision,
	do_full, offset)

pointer wd                   # I:   the WCSLAB descriptor
double  value                # I:   the value to use as the label
int     side                 # I:   the side the label is going on
real    x, y                 # I:   position of the label in NDC coordinates
double  angle                # I:   the angle the text should be written at
int     axis                 # I:   which axis is being labelled
int     precision            # I:   level of precision for labels
bool    do_full              # I:   true if the full label should be printed
real    offset               # I/O: offset for titles in NDC units

int	tside
pointer	sp, label, label_format, units, units_format
real	char_height, char_width, in_off_x, in_off_y, length
real	lx, ly, new_offset, rx, ry, text_angle
real	unit_off_x, unit_off_y, ux, uy

bool	fp_equalr()
double	wl_string_angle()
int	wl_opposite_side(), strlen()
real	ggetr(), gstatr()

begin
	# Get some memory.
	call smark (sp)
	call salloc (label, SZ_LINE, TY_CHAR)
	call salloc (units, SZ_LINE, TY_CHAR)
	call salloc (label_format, SZ_LINE, TY_CHAR)
	call salloc (units_format, SZ_LINE, TY_CHAR)

	# Get character size.  This info is used to move the character string
	# by the appropriate amounts.

	char_height = ggetr (WL_GP(wd), "ch") * gstatr (WL_GP(wd), G_TXSIZE)
	char_width = ggetr (WL_GP(wd), "cw") * gstatr (WL_GP(wd), G_TXSIZE)

	# Determine the "corrected" angle to write text in.
	text_angle = wl_string_angle (angle, WL_LABOUT(wd))

	# Determine the units offset.
	call wl_rotate (0., char_height / 2., 1, text_angle - 90., unit_off_x, 
	    unit_off_y)

	# If the labels are to appear inside the graph and the major grid lines
	# have been drawn, then determine the necessary offset to get the label
	# off the line.

	if ((WL_LABOUT(wd) == NO) && (WL_MAJ_GRIDON(wd) == YES))
	    call wl_rotate (0., 0.75 * char_height, 1, text_angle - 90.,
	        in_off_x, in_off_y)
	else {
	    in_off_x = 0.
	    in_off_y = 0.
	}

	# Decode the coordinate into a text string.
	switch (WL_SYSTEM_TYPE(wd)) {
	case RA_DEC:
	    if  (axis == LONGITUDE)
	        call wl_hms (value, Memc[label], Memc[units], SZ_LINE,
		    precision, do_full)
	    else 
	        call wl_dms (value, Memc[label], Memc[units], SZ_LINE,
		    precision, do_full)
	default:
	    call sprintf (Memc[label], SZ_LINE, "%.2g")
	    call pargd (value)
	}

	# Set the text justification.
	call sprintf (Memc[label_format], SZ_LINE, "h=c;v=c;u=%f")
	    call pargr (text_angle)
	call sprintf (Memc[units_format], SZ_LINE, "h=c;v=c;u=%f")
	    call pargr (text_angle)

	# Determine offset needed to rotate text about the point of placement.
	# NOTE: The STDGRAPH kernel messes up rotate text placement.  Try to
	# accomodate with extra offset.

	length = .5 * char_width *  (2 + strlen (Memc[label]))
	call wl_rotate (length, 0., 1, text_angle - 90., rx, ry)
	rx = abs (rx)
	ry = abs (ry)

	# If labels are to appear inside the graph, then justification should
	# appear as if it were done for the opposite side.
	if (WL_LABOUT(wd) == YES)
	    tside = side
	else
	    tside = wl_opposite_side (side)

	# Now add the offsets appropriately.
	switch (tside) {
	case TOP:
	    ly = y + ry + in_off_y + unit_off_y
	    if (fp_equalr (text_angle, 90.)) {
	        lx = x
	        ly = ly + unit_off_y
	    } else if (text_angle < 90.)
	        lx = x - rx
	    else
	        lx = x + rx
	    lx = lx + in_off_x
	    new_offset = ry + ry

	case BOTTOM:
	    ly = y - ry - in_off_y - unit_off_y
	    if (fp_equalr (text_angle, 90.)) {
	        lx = x
	        ly = ly - unit_off_y
	    } else if (text_angle < 90.)
	        lx = x + rx
	    else
	        lx = x - rx
	    lx = lx - in_off_x
	    new_offset = ry + ry

	case LEFT:
	    lx = x - rx - abs (unit_off_x)
	    if (text_angle < 90.) {
	        ly = y + ry - in_off_y
	        lx = lx - in_off_x
	    } else {
	        ly = y - ry + in_off_y
	        lx = lx + in_off_x
	    }
	    new_offset = rx + rx + abs (unit_off_x)

	case RIGHT:
	    lx = x + rx + abs (unit_off_x)
	    if (text_angle < 90.) {
	        ly = y - ry + in_off_y
	        lx = lx + in_off_x
	    } else {
	        ly = y + ry - in_off_y
	        lx = lx - in_off_x
	    }
	    new_offset = rx + rx + abs (unit_off_x)
	}

	lx = lx - (unit_off_x / 2.)
	ly = ly - (unit_off_y / 2.)
	ux = lx + unit_off_x
	uy = ly + unit_off_y
	  
	# Print the label.
	call gtext (WL_GP(wd), lx, ly, Memc[label], Memc[label_format])

	# Print the units (if appropriate).
	if (WL_SYSTEM_TYPE(wd) == RA_DEC)
	    call gtext (WL_GP(wd), ux, uy, Memc[units], Memc[units_format])

	# Determine new maximum string size.
	if  ((WL_LABOUT(wd) == YES) &&  (abs (offset) < new_offset))
	    if (side == LEFT || side == BOTTOM)
	        offset = -new_offset
	    else
	        offset = new_offset

	# Release memory.
	call sfree (sp)
end


# WL_STRING_ANGLE -- Produce the angle that a label string should be written to.
#
# Description
#   Fixes the input angle so that the output angle is in the range 0 to 180.
#
# Returns
#   the angle that the label should be written as.

double procedure wl_string_angle (angle, right_to_up)

double	angle            # I: the input angle in degrees
int	right_to_up      # I: true if angle near horizontal/vertical are fixed

double	output_angle

begin
	# Try to ensure that the angle is "upright", i.e. the string will not
	# be printed upside-down.

	output_angle = angle
	if (output_angle > QUARTER_CIRCLE)
	    output_angle = output_angle - HALF_CIRCLE
	if (output_angle < -QUARTER_CIRCLE)
	    output_angle = output_angle + HALF_CIRCLE

	# If the angle is close to parallel with one of the axis, then just
	# print it normally.

	if ((right_to_up == YES) && ((mod (abs (output_angle),
	    QUARTER_CIRCLE) < MIN_ANGLE) || (QUARTER_CIRCLE -
	    mod (abs (output_angle), QUARTER_CIRCLE) < MIN_ANGLE)))
	    output_angle = 0.

	# Return the angle modified for the idiocincracy of GIO text angle
	# specification.

	return (output_angle + QUARTER_CIRCLE)
end


# WL_ANGLE -- Return the average angle of the labels in the list.
#
# Returns
#  Average angle
#
# Description
#  So that labels on a side are uniform (in some sense), the average angle
#  of all the labels is taken and is defined as the angle that all the labels
#  will be printed at.

double procedure wl_angle (wd, labels, nlabels)

pointer wd                   # I: the WCSLAB descriptor
int     labels[nlabels]      # I: the indexes of the labels to be printed out
int     nlabels              # I: the number of indexes in the list

double	total, average
int	i

begin
	total = 0.0
	for (i = 1; i <= nlabels; i = i + 1)
	    total = total + WL_LABEL_ANGLE(wd,labels[i])
	average = real (total / nlabels)

	return (average)
end
