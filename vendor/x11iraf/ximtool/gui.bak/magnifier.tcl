
################################################################################
# MAGNIFIER.  A subraster around the cursor in the main image window is
# displayed at a high resolution in a marker (known as the magnifier window)
# within the main image window.
################################################################################

set magnifier_x 0
set magnifier_y 0
set magnifier_width 0
set magnifier_height 0
set mrm_width 0
set mrm_height 0

set magnifier_enable 0
set magnifier_mag_enable 0
set magnifier_mapping 0

createMenu magzoomMenu imagewin {
    {   "Zoom Factors"        	f.title                         }
    {                           f.dblline                       }
    {   "Zoom 1"                f.exec "setMagnifierZoom 1"     }
    {   "Zoom 2"                f.exec "setMagnifierZoom 2"     }
    {   "Zoom 4"                f.exec "setMagnifierZoom 4"     }
    {   "Zoom 8"                f.exec "setMagnifierZoom 8"     }
    {   "Zoom 16"               f.exec "setMagnifierZoom 16"    }
} 

# Magnifier window translations.
set magnifierWinTranslations { \
       !Shift <Btn1Motion>:	m_rotateResize()
	      <Btn1Motion>:	m_moveResize()
	 !Shift <Btn1Down>:	m_raise() m_markpos()
		<Btn1Down>:	m_raise() m_markposAdd()
		  <Btn1Up>:	m_redraw() m_destroyNull()
		<Btn2Down>:	m_lower()
		<Btn3Down>:     popup(magzoomMenu)
		  <Btn3Up>:     popdown(magzoomMenu)
	      !Ctrl <Key>m:     call(toggleMagnifier)
	      !Ctrl <Key>p:     call(togglePanner)
	    <Key>BackSpace:	m_deleteDestroy()
	       <Key>Delete:	m_deleteDestroy()
		<KeyPress>:     graphics-input()
		  <Motion>:	track-cursor() call(wcsUpdate,$x,$y)
}


# setMagnifier -- Turn the magnifier on or off.

set magAlreadyOn	0

proc setDynamicMagnifier {state} \
{
    global magAlreadyOn magnifier_enable

    if {$state} {
	set magAlreadyOn $magnifier_enable
    }
    if {$magAlreadyOn} {
	return
    } else {
	setMagnifier $state
    }
}

proc toggleMagnifier args \
{
    global magnifier_enable
    if {$magnifier_enable} {
	setMagnifier 0
    } else {
	setMagnifier 1
    }
}


proc setMagnifier {state} \
{
    global winWidth winHeight frameWidth frameHeight
    global frame magnifier_mapping magnifierWinTranslations magnifierArea
    global magnifier_enable magnifier_height
    global magnifierGeom magnifier_x magnifier_y magnifier_width

    if {$state} {
	if {$magnifier_enable} \
	    return

	# Determine where to place the magnifier.
	set scale  [expr sqrt(double($magnifierArea) / (512 * 512))]
	set scaled_width [expr int(512 * $scale) / 2 * 2 + 1]
	set scaled_height [expr int(512 * $scale) / 2 * 2 + 1]
	set defGeom [format "%sx%s-5+5" $scaled_width $scaled_height]
	send imagewin parseGeometry $magnifierGeom $defGeom x y width height

	# Create the main magnifier window (marker).
	send imagewin createMarker magnifierWin \
	    type		rectangle \
	    createMode		noninteractive \
	    width		[expr $width / 2] \
	    height		[expr $height / 2] \
	    x			[expr $x + $width / 2] \
	    y			[expr $y + $height / 2] \
	    lineColor		8 \
	    highlightColor	8 \
	    translations	$magnifierWinTranslations \
	    visible		true \
	    sensitive		true \
	    activated		true

	# Update the magnifier window position variables so that it comes up
	# in the same place the next time.

	send magnifierWin getRect boundary \
	    magnifier_x magnifier_y magnifier_width magnifier_height
	set magnifierGeom [send imagewin getGeometry \
	    $magnifier_x $magnifier_y $magnifier_width $magnifier_height]

	# Register callbacks.
	send imagewin addCallback magnifierImagewinResized resize
	send resize addCallback magnifierImagewinResized
	send magnifierWin addCallback magnifierMovedMapImage moveResize

	send magnifierWin {
	    addCallback magnifierMoved moveResize;
	    addCallback magnifierDestroy destroy;
	    addCallback magnifierWinConstraint constraint;
	}

	# Map display frame to magnifier window.
	set magnifier_enable 1
	set magnifier_mapping [send imagewin nextMapping]
	send imagewin refreshMapping $magnifier_mapping

	# create cross-hair
	send imagewin createMarker magPointer \
		type                rectangle \
		createMode          noninteractive \
		width               3 \
		height              3 \
		lineWidth           3 \
		lineColor           green \
		highlightcolor      green \
		activated           true \
		visible             true

	# set its position and size
	setMagPointerPosition 
	magnifierMapImage [expr $winWidth / 2] [expr $winHeight / 2]

    } elseif {$magnifier_enable} {
	magnifierDestroy
    }
}


# magnifierDestroy -- Delete the magnifier.

proc magnifierDestroy args \
{
    global magnifier_enable 
    global magnifier_mapping

    if {$magnifier_enable} {
	set magnifier_enable 0

	send imagewin freeMapping $magnifier_mapping
	send imagewin deleteCallback magnifierImagewinResized
	send resize deleteCallback magnifierImagewinResized
	send frame deleteCallback magnifierMapImage

	if [send server queryObject magnifierWin] {
	    send magnifierWin destroy
	}
	if [send server queryObject magPointer] {
	    send magPointer destroy
	}  
    }
}


# magnifierMoved -- Called when the user moves the magnifier window.  We need to
# move the region marker to the new window location and record the new location
# so that the window will come up in the same place if closed and reopened.

proc magnifierMoved {marker event position} \
{
    global winWidth winHeight magnifierGeom frame 
    global magnifier_x magnifier_y magnifier_width magnifier_height

    # Move the region marker to the new location.
    send client getSource raster sx sy snx sny

    # Update the magnifier window position variables so that it comes up
    # in the same place the next time.

    send magnifierWin getRect boundary \
	magnifier_x magnifier_y magnifier_width magnifier_height
    set magnifierGeom [send imagewin getGeometry \
	$magnifier_x $magnifier_y $magnifier_width $magnifier_height]
}


# magnifierWinConstraint -- Called when the magnifier window is moved, resized,
# or rotated. Constrain the magnifier window to remain within the image window;
# rotation is not permitted.

proc magnifierWinConstraint {marker event attributes} \
{
    global winWidth winHeight
    global magnifier_width magnifier_height

    set width $magnifier_width
    set height $magnifier_height
    set constraints [list {}]

    # Check the width and height first as we need these below.
    foreach i $attributes {
	set new [lindex $i 2]
	switch [lindex $i 0] {
	    width	{   set ww [expr $winWidth / 2]
			    if {$new > $ww} {
				lappend constraints "width $ww"
				set width $ww
			    } else {
				set width $new
			    }
			}
	    height	{   set wh [expr $winHeight / 2]
			    if {$new > $wh} {
				lappend constraints "height $wh"
				set height $wh
			    } else {
				set height $new
			    }
			}
	    rotangle	{    lappend constraints "rotangle 0"
			}
	}
    }

    # Constrain X and Y.
    foreach i $attributes {
	set new [lindex $i 2]
	switch [lindex $i 0] {
	    x		{   set pw [expr $width / 2]
			    if {$new < $pw} {
				lappend constraints "x $pw"
			    } elseif {$new > $winWidth - $pw} {
				lappend constraints "x [expr $winWidth - $pw]"
			    }
			}
	    y		{   set ph [expr $height / 2]
			    if {$new < $ph} {
				lappend constraints "y $ph"
			    } elseif {$new > $winHeight - $ph} {
				lappend constraints "y [expr $winHeight - $ph]"
			    }
			}
	}
    }

    return $constraints
}


# magnifierRegionConstraint -- Called when the region marker in the magnifier
# window is moved, resized, or rotated.

proc magnifierRegionConstraint {marker event attributes} \
{
    global winWidth winHeight

    set constraints [list {}]
    send magnifierWin getRect interior p_x p_y p_width p_height

    # Since the magnifier region marker is a box marker x,y and width,height
    # will not both change in the same call, so we can process them all
    # independently.

    foreach i $attributes {
	set new [lindex $i 2]

	switch [lindex $i 0] {
	    x		{   set left [expr $p_x + $rwidth + 1]
			    set right [expr $p_x + $p_width - $rwidth - 1]
			    if {$new < $left} {
				lappend constraints "x $left"
			    } elseif {$new > $right} {
				lappend constraints "x $right"
			    }
			}
	    y		{   set top [expr $p_y + $rheight + 1]
			    set bottom [expr $p_y + $p_height - $rheight - 1]
			    if {$new < $top} {
				lappend constraints "y $top"
			    } elseif {$new > $bottom} {
				lappend constraints "y $bottom"
			    }
			}
	    width	{   set ww [expr $winWidth / 2]
			    if {$new > $ww / 2} {
				lappend constraints "width $ww"
			    }
			}
	    height	{   set wh [expr $winHeight / 2]
			    if {$new > $wh / 2} {
				lappend constraints "height $wh"
			    }
			}
	    rotangle	{    lappend constraints "rotangle 0"
			}
	}
    }

    return $constraints
}


# magnifierImagewinResized -- If the display window is resized make the
# magnifier track the corner.

proc magnifierImagewinResized args \
{
    global magnifier_enable magnifier_mapping magnifier_height
    global magnifierGeom magnifier_x magnifier_y magnifier_width

    if {$magnifier_enable} {
	set old_x $magnifier_x;  set old_width $magnifier_width
	set old_y $magnifier_y;  set old_height $magnifier_height

	# Get new location of magnifier window.
	set defGeom [format "%sx%s+5+5" $magnifier_width $magnifier_height]
	send imagewin parseGeometry $magnifierGeom $defGeom x y width height

	# Reposition the marker.
	send magnifierWin "\
	    markpos; \
	    setAttributes \
		x		[expr $x + $width / 2] \
		y		[expr $y + $height / 2] \
		width		[expr $width / 2] \
		height		[expr $height / 2]; \
	    redraw"

	# Update the magnifier window position variables so that it comes up
	# in the same place the next time.
	send magnifierWin getRect boundary \
	    magnifier_x magnifier_y magnifier_width magnifier_height
	set magnifierGeom [send imagewin getGeometry \
	    $magnifier_x $magnifier_y $magnifier_width $magnifier_height]

	# Make sure the magnifier window is on top.
	send imagewin raiseMapping $magnifier_mapping

	# Refresh the magnifier window if it did not move.
	if {$magnifier_x == $old_x && $magnifier_y == $old_y &&
	    $magnifier_width == $old_width && $magnifier_height == $old_height} {
	    send imagewin refreshMapping $magnifier_mapping
	}
    }
}


# resetMagnifier -- Reinitialize the magnifier.

proc resetMagnifier {param old new} \
{
    global magnifierGeom displayMagnifier

    if {$new == "done"} {
	setMagnifier [true $displayMagnifier]
    } else {
	setMagnifier 0
	if {$new != "startup"} {
	    set magnifierGeom +5+5
	}
    }
}; send initialize addCallback resetMagnifier


set last_mag_x [expr $winWidth / 2]
set last_mag_y [expr $winHeight / 2]

# magnifierMovedMapImage -- Front end to magnifierMapImage, called when 
# magnifier window is moved or resized.

proc magnifierMovedMapImage args \
{
    global last_mag_x last_mag_y

    magnifierMapImage $last_mag_x $last_mag_y
    setMagPointerPosition
}

# The following code was borrowed from the SAOtng GUI by Eric Mandel of SAO
#--------------------------------------------------------------------------

# globals for magnifier
set mag_w 0
set mag_h 0

#
# setMagnifierZoom -- set the zoom factor for the magnifier
#
proc setMagnifierZoom { zoom } \
{
    global mag_w mag_h

    send magnifierWin getRect boundary \
		magnifier_x magnifier_y magnifier_width magnifier_height
    set mag_w [expr int( ( $magnifier_width  + $zoom - 1 ) / $zoom) ]
    set mag_h [expr int( ( $magnifier_height + $zoom - 1 ) / $zoom) ]

    #set mw [expr int (($magnifier_width - 1) / ($mag_w * 2))]
    #if { $mw < 4 } { set mw 4 }
    #set mh [expr int (($magnifier_height - 1) / ($mag_h * 2))]
    #if { $mh < 4 } { set mh 4 }
    #send magPointer "setAttributes width $mw height $mh; redraw"
}

# magnifierMapImage -- Map the of the current display frame centered on the
# pointer into the magnifier window.  Called when the frame changes, the
# pointer moves in the main image window, or (via magnifierMovedMapImage)
# the magnifier window is moved or resized.

set magnifierROP 0

proc magnifierMapImage {x y} \
{
    global magnifier_enable frame
    global last_mag_x last_mag_y
    global magnifier_mapping
    global winWidth winHeight
    global mag_w mag_h
    global magnifierROP

    if {!$magnifier_enable || $frame == 0} \
	return

#    if {abs($last_mag_x-$x) != 1 && abs($last_mag_y-$y) != 1} {
#	# For efficiency we won't compute the sqrt of the distance but instead
#	# just compare the square.  Use a threshold of 6 pixels so when we
#	# narrow in on the ROI the magnifier is still (mostly) correct.
#        set diff  [expr (($last_mag_x-$x) * ($last_mag_x-$x)) + \
#			(($last_mag_y-$y) * ($last_mag_y-$y))]
#        if {$diff > 36} {
#    	    set last_mag_x $x
#    	    set last_mag_y $y
#	    return
#  	}
#    }

    set last_mag_x $x
    set last_mag_y $y

    send magnifierWin getRect interior dx dy dnx dny
    if [send imagewin activeMapping $magnifier_mapping] {
	send imagewin raiseMapping $magnifier_mapping
    }
    # initialize the magnifier zoom factor, if necessary
    if { !$mag_w || !$mag_h } {
	setMagnifierZoom 4
    }

    set sx [expr $x - $mag_w / 2 ]
    set sy [expr $y - $mag_h / 2 ]

    # Constrain the source rectangle within the main image window 
    # (not overlapping an edge or the magnifier window).

    if {$sx > [expr $dx - $mag_w] && $sx < [expr $dx + $dnx] && 
	$sy > [expr $dy - $mag_h] && $sy < [expr $dy + $dny]    } {

	# The source rectangle would overlap the magnifier window; fix that.

	set dist(l) [expr $sx - ($dx - $mag_w)]
	set dist(b) [expr $dy + $dny - $sy]
	set dist(r) [expr $dx + $dnx - $sx]
	set dist(t) [expr $sy - ($dy - $mag_h)]

	#  Put the distances in order.

	foreach j [array names dist] {
	    set alreadyset($j) 0
	}

	for {set i 0} {$i < 4} {incr i} {
	    set candidate ""
	    foreach j [array names dist] {
		if {!$alreadyset($j)} {
		    if {$candidate == ""} {
			set candidate $j
			set minsofar $dist($j)
		    } elseif {$dist($j) < $minsofar} {
			set candidate $j
			set minsofar $dist($j)
		    }
		}
	    }
	    set order($i) $candidate
	    set alreadyset($candidate) 1
	}

	# Try the sides in order, using the first one where there's room.

	for {set i 0} {$i < 4} {incr i} {
	    if {$order($i) == "l"} {
		if {$dx >= $mag_w} {
		    set sx [expr $dx - $mag_w]
		    break
		}
	    } elseif {$order($i) == "b"} {
		if {$winHeight >= $dy + $dny + $mag_h} {
		    set sy [expr $dy + $dny]
		    break
		}
	    } elseif {$order($i) == "r"} {
		if {$winWidth >= $dx + $dnx + $mag_w} {
		    set sx [expr $dx + $dnx]
		    break
		}
	    } elseif {$order($i) == "t"} {
		if {$dy >= $mag_h} {
		    set sy [expr $dy - $mag_h]
		    break
		}
	    }
	}
    }

    # Make sure we don't go beyond an edge of the main window.

    if {$sx < 0} {
	set sx 0
    } else {
	set sxmax [expr $winWidth - $mag_w]
	if {$sx > $sxmax} {
	    set sx $sxmax
	}
    }

    if {$sy < 0} {
	set sy 0
    } else {
	set symax [expr $winHeight - $mag_h]
	if {$sy > $symax} {
	    set sy $symax
	}
    }

    # Map 32 x 32 centered on pointer in main window.
    set err [catch {send imagewin setMapping $magnifier_mapping $magnifierROP \
	0 pixel $sx $sy $mag_w $mag_h \
	0 pixel $dx $dy $dnx   $dny}]
    if { $err != 0 } {
	Print "There was a problem setting up the magnifier ... recovering"
    }
}


# set the position of the magnifier marker in the center of the mag window
proc setMagPointerPosition args \
{
    global magnifier_enable

    if { !$magnifier_enable } \
	return

    send magnifierWin "getAttributes x x y y"
    set xpos [expr $x + 1]
    set ypos [expr $y + 1]

    send magPointer "setAttributes x $xpos y $ypos visible true; redraw"
}


