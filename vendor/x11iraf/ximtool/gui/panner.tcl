
################################################################################
# PANNER.  The full frame mapped into the main image window is displayed at a
# reduced resolution in a marker (known as the panner window) within the main
# image window.  The currently displayed region of the frame is indicated
# using a small marker within the panner window.  This small marker may be
# moved or resized to pan or zoom the image in the main display window.
################################################################################

set panner_x 0
set panner_y 0
set panner_width 0
set panner_height 0
set prm_width 0
set prm_height 0

set panner_enable 0
set panner_pan_enable 0
set panner_mag_enable 0
set panner_region_enable 0
set panner_mapping 0

# Panner window translations.
set pannerWinTranslations { \
       !Shift <Btn1Motion>:	m_rotateResize()
	      <Btn1Motion>:	m_moveResize()
	 !Shift <Btn1Down>:	m_raise() m_markpos()
		<Btn1Down>:	m_raise() m_markposAdd()
		  <Btn1Up>:	m_redraw() m_destroyNull()
		<Btn2Down>:	m_lower()
		  <Btn2Up>:	call(pannerPanXY,$x,$y)
	      !Ctrl <Key>b:     call(prevFrame,$name)
	      !Ctrl <Key>f:     call(nextFrame,$name)
	      !Ctrl <Key>h:     call(move_cursor,-1,0)
	      !Ctrl <Key>j:     call(move_cursor,0,1)
	      !Ctrl <Key>k:     call(move_cursor,0,-1)
	      !Ctrl <Key>l:     call(move_cursor,1,0)
	      !Ctrl <Key>n:     call(normalize)
	      !Ctrl <Key>c:     call(cpZoomAction,centerFrame)
	      !Ctrl <Key>i:     call(cpInvert)
	      !Ctrl <Key>m:     call(toggleMagnifier)
	      !Ctrl <Key>p:     call(togglePanner)
	      !Ctrl <Key>r:     call(cpRegisterFrames)
	      !Ctrl <Key>s:     call(cpMatchFrames)
	       !Alt <Key>1: 	call(cpSetFrame,frame1)
	       !Alt <Key>2: 	call(cpSetFrame,frame2)
	       !Alt <Key>3: 	call(cpSetFrame,frame3)
	       !Alt <Key>4: 	call(cpSetFrame,frame4)
	      !Ctrl <Key>1: 	call(cpZoom,1,1,fixed)
	      !Ctrl <Key>2: 	call(cpZoom,2,2,fixed)
	      !Ctrl <Key>3: 	call(cpZoom,3,3,fixed)
	      !Ctrl <Key>4: 	call(cpZoom,4,4,fixed)
	      !Ctrl <Key>5: 	call(cpZoom,5,5,fixed)
	      !Ctrl <Key>6: 	call(cpZoom,6,6,fixed)
	      !Ctrl <Key>7: 	call(cpZoom,7,7,fixed)
	      !Ctrl <Key>8: 	call(cpZoom,8,8,fixed)
	      !Ctrl <Key>9: 	call(cpZoom,9,9,fixed)
	    <Key>BackSpace:	m_deleteDestroy()
	       <Key>Delete:	m_deleteDestroy()
		<KeyPress>:     graphics-input()
		  <Motion>:	track-cursor() call(wcsUpdate,$x,$y)
}


# setPanner -- Turn the panner on or off.

proc togglePanner args \
{
    global panner_enable

    if {$panner_enable} {
	setPanner 0
    } else {
	setPanner 1
    }
}


proc setPanner {state} \
{
    global winWidth winHeight frameWidth frameHeight
    global frame panner_mapping pannerWinTranslations pannerArea
    global panner_enable panner_region_enable panner_pan_enable
    global pannerGeom panner_x panner_y panner_width panner_height
    global last_compass

    if {$state} {
	if {$panner_enable} \
	    return

	# Determine where to place the panner.
	set scale \
	    [expr sqrt(double($pannerArea) / ($frameWidth * $frameHeight))]
	set scaled_width [expr int($frameWidth * $scale) / 2 * 2 + 1]
	set scaled_height [expr int($frameHeight * $scale) / 2 * 2 + 1]
	set defGeom [format "%sx%s-5+5" $scaled_width $scaled_height]
	send imagewin parseGeometry $pannerGeom $defGeom x y width height

	# Create the main panner window (marker).
	send imagewin createMarker pannerWin \
	    type		rectangle \
	    createMode		noninteractive \
	    width		[expr $width / 2] \
	    height		[expr $height / 2] \
	    x			[expr $x + $width / 2] \
	    y			[expr $y + $height / 2] \
	    lineColor		8 \
	    highlightColor	8 \
	    translations	$pannerWinTranslations \
	    visible		true \
	    sensitive		true \
	    autoRedraw		true \
	    activated		true

	# Update the panner window position variables so that it comes up
	# in the same place the next time.

	send pannerWin getRect boundary \
	    panner_x panner_y panner_width panner_height
	set pannerGeom [send imagewin getGeometry \
	    $panner_x $panner_y $panner_width $panner_height]

	# Register callbacks.
	send frame addCallback pannerMapImage
	send frameRegion addCallback pannerSetRegion
	send imagewin addCallback pannerImagewinResized resize
	send resize addCallback pannerImagewinResized

	send pannerWin {
	    addCallback pannerMapImage moveResize;
	    addCallback pannerMoved moveResize;
	    addCallback pannerDestroy destroy;
	    addCallback pannerWinConstraint constraint;
	}

	# Map display frame to panner window.
	set panner_enable 1
	set panner_region_enable 1
	set panner_mapping [send imagewin nextMapping]
	pannerMapImage init; send imagewin refreshMapping $panner_mapping

	# Redraw the compass if necessary.
	send compass "setSensitive True ; set on $last_compass"
	if { $last_compass } \
	    drawCompass

	# Draw a marker in the panner window outlining displayed region.
	send imagewin createMarker pannerRegionMarker \
	    type		box \
	    createMode		noninteractive \
	    translations	$pannerWinTranslations \
	    lineColor		green \
	    highlightColor	green \
	    sensitive		true

	# Fire up the panner region marker.
	send client getSource raster sx sy snx sny
	pannerSetRegion dummy dummy [concat $frame $sx $sy $snx $sny]
	send pannerRegionMarker "\
	    addCallback pannerPanImage moveResize; \
	    addCallback pannerDestroy destroy; \
	    addCallback pannerRegionConstraint constraint; \
	    setAttributes visible true activated true; \
	    redraw"
	set panner_pan_enable 1

    } elseif {$panner_enable} {
	pannerDestroy
    }
}


# pannerDestroy -- Delete the panner.

proc pannerDestroy args \
{
    global panner_enable panner_region_enable panner_pan_enable
    global panner_mapping

    if {$panner_enable} {
	set panner_enable 0
	set panner_pan_enable 0
	set panner_region_enable 0

	send imagewin freeMapping $panner_mapping
	send imagewin deleteCallback pannerImagewinResized
	send resize deleteCallback pannerImagewinResized
	send frame deleteCallback pannerMapImage
	send frameRegion deleteCallback pannerSetRegion

	if [send server queryObject pannerRegionMarker] {
	    send pannerRegionMarker destroy
	}
	if [send server queryObject pannerWin] {
	    send pannerWin destroy
	}

	# Disable the compass.
	send compass "setSensitive False ; set on False"
    }
}


# pannerMapImage -- Map the current display frame into the panner window.
# Called when the frame changes or the panner window is moved or resized.
# The panner window displays a small dezoomed version of the full frame.

proc pannerMapImage args \
{
    global panner_enable frame
    global panner_mapping

    if {!$panner_enable || $frame == 0} \
	return

    set raster [send client getRaster]
    send pannerWin getRect interior dx dy dnx dny
    send imagewin queryRaster $raster width height

    if [send imagewin activeMapping $panner_mapping] {
	send imagewin raiseMapping $panner_mapping
    }
    send imagewin setMapping $panner_mapping 0 \
	$raster pixel 0 0 $width $height \
	0 pixel $dx $dy $dnx $dny
}


# pannerSetRegion -- Adjust the pannerWin region marker to outline the
# region displayed in the main display window.  This is called in response
# to a frameRegion event when the main display mapping changes, e.g. when
# the frame changes or the user zooms or pans the main window.  The region
# marker is moved and resized to reflect the new view.

proc pannerSetRegion {param old new} \
{
    global panner_enable panner_region_marker
    global panner_region_enable panner_pan_enable
    global frame frameWidth frameHeight prm_width prm_height

    if {!$panner_enable || !$panner_region_enable || $frame == 0} \
	return

    # new: frame sx sy snx sny
    set src_frame [lindex $new 0]
    set sx [lindex $new 1];  set snx [lindex $new 3]
    set sy [lindex $new 2];  set sny [lindex $new 4]

    if {$src_frame != $frame} \
	return

    send pannerWin getRect interior px py pnx pny

    set x [expr ($sx + $snx/2.0) / $frameWidth * $pnx + $px]
    set y [expr ($sy + $sny/2.0) / $frameHeight * $pny + $py]
    set width  [expr ($snx/2.0) / $frameWidth * $pnx + 1]
    set height [expr ($sny/2.0) / $frameHeight * $pny + 1]

    set pan_save $panner_pan_enable;  set panner_pan_enable 0
    set panner_region_enable 0

    send pannerRegionMarker "\
	markpos; \
	setAttributes x $x y $y width $width height $height; \
	redraw; raise"
    send pannerRegionMarker getAttributes width prm_width height prm_height

    set panner_region_enable 1
    set panner_pan_enable $pan_save
}


# pannerPanImage -- Pan or zoom the image in the main image window.  This is
# called when the user moves the region marker within the panner window.

proc pannerPanImage {marker event position} \
{
    global panner_pan_enable
    global winWidth winHeight
    global prm_width prm_height
    global frame auto_reg frameOffsetX frameOffsetY

    if {!$panner_pan_enable} \
	return

    # position: x y width height.
    set new_width [lindex $position 2]
    set new_height [lindex $position 3]

    # region: type raster x y width height.
    set region [send pannerRegionMarker getRegion unmap]
    set x [expr [lindex $region 2] + 1];  set width [lindex $region 4]
    set y [expr [lindex $region 3] + 1];  set height [lindex $region 5]

    set panner_pan_enable 0
    if {$new_width == $prm_width && $new_height == $prm_height} {
	send client pan $x $y
    } else {
	set xscale [expr ($winWidth / 2.0) / $width]
	set yscale [expr ($winHeight / 2.0) / $height]
	if {$auto_reg == 1} {
	    send client zoomAbs $xscale $yscale $x $y \
		$frameOffsetX($frame) $frameOffsetY($frame)
	} else {
	    send client zoom $xscale $yscale $x $y
	}
    }
    set panner_pan_enable 1
}


# pannerPanXY -- Pan to the point X,Y in the panner window coordinate
# system.  Called when the user clicks MB2 in the panner window.

proc pannerPanXY {x y} \
{
    send imagewin unmapPixel $x $y raster rx ry
    send client pan $rx $ry
}


# pannerMoved -- Called when the user moves the panner window.  We need to
# move the region marker to the new window location and record the new location
# so that the window will come up in the same place if closed and reopened.

proc pannerMoved {marker event position} \
{
    global winWidth winHeight
    global frame panner_pan_enable pannerGeom
    global panner_x panner_y panner_width panner_height

    # Move the region marker to the new location.
    set pan_save $panner_pan_enable;  set panner_pan_enable 0
    send client getSource raster sx sy snx sny
    pannerSetRegion dummy dummy [concat $frame $sx $sy $snx $sny]
    set panner_pan_enable $pan_save

    # Update the panner window position variables so that it comes up
    # in the same place the next time.

    send pannerWin getRect boundary \
	panner_x panner_y panner_width panner_height
    set pannerGeom [send imagewin getGeometry \
	$panner_x $panner_y $panner_width $panner_height]

    drawCompass

    send pannerRegionMarker raise
}


# pannerWinConstraint -- Called when the panner window is moved, resized, or
# rotated.  Constrain the panner window to remain within the image window;
# rotation is not permitted.

proc pannerWinConstraint {marker event attributes} \
{
    global winWidth winHeight
    global panner_width panner_height

    set width $panner_width
    set height $panner_height
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


# pannerRegionConstraint -- Called when the region marker in the panner
# window is moved, resized, or rotated.

proc pannerRegionConstraint {marker event attributes} \
{
    global winWidth winHeight
    global frame

    set constraints [list {}]
    send pannerWin getRect interior p_x p_y p_width p_height
    send pannerRegionMarker getAttributes width rwidth height rheight

    # Since the panner region marker is a box marker x,y and width,height
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
    drawCompass

    return $constraints
}


# pannerImagewinResized -- If the display window is resized make the panner
# track the corner.

proc pannerImagewinResized args \
{
    global panner_enable panner_mapping 
    global pannerGeom panner_x panner_y panner_width panner_height
    global frame

    if {$panner_enable} {
	set old_x $panner_x;  set old_width $panner_width
	set old_y $panner_y;  set old_height $panner_height

        eraseCompass

	# Get new location of panner window.
	set defGeom [format "%sx%s-5+5" $panner_width $panner_height]
	send imagewin parseGeometry $pannerGeom $defGeom x y width height

	# Reposition the marker.
	send pannerWin "\
	    markpos; \
	    setAttributes \
		x		[expr $x + $width / 2] \
		y		[expr $y + $height / 2] \
		width		[expr $width / 2] \
		height		[expr $height / 2]; \
	    redraw"

	# Update the panner window position variables so that it comes up
	# in the same place the next time.
	send pannerWin getRect boundary \
	    panner_x panner_y panner_width panner_height
	set pannerGeom [send imagewin getGeometry \
	    $panner_x $panner_y $panner_width $panner_height]

	# Make sure the panner window is on top.
	send imagewin raiseMapping $panner_mapping

	# Refresh the panner window if it did not move.
	if {$panner_x == $old_x && $panner_y == $old_y &&
	    $panner_width == $old_width && $panner_height == $old_height} {
	    send imagewin refreshMapping $panner_mapping
	}

        drawCompass
    }
}


# resetPanner -- Reinitialize the panner.

proc resetPanner {param old new} \
{
    global pannerGeom displayPanner
    if {$new == "done"} {
	setPanner [true $displayPanner]
    } else {
	setPanner 0
	if {$new != "startup"} {
	    set pannerGeom -5+5
	}
    }
}; send initialize addCallback resetPanner


