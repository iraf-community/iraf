
################################################################################
# CURSOR CENTEROID AND AUTO-REGISTER FUNCTIONS.
################################################################################

# Set the centroiding box size.
set ctid	0
set cid		0

proc cbxDestroy args \
{
    global centerBoxSize cid ctid
    catch { 
	if {$ctid != 0} {
	    send cbm$cid destroy 
	    deleteTimedCallback $ctid
	    set ctid 0
	}
    }
}

proc setCtrBoxSize { x y delta args } \
{
    global centerBoxSize cid ctid
    global cpXscale cpYscale

    incr centerBoxSize $delta
    if {$centerBoxSize <= 1} { set centerBoxSize 1 }

    # Kill off any old markers before drawing the new one.
    catch { 
	if {$ctid != 0} {
	    send cbm$cid destroy 
	    deleteTimedCallback $ctid
	    set ctid 0
	}
    }

    # create a transient marker indicating the centering box and post a
    # callback to delete it in about a second.
    incr cid
    send imagewin createMarker cbm$cid \
	type            box \
	createMode      noninteractive \
	lineColor       red \
	x               $x \
	y               $y \
	width           [expr $cpXscale * $centerBoxSize] \
	height          [expr $cpXscale * $centerBoxSize] \
	activated       True \
	visible         True \
	sensitive       False

    set ctid [ postTimedCallback cbxDestroy 500]
}


# Box size is half-width of the marker size.  Value is the slider value.
set focusBoxSize	$winWidth			
set focusValue	       100.0
set fid			 0
set ftid		 0
set moving		 0

proc setFocusBoxSize { sz args } \
{
    global focusBoxSize fid ftid focusValue moving
    global winWidth winHeight


    if { $moving == 0 } {
        return done
    }

    if { $winWidth < $winHeight } {
	set max [expr $winWidth / 2 - 64]
    } else {
	set max [expr $winHeight / 2 - 64]
    }
    set focusBoxSize [expr 64 + ($sz * $max) - 1]
    #send client setOption cmfocus [expr ($focusBoxSize / 2)]
    send client setOption cmfocus $focusBoxSize

    # Destroy any existing markers.
    catch { 
        if {$ftid != 0} {
            send fm$fid destroy 
            set ftid 0
	}
    }

    # create a transient marker indicating the centering box and post a
    # callback to delete it in about a second.
    incr fid
    send imagewin createMarker fm$fid \
	type            box \
	createMode      noninteractive \
	lineColor       green \
	lineWidth       4 \
	x               [expr $winWidth / 2] \
	y               [expr $winHeight / 2] \
	width           $focusBoxSize \
	height          $focusBoxSize \
	activated       True \
	visible         True \
	sensitive       False

    set ftid [ postTimedCallback fbxDestroy 500]
    set moving  0
}

proc fbxDestroy args \
{
    global fid ftid moving
    catch { 
        if {$ftid != 0} {
            send fm$fid destroy 
            set ftid 0
        }
    }
}

proc setFocusSize { widget cbtype x y } \
{
    global focusValue ftid moving

    # Only update once we've stopped the movement.
    if { $x == $focusValue && $moving == 1 } {
	set ftid [ postWorkProc setFocusBoxSize $x ]
    } else {
        set moving  1
    }
    set focusValue $x
} ; send focusSlider addCallback setFocusSize scroll


# Compute a centroid offset for the current position to peak-up on the
# feature.

proc centroid { x y type args } \
{
    global centerBoxSize
    global cpXscale cpYscale

    # Convert to image coords.
    set sz [expr "int ($centerBoxSize * $cpXscale)"]

    # Get the centroid position.
    if {$type != "min"} {
        if {[send peakupButton get on]} {
            set center [ send client centroid $x $y $sz ]
        } else {
            set center [ send client centroid $x $y $sz max ]
        }
    } else {
        set center [ send client centroid $x $y $sz min ]
    }

    # Now reposition the cursor.
    set xoff [lindex $center 0 ]
    set yoff [lindex $center 1 ]
    move_cursor $xoff $yoff
}


# Set the auto-register center offset position
proc offset { x y args } \
{
    global frame blinkFrames auto_reg
    global frameCenterX frameCenterY
    global frameOffsetX frameOffsetY

    # No-op of auto-register isn't on.
    if { $auto_reg == 0 } {
	Wexec client "Auto-Register is not enabled!"
	return
    }

    # If we're not in the blink frames list ignore the request.
    if { [string first $frame $blinkFrames] == -1 } {
	Wexec client "Frame not in current\nregister list."
	return
    }

    set rx $x;  set ry $y
    set raster 0

    # Convert raw screen coordinates to frame buffer raster coordinates.
    send imagewin unmapPixel $x $y raster rx ry

    # Select a pixel.
    set xoff  [expr "int ($rx) - $frameCenterX($frame)" ]
    set yoff  [expr "int ($ry) - $frameCenterY($frame)" ]

    set frameOffsetX($frame)  $xoff
    set frameOffsetY($frame)  $yoff

    # Adjust the display.
    send client setOffset $xoff $yoff
}


