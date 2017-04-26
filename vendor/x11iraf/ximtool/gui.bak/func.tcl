
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


