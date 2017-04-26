

################################################################################
# Compass indicator procedures.
################################################################################

set compassColor	207			;# normally this is yellow
set last_compass	[send compass get on]	;# save compass state

proc drawCompass args \
{
    global ism_enable frame frameCache compassColor Compass Orient
    global panner_x panner_y panner_width panner_height cur_objid
    global redraw_compass last_compass

	
    if {! [send compass get on]} \
	return

    eraseCompass 				;# erase the old compass

    if {! [info exists frameCache($frame)] } {
        set id -1
    } elseif {$cur_objid != [lindex $frameCache($frame) 1]} {
        set id [lindex $frameCache($frame) 1]
    } else {
        set id $cur_objid
    }

    if { [info exists Compass($id)] } {
        set angle     [lindex $Compass($id) 0]
        set north_x   [lindex $Compass($id) 1]
        set north_y   [lindex $Compass($id) 2]
        set east_x    [lindex $Compass($id) 3]
        set east_y    [lindex $Compass($id) 4]
        set transpose [lindex $Compass($id) 5]
        set xlab      [lindex $Compass($id) 6]
        set ylab      [lindex $Compass($id) 7]
    } else {
	set north_x  0.0   	; set north_y  1.0
	set east_x   1.0	; set east_y   0.0
	set xlab       X   	; set ylab       Y
        set angle    0.0   	; set transpose  0
	set Compass($id) { 0.0 1.0 0.0 1.0 0.0 0 X Y }
    }
    set xflip 1
    set yflip 1

    # Adjust the compass for the display orientation (e.g. image sections
    # used to flip an image during display).
    if { [info exists Orient($id)] } {
        set xflip [expr $xflip * [lindex $Orient($id) 1] ]
        set yflip [expr $yflip * [lindex $Orient($id) 2] ]
    }

    # Get the panner center position.
    set pcx [expr ($panner_x + $panner_width  / 2)]
    set pcy [expr ($panner_y + $panner_height / 2)]

    # Setup for the overlay.
    send imagewin getLogRes  sv_xl sv_yl
    send imagewin getPhysRes sv_xp sv_yp
    send imagewin setLogRes $sv_xp $sv_yp
    send imagewin setLineWidth 2

    set xflip [ expr ($xflip * ([send xflipButton get state] ?  -1 : 1))]
    set yflip [ expr ($yflip * ([send yflipButton get state] ?  -1 : 1))]

    # Normalized compass points.  The first row are the axes, second is
    # the pointer head, and last are the X/Y label coords.  Assumes a
    # zero rotation with North up and East left, or standard X/Y orientation.
    set cpoints {
	{-1 0} {0 0} {0 -1}
	{-0.07 -0.85} {0 -1} {0.07 -0.85} {-0.07 -0.85}
	{-1.2 0} {0 -1.2}
    }


    # Get rotation and scale factors.
    set angle [expr "atan2($north_y,$north_x)"]
    set coso  [expr "cos (-$angle)"]
    set sino  [expr "sin (-$angle)"]
    set scale [expr ([min $panner_width $panner_height] * 0.3)]

    # Initialize the drawing points.
    set pts {}

    set cpoints { }
    lappend cpoints [list $east_x  $east_y ]
    lappend cpoints [list 0 0]
    lappend cpoints [list $north_x $north_y]
    foreach p $cpoints {
	# Get the scaled position.
	set sx [expr ($scale * [lindex $p [expr "($transpose > 0) ? 1 : 0"]])]
	set sy [expr ($scale * [lindex $p [expr "($transpose > 0) ? 0 : 1"]])]

	# Translate to the scaled position.
	set rx [expr int($pcx + $sx + 0.5)]
	set ry [expr int($pcy - $sy + 0.5)]

	# Now handle the axis flip.
	set rx [expr (($xflip < 0) ? ($pcx + ($pcx - $rx)) : $rx)]
	set ry [expr (($yflip < 0) ? ($pcy + ($pcy - $ry)) : $ry)]
	lappend pts $rx $ry
    }


    set rpoints { }
    set hpoints { {0.0 0.0} {-0.1 -0.07} {-0.1 0.07} {0.0 0.0} }
    foreach p $hpoints {
	# Break out the position.
	set sx [lindex $p [expr "($transpose > 0) ? 1 : 0"]]
	set sy [lindex $p [expr "($transpose > 0) ? 0 : 1"]]

	# Do the rotation of the head at the origin.
	set rx [expr ($north_x + ($sx * $coso + $sy * $sino))]
	set ry [expr ($north_y - ($sx * $sino + $sy * $coso))]

	lappend rpoints [list $rx $ry]
    }
    foreach p $rpoints {
	# Get the scaled position.
	set sx [expr ($scale * [lindex $p [expr "($transpose > 0) ? 1 : 0"]])]
	set sy [expr ($scale * [lindex $p [expr "($transpose > 0) ? 0 : 1"]])]

	# Translate to the scaled position.
	set rx [expr int($pcx + $sx + 0.5)]
	set ry [expr int($pcy - $sy + 0.5)]

	# Now handle the axis flip.
	set rx [expr (($xflip < 0) ? ($pcx + ($pcx - $rx)) : $rx)]
	set ry [expr (($yflip < 0) ? ($pcy + ($pcy - $ry)) : $ry)]
	lappend pts $rx $ry
    }

    set lpoints { }
    lappend lpoints [list [expr "$east_x-0.2"] $east_y ]
    lappend lpoints [list $north_x [expr "$north_y+0.2"] ]
    foreach p $lpoints {
	# Get the scaled position.
	set sx [expr ($scale * [lindex $p [expr "($transpose > 0) ? 1 : 0"]])]
	set sy [expr ($scale * [lindex $p [expr "($transpose > 0) ? 0 : 1"]])]

	# Translate to the scaled position.
	set rx [expr int($pcx + $sx + 0.5)]
	set ry [expr int($pcy - $sy - 0.5)]

	# Now handle the axis flip.
	set rx [expr (($xflip < 0) ? ($pcx + ($pcx - $rx)) : $rx)]
	set ry [expr (($yflip < 0) ? ($pcy + ($pcy - $ry)) : $ry)]
	lappend pts $rx $ry
    }


    # Draw the compass axes.
    set compassPts [lrange $pts 0 5]
    send imagewin setColorIndex $compassColor
    send imagewin drawPolyline $compassPts

    
    # Draw the compass pointer.
    set head [lrange $pts 6 13]
    send imagewin setFillType solid
    send imagewin drawPolygon $head

    # Draw the labels.
    send imagewin drawAlphaText [lindex $pts 14] [lindex $pts 15] $xlab
    send imagewin drawAlphaText [lindex $pts 16] [lindex $pts 17] $ylab

    send imagewin setLogRes $sv_xl $sv_yl

    set redraw_compass 0

} ; foreach w {xflip yflip} { send $w addCallback drawCompass }


# This is a kludge to redraw the compass after it is erased when displaying
# a new image.  Once the user moves the mouse back into the main window we'll
# do the redraw.
send imagewin addEventHandler drawCompass enterWindowMask



proc createCompassMarker { pts args } \
{
    set cm_points { }
    lappend cm_points [lrange $pts 0 1]
    lappend cm_points [lrange $pts 2 3]
    lappend cm_points [lrange $pts 4 5]

    print [list $cm_points]

}


proc eraseCompass args \
{
    global panner_mapping
    send imagewin refreshMapping $panner_mapping
}

proc toggleCompass { widget type state args } \
{
    global last_compass

    if {$state} {
	drawCompass
	set last_compass True
    } else {
	eraseCompass
	set last_compass False
    }
} ; send compass addCallback toggleCompass


