

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
        set angle  [lindex $Compass($id) 0]
        set n_x    [lindex $Compass($id) 1]
        set n_y    [lindex $Compass($id) 2]
        set e_x    [lindex $Compass($id) 3]
        set e_y    [lindex $Compass($id) 4]
        set trans  [lindex $Compass($id) 5]
        set xlab   [lindex $Compass($id) 6]
        set ylab   [lindex $Compass($id) 7]
    } else {
	set n_x    0.0   ; set n_y    1.0
	set e_x    1.0	 ; set e_y    0.0
	set xlab     X   ; set ylab     Y
        set angle  0.0   ; set trans    0
	set Compass($id) { 0.0 0.0 1.0 1.0 0.0 0 X Y }
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
    set angle [expr "atan2($n_y,$n_x)"]
    set scale [expr ([min $panner_width $panner_height] * 0.3)]

    # Initialize the graphics.
    send imagewin setColorIndex $compassColor
    send imagewin setFillType solid

    # Now draw the parts of the compass.
    drawCompassAxes   $n_x $n_y $e_x $e_y $trans $xflip $yflip $scale \
	$pcx $pcy
    drawCompassLabels $n_x $n_y $e_x $e_y $trans $xflip $yflip $scale\
	 $pcx $pcy $xlab $ylab
    drawCompassPtr   $n_x $n_y $e_x $e_y $trans $xflip $yflip $scale \
	$pcx $pcy $angle

    # Reset the logical resolution of the window.
    send imagewin setLogRes $sv_xl $sv_yl
    set redraw_compass 0

} ; foreach w {xflip yflip} { send $w addCallback drawCompass }


proc drawCompassAxes {n_x n_y e_x e_y trans xflip yflip scale pcx pcy} \
{
    set cpoints { }
    lappend cpoints [list $e_x  $e_y ]
    lappend cpoints [list 0 0]
    lappend cpoints [list $n_x $n_y]
    foreach p $cpoints {
	# Get the scaled position.
	set sx [expr ($scale * [lindex $p [expr "($trans > 0) ? 1 : 0"]])]
	set sy [expr ($scale * [lindex $p [expr "($trans > 0) ? 0 : 1"]])]

	# Translate to the scaled position.
	set rx [expr int($pcx + $sx + 0.5)]
	set ry [expr int($pcy - $sy + 0.5)]

	lappend pts $rx $ry
    }

    # Draw the compass axes.
    send imagewin drawPolyline $pts
}

proc drawCompassLabels {n_x n_y e_x e_y trans xflip yflip scale pcx pcy xlab ylab} \
{

    set pts { }
    set lpoints { }
   
    set xo [expr (0.2 * [expr "($xflip > 0) ? -1 : 1"])]
    set yo [expr (0.2 * [expr "($yflip > 0) ? 1 : -1"])]

    lappend lpoints [list [expr "$e_x+$xo"] $e_y ]
    lappend lpoints [list $n_x [expr "$n_y+$yo"] ]
    foreach p $lpoints {
	# Get the scaled position.
	set sx [expr ($scale * [lindex $p [expr "($trans > 0) ? 1 : 0"]])]
	set sy [expr ($scale * [lindex $p [expr "($trans > 0) ? 0 : 1"]])]

	# Translate to the scaled position.
	set rx [expr int($pcx + $sx + 0.5)]
	set ry [expr int($pcy - $sy - 0.5)]

	lappend pts $rx $ry
    }

    # Draw the labels.
    send imagewin drawAlphaText [lindex $pts 0] [lindex $pts 1] $xlab
    send imagewin drawAlphaText [lindex $pts 2] [lindex $pts 3] $ylab
}

proc drawCompassPtr {n_x n_y e_x e_y trans xflip yflip scale pcx pcy angle} \
{

    set coso  [expr "cos (-$angle)"]
    set sino  [expr "sin (-$angle)"]

    # Initialize the drawing points.
    set pts {}
    set hpoints { {0.0 0.0} {-0.1 -0.07} {-0.1 0.07} {0.0 0.0} }
    foreach p $hpoints {
	# Break out the position.
	set sx [lindex $p [expr "($trans > 0) ? 1 : 0"]]
	set sy [lindex $p [expr "($trans > 0) ? 0 : 1"]]

	# Do the rotation of the head at the origin.
	set rx [expr ($n_x + ($sx * $coso + $sy * $sino))]
	set ry [expr ($n_y - ($sx * $sino + $sy * $coso))]

	# Get the scaled position.
	set sx [expr ($scale * $rx)]
	set sy [expr ($scale * $ry)]

	# Translate to the scaled position.
	set rx [expr int($pcx + $sx + 0.5)]
	set ry [expr int($pcy - $sy + 0.5)]

	lappend pts $rx $ry
    }

    # Draw the compass pointer.
    send imagewin drawPolygon $pts
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


