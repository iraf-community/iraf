

################################################################################
# Cut-plot handling routines.
################################################################################

set doHcut	0
set doVcut	0
set plotSpeed	1 ; send plotSpeed  set on True
set curJump	1 ; send curJump  set on True
set curTrack	1 ; send curTrack set on True

set cutXPos	[expr "$winWidth  / 2"]
set cutYPos	[expr "$winHeight / 2"]
set cutXScale 	1.0
set cutYScale 	1.0



# Change the cursor to the crosshair when in the plot
proc cutCursor { widget event args } \
{
    global doHcut doVcut curTrack

    if {! $curTrack} \
	return

    if { $event == "enterNotify" } {
	send $widget setCursorType ginMode

	# Disable the update of the graph we're in while in the plot window.
	if {$widget == "hcutPlot"} { set doHcut 0 } else { set doVcut 0 }
    } elseif { $event == "leaveNotify" } {
        send $widget setCursorType idle

	# Enable the update of the graph we're leaving.
	if {$widget == "hcutPlot"} { set doHcut 1 } else { set doVcut 1 }
    }
    cutPlotRefresh
}
foreach w {hcutPlot vcutPlot} {
    send $w addEventHandler cutCursor enterWindowMask
    send $w addEventHandler cutCursor leaveWindowMask
}

proc cutPlotRefresh args \
{
    global doHcut doVcut cutXPos cutYPos

    if {$doHcut} {
        send hcutPlot clearScreen
        hcutInit
        send hcutAxes1 redraw ; send hcutAxes2 redraw
        cutPlots $cutXPos $cutYPos
    }
    if {$doVcut} {
        send vcutPlot clearScreen
        vcutInit
        send vcutAxes1 redraw ; send vcutAxes2 redraw
        cutPlots $cutXPos $cutYPos
    }
} ; send imagewin addEventHandler cutPlotRefresh enterWindowMask

proc cutPlotRedraw args \
{
    global doHcut doVcut cutXPos cutYPos

    if {$doHcut} {
        send hcutAxes1 redraw ; send hcutAxes2 redraw
        cutPlots $cutXPos $cutYPos
    }
    if {$doVcut} {
        send vcutAxes1 redraw ; send vcutAxes2 redraw
        cutPlots $cutXPos $cutYPos
    }
}


# Disable the options when we first start up.
#send plotOpts "set height 1 ; set width 1 ; unmap"
foreach w {plotOpts hcutFrame vcutFrame} { send $w unmap }


# Cut-Plot options callback.
proc doPlotOpts { widget type state args } \
{
    global plotSpeed curJump curTrack doHcut doVcut
    global cutXPos cutYPos

    if {$state} { set not 0 } else { set not 1 }

    switch $widget {
    plotSpeed 	 { if {$state} {
    		       send plotAccurate set on 0 ; send plotImgPix set on 0
		   } else {
    		       send plotSpeed set on True ; 
		   }
		   set plotSpeed $not
		 }
    plotAccurate { if {$state} {
    		       send plotImgPix set on 0 ; send plotSpeed set on 0
		   } else {
    		       send plotSpeed set on True ; 
		   }
		   set plotSpeed $not
		 }
    plotImgPix   { if {$state} {
    		       send plotAccurate set on 0 ; send plotSpeed set on 0
		   } else {
    		       send plotSpeed set on True ; 
		   }
		   set plotSpeed $not
		 }
    curJump	 { send curSmooth set on $not    ; set curJump   $state }
    curSmooth	 { send curJump set on $not      ; set curJump   $not 	}
    curTrack	 { set curTrack $state 					}
    }

    # Redraw the plots right away.
    if {$widget == "plotSpeed" || $widget == "plotAccurate"} { 
	cutPlots $cutXPos $cutYPos
    }
}
foreach w { plotSpeed plotAccurate plotImgPix curJump curSmooth curTrack } {
    send $w addCallback doPlotOpts 
}


# Toggle the display of the horizontal or vertical cut plot windows.

proc cutPlotToggle { widget type state args } \
{
    global doHcut doVcut cutXPos cutYPos
    set    debug  0

    set hstate [send hcut get state]
    set vstate [send vcut get state]
    set w [send display get width]
    set h [send display get height]

    if {$debug} { print " " ; print [format "display: %d x %d\n" $w $h] }

    if {$widget == "hcut"} {
	set hfw [expr [send hcutFrame get width] - 4]
	set hpw [send hcutPlot  get width]
        if {$state} {
	    # Enable the plot and resize the main window
	    if {$vstate} { 
		send plotOpts set width 134 
	    }
	    send hcutFrame "set width $hpw ; set height 132; map"
	    send hcutPlot "set width $hfw ; set height 128"
	    send display "set height [ expr ($h + 132) ]; set width $w"
            drawHcutAxes  1
	    setHcutCursor 1
	    if {$vstate} { 
		send plotOpts "set height 134 ; map"
	        vcutInit
	    }
	    hcutInit 				;# Initialize the plot.
        } else {
	    # Disable the plot and resize the main window
	    setHcutCursor 0
            drawHcutAxes  0
	    send hcutPlot clearScreen
	    send plotOpts "unmap; set height 4"
	    send hcutFrame "unmap; set width $hfw; set height 4"
	    send plotOpts "set width 4"
	    send display "set height [ expr ($h - 128) ] ; set width $w"
	    if {$vstate} { 
	        vcutInit
	    }
        }
        set doHcut $state
    } else {
	set vfh  [expr [send vcutFrame get height] - 4]
	set vph  [send vcutPlot  get height]
        if {$state} {
	    # Enable the plot and resize the main window
	    if {$hstate} { 
		send plotOpts set height 134
	    }
	    send vcutFrame "set height $vph ; set width 132 ; map"
	    send vcutPlot "set height $vfh ; set width 128"
	    send display "set height $h; set width [ expr ($w + 132) ]"
            drawVcutAxes  1
	    setVcutCursor 1
	    if {$hstate} { 
		send plotOpts "set height 134 ; set width 134; map"
     	        hcutInit
	    }
     	    vcutInit 				;# Initialize the plot.
        } else {
	    # Disable the plot and resize the main window
	    setVcutCursor 0
            drawVcutAxes  0
	    send vcutPlot clearScreen
	    send plotOpts "unmap; set width 4"
	    send vcutFrame "unmap; set height $vfh; set width 4"
	    send plotOpts "set height 4"
	    send display "set width [ expr ($w - 128) ] ; set height $h"
	    if {$hstate} { 
	        hcutInit
	    }
        }
        set doVcut $state
    }
		
    if {$debug} {
	print [format " hFrame: %d x %d\n" \
		[send hcutFrame get width] [send hcutFrame get height] ]
	print [format "  hPlot: %d x %d\n" \
		[send hcutPlot get width] [send hcutPlot get height] ]
	print [format " vFrame: %d x %d\n" \
		[send vcutFrame get width] [send vcutFrame get height] ]
	print [format "  vPlot: %d x %d\n" \
		[send vcutPlot get width] [send vcutPlot get height] ]
	print [format "state: %d  %d\n" $hstate $vstate]
	print [format "display: %d x %d\n" $w $h]
    }

    cutPlots $cutXPos $cutYPos
} ; foreach w { hcut vcut } { send $w addCallback cutPlotToggle }


# Draw the cut plots.
proc cutPlots { xpos ypos args } \
{
    global doHcut doVcut cutXPos cutYPos

    catch {
        if {$doHcut} { plotHcut $xpos $ypos }
        if {$doVcut} { plotVcut $xpos $ypos }
    }

    set cutXPos $xpos  ;  set cutYPos $ypos
}


################################################################################
# Horizontal Cut-Plot Routines
################################################################################

set hcutVec	{}

# Initiailize the horizontal cut-plot
proc hcutInit args \
{
    global logz cutXScale winWidth cutXPos cutYPos

    # Just get some dummy pixels, we only want the z1/z2 values so we can 
    # initialize the labels.
    set xp   [expr [send imagewin get width] / 2 ]
    set yp   [expr [send imagewin get height] / 2 ]
    set pix  [send client getPixels $xp $yp 2 2 ]
    set z1   [lindex $pix 0]
    set z2   [lindex $pix 1]

    send hcutPlot getPhysRes  xr  yr
    send hcutPlot setLogRes  $xr $yr

    set logx    [send imagewin get width]
    set logz    [expr ($z2 - $z1)]
    set cutXScale [expr ($xr * 1.0) / ($logx * 1.0)]

    # Initialize the labels.
    send vcutPlot "setColorIndex 6"
    drawHcutLabels $z1 $z2
}

# Draw the horizontal cut-plot.
proc plotHcut { xpos ypos } \
{
    global doHcut cutXScale
    global hcutVec cutXPos plotSpeed


    if { ($xpos == 0 && $ypos == 0) || ! $doHcut } \
	return

    # Do the horizontal cut plot.
    set width [send imagewin get width]
    if {$plotSpeed} {
        set pix  [send client getPixels 0 $ypos $width 1 2 5 $cutXScale]
    } else {
        set pix  [send client getPixels 0 $ypos $width 1 2 1 $cutXScale]
    }
    set z1   [lindex $pix 0]
    set z2   [lindex $pix 1]
    set vec  [lrange $pix 2 end]

    # Erase the last plot rather than clear the screen and redraw 
    # the new vector.
    send hcutPlot setColorIndex background
    send hcutPlot drawPolyline $hcutVec
    send hcutPlot setColorIndex foreground
    send hcutPlot drawPolyline $vec
    set  hcutVec $vec 			;# save for later erasure

    # Mark the cursor position.
    drawHcutIndicator $xpos

    # Minimize the screen refreshes to speed things up.
    if { [expr "$ypos % 3"] == 0} {
	catch {
            drawHcutLabels $z1 $z2	;# redraw the labels
	}
        send hcutAxes1 redraw		;# redraw the axes markers
        send hcutAxes2 redraw
    }
}


# Create markers to indicate axes on the horizontal cut-plot.
proc drawHcutAxes { state } \
{
    if {$state} {
        send hcutPlot createMarker hcutAxes1 \
            type            box \
            createMode      noninteractive \
            lineColor       gray60 \
            lineStyle       0 \
            x               1 \
            y               60 \
            height          30 \
	    width           4096 \
            activated       True \
            visible         True \
            sensitive       False
        send hcutPlot createMarker hcutAxes2 \
            type            box \
            createMode      noninteractive \
            lineColor       gray60 \
            lineStyle       0 \
            x               1 \
            y               1 \
            height          60 \
	    width           4096 \
            activated       True \
            visible         True \
            sensitive       False
    } else {
	send hcutAxes1 destroy ; send hcutAxes2 destroy
    }
}

# Create a marker to be used as the cursor indicator.
proc setHcutCursor { state } \
{
    if {$state} {
	set pts { {252 10} {260 10} {256 1} }

        send hcutPlot createMarker hcutCursor \
            type          polygon \
            createMode    noninteractive \
            lineColor     black \
            fill     	  True \
            fillColor     yellow \
            x             256 \
            y             12 \
            width         8 \
            height        10 \
            knotSize      0 \
            activated     True \
            visible       False \
            sensitive     False
        send hcutCursor setVertices $pts
        send hcutCursor set visible True

    } else {
	send hcutCursor destroy
    }
}

# Label the axes on the horizontal cut plot.
proc drawHcutLabels { z1 z2 } \
{
    set mid  [expr "($z2-$z1)/2.0+$z1"]
    set low [expr "($mid-$z1)/2.0+$z1"]
    set high  [expr "($z2-$mid)/2.0+$mid"]

    send hcutPlot "setColorIndex 6 ; \
	 drawAlphaText 2 10  [format "%.1f" $z2] ; \
         drawAlphaText 2 34  [format "%.1f" $high] ; \
         drawAlphaText 2 64  [format "%.1f" $mid] ; \
         drawAlphaText 2 94  [format "%.1f" $low] ; \
         drawAlphaText 2 120 [format "%.1f" $z1]"
}

# Draw the cursor position indicator on the horizontal cut plot.
proc drawHcutIndicator { xpos } \
{
    global cutXScale cutXPos

    send hcutCursor move [expr ($xpos * $cutXScale)] 12
    set cutXPos $xpos
}

# Track the cursor while in the cut-graph window.
proc hcutWCSUpdate { x y args } \
{
    global cutYPos curTrack
    if {$curTrack} { 
        wcsUpdate $x $cutYPos 
    }
    drawHcutIndicator $x
}




################################################################################
# Vertical Cut-Plot Routines
################################################################################

set vcutVec	{}

# Initiailize the vertical cut-plot
proc vcutInit args \
{
    global cutYScale winWidth cutXPos cutYPos

    # Just get some dummy pixels, we only want the z1/z2 values so we can 
    # initialize the labels.
    set xp   [expr [send imagewin get width] / 2 ]
    set yp   [expr [send imagewin get height] / 2 ]
    set pix  [send client getPixels $xp $yp 2 2 ]
    set z1   [lindex $pix 0]
    set z2   [lindex $pix 1]

    send vcutPlot getPhysRes  xr  yr
    send vcutPlot setLogRes  $xr $yr

    set logy    [send imagewin get height]
    set logz    [expr ($z2 - $z1)]
    set cutYScale [expr ($yr * 1.0) / ($logy * 1.0)]

    # Initialize the labels.
    send vcutPlot "setColorIndex 6; reset"
    drawVcutLabels $z1 $z2
}

# Draw the horizontal cut-plot.
proc plotVcut { xpos ypos } \
{
    global doVcut cutYScale
    global vcutVec cutXPos plotSpeed


    if { ($xpos == 0 && $ypos == 0) || ! $doVcut } \
	return

    # Do the vertical cut plot.
    set height [send imagewin get height]
    if {$plotSpeed} {
        set pix  [send client getPixels $xpos 0 1 $height 3 5 $cutYScale]
    } else {
        set pix  [send client getPixels $xpos 0 1 $height 3 1 $cutYScale]
    }
    set z1   [lindex $pix 0]
    set z2   [lindex $pix 1]
    set vec  [lrange $pix 2 end]

    # Draw the vector.
    send vcutPlot setColorIndex background
    send vcutPlot drawPolyline $vcutVec
    send vcutPlot setColorIndex foreground
    send vcutPlot drawPolyline $vec
    set  vcutVec $vec 			;# save for later erasure

    # Mark the cursor position.
    drawVcutIndicator $ypos

    # Minimize the screen refreshes to speed things up.
    if { [expr "$xpos % 3"] == 0} {
	catch {
            drawVcutLabels $z1 $z2	;# redraw the labels
	}
        send vcutAxes1 redraw		;# redraw the axes markers
        send vcutAxes2 redraw
    }
}

# Erase the last plot rather than clear the screen and redraw it all. The
# erase is done by redrawing the last vector in the the background color.
proc eraseOldVcut args \
{
    global cutYPos vcutVec

    send vcutPlot setColorIndex background
    send vcutPlot drawPolyline $vcutVec
    send vcutPlot setColorIndex foreground
}

# Draw the horizontal cut-plot.
# Create markers to indicate axes on the vertical cut-plot.
proc drawVcutAxes { state } \
{
    if {$state} {
        send vcutPlot createMarker vcutAxes1 \
            type            box \
            createMode      noninteractive \
            lineColor       gray60 \
            lineStyle       0 \
            x               60 \
            y               1 \
            height          4096 \
            width           30 \
            activated       True \
            visible         True \
            sensitive       False
        send vcutPlot createMarker vcutAxes2 \
            type            box \
            createMode      noninteractive \
            lineColor       gray60 \
            lineStyle       0 \
            x               1 \
            y               1 \
            width           60 \
            height          4096 \
            activated       True \
            visible         True \
            sensitive       False
    } else {
	send vcutAxes1 destroy ; send vcutAxes2 destroy
    }
}

# Create a marker to be used as the cursor indicator.
proc setVcutCursor { state } \
{
    if {$state} {
	set pts { {10 252} {10 260} {1 256} }

        send vcutPlot createMarker vcutCursor \
            type          polygon \
            createMode    noninteractive \
            lineColor     black \
            fill     	  True \
            fillColor     yellow \
            x             12 \
            y             256 \
            width         10 \
            height        8 \
            knotSize      0 \
            activated     True \
            visible       False \
            sensitive     False
        send vcutCursor setVertices $pts
        send vcutCursor set visible True

    } else {
	send vcutCursor destroy
    }
}

# Label the axes on the vertical cut plot.
proc drawVcutLabels { z1 z2 } \
{
    set mid  [expr "($z2-$z1)/2.0+$z1"]
    set low  [expr "($mid-$z1)/2.0+$z1"]
    set high [expr "($z2-$mid)/2.0+$mid"]

    # Initialize the label strings and positions.
    set labels {}
    foreach i [list $z2 $high $mid $low $z1] {
	lappend labels  [ format "%.1f" $i ]
    }
    set xposns { 2 28 58 88 112 }

    send vcutPlot "setColorIndex 6"

    # Draw each label vertically down the position since we can't rotate
    # the text.
    set xp  0
    foreach lab $labels {
	set chars [split $lab {} ]
        set yp  12
	set xpos [lindex $xposns $xp]
        foreach ch $chars {
	    if {$ch == "."} { incr yp -4 }
    	    send vcutPlot drawAlphaText $xpos $yp $ch
	    incr yp 10
        }
	incr xp
    }
}


# Draw the cursor position indicator on the horizontal cut plot.
proc drawVcutIndicator { ypos } \
{
    global cutYScale cutYPos

    send vcutCursor move 12 [expr ($ypos * $cutYScale)]
    set cutYPos $ypos
}


# Track the cursor while in the cut-graph window.
proc vcutWCSUpdate { x y args } \
{
    global cutXPos curTrack
    if {$curTrack} { 
        wcsUpdate $cutXPos $y 
    }
    drawVcutIndicator $y
}



################################################################################
# UTILITY ROUTINES
################################################################################


# TICSTEP -- Utility routine to compute nice ticmark steps in plots.
# [ NOT CURRENTLY USED. ]

proc ticstep { range nsteps } \
{
    set t2 0.301029996
    set t5 0.698970004
    set df [ expr "$range / double($nsteps + 1)" ]
    if {$df > 0.0} {
        set p1 [ expr "log10(double($df))" ]
    } else {
        set p1 [ expr "log10(double(-$df))" ]
    }
    set p2 [ expr "int($p1)" ]
    set p3 [ expr "$p1 - $p2" ]

    if { $p3 < 0.0 } {
        set p3 [ expr "$p2 + 1.0" ]
        set p2 [ expr "$p2 - 1.0" ]
    }

    if { $p3 < 1.0e-10 } {
       set ticstep [ expr "pow(double(10.0),double($p2))" ]
    } elseif { $p3 > 0. &&  $p3 <=  $t2 } {
       set ticstep [ expr "pow(double(10.0),double($p2 + $t2))" ]
    } elseif { $p3 >  $t2 &&  $p3 <=  $t5 } {
       set ticstep [ expr "pow(double(10.0),double($p2 + $t5))" ]
    } elseif { $p3 >  $t5 &&  $p3 <= 1.0 } {
       set ticstep [ expr "pow(double(10.0),double($p2 + 1.))" ]
    } else {
       set ticstep $df
    }

    set logtic  [ expr "int(log10($ticstep)) - 1" ]
    set scale   [ expr "pow(double(10.0),double($logtic))" ]
    set ticstep [ expr "int( ($ticstep / $scale) * $scale)" ]

    if {$ticstep < 0.1} { set ticstep 0.10 }

    return $ticstep
}


