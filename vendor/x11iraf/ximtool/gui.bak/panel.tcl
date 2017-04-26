
################################################################################
# MAIN CONTROL PANEL
################################################################################


# Global control panel buttons.
# -------------------------------
proc cpInitialize args \
{
    send imagewin setCursorType busy
    send client initialize
    send imagewin setCursorType idle
}
send initializeButton 	addCallback cpInitialize
send normalizeButton 	addCallback normalize

# Temporarily deactivate some buttons.
send optimizeButton setSensitive false

#-----------------------------------------------------------------------------

foreach i $frames {set saveView($i) "1 1"}

send prevFrame 		set bitmap larrow
send nextFrame 		set bitmap rarrow
send contrastLabel	set bitmap contrast
send brightnessLabel	set bitmap brightness
send contrastSlider	resizeThumb 0.1 1.0
send brightnessSlider	resizeThumb 0.1 1.0


# panel -- Toggle control panel display.
proc panel args \
{
    global panel_up
    if {$panel_up} {
	send panelShell unmap
	set panel_up 0
    } else {
	send panelShell map
	set panel_up 1
    }
}

proc pbTracePanel {name element op} \
{
    catch {
        upvar $name panel_up
        send controlButton set state $panel_up
    }
} ; trace variable panel_up w pbTracePanel


# resetPanel -- Calling during startup or in an initialize, to reset things.
proc resetPanel {param old new} \
{
    global frame nframes frames
    global displayPanner displayMagnifier displayCoords
    global blinkFrames warnings peakCentroid

    switch $new {
    startup	{
		}
    restart	{   foreach i $frames {
			send frame$frame set on 0
		    }

		    # Initialize to hide the extra frames.
		    send frlistBox set width 30
		}
    done	{   if {$frame} {
			send frame$frame set on 1
		    }

		    cpResetBlink
		    set button 1
		    foreach i {1 2 3 4} {
			send blinkFrame$button set label $i
			incr button
		    }
		    for {set i 1} {$i <= 16} {incr i} {
			send brFrame$i set label $i
		    }

		    cpResetEnhance
		    send pannerButton set on [true $displayPanner]
		    send magnifierButton set on [true $displayMagnifier]
		    send coordsBoxButton set on [true $displayCoords]
		    send warningsButton set on $warnings
		    send peakupButton set on [true $peakCentroid]
		}
    }
}; send initialize addCallback resetPanel



#  Control Panel Tabs widget procedures.
#----------------------------------------

set cpTabs   { display_panel print_panel load_panel save_panel \
	       info_panel wcs_panel tile_panel}
set tabTop   "display_panel"


# Resize the control panel depending on the Tab selected.
proc cpResizeCB { widget event a b c d e args } \
{
    global tabTop fileList

    # Handle only the first exposure event generated.
    if { $a == 0 && $b == 0 && $c == 0 && $d == 0 && $e == 0 } {
	set tabTop $widget

	switch $widget {
	display_panel   { send panel set height 595 }
	print_panel     { send panel set height 545 }
	load_panel      { send panel set height 485 
			  if { [send browseHdrs get on] } {
			      send imageList setList $fileList resize
			  } else {
        		      send client setLoadOption rescan
			  }
        		}
	save_panel      { send panel set height 325 }
	info_panel      { send panel set height 380 }
	tile_panel      { send panel set height 405 }
	wcs_panel       { setCoordPanelHeight	    }
	}
    }
} ; foreach w $cpTabs {  send $w addEventHandler cpResizeCB exposureMask }


proc panelDismiss args \
{
    global panel_up
    send panelShell unmap
    set panel_up 0
} ; send panelClose addCallback panelDismiss


proc panelTabToggle { panel args } \
{
    global tabTop panel_up

    if {$tabTop == $panel && $panel_up} {
	send panelShell unmap
	send $widget set state 0
	set panel_up 0
	return
    }

    # Special cases for each panel.
    if {$panel == "load_panel"} {
        send client setLoadOption rescan
    }

    send panelTabs setTop $panel
    set tabTop $panel

    # Now fire it up if it's not already open.
    if {$panel_up == 0} {
	send panelShell map
	set panel_up 1
    }
}

proc displayPanel args  { panelTabToggle display_panel }
proc infoPanel args     { panelTabToggle info_panel    }
proc loadPanel args     { panelTabToggle load_panel    }
proc savePanel args     { panelTabToggle save_panel    }
proc printPanel args    { panelTabToggle print_panel   }
proc tilePanel args     { panelTabToggle tile_panel    }
proc wcsPanel args      { panelTabToggle wcs_panel     }




# Frame selection.
# -------------------------------
proc cpSetFrame {widget args} \
{
    send $widget set on 0
    send client setFrame [send $widget get label]
}

proc cpFrameChanged {param old new} \
{
    global frameCache

    if {$old > 0} {
	send frame$old set on 0
    }
    if {$new > 0} {
	send frame$new set on 1
    }

    # The first time we request frame 5 or higher reset 
    # the extra frame buttons on the control panel to make
    # them visible.
    if {$new >= 5} {
	send frlistBox set width 49
    }

    # Update the header panel object list.
    catch {
	# Only update when the header panel is open.
        if {[info exists frameCache($new)] && [send imageHeader get on]} {
            setHdrObjMenu $new
            getHeader [lindex $frameCache($new) 0] [lindex $frameCache($new) 1]
        }
    }
}

send prevFrame addCallback prevFrame
send nextFrame addCallback nextFrame
send frame addCallback cpFrameChanged
for {set i 1} {$i <= 16} {incr i} {send frame$i addCallback cpSetFrame}

# Initialize to hide the extra frames.
send frlistBox  set width 30

proc blinkPanelCB { widget type state args } \
{
    if {$state} {
	send blink_panel map
    } else {
	send blink_panel unmap
    }
} ; send blinkPanel addCallback blinkPanelCB

proc blinkPanelClose args \
{
    send blink_panel unmap
    send blinkPanel set state 0
} ; send brClose addCallback blinkPanelClose



# Frame buttons.
proc cpFrameAction {widget args} \
{
    global frameZoomX frameZoomY frame
    switch $widget {
    aspect	{   set xmag $frameZoomX($frame)
		    set ymag $frameZoomY($frame)
		    set zoom [expr round (($xmag + $ymag) / 2.0)]
		    cpZoom $zoom $zoom fixed
		}
    flipX	{   send client flip x }
    flipY	{   send client flip y }
    flipXY	{   send client flip x y }
    clearFrame	{   clearFrame }
    fitFrame	{   fitFrame }
    }
}
foreach widget {aspect flipX flipY flipXY clearFrame fitFrame} {
    send $widget addCallback cpFrameAction
}

# clearFrame -- Clear the current display frame.
proc clearFrame	args \
{
    global warnings
    if {$warnings} {
	Wexec client \
	    "Clearing the frame will destroy\n\
	    all data in the frame" \
	    clearFrame
    } else {
	send client clearFrame
    }
}

# fitFrame -- Resize the display window to fit the frame buffer.
proc fitFrame args \
{
    global frameWidth frameHeight winWidth winHeight
    set dw [expr [send display get width] - $winWidth]
    set dh [expr [send display get height] - $winHeight]
    send display "resize [expr $frameWidth + $dw] [expr $frameHeight + $dh]"
}

proc initFitFrame { param old new } \
{
    if { [lindex $new 0] == "resize"} {
	fitFrame
    }
    pannerMapImage init
}; send frameFit addCallback initFitFrame



# Zoom and pan buttons.
# -------------------------------
proc cpZoomAction {widget args} \
{
    global frameWidth frameHeight

    switch $widget {
    x1		{   cpZoom 1 1 fixed }
    z2		{   cpZoom 2 2 fixed }
    z3		{   cpZoom 3 3 fixed }
    z4		{   cpZoom 4 4 fixed }
    z5		{   cpZoom 5 5 fixed }
    z8		{   cpZoom 8 8 fixed }

    d2		{   cpZoom [expr 1.0/2] [expr 1.0/2] fixed }
    d3		{   cpZoom [expr 1.0/3] [expr 1.0/3] fixed }
    d4		{   cpZoom [expr 1.0/4] [expr 1.0/4] fixed }
    d5		{   cpZoom [expr 1.0/5] [expr 1.0/5] fixed }
    d8		{   cpZoom [expr 1.0/8] [expr 1.0/8] fixed }

    zoomIn	{   cpZoom 2.0 2.0 relative }
    zoomOut	{   cpZoom 0.5 0.5 relative }

    centerFrame	{   send client pan \
			[expr $frameWidth/2.0] \
			[expr $frameHeight/2.0]
		}
    toggleZoom	{   toggleZoom }
    }
}

proc cpZoom {zoom_x zoom_y mode} \
{
    global frameZoomX frameZoomY zoomindex frame
    global frameOffsetX frameOffsetY frameCenterX frameCenterY

    if {$mode == "fixed"} {
	#send client zoom $zoom_x $zoom_y
	send client zoomAbs $zoom_x $zoom_y \
	    $frameCenterX($frame) $frameCenterY($frame) \
	    $frameOffsetX($frame) $frameOffsetY($frame)
    } else {
	#send client zoom \
	#    [expr $frameZoomX($frame) * $zoom_x] \
	#    [expr $frameZoomY($frame) * $zoom_y]
	send client zoomAbs \
	    [expr $frameZoomX($frame) * $zoom_x] \
	    [expr $frameZoomY($frame) * $zoom_y] \
	    $frameCenterX($frame) $frameCenterY($frame) \
	    $frameOffsetX($frame) $frameOffsetY($frame)
    }

    set zoomindex($frame) 0
}

proc toggleZoom	args \
{
    global frameZoomX frameZoomY frameCenterX frameCenterY
    global frameWidth frameHeight saveView frame

    if {$frameZoomX($frame) != 1 && $frameZoomY($frame) != 1} {
	set saveView($frame) \
	    "$frameZoomX($frame) $frameZoomY($frame) \
	     $frameCenterX($frame) $frameCenterY($frame)"
	send client zoom 1 1 \
	    [expr $frameWidth/2.0] \
	    [expr $frameHeight/2.0]
    } else {
	send client zoom $saveView($frame)
    }
}

foreach widget { toggleZoom centerFrame zoomIn zoomOut \
	x1  z2 z3 z4 z5 z8  d2 d3 d4 d5 d8  } {
    send $widget addCallback cpZoomAction
}

# Frame data display.
# -------------------------------
set cpFrame  0
set cpXcen   0
set cpYcen   0
set cpXmag   0
set cpYmag   0
set cpXscale 0
set cpYscale 0
set cpXoff   0
set cpYoff   0

proc cpDisplayFrameData {name old new} \
{
    global cpFrame cpXcen cpYcen cpXoff cpYoff
    global cpXmag cpYmag cpXscale cpYscale

    set update 0
    switch $name {
    frame	{   if {$new != $cpFrame} {
			set cpFrame $new
			set update 1
		    }
		}
    frameView	{   # Parse the frameView input.
		    set xmag  [lindex $new 0]; set ymag  [lindex $new 1]
		    set xcen  [lindex $new 2]; set ycen  [lindex $new 3]
		    set xnorm [lindex $new 4]; set ynorm [lindex $new 5]
		    set xoff  [lindex $new 6]; set yoff  [lindex $new 7]

		    # We need client coords and the overall scale factors.
		    set text [send client encodewcs $xcen $ycen]
		    set xcen [lindex $text 0]
		    set ycen [lindex $text 1]
		    set xscale [expr $xmag * $xnorm]
		    set yscale [expr $ymag * $ynorm]

		    if {$xcen != $cpXcen || $ycen != $cpYcen ||
			$xmag != $cpXmag || $ymag != $cpYmag ||
			$xoff != $cpXoff || $yoff != $cpYoff ||
			$xscale != $cpXscale || $yscale != $cpYscale} {

			    set cpXcen $xcen;  set cpXscale $xscale
			    set cpYcen $ycen;  set cpYscale $yscale
			    set cpXmag $xmag;  set cpXoff   $xoff;  
			    set cpYmag $ymag;  set cpYoff   $yoff;
			    set update 1
		    }
		}
    }

    if {$update} {
	set header [format "-- Frame %d --" $cpFrame]
	set center [format "X center: %0.1f\nY center: %0.1f" $cpXcen $cpYcen]
	if {int($cpXmag) >= 10} {
	    set zoom1 [format " X zoom: %0.1f" $cpXmag]
	    set zoom2 [format " Y zoom: %0.1f" $cpYmag]
	} else {
	    set zoom1 [format "X zoom: %0.1f" $cpXmag]
	    set zoom2 [format "Y zoom: %0.1f" $cpYmag]
	}
	if {int($cpXscale) >= 10} {
	    set scale1 [format "X scale: %0.1f" $cpXscale]
	    set scale2 [format "Y scale: %0.1f" $cpYscale]
	} else {
	    set scale1 [format "X scale: %0.2f" $cpXscale]
	    set scale2 [format "Y scale: %0.2f" $cpYscale]
	}
	set offset [format "  Offset: (%0.1f,%0.1f)" $cpXoff $cpYoff]

	send frameData set label [format "%s\n%s\n%s\n%s\n%s\n%s\n%s" \
	    $header $center $scale1 $scale2 $zoom1 $zoom2 $offset \
	]
    }
}; foreach p {frame frameView} {send $p addCallback cpDisplayFrameData}


# Frame enhancement.
# -------------------------------
set cpEnhanceDisable 0
set cpEnhanceId 0
set cpEnhanceMode none
set cpEnhanceVal 0
set cpListItem none

# Windowing the colormap is slow when the mouse is not in the image window,
# so it is necessary to execute the windowColormap in a work procedure.
# This allows any number of slider motion events to be processed for each
# windowColormap, preventing slider events from queueing up.

proc cpResetEnhance args \
{
    global cpListItem cpEnhanceId
    set cpListItem none
    set cpEnhanceId 0
}

proc cpSetEnhancement {widget cbtype x y} \
{
    global cpEnhanceMode cpEnhanceVal cpEnhanceId cpEnhanceDisable
    set cpEnhanceMode $widget
    set cpEnhanceVal $x
    if {!$cpEnhanceId && !$cpEnhanceDisable} {
	set cpEnhanceId [postWorkProc cpEnhanceProc]
    }
}
proc cpEnhanceProc args \
{
    global cpEnhanceMode cpEnhanceVal cpEnhanceId
    global enhancement frame maxContrast
    set val $cpEnhanceVal

    if {$cpEnhanceMode == "contrastSlider"} {
	set contrast [lindex $enhancement($frame) 2]
	send client windowColormap [lindex $enhancement($frame) 1] \
	    [expr (($contrast < 0) ? -$val : $val) * $maxContrast] 
    } else {
	send client windowColormap $val
    }

    set cpEnhanceId 0
    return done
}

proc cpInvert args \
{
    global enhancement frame
    set contrast [lindex $enhancement($frame) 2]
    send client windowColormap [lindex $enhancement($frame) 1] \
	[expr -1.0 * $contrast]
}

proc cpDisplayEnhancement {param old new} \
{
    global cpEnhanceId maxContrast cpEnhanceDisable
    global cpListItem enhancement frame

    if {!$frame} \
	return
    set enhance $enhancement($frame)
    if {[llength $enhance] < 3} \
	return

    set colortable [lindex $enhance 0]
    set offset     [lindex $enhance 1]
    set scale      [lindex $enhance 2]

    send colordata set label [format "-- %s --\nCon %0.2f  Brt %0.2f" \
	$colortable $scale $offset]

    if {$colortable != $cpListItem} {
	send colorlist highlight $colortable
	set cpListItem $colortable
    }

    if {!$cpEnhanceId && !$cpEnhanceDisable} {
	set cpEnhanceDisable 1
	send contrastSlider moveThumb [expr abs($scale) / $maxContrast]
	send brightnessSlider moveThumb $offset
	set cpEnhanceDisable 0
    }
}

foreach i {enhancement frame} {
    send $i addCallback cpDisplayEnhancement
}
send contrastSlider addCallback cpSetEnhancement scroll
send brightnessSlider addCallback cpSetEnhancement scroll
send invertButton addCallback cpInvert


# Colortable display and selection.
# -------------------------------
set colorTables	   {}

proc cpSetColorList {param old new} \
{
    set colorTables $new
    send colorlist setList $new resize
}; send colortables addCallback cpSetColorList

proc colorlistResize args \
{
    global colorTables
    send colorlist setList $colorTables resize
}; send colorlist addEventHandler colorlistResize ResizeRedirectMask

proc cpSelectColor {widget cbtype selections indices} \
{
    global colortable
    foreach selection $selections {
	send client setColormap $selection
    }
}; send colorlist addCallback cpSelectColor
    

# Frame blink.
# -------------------------------
send BRtext   set label $blinkRate
send brBRtext set label $blinkRate

proc cpSetBlinkRate {w args} \
{
    global blinkRate
    if {$w == "BRincrease" || $w == "brBRincrease"} {
	if {$blinkRate < 0.01} {
	    set blinkRate 0.125
	} else {
	    set blinkRate [expr $blinkRate * 2.0]
	}
    } else {
	set blinkRate [expr $blinkRate / 2.0]
	if {$blinkRate < 0.01} {
	    set blinkRate 0
	}
    }
    send BRtext   set label $blinkRate
    send brBRtext set label $blinkRate
}
foreach w {BRincrease BRdecrease brBRincrease brBRdecrease} {
    send $w addCallback cpSetBlinkRate
}

proc cpSetBlinkFrame {widget args} \
{
    global blinkFrames frames nframes

    set frame [send $widget get label]
    if {$frame == " "} {
	set frame 1
    } else {
	incr frame
	if {$frame > $nframes} {
	    set frame " "
	}
    }
    send $widget set label $frame

    set blinkFrames {}
    foreach i {1 2 3 4} {
	set frame [send blinkFrame$i get label]
	if {$frame != " "} {
	    lappend blinkFrames $frame
	}
    }
}; foreach i {1 2 3 4} {send blinkFrame$i addCallback cpSetBlinkFrame}

proc cpBlink {widget args} \
{
    global blinkRate blinkId

    if {$blinkRate < 0.01} {
	send blinkButton set on False
	send brBlinkButton set on False
	blink
    } elseif {($blinkId != 0) != [send $widget get on]} {
	toggleBlink
    }
} ; foreach w {blinkButton brBlinkButton} {send $w addCallback cpBlink}


proc cpAutoRegister {widget type state args} \
{
    global auto_reg frame frames blinkFrames
    global frameZoomX frameZoomY frameOffsetX frameOffsetY

    set auto_reg $state
    if {$auto_reg == 1} {
	send autoregButton set on True
	send brAregButton set on True
    } else {
	send autoregButton set on False
	send brAregButton set on False
    }

    # Register the frames to zero the offsets.
    send client registerFrames \{$blinkFrames\}
    foreach f $frames {
	set frameOffsetX($f) 0
	set frameOffsetY($f) 0
    }
} ; foreach w {autoregButton brAregButton} {send $w addCallback cpAutoRegister}


proc toggleAutoReg args \
{
    global auto_reg
    if {$auto_reg} {
	cpAutoRegister autoregButton dummy 0 
	set auto_reg 0
    } else {
	cpAutoRegister autoregButton dummy 1 
	set auto_reg 1
    }
}

proc resetAutoReg args \
{
    global auto_reg
    if {$auto_reg} \
	toggleAutoReg
}; send initialize addCallback resetAutoReg


proc cpResetBlink args \
{
    global blinkRate blinkFrames blinkIndex frames
    global defaultBlinkRate

    foreach i {1 2 3 4} {
	send blinkFrame$i set label " "
    }
    for {set i 1} {$i <= 16} {incr i} {
	send brFrame$i set label $i
    }
    set blinkRate $defaultBlinkRate
    send BRtext   set label $blinkRate
    send brBRtext set label $blinkRate
    set blinkIndex 0
} 
send blinkReset addCallback cpResetBlink
send brReset    addCallback cpResetBlink

proc cpTraceBlink {name element op} \
{
    upvar $name blinkId
    send blinkButton set on [expr $blinkId != 0]
    send brBlinkButton set on [expr $blinkId != 0]
}; trace variable blinkId w cpTraceBlink

proc cpSetBlinkFrames {param old new} \
{
    global blinkFrames frames

    set blinkFrames {}
    foreach i $frames {
	if {$i <= $new} {
	    lappend blinkFrames $i
	}
    }
    cpResetBlink
    set button 1
    for {set i 1} {$i <= $new} {incr i} {
	if {$i <= 4} {
	    send blinkFrame$button set label $i
	}
	send brFrame$button set label $i
	incr button
    }
}; send nframes addCallback cpSetBlinkFrames

proc cpRegisterFrames args \
{
    global frames blinkFrames
    global frameOffsetX frameOffsetY

    foreach f $frames {
	set frameOffsetX($f)  0
	set frameOffsetY($f)  0
    }
    send client registerFrames \{$blinkFrames\}
}
send registerButton addCallback cpRegisterFrames
send brRegButton    addCallback cpRegisterFrames

proc cpMatchFrames args \
{
    global blinkFrames
    send client matchFrames \{$blinkFrames\}
}
send matchButton   addCallback cpMatchFrames
send brMatchButton addCallback cpMatchFrames


# Options buttons.
# -------------------------------
proc cpSetPanner {widget args} \
{
    setPanner [send $widget get on]
}; send pannerButton addCallback cpSetPanner

proc cpTracePanner {name element op} \
{
    upvar $name panner_enable
    send pannerButton set on $panner_enable
}; trace variable panner_enable w cpTracePanner

proc cpSetMagnifier {widget args} \
{
    setMagnifier [send $widget get on]
}; send magnifierButton addCallback cpSetMagnifier

proc cpTraceMagnifier {name element op} \
{
    upvar $name magnifier_enable
    send magnifierButton set on $magnifier_enable
}; trace variable magnifier_enable w cpTraceMagnifier

proc cpSetCoordsBox {widget args} \
{
    setTrack [send $widget get on]
}; send coordsBoxButton addCallback cpSetCoordsBox

proc cpTraceCoordsBox {name element op} \
{
    upvar $name track_enable
    send coordsBoxButton set on $track_enable
}; trace variable track_enable w cpTraceCoordsBox

proc cpSetWarnings args \
{
    global warnings
    set warnings [send warningsButton get on]
}; send warningsButton addCallback cpSetWarnings

proc cpSetAutoscale args \
{
    set value [send autoscaleButton get on]
    send client setOption autoscale [expr {$value ? "True" : "False"}]
}; send autoscaleButton addCallback cpSetAutoscale

proc cpTrackAutoscale {param old new} \
{
    send autoscaleButton set on [true $new]
}; send autoscale addCallback cpTrackAutoscale

proc cpSetAntialias args \
{
    set value [send antialiasButton get on]
    send client setOption antialias [expr {$value ? "True" : "False"}]
}; send antialiasButton addCallback cpSetAntialias

proc cpTrackAntialias {param old new} \
{
    send antialiasButton set on [true $new]
}; send antialias addCallback cpTrackAntialias

proc cpSetTileFrames { widget type state args } \
{
    global tile_frames tileOpt

    set value [send tileFramesButton get on]
    if {$value} {
        selectTileOrientation junk junk [tileSelToLabel $tileOpt]
    } else {
        selectTileOrientation junk junk Disabled
    }
   send client setOption tileFrames \
	[expr {$value ? "True" : "False"}] \{ $tile_frames \}
} ; send tileFramesButton addCallback cpSetTileFrames

proc cpTrackTileFrames {param old new} \
{
    send tileFramesButton set on [true $new]
}; send tileFrames addCallback cpTrackTileFrames

proc tileFramesToggle args \
{
    set value [send tileFramesButton get on]
    send tileFramesButton set on [expr !$value]
    cpSetTileFrames
}

