
     
################################################################################
# RULER MARKERS
################################################################################

set rulerX	0			;# ruler start in screen coords
set rulerY	0
set rulerPts	{ {0 0} {0 0} {0 0} }	;# ruler vertices
set rulerList	{ }			;# ruler list

set rulerSticky	{ }			;# list of sticky rulers
set isSticky	"Sticky"		;# menu label

set rulerWCS	0			;# use WCS coords
set rulerXWCS	0			;# ruler start in WCS units
set rulerYWCS	0
set rulerXWCS2	0			;# ruler end in WCS units
set rulerYWCS2	0
set rulerFmt	pixel			;# ruler label format


# Translations when pointer is inside marker.
set rulerTranslations { \
      !Ctrl <Key>b: call(prevFrame,$name)
      !Ctrl <Key>b: call(prevFrame,$name)
      !Ctrl <Key>f: call(nextFrame,$name)
      !Ctrl <Key>h: call(move_cursor,-1,0)
      !Ctrl <Key>j: call(move_cursor,0,1)
      !Ctrl <Key>k: call(move_cursor,0,-1)
      !Ctrl <Key>l: call(move_cursor,1,0)
      !Ctrl <Key>n: call(normalize)
      !Ctrl <Key>c: call(cpZoomAction,centerFrame)
      !Ctrl <Key>i: call(cpInvert)
      !Ctrl <Key>m: call(cpMatchFrames)
      !Ctrl <Key>r: call(cpRegisterFrames)
      !Ctrl <Key>p: call(togglePanner)
       !Alt <Key>1: call(cpSetFrame,frame1)
       !Alt <Key>2: call(cpSetFrame,frame2)
       !Alt <Key>3: call(cpSetFrame,frame3)
       !Alt <Key>4: call(cpSetFrame,frame4)
      !Ctrl <Key>1: call(cpZoom,1,1,fixed)
      !Ctrl <Key>2: call(cpZoom,2,2,fixed)
      !Ctrl <Key>3: call(cpZoom,3,3,fixed)
      !Ctrl <Key>4: call(cpZoom,4,4,fixed)
      !Ctrl <Key>5: call(cpZoom,5,5,fixed)
      !Ctrl <Key>6: call(cpZoom,6,6,fixed)
      !Ctrl <Key>7: call(cpZoom,7,7,fixed)
      !Ctrl <Key>8: call(cpZoom,8,8,fixed)
      !Ctrl <Key>9: call(cpZoom,9,9,fixed)
    <Key>BackSpace: call(deleteNamedRuler,NAME,$x,$y)
       <Key>Delete: call(deleteNamedRuler,NAME,$x,$y)
	<KeyPress>: m_input()
	<Btn3Down>: call(setRulerMenu) popup(rulerMenu)
	  <Btn3Up>: popdown(rulerMenu)
!Ctrl <Btn1Motion>: track-cursor() call(wcsUpdate,$x,$y) call(resizeRuler,$x,$y,0)
    !Ctrl <Btn1Up>: call(deleteRuler,$x,$y)
	  <Motion>: track-cursor() call(wcsUpdate,$x,$y) call(magnifierMapImage,$x,$y)
}



# Popup menu in effect when inside marker.
set rulerMenuDescription {
    {	"Ruler"    	  f.title			}
    {		    	  f.dblline			}
    {	"$isSticky"	  f.exec { 
			      toggleSticky $ruler
			  }				}
    {		    	  f.line			}
    {	"Units"	  	  f.menu rulerUnits		}
    {	"Color"	  	  f.menu rulerColor		}
    {		    	  f.line			}
    {	"Draw into Frame" f.exec { 
			      writeRuler $ruler
			  } sensitive False		}
    {		    	  f.line			}
    {	"Destroy"	  f.exec { 
		              scan $ruler "ruler%d" num
		              deleteNamedRuler $num x y
		    	  }				}
} ; createMenu rulerMenu imagewin $rulerMenuDescription

set rulerUnitsDescription {
    {	Units		f.title				  }
    {			f.dblline			  }
    {	"Pixels"	f.exec { setUnits $ruler pixel  } }
    {	"Arc Seconds"  	f.exec { setUnits $ruler arcsec  
			} sensitive { ($rulerWCS > 0) ? "True" : "False"} }
    {	"Arc Minutes"   f.exec { setUnits $ruler arcmin  
			} sensitive { ($rulerWCS > 0) ? "True" : "False"} }
    {	"Degrees"   	f.exec { setUnits $ruler degrees 
			} sensitive { ($rulerWCS > 0) ? "True" : "False"} }
} ; createMenu rulerUnits rulerMenu $rulerUnitsDescription

set rulerColorDescription {
    {	Color	f.title					}
    {		f.dblline				}
    {	""	f.exec "r_setColor $ruler black yellow"
		    bitmap solid foreground black 	}
    {	""	f.exec "r_setColor $ruler white black" 
		    bitmap solid foreground white 	}
    {	""	f.exec "r_setColor $ruler red yellow" 
		    bitmap solid foreground red 	}
    {	""	f.exec "r_setColor $ruler green black" 
		    bitmap solid foreground green 	}
    {	""	f.exec "r_setColor $ruler blue white" 
		    bitmap solid foreground blue 	}
    {	""	f.exec "r_setColor $ruler magenta black" 
		    bitmap solid foreground magenta 	}
    {	""	f.exec "r_setColor $ruler cyan black" 
		    bitmap solid foreground cyan 	}
    {	""	f.exec "r_setColor $ruler yellow black" 
		    bitmap solid foreground yellow 	}
} ; createMenu rulerColor rulerMenu $rulerColorDescription


proc makeRuler {parent x y} \
{
    global rulerTranslations ruleno rulerWCS rulerXWCS rulerYWCS
    global rulerPts rulerX rulerY rulerList
    global isSticky rulerMenuDescription rulerUnitsDescription
    global coord coordLab


    incr ruleno ; set ruler ruler$ruleno

    # Substitute so the marker translation will delete the marker
    # by it's number rather than the default parent widget name.
    regsub -all NAME $rulerTranslations $ruleno translations

    # Create the polygon for the marker.
    send $parent createMarker $ruler \
	type		polygon\
	createMode	noninteractive\
	translations	$translations\
        lineColor       yellow\
        fill       	False\
        highlightWidth  1\
        highlightColor  yellow\
        knotSize        0\
        activated       True\
        visible         False\
        sensitive       True\
	x		$x\
	y		$y

    # Define a callback so we can identify the ruler.
    send $ruler addCallback selectRuler focusIn focusOut

    # Create the text markers for the labels.
    send $parent set markerTextFont 6x9
    makeLabelMarker $parent rulerXlab$ruleno 5ch 1ch
    makeLabelMarker $parent rulerYlab$ruleno 5ch 1ch
    makeLabelMarker $parent rulerHlab$ruleno 8ch 1ch
    send $parent set markerTextFont 6x13

    set rulerX	$x				;# save the reference point
    set rulerY	$y

    set ref [ list $x $y ]			;# initialize the polygon
    set rx [ list [expr "$x +1"] $y ]
    set ry [ list $x [expr "$y +1"] ]
    set rulerPts [list $ref $rx $ry ]

    send $ruler setVertices $rulerPts		;# set attributes
    send $ruler set visible True"
    send $ruler setAttribute autoRedraw True

    # See whether we have a WCS to use.
    set rulerWCS  0
    set rulerXWCS 0
    set rulerYWCS 0
    for {set num 1} {$num <= 4 && $rulerWCS == 0} {incr num} {
	if { [info exists coordLab(wcs$num)] } {
	    set xl  [string tolower [lindex $coordLab(wcs$num) 0]]
	    set yl  [string tolower [lindex $coordLab(wcs$num) 1]]
	    set fmt [string tolower [send fmtWcs$num get label] ]
	    if {$xl=="  ra" || $xl=="elon" || $xl=="glon" || $xl=="slon"} {
		set rulerWCS $num
        	set rulerXWCS [wcs2log [lindex $coord(wcs$num) 0] $xl $fmt]
        	set rulerYWCS [wcs2log [lindex $coord(wcs$num) 1] $yl $fmt]
	    }
	}
    }

    # Edit the menus.
    set isSticky "Sticky"
    editMenu rulerMenu  imagewin $rulerMenuDescription
    editMenu rulerUnits imagewin $rulerUnitsDescription


    lappend rulerList $ruleno
}

proc wcs2log { val label fmt} \
{
    set newval $val
    if {$fmt == "sexigesimal" || $fmt == "default"} {
	scan $val "%d:%d:%f"  h m s
	set newval [expr "double($h) + double($m) / 60.0 + double($s) / 3600.0"]
	if {$label == "  ra"} {
	    set newval [expr "double($newval * 15.0)"]
	}
    }
    return [expr "double($newval)" ]
}


proc resizeRuler {x y redraw} \
{
    global rulerPts rulerX rulerY ruleno coord coordLab
    global rulerWCS rulerXWCS rulerYWCS rulerFmt
    global rulerXWCS2 rulerYWCS2


    # Track the mouse.
    set ref [ list $rulerX $rulerY ]
    set rx  [ list $x      $rulerY ]
    set ry  [ list $x      $y      ]
    set rulerPts [list $ref $rx $ry ]

    # Compute the distances.
    if {$rulerWCS > 0} {
 	set num $rulerWCS
	set xl  [string tolower [lindex $coordLab(wcs$num) 0]]
	set yl  [string tolower [lindex $coordLab(wcs$num) 1]]
	set fmt [string tolower [send fmtWcs$num get label] ]

	# Save the cursor coords in WCS so we can convert labels.
	if {$redraw} {
            set nx $rulerXWCS2
            set ny $rulerYWCS2
	} else {
            set nx  [wcs2log [lindex $coord(wcs$num) 0] $xl $fmt]
       	    set ny  [wcs2log [lindex $coord(wcs$num) 1] $yl $fmt]
            set rulerXWCS2 $nx
            set rulerYWCS2 $ny
	}
    }

    if {$rulerFmt == "pixel"} {
        set xdist [ expr "abs($x - $rulerX)" ]
        set ydist [ expr "abs($y - $rulerY)" ]
        set hdist [ expr "sqrt($xdist * $xdist + $ydist * $ydist)" ]
    } else {
        set xdist [ expr "abs($nx - $rulerXWCS)" ]
        set ydist [ expr "abs($ny - $rulerYWCS)" ]
        set hdist [ expr "sqrt($xdist * $xdist + $ydist * $ydist)" ]
    }

    # Redraw the polygon.
    send ruler$ruleno setVertices $rulerPts

    # Label the distances.
    setXRulerLabel $x $y $xdist
    setYRulerLabel $x $y $ydist
    setHRulerLabel $x $y $hdist
}

# Create a label marker for the ruler.
proc makeLabelMarker { parent name width height } \
{
    send $parent createMarker $name \
        type            text \
        createMode      noninteractive \
        width           $width \
        height          $height \
        lineWidth       0 \
        imageText       true \
        textBgColor     yellow \
        textColor       black \
        activated       true \
        visible         false
}

proc setXRulerLabel { cx cy dist } \
{
    global rulerX rulerY ruleno winWidth winHeight
    global cpXscale rulerFmt

    send rulerXlab$ruleno set visible False

    if {[expr "abs($cx - $rulerX)"] > 30} {
	switch $rulerFmt {
	pixel	{ set text [format "%.1f" [expr "$dist / $cpXscale"] ]	}
	arcsec	{ set text [format "%.2f\"" [expr "$dist * 3600.0"] ] 	}
	arcmin	{ set text [format "%.2f\'" [expr "$dist * 60.0"] ] 	}
	degrees	{ set text [format "%.2fd" "$dist" ] 			}
	}
	set len [expr [string length $text] + 1]
	send rulerXlab$ruleno "set width ${len}ch"

	# Compute the placement of the label marker.
	if {$cy > $rulerY} {
	    set yp [expr "$rulerY - 14"]
	} else {
	    set yp [expr "$rulerY + 2"]
	}
	if {$cx > $rulerX} {
	    set xp [expr "$rulerX + abs($cx - $rulerX)/2 - 10"]
	} else {
	    set xp [expr "$rulerX - abs($cx - $rulerX)/2 - 10"]
	}

	# Bounds checking.
	if {$xp < 0} { set xp 1 }
	if {$yp < 0} { set yp 1 }
	if {$xp > $winWidth}  { set xp [expr "$winWidth - 20" }
	if {$yp > $winHeight} { set yp [expr "$winHeight - 20" }

	send rulerXlab$ruleno "setAttributes x $xp y $yp"
	send rulerXlab$ruleno "set text \{$text\}; redraw erase"

	send rulerXlab$ruleno set visible True
    }
}

proc setYRulerLabel { cx cy dist } \
{
    global rulerX rulerY ruleno winWidth winHeight
    global cpYscale rulerFmt

    send rulerYlab$ruleno set visible False

    if {[expr "abs($cy - $rulerY)"] > 20} {
	switch $rulerFmt {
	pixel	{ set text [format "%.1f" [expr "$dist / $cpYscale"] ]	}
	arcsec	{ set text [format "%.2f\"" [expr "$dist * 3600.0"] ] 	}
	arcmin	{ set text [format "%.2f\'" [expr "$dist * 60.0"] ] 	}
	degrees	{ set text [format "%.2fd" "$dist" ] 			}
	}
	set len [expr [string length $text] + 1]
	send rulerYlab$ruleno "set width ${len}ch"

	# Compute the placement of the label marker.
	if {$cx > $rulerX} {
	    set xp [expr "$cx + 2"]
	} else {
	    set xp [expr "$cx - $len * 6 - 5"]
	}
	if {$cy > $rulerY} {
	    set yp [expr "$rulerY + abs($cy - $rulerY)/2"]
	} else {
	    set yp [expr "$rulerY - abs($cy - $rulerY)/2"]
	}

	# Bounds checking.
	if {$xp < 0} { set xp 1 }
	if {$yp < 0} { set yp 1 }
	if {$xp > $winWidth}  { set xp [expr "$winWidth - 20" }
	if {$yp > $winHeight} { set yp [expr "$winHeight - 20" }

	send rulerYlab$ruleno "setAttributes x $xp y $yp"
	send rulerYlab$ruleno "set text \{$text\}; redraw erase"
	send rulerYlab$ruleno set visible True
    } 
}

proc setHRulerLabel { cx cy dist } \
{
    global rulerX rulerY ruleno winWidth winHeight
    global cpYscale cpXscale rulerFmt

    send rulerHlab$ruleno set visible False

    set xdist [ expr "abs($cx - $rulerX)" ]
    set ydist [ expr "abs($cy - $rulerY)" ]
    set hdist [ expr "sqrt($xdist * $xdist + $ydist * $ydist)" ]

    if {$hdist > 30} {
	switch $rulerFmt {
	pixel	{ set text [format "%.1f" [expr "$dist / $cpXscale"] ]	}
	arcsec	{ set text [format "%.2f\"" [expr "$dist * 3600.0"] ] 	}
	arcmin	{ set text [format "%.2f\'" [expr "$dist * 60.0"] ] 	}
	degrees	{ set text [format "%.2fd" "$dist" ] 			}
	}
	set len [expr [string length $text] + 1]
	send rulerHlab$ruleno "set width ${len}ch"

	# Compute the placement of the label marker.
	if {$cx > $rulerX} {
	    set xp [expr "$rulerX + abs($cx - $rulerX)/2 - $len * 6"]
	} else {
	    set xp [expr "$rulerX - abs($cx - $rulerX)/2 - $len * 3"]
	}
	if {$cy > $rulerY} {
	    set yp [expr "$rulerY + abs($cy - $rulerY)/2"]
	} else {
	    set yp [expr "$rulerY - abs($cy - $rulerY)/2"]
	}

	# Bounds checking.
	if {$xp < 0} { set xp 1 }
	if {$yp < 0} { set yp 1 }
	if {$xp > $winWidth}  { set xp [expr "$winWidth - 20" }
	if {$yp > $winHeight} { set yp [expr "$winHeight - 20" }

	send rulerHlab$ruleno "setAttributes x $xp y $yp"
	send rulerHlab$ruleno "set text \{$text\}; redraw erase"
	send rulerHlab$ruleno set visible True
    } else {
	send rulerHlab$ruleno set visible False
    }
}


# Callback executed when a marker gets or loses the focus.
proc selectRuler {active_ruler event event_data} \
{
    global ruler
    switch $event {
	focusIn		{ set ruler $active_ruler }
	focusOut	{  }
    }
}

# Reset the ruler format type.
proc setUnits { ruler units } \
{
    global rulerFmt rulerX rulerY

    send $ruler getVertices pts
    set rulerX [lindex [lindex [lindex $pts 0] 0] 0]
    set rulerY [lindex [lindex [lindex $pts 0] 0] 1]
    set cx [lindex [lindex [lindex $pts 0] 1] 0] ; incr cx -1
    set cy [lindex [lindex [lindex $pts 0] 2] 1] ; incr cy -1

    set rulerFmt $units
    resizeRuler $cx $cy 1
}

# Menu option toggle callbacks.
proc toggleSticky { ruler } \
{
    global isSticky rulerSticky rulerMenuDescription

    set index [ lsearch $rulerSticky $ruler]
    if { $index >= 0 } {
	# Remove it from the list.
    	set rulerSticky [lreplace $rulerSticky $index $index]
	set isSticky "Sticky"
    } else {
	# Add it to the list.
	lappend rulerSticky $ruler 
	set isSticky "UnSticky"
    }
    editMenu rulerMenu imagewin $rulerMenuDescription
}

proc setRulerMenu args \
{
    global ruler isSticky rulerSticky rulerMenuDescription

    if { [lsearch $rulerSticky $ruler] >= 0 } {
	set isSticky "UnSticky"
    } else {
	set isSticky "Sticky"
    }
    editMenu rulerMenu imagewin $rulerMenuDescription
}


# Draw the ruler to the frame buffer as a graphic.
proc writeRuler { ruler } \
{
}

# Change the color of the ruler.
proc r_setColor {ruler bgcolor fgcolor}  {
			    
    # Recolor the polygon.
    send $ruler \
	"markpos; set lineColor $bgcolor; set highlightColor $bgcolor; redraw"

    # Recolor the labels.
    scan $ruler "ruler%d" num
    send rulerXlab$num \
	"markpos; set textBgColor $bgcolor; set textColor $fgcolor; redraw"
    send rulerYlab$num \
	"markpos; set textBgColor $bgcolor; set textColor $fgcolor; redraw"
    send rulerHlab$num \
	"markpos; set textBgColor $bgcolor; set textColor $fgcolor; redraw"
}


# Delete the current ruler, called when we have a Btn1Up on the current ruler.
proc deleteRuler {x y} { global ruleno ; deleteNamedRuler $ruleno $x $y }

# Delete all rulers on the screen, usually called when the view changes.
# We preserve the rulers marked as 'sticky'.
proc deleteAllRulers args \
{
    global rulerList rulerSticky

    foreach r $rulerList { 
        # Delete the ruler if it's not in the sticky list.
        if { [lsearch $rulerSticky ruler$r] == -1 } {
	    deleteNamedRuler $r x y 
        }
    }
}

# Delete a particular ruler, usually called from the translation table on
# the marker itself.
proc deleteNamedRuler {name x y} \
{
    global rulerList

    catch {
	send ruler$name      destroy
	send rulerXlab$name  destroy
	send rulerYlab$name  destroy
	send rulerHlab$name  destroy
    }

    # Remove the ruler from the list.
    set index [lsearch $rulerList $name]
    set rulerList [lreplace $rulerList $index $index]
}




