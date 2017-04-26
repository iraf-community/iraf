
################################################################################
#  Coords Panel Callbacks.
################################################################################

# In case we need to change the values....
#global wcsPHeight wcsPTxtHeight wcsPGrHeight wcsPOptHeight
#set wcsPHeight		267
#set wcsPTxtHeight	132
#set wcsPGrHeight	175
#set wcsPOptHeight	233
#setCoordPanelHeight

set wcsPHeight		267		;# full panel no options
set wcsPTxtHeight	132		;# size of text area box
set wcsPGrHeight	175		;# size of text area group
set wcsPOptHeight	233		;# extra height for opts boxes

# Set the WCS readout panel sensitivity depending on whether the ISM
# is currently enabled.
proc setCoordPanelSensitivity args \
{
    set widgets { 
	wpWcs2 wpWcs3 wpWcs4 
	wiWcs2 wiWcs3 wiWcs4 
	wlWcs2 wlWcs3 wlWcs4
	sysWcs2 sysWcs3 sysWcs4 
	fmtWcs2 fmtWcs3 fmtWcs4
    }

    send sysWcs1 set label "Display"
    send fmtWcs1 set label "Default"

    for {set i 2} {$i <= 4} {incr i} {
        send sysWcs$i set label "None" ; send fmtWcs$i set label "Default"
        send wpWcs$i  set on False     ; send wiWcs$i  set on False
        send wtWcs$i  set height 4
    }

    if {[send ismToggle get on]} {
        send sysWcs2 set label "World" ; send fmtWcs2 set label "Default"
	send wpWcs2  set on True       ; send wiWcs2  set on True
        send wtWcs2  set height 17

        foreach w $widgets { send $w setSensitive True }
    } else {
        foreach w $widgets { send $w setSensitive False }
    }
}


# Set the Coords Panel height  depending on the option settings.
proc setCoordPanelHeight args \
{
    global wcsPHeight wcsPOptHeight wcsPTxtHeight wcsPGrHeight
    global tabTop

    if {$tabTop != "wcs_panel"} \
	return

    # Get the height of the text area
    set panel_h $wcsPHeight
    set shrinkage 0
    foreach w { wpWcs1 wpWcs2 wpWcs3 wpWcs4 woptFBinfo } {
	if {[send $w get on] == 0} {
	    incr shrinkage 13
	}
    }
    if {[send woptTitles get on] == 0} {
	incr shrinkage 26
    }

    set ph [expr ($wcsPHeight - $shrinkage)]
    if {[send wcsOptions get on] == 1} {
	incr ph $wcsPOptHeight
    }

    send wcsGroup set height [ expr ($wcsPGrHeight - $shrinkage) ]
    send wcsFrame set height [ expr ($wcsPTxtHeight - $shrinkage) ]
    send panel    set height $ph
}

# Toggle the options display for the panel.
proc wcsOptToggle { widget type state args } \
{
    global wcsPOptHeight
    set h [ send panel get height ] 
    if {$state == 1} {
	send panel set height [ expr ($h + $wcsPOptHeight) ]
    } else { 
	send panel set height [ expr ($h - $wcsPOptHeight) ]
    }
} ; send wcsOptions addCallback wcsOptToggle

# Handle the panel display toggles.
proc wcsCoordsCB { widget type state args } \
{

    set hght [ expr (($state == 1) ? 17 : 4)]
    switch $widget {
    wpWcs1	{ send wtWcs1 set height $hght	}
    wpWcs2	{ send wtWcs2 set height $hght	}
    wpWcs3	{ send wtWcs3 set height $hght	}
    wpWcs4	{ send wtWcs4 set height $hght	}
    woptFBinfo	{ send wtFBCfg set height $hght }
    woptTitles	{ send wtName set height $hght ; send wtTitle set height $hght }
    }
    setCoordPanelHeight                 
}
set wcValues { wpWcs1 wpWcs2 wpWcs3 wpWcs4 woptFBinfo woptTitles }
foreach w $wcValues { send $w addCallback wcsCoordsCB }


# Handle WCS label string options.
set wcsLabels 1
proc wcsLabelsCB { widget type state args } \
{
    global up_todo wcsLabels
    set wcsLabels $state
    #resizeCoordsBox 0
    resizeCoordsBox $up_todo
    updateCoordsBox
} ; send woptLabels addCallback wcsLabelsCB


# Toggle the BPM tracking state.
proc wcsBPMCB { widget type state args } \
{
    global ism_enable
    if ($ism_enable) { catch { send wcspix set bpm $state } }
} ; send woptBPM addCallback wcsBPMCB


# Procedures to format lines in the wcsText box.
proc wcsFmtImname { name } \
{
    send wtName    set string [format "     Name:  %s" [string trimleft $name]]
}

proc wcsFmtImtitle { title } \
{
    send wtTitle   set string [format "    Title:  %s" [string trimleft $title]]
}

proc wcsFmtFBConfig args \
{
    global frameWidth frameHeight frame nframes
    set buf [ format "%5d x %-5d" $frameWidth $frameHeight ]
    set line [ format "Frame Buf:  %-13s             Frame:  %d of %d" \
	[string trimleft $buf] $frame $nframes ]
    send wtFBCfg set string $line
}

proc wcsFmtIValue { value } \
{
    global coord
    set line [ format " Pixel:  %.11s" $value ]
    send wtIPixval set string $line
    if {[info exists coord(ival)]} {
        set coord(ival) $value
        updateCoordsBox
    }
}

proc wcsFmtSValue { value } \
{
    global coord
    set line [ format " Scaled:  %.8s" $value ]
    send wtSPixval set string $line
    set coord(sval) [format "%s" $value]
}

proc wcsFmtBValue { value } \
{
    global coord

    if { [send woptBPM get on] } {
        set line [ format " BPM:  %s" $value ]
        set color [expr { ($value == 0) ? "black" : "red" } ]
        set msg [format "set string \{%s\}; set background %s" $line $color ]
        set coord(bval) [format "%s" $value]
    } else {
        set line [ format " BPM:  (off)" ]
        set msg [format "set string \{%s\}" $line]
    }
    send wtBPixval $msg
}

proc wcsFmtWcs { num wcsname x y xunit yunit args } \
{ 
    global coord coordLab wcsLabels

    if {$wcsLabels} {
        set line [ format "%4s: %12s  %4s: %12s  WCS: %s" \
            $xunit $x $yunit $y [string trimleft $wcsname] ]
    } else {
        set line [ format "%4s  %12s  %4s  %12s       %s" \
            "    " $x "    " $y [string trimleft $wcsname] ]
    }
    send wtWcs$num set string $line

    if {[info exists coord(wcs$num)]} {
	if {$num == 1} {
            set coord(wcs1) [ format "\{%s\} \{%s\} \{%s\}" $x $y $coord(sval) ]
	} elseif {$num == 2} {
            set coord(wcs2) [ format "\{%s\} \{%s\} \{%s\}" $x $y $coord(ival) ]
	} else {
	    set coord(wcs$num) [ format "\{%s\} \{%s\} \{%s\}" $x $y $wcsname  ]
	}

	set coordLab(wcs$num) [ format "\{%s\} \{%s\} \{%s\}" \
	    $xunit $yunit [string trimleft $wcsname ] ]
        updateCoordsBox
    }
}



# Handle the wcsbox readout.
#------------------------------
set  up_todo	  2
set  up_done	  0
set  coord(ival)  0.
set  coord(sval)  0.
set  coord(bval)  0
set  coord(wcs1)  { 0. 0. 0. }
set  coord(wcs2)  { 0. 0. 0. }
set  coord(wcs3)  { 0. 0. 0. }
set  coord(wcs4)  { 0. 0. 0. }

proc wcsCoordB { widget type state args } \
{
    global coord up_todo

    switch $widget {
    wiWcs1	{ set line wcs1  ;set coord($line) { 0. 0. "" } }
    wiWcs2	{ set line wcs2  ;set coord($line) { 0. 0. "" } }
    wiWcs3	{ set line wcs3  ;set coord($line) { 0. 0. "" } }
    wiWcs4	{ set line wcs4  ;set coord($line) { 0. 0. "" } }
    }

    if {$state} {
	incr up_todo
    } else {
	unset coord($line)
	incr up_todo -1
    }

    resizeCoordsBox $up_todo
    updateCoordsBox
}
set wiValues { wiWcs1 wiWcs2 wiWcs3 wiWcs4 }
foreach w $wiValues { send $w addCallback wcsCoordB }


# Resize the coords box depending on the panel options.
proc resizeCoordsBox { nlines } \
{
    global track_enable wcsLabels winWidth winHeight wcsboxGeom 

    if {! $track_enable} \
	return

    send wcsbox getAttributes width cur_w height cur_h 
    set defGeom [format "%sx%s-5-5" $cur_w $cur_h]
    send imagewin parseGeometry $wcsboxGeom $defGeom x y width height

    set ew [expr (($wcsLabels == 1) ? 125 : 65)]

    # Reset to the default geometry
    if {$nlines == 0} {
	set x [expr ($x + $ew)]
	set y [expr ($y + $height - 17 + 1)]
	set new_w  166
	set new_h  17

    } else {
	if {$width > 166} {			;# not using default wcsbox
	    set new_w $width
	} else {
	    set new_w [expr ($width + $ew)]
	    set x [expr ($x - $ew)]
	    if {$wcsLabels == 0} {
		incr x 60
	    }
	}
        set new_h [ expr ($nlines * 17) ]
	set y [expr ($y + $height - $new_h + 1)]
    }

    # Bounds checking.
    if {$x < 5} {
	set x 5
    } elseif {$x > [expr ($winWidth - $new_w - 5)]} {
        set x [expr ($winWidth - $new_w - 5)]
    }
    if {$y < 5} {
	set y 5
    } elseif {$y > [expr ($winHeight - $new_h - 5)]} {
        set y [expr ($winHeight - $new_h - 5)]
    }

    # Finally redraw the marker.
    send wcsbox "\
        setAttributes     \
	    width  $new_w \
	    height $new_h \
	    x	   $x     \
	    y	   $y;    \
	redraw"

    set wcsboxGeom [send imagewin getGeometry $x $y $new_w $new_h]
    send client encodewcs [expr ($winWidth / 2)] [expr ($winHeight / 2)]
    updateCoordsBox
}


# Shortcuts for known WCS labels.
set labels(display)		"TV"
set labels(logical)		"Log"
set labels(physical)		"Phys"
set labels(equatorial)		"Eq"
set labels(ecliptic)		"Ecl"
set labels(galactic)		"Gal"
set labels(supergalactic)	"SGal"
set labels(amplifier)		"Amp"
set labels(ccd)			"CCD"
set labels(detector)		"Det"


# Format the coords box marker with the selected output options.
proc updateCoordsBox args \
{
    global ism_enable coord coordLab wcsLabels
    global up_done up_todo labels track_enable
    global coord

    if {! $ism_enable} \
	return

    incr up_done

    set text ""
    foreach l {wcs1 wcs2 wcs3 wcs4} {
	if {[info exists coord($l)]} {
	    set x [lindex $coord($l) 0]
	    set y [lindex $coord($l) 1]
	    set z [string tolower [string trimleft [lindex $coord($l) 2] ] ]
	    if {[info exists labels($z)]} {
		set z $labels($z)
	    }

	    if {$wcsLabels && [info exists coordLab($l)]} {
	        set lx [lindex $coordLab($l) 0]
	        set ly [lindex $coordLab($l) 1]
	        append text [format " %4s %12.12s %4s %12.12s %9.9s \n" \
		    $lx $x $ly $y $z ]
	    } else {
	        append text [format " %12.12s %12.12s %9.9s \n" $x $y $z ]
	    }
	}
    }

    # Now send the string.
    if {$track_enable} {
	set color [expr { ($coord(bval) == 0) ? "black" : "red" } ]
        set txt [format "set text \{%s\}; set textBgColor %s; redraw noerase" \
		$text $color ]
        send wcsbox $txt
    }

    if {$up_done >= $up_todo} {
        set up_done	0			;# reset counter
    }
}


# Create the WCS format menus.
#------------------------------------
proc setWcsFmt { format line } \
{
    catch { send wcspix set format $format $line }
}

for {set i 1} {$i <= 4} {incr i} {
   set items {}
   lappend items "\"Default\"     f.exec \{setWcsFmt default $i\}"
   lappend items "\"Sexigesimal\" f.exec \{setWcsFmt hms     $i\}"
   lappend items "\"Degrees\"     f.exec \{setWcsFmt deg     $i\}"
   lappend items "\"Radians\"     f.exec \{setWcsFmt rad     $i\}"
   editMenu fmtMenu$i fmtWcs$i $items
}


# Create the default WCS type menus.
#------------------------------------

set defaultWcsMenu {
   { "None"		f.exec	{setWcsSys none     WCS_LINE } }
   { "Display"		f.exec	{setWcsSys display  WCS_LINE } }
   { "World"		f.exec	{setWcsSys world    WCS_LINE } }
   { "Logical"		f.exec	{setWcsSys logical  WCS_LINE } }
   { "Physical"		f.exec	{setWcsSys physical WCS_LINE } }
   { 			f.dblline	}
}

proc setWcsSys { sys line } \
{
    if {[string tolower $sys] == "none"} {
        wcsCoordB wiWcs$line junk 0
        send sysWcs$line set label "None"; send fmtWcs$line set label "Default"
        send wpWcs$line  set on False    ; send wiWcs$line  set on False
        send wtWcs$line  set height 4
	setCoordPanelHeight

    } else {
        catch { send wcspix set wcs $sys $line }
    }
}

proc resetDefaultWcsMenu args \
{
    global defaultWcsMenu

    for {set i 1} {$i <= 4} {incr i} {
        regsub -all WCS_LINE $defaultWcsMenu $i menu_def
        editMenu sysMenu$i sysWcs$i $menu_def
    }
} ; resetDefaultWcsMenu


# Initialize the coordinates panel.
#------------------------------------

proc initCoordsPanel args \
{
    #send wcLine set height 2		;# kludge for label widget

    # Initialize the display strings in the coords box.
    wcsFmtImname   ""
    wcsFmtImtitle  ""
    wcsFmtFBConfig
    wcsFmtIValue  "0." ; wcsFmtSValue "0." ; wcsFmtBValue "0"
    wcsFmtWcs      1 "" "" "" "   X" "   Y"
    wcsFmtWcs      2 "" "" "" "   X" "   Y"
    wcsFmtWcs      3 "" "" "" "   X" "   Y"
    wcsFmtWcs      4 "" "" "" "   X" "   Y"

    send fmtWcs1 set label Default
    send wpWcs1 set on True  ;send wiWcs1 set on True
    send wpWcs2 set on True  ;send wiWcs2 set on True
    send wpWcs3 set on False ;send wiWcs3 set on False ;send wtWcs3 set height 4
    send wpWcs4 set on False ;send wiWcs4 set on False ;send wtWcs4 set height 4

    # Set the Coords Panel height.
    set wcsPTxtHeight [send wcsText get height]
    setCoordPanelHeight

    # Set the WCS readout panel sensitivity.
    setCoordPanelSensitivity

} ; initCoordsPanel


