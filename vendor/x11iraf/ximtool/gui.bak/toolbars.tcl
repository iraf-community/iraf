

################################################################################
#
#  XIMTOOL-ALT -- Procedures and declarations for the ALT GUI.
#
################################################################################

# Creat the bitmaps needed for the alternate optional bars.

createBitmap tools 16 16 {
   0xff, 0xff, 0xff, 0xff, 0x03, 0xc0, 0xf3, 0xcf, 0xf3, 0xcf, 0xf3, 0xcf,
   0x83, 0xc1, 0x83, 0xc1, 0x83, 0xc1, 0x83, 0xc1, 0x83, 0xc1, 0x83, 0xc1,
   0x83, 0xc1, 0x03, 0xc0, 0xff, 0xff, 0xff, 0xff};

createBitmap control 16 16 {
   0xff, 0xff, 0xff, 0xff, 0x03, 0xc0, 0xe3, 0xc7, 0xe3, 0xcf, 0x33, 0xcc,
   0x33, 0xc0, 0x1b, 0xc0, 0x1b, 0xc0, 0x1b, 0xc0, 0x33, 0xcc, 0xf3, 0xcf,
   0xe3, 0xc7, 0x03, 0xc0, 0xff, 0xff, 0xff, 0xff};

createBitmap tile 16 16 {
   0xff, 0xff, 0xff, 0xff, 0x83, 0xc1, 0x83, 0xc1, 0x83, 0xc1, 0x83, 0xc1,
   0x83, 0xc1, 0xff, 0xff, 0xff, 0xff, 0x83, 0xc1, 0x83, 0xc1, 0x83, 0xc1,
   0x83, 0xc1, 0x83, 0xc1, 0xff, 0xff, 0xff, 0xff};

createBitmap compass 16 16 {
   0x00, 0x00, 0x00, 0x02, 0x00, 0x07, 0x80, 0x0f, 0xc0, 0x1f, 0x00, 0x07,
   0x00, 0x07, 0x00, 0x07, 0x00, 0x07, 0x00, 0x07, 0x00, 0x07, 0xfc, 0x07,
   0xfc, 0x07, 0xfc, 0x07, 0x00, 0x00, 0x00, 0x00}

createBitmap plus 11 11 {
   0x00, 0x00, 0x70, 0x00, 0x70, 0x00, 0x70, 0x00, 0xfe, 0x03, 0xfe, 0x03,
   0xfe, 0x03, 0x70, 0x00, 0x70, 0x00, 0x70, 0x00, 0x00, 0x00};

createBitmap minus 11 11 {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe, 0x03, 0xfe, 0x03,
   0xfe, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

createBitmap disk 16 16 {
 0xfe,0x1f,0x11,0x25,0x11,0x45,0x11,0x44,0xf1,0x47,0x01,0x40,0x01,0x40,0xf9,
 0x4f,0x05,0x50,0x05,0x50,0x05,0x50,0x05,0x50,0x05,0x50,0x05,0x50,0xff,0x7f,
 0x00,0x00};

createBitmap printer 32 16 {
   0x00, 0xfe, 0x07, 0x00, 0x00, 0x02, 0x0a, 0x00, 0x00, 0x02, 0x12, 0x00,
   0x00, 0x02, 0x22, 0x00, 0x00, 0x02, 0x3e, 0x00, 0x00, 0x02, 0x20, 0x00,
   0x00, 0x02, 0x20, 0x00, 0xe0, 0x03, 0xe0, 0x03, 0x10, 0x03, 0xe0, 0x06,
   0x08, 0xff, 0x7f, 0x09, 0x04, 0x00, 0x00, 0x10, 0xfc, 0xff, 0xff, 0x1f,
   0x04, 0x00, 0x00, 0x10, 0xfc, 0xff, 0xff, 0x1f, 0x08, 0x00, 0x00, 0x08,
   0xf8, 0xff, 0xff, 0x0f};


send tbBlinkDec   "set bitmap larrow" 	;# Assign the bitmaps to the buttons.
send tbBlinkInc   "set bitmap rarrow"
send tbZoomIn     "set bitmap plus"
send tbZoomOut    "set bitmap minus"
send tbTile       "set bitmap tile"
send tbCompass    "set bitmap compass;  addCallback toggleCompass"
#send helpButton   "set bitmap qmark;    addCallback Help"
send mXflipButton "set bitmap xflip;    addCallback xflip"
send mYflipButton "set bitmap yflip;    addCallback yflip"
send mNextButton  "set bitmap rarrow;   addCallback nextFrame"
send mPrevButton  "set bitmap larrow;   addCallback prevFrame"


# toolBox -- Toggle the toolbox and panelbar display.

set toolbox_up [ true $showToolBar ]
set panelbar_up [ true $showPanelBar ]

proc toolBoxToggle args \
{
    global toolbox_up 

    set w  [send display get width]  ; set h  [send display get height]
    set iw [send imagewin get width] ; set ih [send imagewin get height]

    if {$toolbox_up} {
	send toolbar set height 0
	send toolButton set state 0
        send imagewin "set width $iw; set height $ih"
	set nh [expr $h - 25]
	send display "set width $w; set height $nh"
	set toolbox_up 0
	send mXflipButton map
	send mYflipButton map
	send mPrevButton map
	send mFrameButton map
	send mNextButton map
    } else {
	send toolbar set height 25
	send toolButton set state 1
        send imagewin "set width $iw; set height $ih"
	set nh [expr $h + 25]
	send display "set width $w ; set height $nh"
	set toolbox_up 1
	send mXflipButton unmap
	send mYflipButton unmap
	send mPrevButton unmap
	send mFrameButton unmap
	send mNextButton unmap
    }
}

proc panelBarToggle args \
{
    global panelbar_up

    set w  [send display get width]  ; set h  [send display get height]
    set iw [send imagewin get width] ; set ih [send imagewin get height]

    if {$panelbar_up} {
	send panelbar set height 0
	send panelButton set state 0
        send imagewin "set width $iw; set height $ih"
	set nh [expr $h - 25]
	send display "set width $w ; set height $nh"
	set panelbar_up 0
    } else {
	send panelbar set height 25
	send panelButton set state 1
        send imagewin "set width $iw; set height $ih"
	set nh [expr $h + 25]
	send display "set width $w ; set height $nh"
	set panelbar_up 1
    }
}



# Initialize the bars to be displayed if the resource was set.

send toolButton   "set bitmap tools;    addCallback toolBoxToggle"
if { ! [ true $showToolBar ] } {
    send display set height [expr [send display get height] - 25]
    send toolbar set height 0
    send toolButton set state 0
} else {
    send toolButton set state 1
    send mXflipButton unmap
    send mYflipButton unmap
    send mPrevButton unmap
    send mFrameButton unmap
    send mNextButton unmap
}

send panelButton  "deleteCallback panel"
send panelButton  "set bitmap control;  addCallback panelBarToggle"

if { ! [ true $showPanelBar ] } {
    send display set height [expr [send display get height] - 25]
    send panelbar set height 0
    send panelButton set state 0
} else {
    send panelButton set state 1
}


##############################################
# Panelbar callbacks.
##############################################

send pbQuit  addCallback Quit
send helpClose addCallback "send helpButton set state 0"


# Control Panel.
#------------------------------------------------------
proc pbToggleControl {name element op} \
{
    upvar $name panel_up
    send pbDisplayP set state [expr !($panel_up)]
} ; #trace variable panel_up w pbToggleControl

proc pbResetPanel {param old new} \
{
    global displayPanner displayMagnifier displayCoords
    switch $new {
    done	{ send pbPanM set state [true $displayPanner]
		  send pbMagM set state [true $displayMagnifier]
		  send pbWcsM set state [true $displayCoords]
		}
    }
} ; send initialize addCallback pbResetPanel

set WidgetToTab(pbDisplayP)	display_panel
set WidgetToTab(pbInfoP)	info_panel
set WidgetToTab(pbLoadP)	load_panel
set WidgetToTab(pbPrintP)	print_panel
set WidgetToTab(pbSaveP)	save_panel
set WidgetToTab(pbTileP)	tile_panel
set WidgetToTab(pbCoordP)	wcs_panel

set TabToWidget(display_panel)	pbDisplayP
set TabToWidget(print_panel)	pbPrintP
set TabToWidget(load_panel)	pbLoadP
set TabToWidget(save_panel)	pbSaveP
set TabToWidget(info_panel)	pbInfoP
set TabToWidget(tile_panel)	pbTileP
set TabToWidget(wcs_panel)	pbCoordP

set pbTabTop	"pbDisplayP"

proc pbPanelDismiss args \
{
    global tabTop TabToWidget WidgetToTab
    set panel $TabToWidget($tabTop)
    send $panel set state 0
} ; send panelClose addCallback pbPanelDismiss

proc pbResizeCB {widget event a b c d e args} \
{
    global pbTabTop TabToWidget WidgetToTab
    if { $a == 0 && $b == 0 && $c == 0 && $d == 0 && $e == 0 } {
	send $pbTabTop set state 0
	set new $TabToWidget($widget)
	send $new set state 1
	set pbTabTop $new
    }
} ; foreach w $cpTabs {send $w addEventHandler pbResizeCB exposureMask}

proc pbPanelTabs { widget type state args } \
{
    global pbTabTop tabTop panel_up
    global TabToWidget WidgetToTab

    set panel $WidgetToTab($widget)
    if {$tabTop == $panel && $panel_up} {
        send panelShell unmap
        send $widget set state 0
        set panel_up 0
        return
    }

    send $TabToWidget($tabTop) set state 0
    set tabTop $panel
    set pbTabTop $TabToWidget($panel)
    send panelTabs setTop $panel

    # Now fire it up if it's not already open.
    if {$panel_up == 0} {
        send $widget set state 1
        send panelShell map
        set panel_up 1
    }
}
foreach w { pbDisplayP pbInfoP pbLoadP pbPrintP pbSaveP pbTileP pbCoordP } {
        send $w addCallback pbPanelTabs
}



# Load Panel.  (Really need to clean this up.)
#------------------------------------------------------
proc pbDoLoadOptions { param old new } \
{
    set val     [ join [lrange $new 1 end] " " ]
    switch [lindex $new 0] {
    newfile     { send fnameText set string $val }
    }
} ; send loadOptions addCallback pbDoLoadOptions

proc pbFileLoad { widget mode fname args } \
{
    set fpath [format "%s/%s" \
        [string range [send dirLabel get label] 12 end] $fname ]
    send imagewin setCursorType idle
} ; send fnameText addCallback pbFileLoad

proc pbflSelectPrint {widget cbtype selections args} \
{
    foreach selection $selections {
	;
    }
}; #send imageList addCallback pbflSelectPrint

proc pbFileLoadB args \
{
    set fname [send fnameText get string]
    if {$fname != ""} {
        set fpath [format "%s/%s" \
            [string range [send dirLabel get label] 12 end] $fname ]
    }
} ; send filesLoadButton addCallback pbFileLoadB


# Panner Marker.
#------------------------------------------------------
proc pbTracePanner {name element op} \
{
    global last_compass

    catch {
        upvar $name panner_enable
        send pbPanM set state $panner_enable

	if { $panner_enable } {
	    send tbCompass "setSensitive True  ; set state $last_compass"
	    drawCompass
	} else {
	    send tbCompass "setSensitive False ; set state False"
	    eraseCompass
	}
    }
} ; trace variable panner_enable w pbTracePanner

proc pannerPanel args \
{
    global panner_enable displayPanner

    setPanner [expr !$panner_enable]
    send pannerButton set on [expr $panner_enable]
}; send pbPanM  addCallback pannerPanel


# Magnifier Marker.
#------------------------------------------------------
proc pbTraceMagnifier {name element op} \
{
    upvar $name magnifier_enable
    send pbMagM set state $magnifier_enable
} ; trace variable magnifier_enable w pbTraceMagnifier

proc magnifierPanel args \
{
    global magnifier_enable displayMagnifier
    setMagnifier [expr !$magnifier_enable]
    send magnifierButton set on  [expr $magnifier_enable]
}; send pbMagM addCallback magnifierPanel


# CoordsBox Marker.
#------------------------------------------------------
proc pbTraceCoordsBox {name element op} \
{
    upvar $name track_enable
    send pbWcsM set state $track_enable
} ; trace variable track_enable w pbTraceCoordsBox

proc wcsPanel args \
{
    global track_enable
    setTrack [expr !$track_enable]
}; send pbWcsM  addCallback wcsPanel



# WPIX ISM Callbacks.
#------------------------------------------------------

proc altIsmToggle { widget type state args } \
{
    ismToggle pbIsm junk $state
} ; send pbIsm addCallback altIsmToggle

proc pbTraceIsm {name element op} \
{
    upvar $name ism_enable
    send pbIsm set state $ism_enable
} ; trace variable ism_enable w pbTraceIsm


proc altIsmMsgCB { param old new } \
{
    set cmd  [lindex $new 0]                            ;# command name
    set ism  [lindex $new 1]                            ;# determine ISM name
    set argv [lrange $new 2 end]                        ;# get args
    set argc [llength $argv]

    switch  $cmd {
    deliver  { ${ism}_alt_msg $argc $argv           }
    }
} ; send ism_msg addCallback altIsmMsgCB


proc wcspix_alt_msg { argc argv } \
{
    set arg [string trimleft [ string trimright [lindex $argv 0] ] ]
    set cmd [lindex $arg 0]
    switch $cmd {
    startup	{ }
    shutdown	{ }
    disable	{ send pbIsm "set state False ; setSensitive False" }
    capable	{ send pbIsm "setSensitive True" }
    }
}



##############################################
# Toolbar callbacks.
##############################################

send tbNormalize	addCallback normalize
send tbInvert		addCallback cpInvert
send tbRegister		addCallback cpRegisterFrames
send tbMatchLUT		addCallback cpMatchFrames


# Frame Selection.
#------------------
createMenu mFrameMenu mFrameButton $frameMenuDescription

proc altFrameChanged {param old new} \
{
    send mFrameButton set label $new
} ; send frame addCallback altFrameChanged


# Image Flipping.
#------------------------------------------------------
proc tbSetFlip {param old new} \
{
    if {$param == "xflip"} {
        send mXflipButton set state [true $new]
    } else {
        send mYflipButton set state [true $new]
    }
}; foreach i {xflip yflip} { send $i addCallback tbSetFlip }


# Zoom/Pan buttons.
#------------------------------------------------------
proc tbZoomAction { widget args } \
{
    global frameWidth frameHeight

    switch $widget {
    tbZoom0   { cpZoom 1 1 fixed }
    tbZoomIn  { cpZoom 2.0 2.0 relative }
    tbZoomOut { cpZoom 0.5 0.5 relative }
    tbCenter  { send client pan [expr $frameWidth/2.0] [expr $frameHeight/2.0] }
    }
}
foreach widget { tbCenter tbZoomIn tbZoom0 tbZoomOut } {
    send $widget addCallback tbZoomAction
}


# Frame Blink.
#------------------------------------------------------
proc tbSetBlinkRate {w args} \
{
    if {$w == "tbBlinkInc"} {
	cpSetBlinkRate BRincrease
    } else {
	cpSetBlinkRate BRdeccrease
    }
}
foreach w  {tbBlinkDec tbBlinkInc} { send $w addCallback tbSetBlinkRate }

proc tbBlink { widget args } \
{
    global blinkRate blinkId

    if {$blinkRate < 0.01} {
	send tbBlink set state 0
    } else {
        if {$widget != "tbBlink"} {
            if {($blinkId != 0) != [send $widget get on]} {
                toggleBlink
            }
        } else {
            if {($blinkId != 0) != [send $widget get state]} {
                toggleBlink
            }
        }
    }
} ; send tbBlink addCallback tbBlink

proc tbTraceBlink {name element op} \
{
    upvar $name blinkId
    send tbBlink set state [expr $blinkId != 0]
} ; trace variable blinkId w tbTraceBlink


# Auto-register.
#------------------------------------------------------
proc tbAutoRegister { widget type state args } \
{
    send tbAutoReg set state $state
}
foreach w {autoregButton brAregButton tbAutoReg} {
    send $w addCallback cpAutoRegister
}

proc tbToggleAutoReg args \
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


# Frame Tiles.
#------------------------------------------------------
proc tbSetTileFrames args \
{
    global tileOpt

    # Send the option to the client.
    if {[send tbTile get state ]} {
	if {$tileOpt == 0} {
	    # If mode is disabled, turn on the tile selection.
            selectTileOrientation junk junk Best
	} else {
	    # Otherwise, turn on in current mode.
            selectTileOrientation junk junk [tileSelToLabel $tileOpt]
	}
    } else {
        selectTileOrientation junk junk Disabled
    }
}; send tbTile addCallback tbSetTileFrames

proc tbTrackTileFrames {param old new} \
{
    send tileFramesButton set on [true $new]
    send tbTile set state $new
}; send tileFrames addCallback tbTrackTileFrames

proc tbTileFramesToggle args \
{
    set value [send tileFramesButton get on]
    if {$value} { set not 0 } else { set not 1 }

    send tileFramesButton set on [expr $not]
    send tbTile set on [expr $not]
    cpSetTileFrames
}


# Compass Indicator.
#------------------------------------------------------
proc tbToggleCompass { widget type state args } \
{
    global frame

    if {$state} {
	send compass set on True
	send tbCompass set state 1
        drawCompass
    } else {
	send compass set on False
	send tbCompass set state 0
        eraseCompass
    }
} ; foreach w {compass tbCompass} { send $w addCallback tbToggleCompass }


