
# Cursor positioning routines
#----------------------------

proc move_cursor { xstep ystep args } \
{
    set raster 0
    send imagewin getCursorPos rx ry
    send imagewin setCursorPos [expr $rx + $xstep] [expr $ry + $ystep] $raster
}


# Called when the number of frames changes.
proc setNFrames {param old new} \
{
    global frameMenuDescription nframes frames

    set nframes $new
    if {$old != $new} {
	foreach i {prevButton nextButton} {
	    send $i set sensitive [expr "$nframes > 1"]
	}

	if {$nframes > 0} {

	    # Creates the Frames menu on the main image window.
	    set items { }
	    for {set i 1} {$i <= $nframes} {incr i} {
		set l [format "%2d" $i]
		lappend  items "$l f.exec \{send client setFrame $i\} sensitive \{[expr \"$nframes >= $i\"]\}"
	    }
	    editMenu frameMenu frameButton $items

	    # Create the menu for the blink frames list.
	    set items { }
	    for {set i 0} {$i <= $nframes} {incr i} {
		set j [expr ($i + 1)]
		if {$i == 0} {
	   	    set s "\"none\" f.exec \{send brFrameBTN set label \" \"\}"
		    lappend items $s
		} else {
		    set l [format "%2d" $i]
		    lappend items "$l f.exec \{send brFrameBTN set label $l\} sensitive \{[expr \"$nframes >= $i\"]\}"
		}
	    }
	    for {set i 1} {$i <= $nframes} {incr i} {
		send brFrame$i setSensitive True
		send tFrame$i  setSensitive True
	        regsub -all BTN $items $i nmenu
		editMenu frame${i}Menu brFrame$i $nmenu
	    }

	} else {
	    editMenu frameMenu frameButton $frameMenuDescription

	    for {set i 1} {$i <= $nframes} {incr i} {
		send brFrame$i setSensitive True
		send tFrame$i  setSensitive True
		editMenu frame${i}Menu brFrame$i $frameMenuDescription
	    }
	}
    }

    for {set i 1} {$i <= 16} {incr i} {
	if {$i <= $nframes} {
	    send frame$i map
	} else {
	    send frame$i unmap
	}
    }

    if {$nframes > 2} {
        setAllTileFrames
    }
}; send nframes addCallback setNFrames


# Set the default main window frame menu.
set frameMenuDescription {
    {" 1" f.exec "send client setFrame 1"  sensitive {[expr "$nframes >= 1"]} }
    {" 2" f.exec "send client setFrame 2"  sensitive {[expr "$nframes >= 2"]} }
    {" 3" f.exec "send client setFrame 3"  sensitive {[expr "$nframes >= 3"]} }
    {" 4" f.exec "send client setFrame 4"  sensitive {[expr "$nframes >= 4"]} }
}; createMenu frameMenu  frameButton  $frameMenuDescription




# Initialize the frame lists panels (blink panel and tile tab).
for {set i 1} {$i <= 16} {incr i} {
    send brFrame$i set label [format "%2d" $i]
    send tFrame$i  set label [format "%2d" $i]
    if {$i <= 4} {
	send brFrame$i setSensitive True
	send tFrame$i  "setSensitive True ; set state 1"
    } else {
	send brFrame$i setSensitive False
	send tFrame$i  "setSensitive False ; set state 0"
    }
    createMenu frame${i}Menu brFrame$i $frameMenuDescription
}


# Called when the frame being displayed changes.
proc frameChanged {param old new} \
{
    global frame

    set frame $new
    send frameButton set label [format "%2d" $frame]

    # The first time we request frame 5 or higher reset 
    # the extra frame buttons on the control panel to make
    # them visible.
    if {$new >= 5} {
	send frlistBox set width 49
    }
    wcsFmtFBConfig
    drawCompass
}; send frame addCallback frameChanged


# Called when the frame buffer configuration changes.
proc setFrameSize {param old new} \
{
    global frameWidth frameHeight frameDepth
    set frameWidth [lindex $new 0]
    set frameHeight [lindex $new 1]
    set frameDepth [lindex $new 2]

    wcsFmtFBConfig
}; send frameSize addCallback setFrameSize

# Called when the current frame is zoomed or panned.
proc setFrameView {param old new} \
{
    global frameZoomX frameZoomY frameCenterX frameCenterY
    global frameScaleX frameScaleY frameOffsetX frameOffsetY
    global frame auto_reg blinkFrames

    # Update the position.
    set frameZoomX($frame)   [lindex $new 0]
    set frameZoomY($frame)   [lindex $new 1]
    set frameCenterX($frame) [lindex $new 2]
    set frameCenterY($frame) [lindex $new 3]
    set frameScaleX($frame)  [lindex $new 4]
    set frameScaleY($frame)  [lindex $new 5]
    set frameOffsetX($frame) [lindex $new 6]
    set frameOffsetY($frame) [lindex $new 7]

    # If auto-registering is enabled, do it now, but only when we're updating
    # the current display frame, and only if that frame is in the framelist.

    if {$auto_reg == 1 && $frame == [send frameButton get label]} {
	if {[string first $frame $blinkFrames] != -1} {
	    send client offsetRegister \{$blinkFrames\}
	}

	foreach f $blinkFrames {
	    if {$f != $frame} {
    	        set frameZoomX($f)   $frameZoomX($frame)
    	        set frameZoomY($f)   $frameZoomY($frame)
    	        set frameCenterX($f) $frameCenterX($frame)
    	        set frameCenterY($f) $frameCenterY($frame)
    	        set frameScaleX($f)  $frameScaleX($frame)
    	        set frameScaleY($f)  $frameScaleY($frame)
	    }
	}
    }
    deleteAllRulers

}; send frameView addCallback setFrameView


# Called when the color enhancement for a frame changes.
proc setEnhancement {param old new} \
{
    global enhancement
    set enhancement([lindex $new 0]) [lrange $new 1 end]
}; send enhancement addCallback setEnhancement

# Called when the frame title changes (e.g. frame change or new frame loaded).
proc setTitle {param old new} \
{
    set lab  [string trimright $new]
    send imageTitle set label $lab

    set image [lindex [ split $lab '-'] 0 ]
    wcsFmtImname $image

    set title [lindex [ split $lab '-'] 1 ]
    wcsFmtImtitle $title
    deleteAllRulers
}; send frameTitle addCallback setTitle

# Called when the image is flipped in an axis.
proc setFlip {param old new} \
{
    if {$param == "xflip"} { 
	send xflipButton set state [true $new]
    } else {
	send yflipButton set state [true $new]
    }
    deleteAllRulers
}; foreach i {xflip yflip} { send $i addCallback setFlip }


# Various general callbacks.

proc Quit args \
{
    global ism_enable

    if {$ism_enable} {
        catch { send wcspix quit }
    }
    send client Quit 			  
}
proc nextFrame args	    { send client nextFrame 		  }
proc prevFrame args	    { send client prevFrame 		  }
proc setColormap { mapno }  { send client setColormap $mapno 	  }
proc xflip  args	    { send client flip x   ; flipRegister }
proc yflip  args	    { send client flip y   ; flipRegister }
proc xyflip args	    { send client flip x y ; flipRegister }
proc flipRegister args \
{
    global auto_reg frame blinkFrames

    if {$auto_reg == 1 && $frame == [send frameButton get label]} {
	if {[string first $frame $blinkFrames] != -1} {
	   send client offsetRegister \{$blinkFrames\}
	}
    }
}



# Initialize bitmaps.
createBitmap xflip 16 16 {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x08, 0x18, 0x18,
   0x1c, 0x38, 0xfe, 0x7f, 0xfe, 0x7f, 0x1c, 0x38, 0x18, 0x18, 0x10, 0x08,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}

createBitmap yflip 16 16 {
   0x00, 0x00, 0x80, 0x01, 0xc0, 0x03, 0xe0, 0x07, 0xf0, 0x0f, 0x80, 0x01,
   0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0xf0, 0x0f,
   0xe0, 0x07, 0xc0, 0x03, 0x80, 0x01, 0x00, 0x00}

createBitmap qmark 16 16 {
   0x00, 0x00, 0x00, 0x00, 0xf0, 0x07, 0xf8, 0x0f, 0x18, 0x0c, 0x18, 0x0c,
   0x18, 0x0e, 0x00, 0x07, 0x80, 0x03, 0x80, 0x01, 0x80, 0x01, 0x00, 0x00,
   0x80, 0x01, 0x80, 0x01, 0x00, 0x00, 0x00, 0x00}

createBitmap larrow 16 16 {
   0x00, 0x00, 0x00, 0x03, 0x80, 0x03, 0xc0, 0x03, 0xe0, 0x1e, 0x70, 0x1e,
   0x38, 0x18, 0x1c, 0x18, 0x1c, 0x18, 0x38, 0x18, 0x70, 0x1e, 0xe0, 0x1e,
   0xc0, 0x03, 0x80, 0x03, 0x00, 0x03, 0x00, 0x00}

createBitmap rarrow 16 16 {
   0x00, 0x00, 0xc0, 0x00, 0xc0, 0x01, 0xc0, 0x03, 0x78, 0x07, 0x78, 0x0e,
   0x18, 0x1c, 0x18, 0x38, 0x18, 0x38, 0x18, 0x1c, 0x78, 0x0e, 0x78, 0x07,
   0xc0, 0x03, 0xc0, 0x01, 0xc0, 0x00, 0x00, 0x00}

createBitmap panel 16 16 {
   0x00, 0x00, 0xf8, 0x1f, 0xf8, 0x1f, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18,
   0x98, 0x19, 0x98, 0x19, 0x98, 0x19, 0x98, 0x19, 0x98, 0x19, 0x18, 0x18,
   0x18, 0x18, 0x18, 0x18, 0xf8, 0x1f, 0xf8, 0x1f}

createBitmap brightness 15 15 {
   0x00, 0x00, 0x80, 0x00, 0x84, 0x10, 0xe8, 0x0b, 0x10, 0x04, 0x08, 0x08,
   0x08, 0x08, 0x0e, 0x38, 0x08, 0x08, 0x08, 0x08, 0x10, 0x04, 0xe8, 0x0b,
   0x84, 0x10, 0x80, 0x00, 0x00, 0x00}

createBitmap contrast 15 15 {
   0x00, 0x00, 0x00, 0x00, 0xc0, 0x01, 0x30, 0x07, 0x08, 0x0f, 0x08, 0x0f,
   0x04, 0x1f, 0x04, 0x1f, 0x04, 0x1f, 0x08, 0x0f, 0x08, 0x0f, 0x30, 0x07,
   0xc0, 0x01, 0x00, 0x00, 0x00, 0x00}

createBitmap bar 10 10 {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x7e, 0x00, 0x42, 0x00, 0x7e, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}

createBitmap dot 16 16 {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xc0, 0x01,
   0xe0, 0x03, 0xf0, 0x07, 0xf0, 0x07, 0xf0, 0x07, 0xe0, 0x03, 0xc0, 0x01,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}

createBitmap null 16 16 {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}

createBitmap solid 64 24 {
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}

catch { send controlButton "set bitmap panel; addCallback panel" }
catch { send helpButton    "set bitmap qmark; addCallback Help"  }
send xflipButton "set bitmap xflip;	 addCallback xflip"
send yflipButton "set bitmap yflip;	 addCallback yflip"
send prevButton  "set bitmap larrow; addCallback prevFrame"
send nextButton  "set bitmap rarrow; addCallback nextFrame"
send blinkPanel  "set bitmap panel"



# WINDOW the current frame.
set old_cm_x	0
set old_cm_y	0

proc windowColormap {x y} \
{
    global winWidth winHeight maxContrast old_cm_x old_cm_y

    # For efficiency we won't compute the sqrt of the distance but instead
    # just compare the square.  Use a threshold of 6 pixels so when we
    # narrow in on the ROI we refine the colormap more precisely.

    set diff  [expr (($old_cm_x - $x) * ($old_cm_x - $x)) + \
                   (($old_cm_y - $y) * ($old_cm_y - $y))]

    # Keep the old position.
    set old_cm_x $x
    set old_cm_y $y
    if {$diff > 36} {
       return
    }

    send client windowColormap \
	[expr "double($x) / $winWidth"] \
	[expr "(double($y) - $winHeight / 2.0) / $winHeight * \
	    $maxContrast * 2.0"]
}

proc updateColormap {x y} \
{
    global winWidth winHeight maxContrast old_cm_x old_cm_y

    set old_cm_x $x
    set old_cm_y $y
    send client updateColormap \
	[expr "double($x) / $winWidth"] \
	[expr "(double($y) - $winHeight / 2.0) / $winHeight * \
	    $maxContrast * 2.0"]
}



# WINDOW the current frame, but only one color at a time.
proc windowRGB {color x y save_flag} \
{
    global winWidth winHeight maxContrast

    send client windowRGB $color \
	[expr "double($x) / $winWidth"] \
	[expr "(double($y) - $winHeight / 2.0) / $winHeight * \
	    $maxContrast * 2.0"] $save_flag
}


# ZOOM and PAN.
set xcen 0
set ycen 0
foreach i $frames {set zoomindex($i) 0}
set nzoomfactors 0
foreach i $zoomfactors {
    set zoomfactor($nzoomfactors) $i
    incr nzoomfactors
}

# Zoom or pan image at given center.
proc zoom {x y} \
{
    global xcen ycen frame
    global zoomindex zoomfactor
    global nzoomfactors

    set rx $x;  set ry $y
    set raster 0

    # Convert raw screen coordinates to frame buffer raster coordinates.
    send imagewin unmapPixel $x $y raster rx ry

    # Select a pixel.
    set rx [expr "int ($rx)"]
    set ry [expr "int ($ry)"]

    # If the pointer did not move (much) zoom the image, otherwise
    # pan it.

    send imagewin setCursorType busy
    if {sqrt(pow($x-$xcen, 2) + pow($y-$ycen, 2)) < 4} {
	set zoomindex($frame) [expr [incr zoomindex($frame)] % $nzoomfactors]
	set mag $zoomfactor($zoomindex($frame))
	send client zoom $mag $mag $rx $ry
    } else {
	send client pan $rx $ry
	set xcen $x
	set ycen $y
    }

    # Move the pointer so that it tracks the object feature the user
    # selected.

    send imagewin setCursorPos $rx $ry $raster
    send imagewin getCursorPos xcen ycen
    send imagewin setCursorType idle
}


# Zoom using a marker to indicate the region to be displayed.
proc zoomMarker {marker aspect} \
{
    global xcen ycen frame
    global winWidth winHeight
    global zoomindex nzoomfactors
    global auto_reg frame frameOffsetX frameOffsetY

    # getRegion returns: "rectangle raster x y width height rotangle".
    set region [send $marker getRegion unmap]

    set raster [lindex $region 1]
    set xcen [expr "int([lindex $region 2]) + 0.5"]
    set ycen [expr "int([lindex $region 3]) + 0.5"]
    set snx  [expr "[lindex $region 4] * 2"]
    set sny  [expr "[lindex $region 5] * 2"]

    # Compute the magnification ratio.
    set xmag [expr "$winWidth / $snx"]
    set ymag [expr "$winHeight / $sny"]
    if {$aspect == "equal"} {
	set mag [expr "($xmag < $ymag) ? $xmag : $ymag"]
	set xmag $mag;  set ymag $mag
    }

    # Zoom the image.
    send client zoomAbs \
	$xmag $ymag $xcen $ycen \
	$frameOffsetX($frame) $frameOffsetY($frame)

    # The following causes a button2 to redisplay the full image.
    send imagewin setCursorPos $xcen $ycen $raster
    send imagewin getCursorPos xcen ycen
    set zoomindex($frame) [expr "$nzoomfactors - 1"]
}

# Pan the display frame one width/height in a given direction.
proc moveFrame { xs ys args } \
{
    global winWidth winHeight frame
    global frameWidth frameHeight
    global frameZoomX frameZoomY frameCenterX frameCenterY
    global frameOffsetX frameOffsetY

    # Get the step size for the new position.
    set xstep_size [ expr "$xs * $winWidth / $frameZoomX($frame)" ]
    set ystep_size [ expr "$ys * $winHeight / $frameZoomY($frame)" ]

    # Set the boundaries so we only move up to the edges.
    set xl [ expr "($winWidth / $frameZoomX($frame)) / 2 + 1" ]
    set yl [ expr "($winHeight / $frameZoomY($frame)) / 2 + 1" ]
    set xu [ expr "$frameWidth - $xl" ]
    set yu [ expr "$frameHeight - $yl" ]

    # Set the new center position.
    set nxc [ expr "$frameCenterX($frame) + $xstep_size" ]
    set nyc [ expr "$frameCenterY($frame) + $ystep_size" ]
    if {$nxc < $xl} { set nxc $xl }
    if {$nxc > $xu} { set nxc $xu }
    if {$nyc < $yl} { set nyc $yl }
    if {$nyc > $yu} { set nyc $yu }

    # Finally, send the command to move the frame.
    send client zoomAbs \
	$frameZoomX($frame) $frameZoomY($frame) $nxc $nyc \
	$frameOffsetX($frame) $frameOffsetY($frame)
}

proc resetView {param old new} {
    global zoomindex xcen ycen frames
    global frameWidth frameHeight

    if {$new == "done"} {
	foreach i $frames {
	    send client setFrame $i
	    set xcen [expr $frameWidth / 2]
	    set ycen [expr $frameHeight / 2]
	    send client zoom 1 1 $xcen $ycen
	    set zoomindex($i) 0
	    send client setColormap Grayscale
	    normalize
	}
	send client setFrame 1
    }
}; #send initialize addCallback resetView


# CURSOR READ stuff.
proc setCursorMode {param old new} \
{
    global cursorMode

    if {$new == "on"} {
	send imagewin "activate; setCursorType ginMode"
	set cursorMode 1
    } elseif {$new == "off"} {
	send imagewin "setCursorType idle; deactivate"
	set cursorMode 0
    }
}

proc keyInput {widget event sx sy data} \
{
    global cursorMode frame

    if {!$cursorMode || $event != "keyPress"} \
	return
    if {[lindex $data 0] == "??"} \
	return

    # Convert raw screen coordinates to raster pixel coordinates.
    send imagewin unmapPixel $sx $sy raster rx ry

    # Return the cursor value and exit cursor mode.
    send client retCursorVal $rx $ry $frame 1 [lindex $data 0]
}

proc resetCursorMode args {
    global cursorMode frame
    if {$cursorMode} {
	send imagewin getCursorPos x y
	send client retCursorVal $x $y $frame 1 ^D
    }
}; send initialize addCallback resetCursorMode

send cursorMode addCallback setCursorMode
send imagewin   addCallback keyInput input


# MARKER stuff.  The active marker is determined by the global variable
# "marker", which is the marker the pointer is in, or which the pointer
# was most recently in.

# Translations when pointer is inside marker.
set markerTranslations { \
!Shift <Btn1Motion>:	m_rotateResize()
      <Btn1Motion>:	m_moveResize()
 !Shift <Btn1Down>:	m_raise()  m_markpos()
	<Btn1Down>:	m_raise()  m_markposAdd()
	  <Btn1Up>:	m_redraw() m_destroyNull()
	<Btn2Down>:	m_lower()
	<Btn3Down>:	popup(markerMenu)
	  <Btn3Up>:	popdown(markerMenu)
      !Ctrl <Key>b:     call(prevFrame,$name)
      !Ctrl <Key>f:     call(nextFrame,$name)
      !Ctrl <Key>h:     call(move_cursor,-1,0)
      !Ctrl <Key>j:     call(move_cursor,0,1)
      !Ctrl <Key>k:     call(move_cursor,0,-1)
      !Ctrl <Key>l:     call(move_cursor,1,0)
      !Ctrl <Key>n:     call(normalize)
      !Ctrl <Key>c:     call(cpZoomAction,centerFrame)
      !Ctrl <Key>i:     call(cpInvert)
      !Ctrl <Key>m:     call(cpMatchFrames)
      !Ctrl <Key>r:     call(cpRegisterFrames)
      !Ctrl <Key>p:     call(togglePanner)
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
	<KeyPress>:	m_input()
	  <Motion>:	track-cursor() call(wcsUpdate,$x,$y) call(magnifierMapImage,$x,$y)
}

# Popup menu in effect when inside marker.
createMenu markerMenu imagewin {
    {	Marker		f.title			}
    {			f.dblline		}
    {	Zoom		f.exec {
			    zoomMarker $marker equal
			    send $marker destroy
			}			}
    {	Fill		f.exec {
			    zoomMarker $marker fill
			    send $marker destroy
			}			}
    {	Print		f.exec {
			    send $marker getRect interior x0 y0 nx ny
		            setPrintCorners $x0 [expr $y0 + $ny -1] \
		                [expr $x0 + $nx -1] $y0
			    send client print $x0 $y0 $nx $ny
			}			}
    {	Save		f.exec {
		            send imagewin setCursorType busy
			    send $marker getRect interior x0 y0 nx ny
			    send client save $x0 $y0 $nx $ny
		            send imagewin setCursorType idle
			}			}
    {	Info		f.exec {
			    send infoText append \
				[format "%s\n" [send $marker getRegion unmap]]
			}			}
    {	Unrotate	f.exec {
			    send $marker setAttribute rotangle 0
			}			}
    {			f.dblline		}
    {	Color		f.menu markerColor	}
    {	Type		f.menu markerType	}
    {			f.dblline		}
    {	Destroy		f.exec {
			    send $marker destroy
			}			}
}

createMenu markerType markerMenu {
    {	Type		f.title			}
    {			f.dblline		}
    {	Rectangle	f.exec "m_setType $marker rectangle"	}
    {	Box		f.exec "m_setType $marker box"		}
    {	Circle		f.exec "m_setType $marker circle"	}
    {	Ellipse		f.exec "m_setType $marker ellipse"	}
    {	Polygon		f.exec "m_setType $marker polygon"	}
}

createMenu markerColor markerMenu {
    {	Color		f.title			}
    {			f.dblline		}
    {	""		f.exec "m_setColor $marker black"
			    bitmap solid foreground black }
    {	""		f.exec "m_setColor $marker white" 
			    bitmap solid foreground white }
    {	""		f.exec "m_setColor $marker red" 
			    bitmap solid foreground red }
    {	""		f.exec "m_setColor $marker green" 
			    bitmap solid foreground green }
    {	""		f.exec "m_setColor $marker blue" 
			    bitmap solid foreground blue }
    {	""		f.exec "m_setColor $marker magenta" 
			    bitmap solid foreground magenta }
    {	""		f.exec "m_setColor $marker cyan" 
			    bitmap solid foreground cyan }
    {	""		f.exec "m_setColor $marker yellow" 
			    bitmap solid foreground yellow }
}

proc m_setType {marker type}  {
    send $marker "markpos; set type $type; redraw"
}
proc m_setColor {marker color}  {
    send $marker "markpos;
	set lineColor $color;  set highlightColor $color; redraw"
}

# Callback executed when a marker gets or loses the focus.
proc selectMarker {active_marker event event_data} \
{
    global marker
    switch $event {
	focusIn		{ set marker $active_marker }
	focusOut	{  }
    }
}

# Create marker action.  Makes a new marker.
proc makeMarker {parent x y} \
{
    global markerTranslations markno
    set marker marker$markno;  incr markno

    send $parent createMarker $marker \
	type		rectangle \
	createMode	interactive \
	translations	$markerTranslations \
	x		$x \
	y		$y

    send $marker addCallback selectMarker focusIn focusOut
}


     
# WCSBOX -- Real time coordinate display.

set track_enable 	0
set last_x		0
set last_y		0

proc wcsUpdate {x y} \
{
    global track_enable frame pixtab_up tabTop
    global frameWidth frameHeight redraw_compass
    global ism_enable wcsLabels last_x last_y
    global ct_warn plotSpeed doHcut doVcut


    # If the cursor was frozen release it now.
    if {$ct_warn} {
	curtrack_destroy
    }

    # Convert screen coords to raster pixel.
    send imagewin unmapPixel $x $y raster rx ry rz

    # Set the current frame to the frame the pointer is within.
    if {$frame && $raster} {
	set track_frame [send client getFrame $raster]
	if {$frame != $track_frame} {
	    send client setFrame $track_frame
	}
    }

    # Update coords box.
    if {$raster} {
        set text [send client encodewcs $rx $ry $rz]
        scan $text "%g %g %g" nx ny nz
    } else {
        set text [format " %7.2f %7.2f %9.1f " $rx $ry $rz]
        set nx $rx ; set ny $ry ; set nz $rz
    }
	
    # Update the coords panel and pixel table.
    wcsFmtSValue $nz
    if {$pixtab_up} {
        updatePixelTable $x $y $nx $ny
    }

    # If the ISM is running update the coords box with all the selected
    # options, otherwise just write the one-line frame wcs coords.
    if {$ism_enable} {
        updateCoordsBox

    } else {
	# Update the on-screen marker.
        if {$track_enable} {
	    send wcsbox "set text \{$text\}; redraw noerase"
	}

	# Update the control panel readout (always done).
	if {$wcsLabels} {
	    set ln [format "   X: %12s     Y: %12s  WCS: Display" $nx $ny]
	} else {
	    set ln [format "      %12s        %12s       Display" $nx $ny]
	}
	send wtWcs1 set string $ln

	if {$redraw_compass} \
	    drawCompass
    }


    # Update the cut-plots if enabled.  If we're set for speed the don't 
    # track the really large cursor motions, wait till the differences are
    # small indicating a finer motion. 
    if {$doHcut || $doVcut} {
	set dist [ expr "sqrt(pow(($last_x - $x),2) + pow(($last_y - $y),2))" ]
	if {($plotSpeed && $dist < 30) || ! $plotSpeed} {
	    cutPlots $x $y
	}
    }

    # Save the position so we can track differences with last position.
    set last_x 	$x
    set last_y 	$y
}


proc setTrack {state} \
{
    global ism_enable track_enable wcsboxGeom
    global winWidth winHeight up_todo

    if {$state} {
	if {$track_enable} \
	    return

	send imagewin createMarker wcsbox {
	    type		text
	    createMode		noninteractive
	    width		27ch
	    height		1ch
	    lineWidth		0
	    imageText		true
	    textBgColor		black
	    textColor		yellow
	    visible		false
	}

	set box_width [send wcsbox get width]
	set box_height [send wcsbox get height]
	set defGeom [format "%sx%s-5-5" $box_width $box_height]
	send imagewin parseGeometry $wcsboxGeom $defGeom x y width height

	send wcsbox setAttributes    \
	    x			$x   \
	    y			$y   \
	    activated		true \
	    visible		true \
	    sensitive		true

	send wcsbox {
	    addCallback wcsboxDestroyCallback destroy
	    addCallback wcsboxMoved moveResize
	}

	send imagewin addCallback wcsboxWindowResize resize
	set track_enable 1
	send imagewin getCursorPos x y
	wcsUpdate $x $y
	magnifierMapImage $x $y

	# Turn on the option toggles on the control panel.
	foreach n {1 2 3 4} { 
	    if {[send sysWcs$n get label] != "None"} {
	        send wiWcs$n set on True
	    }
	}
	if {$ism_enable} {
	    resizeCoordsBox $up_todo
	}

    } elseif {$track_enable} {
	set track_enable 0
	send wcsbox destroy

	# Turn off the option toggles on the control panel.
	foreach w {wiWcs1 wiWcs2 wiWcs3 wiWcs4} { send $w set on False }
	resizeCoordsBox 0
    }
    updateCoordsBox
}

proc wcsboxDestroyCallback args \
{
    global track_enable
    send imagewin deleteCallback wcsboxWindowResize
    set track_enable 0
}

# If the window is resized make the wcsbox track the corner.
proc wcsboxWindowResize args \
{
    global track_enable
    global wcsboxGeom

    if {$track_enable} {
	# Get new location.
	set box_width [send wcsbox get width]
	set box_height [send wcsbox get height]
	set defGeom [format "%sx%s-5-5" $box_width $box_height]
	send imagewin parseGeometry $wcsboxGeom $defGeom x y width height

	# Move the marker.
	send wcsbox "\
	    deleteCallback wcsboxMoved; \
	    markpos; setAttributes x $x y $y; redraw; \
	    addCallback wcsboxMoved moveResize"
        set wcsboxGeom [send imagewin getGeometry $x $y $width $height]
    }
}

proc wcsboxMoved {marker event position} \
{
    global wcsboxGeom
    send wcsbox getRect boundary x y width height
    set wcsboxGeom [send imagewin getGeometry $x $y $width $height]
}

proc resetWcsbox {param old new} \
{
    global track_enable wcsboxGeom displayCoords
    if {$new == "done"} {
	setTrack [true $displayCoords]
    } elseif {$track_enable} {
	setTrack 0
	if {$new == "restart"} {
	    set wcsboxGeom -5-5
	}
    }
}; send initialize addCallback resetWcsbox



#---------------------
# FRAME BLINK.
#---------------------
set blinkId 0
set blinkIndex 0

proc toggleBlink args \
{
    global blinkId blinkRate blinkIndex
    global optionsMenuDescription

    if {$blinkId} {
	deleteTimedCallback $blinkId
	set blinkId 0
    } else {
	set blinkId [postTimedCallback blink [expr int($blinkRate * 1000)]]
    }

    set blinkIndex 0
    editMenu optionsMenu viewButton $optionsMenuDescription
}

proc blink args \
{
    global blinkId blinkRate blinkFrames blinkIndex

    send client setFrame [lindex $blinkFrames $blinkIndex]
    incr blinkIndex
    if {$blinkIndex >= [llength $blinkFrames]} {
	set blinkIndex 0
    }

    set blinkId [postTimedCallback blink [expr int($blinkRate * 1000)]]
}

proc resetBlink args \
{
    global blinkId
    if {$blinkId} \
	toggleBlink
}; send initialize addCallback resetBlink



# Normalize -- Reset the view parameters for the current frame.
proc normalize args \
{
    global zoomindex zoomfactor
    global frameWidth frameHeight
    global xcen ycen frame

    #set zoomindex($frame) 0
    #set xcen [expr $frameWidth / 2]
    #set ycen [expr $frameHeight / 2]
    #send client zoom 1 1 $xcen $ycen
    send client updateColormap 0.5 1.0
}


# Popdown menus.
createMenu fileMenu fileButton {
    {	"Info"		f.exec { panelTabToggle info_panel 	} }
    {	"Load"		f.exec {
			    panelTabToggle load_panel
	    		    set panel_up 1
			}					  }
    {	"Save"		f.exec { Save 				} }
    {	"Save As..."	f.exec { panelTabToggle save_panel 	} }
    {	"Print"		f.exec { 
			    send imagewin setCursorType busy
			    Print
			    send imagewin setCursorType idle
			}					  }
    {	"Print Setup"	f.exec { panelTabToggle print_panel 	} }
    {			f.dblline				  }
    {	"Reset"		f.exec {
			    # Do a hard reset.
			    send client Reset
			    #resetView initialize done done
			}					  }
    {	"Debug Panel"	f.exec { tclPanel 1			} }
    {	"Quit"		f.exec { Quit				} }
}

createMenu viewMenu viewButton {
    {	"Next frame"	f.exec nextFrame		}
    {	"Prev frame"	f.exec prevFrame		}
    {			f.dblline			}
    {	"Colormap"	f.menu cmapMenu			}
    {	"Flip"		f.menu flipMenu			}
    {			f.dblline			}
    {	"Equal aspect"	f.exec {
			    set xmag $frameZoomX($frame)
			    set ymag $frameZoomY($frame)
			    set zoom [expr ($xmag + $ymag) / 2.0]
			    cpZoom $zoom $zoom fixed
			}				}
    {	"Integer zoom"	f.exec {
			    set xmag [expr round ($frameZoomX($frame))]
			    set ymag [expr round ($frameZoomY($frame))]
			    cpZoom $xmag $ymag fixed
			}				}
    {	"Toggle zoom"	f.exec toggleZoom		}
    {	"Unzoom"	f.exec {
			    set zoomindex($frame) 0
			    set mag $zoomfactor($zoomindex($frame))
			    send client zoom $mag $mag $xcen $ycen
			}				}
}

createMenu cmapMenu viewMenu {
    {	"Normalize"	f.exec normalize	}
    {	"Invert"	f.exec cpInvert		}
    {	"Optimize"	f.exec cpOptimize	sensitive false }
    {			f.dblline		}
    {	"Grayscale"	f.exec "send client setColormap Grayscale" }
    {	"Color"		f.exec "send client setColormap Color" }
    {	"Heat"		f.exec "send client setColormap Heat" }
    {	"HSV"		f.exec "send client setColormap HSV" }
    {	"AIPS0"		f.exec "send client setColormap AIPS0" }
    {	"Halley"	f.exec "send client setColormap Halley" }
    {	"Ramp"		f.exec "send client setColormap Ramp" }
    {	"Standard"	f.exec "send client setColormap Standard" }
    {	"Staircase"	f.exec "send client setColormap Staircase" }
    {	"Rainbow1"	f.exec "send client setColormap Rainbow1" }
    {	"Rainbow2"	f.exec "send client setColormap Rainbow2" }
    {	"Random8"	f.exec "send client setColormap Random8" }
}

createMenu flipMenu viewMenu {
    {	"Flip X"	f.exec xflip		}
    {	"Flip Y"	f.exec yflip		}
    {	"Flip XY"	f.exec xyflip		}
}

set optionsMenuDescription {
    {	"Autoscale"	f.exec {
			    set value [send autoscaleButton get on]
			    send autoscaleButton set on [expr !$value]
			    cpSetAutoscale
			}			}
    {	"Antialias"	f.exec {
			    set value [send antialiasButton get on]
			    send antialiasButton set on [expr !$value]
			    cpSetAntialias
			}			}
    {	"Panner"	f.exec { setPanner [expr !$panner_enable] }	}
    {	"Magnifier"	f.exec { setMagnifier [expr !$magnifier_enable] }}
    {	"Coords box"	f.exec { setTrack [expr !$track_enable] }	}
    {	"Tile frames"	f.exec {
			    set value [send tileFramesButton get on]
			    send tileFramesButton set on [expr !$value]
			    cpSetTileFrames
			}			}
    {	"Clear frame"	f.exec clearFrame	}
    {	"Fit frame"	f.exec fitFrame		}
    {	"Match LUTs"	f.exec cpMatchFrames	}
    {	"Auto-Register"	f.exec cpAutoRegister	}
    {	"Register"	f.exec cpRegisterFrames	}
    {	{$blinkId ? "Stop blink" : "Blink frames"}
			f.exec toggleBlink	}
    {			f.dblline		}
    {   "Control Panel"	f.exec  { panelTabToggle display_panel	} }
    {   "Print Panel"	f.exec  { panelTabToggle print_panel	} }
    {   "Load Panel"	f.exec  { panelTabToggle load_panel	} }
    {   "Save Panel"	f.exec  { panelTabToggle save_panel	} }
}; createMenu optionsMenu optionsButton $optionsMenuDescription



#-------------------------------
# Frozen Cursor Warning Message
#-------------------------------

set ct_warn	0

proc curtrack_msg { x y args } \
{
    global ct_warn last_x last_y winHeight winWidth


    if {! $ct_warn == 0} {
	return
    }
        
    send imagewin createMarker curtrack_warn {
        type                text
        createMode          noninteractive
        width               25ch
        height              21
        lineWidth           0
        imageText           true
        textBgColor         red
        textColor           yellow
        visible             true
        sensitive           true
        activated           true
	x		    5
	y		    5
    }

    send curtrack_warn "set text \{  CURSOR READOUT FROZEN  \}"
    set ypos [ expr ($winHeight - 25) ]
    send curtrack_warn "markpos; setAttributes x 5 y $ypos; redraw"
    send curtrack_warn "redraw noerase"
    set ct_warn 1

    # Mark the position of the cursor.
    send imagewin createMarker x_curpos \
        type            box \
        createMode      noninteractive \
        lineColor       red \
        x               1 \
        y               $last_y \
        width           $winWidth \
        height          1 \
        activated       True \
        visible         True \
        sensitive       False

    send imagewin createMarker y_curpos \
        type            box \
        createMode      noninteractive \
        lineColor       red \
        x               $last_x \
        y               1 \
        width           1 \
        height          $winHeight \
        activated       True \
        visible         True \
        sensitive       False

    # Raise the coordinate marker so we can see where we are.
    send wcsbox raise
}


proc curtrack_destroy args \
{
    global ct_warn

    if {$ct_warn} {
	send curtrack_warn destroy
	send x_curpos destroy
	send y_curpos destroy
	set ct_warn 0
    }
}

