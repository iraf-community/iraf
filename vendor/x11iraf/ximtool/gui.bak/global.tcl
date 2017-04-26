

##############################################################################
#  Utility Procedures.
##############################################################################

# Utility procedure to test True/False strings in resources.
proc true {v} {expr {$v == "true" || $v == "True" || $v == "TRUE"}}

# Utility functions.
proc min {a b} { expr {($a < $b) ? $a : $b} }
proc max {a b} { expr {($a > $b) ? $a : $b} }

# Global variables.
set version	"NOAO/IRAF XImtool Version 1.3EXPORT"

set winWidth 	[send imagewin get width ]	;# display window width
set winHeight 	[send imagewin get height]	;# display window height
set appWidth 	[send display  get width ]	;# application window width
set appHeight 	[send display  get height]	;# application window height
set marker 		none			;# selected marker
set markno 		0			;# used to name new markers
set ruler 		none			;# selected ruler
set ruleno 		0			;# used to name new rulers
set blinkFrames 	"1 2"			;# list of blink frames
set auto_reg 		0

set panel_up 		0			;# control panel mapped
set help_up 		0			;# help panel mapped
set ism_enable		0			;# ISM is running
set ism_capable		1			;# Client is ISM capable
set frameCache(0)	""			;# ISM frame cache

set ctype		"equatorial"		;# default coord type
set eqtype		"fk5"			;# default equatorial type

# Global constants.
set MAX_FRAMES 		16			;# max frame buffers

# TCL constants
set tcl_precision 	8



# Window resize callbacks.
proc winResize {w width height} { 
    global winWidth winHeight

    if {$width <= 1 || $height <= 1} \
	return

    set winWidth  $width
    set winHeight $height 
} ; send imagewin addCallback winResize resize

proc appResize {w width height} \
{
    global doHcut doVcut cutXPos cutYPos
    global appWidth appHeight

    set appWidth $width
    set appHeight $height 

    catch {
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
    }
} ; #send imagewin addCallback appResize resize



# Additional global variables, taking default values from resources.
getResources {
    { zoomfactors }
    { displayCoords }
    { displayPanner }
    { displayMagnifier }
    { blinkRate }
    { pannerArea }
    { pannerGeom }
    { magnifierArea }
    { magnifierGeom }
    { wcsboxGeom }
    { maxContrast }
    { showToolBar }
    { showPanelBar }
    { warnings }
    { centerBoxSize }
    { peakCentroid }
    { highlight }
}

set warnings [true $warnings]
set defaultBlinkRate $blinkRate

# Client state variables (UI parameter objects).  Certain of these parameters
# we mirror in Tcl variables here, updating the values with a callback when
# the parameter value changes.  Others require special callbacks.

set frame       1		;# current display frame
set nframes     0		;# number of frame buffers
set frames {1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16} ;# list of image frames
set frameWidth  0		;# frame buffer width, pixels
set frameHeight 0		;# frame buffer height, pixels
set frameDepth  8		;# frame buffer pixel size, bits
set cursorMode  0		;# true when cursor read pending

foreach i $frames {
    set frameZoomX($i) 0	;# X zoom factor
    set frameZoomY($i) 0	;# Y zoom factor
    set frameCenterX($i) 0	;# X center of field
    set frameCenterY($i) 0	;# Y center of field
    set frameScaleX($i) 0	;# X scale factor
    set frameScaleY($i) 0	;# Y scale factor
    set frameOffsetX($i) 0	;# X register offset
    set frameOffsetY($i) 0	;# Y register offset
    set enhancement($i) none	;# colortable enhancement
}


#trace variable frameOffsetX w debug_pvar	;# Debug stuff
#trace variable frameOffsetY w debug_pvar
#trace variable frameZoomX w debug_pvar
#trace variable frameZoomY w debug_pvar
#trace variable frameScaleX w debug_pvar
#trace variable frameScaleY w debug_pvar

proc debug_pvar { name element op } \
{
    if {$element != ""} {
	set name ${name}($element)
    }
    upvar $name x
    puts "Variable $name set to $x"
}


