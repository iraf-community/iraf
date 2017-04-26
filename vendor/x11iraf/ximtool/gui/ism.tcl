

################################################
# ISM Module support routines.
################################################


# Turn the ISM on or off.
proc ismToggle { widget type state args } \
{
    global ism_enable ism_capable psize

    set ism_enable $state

    # Set the coord-panel header option availability.
    if {$ism_enable == 1 && $ism_capable} {
	send imageHeader setSensitive True
	send woptBPM setSensitive True
	catch { send client ism_start wcspix }
    } else {
	send imageHeader setSensitive False
	send woptBPM set on False
	send woptBPM setSensitive False
	catch { send client ism_stop wcspix }
	send hdr_panel unmap
    }
    setCoordPanelSensitivity

} ; send ismToggle addCallback ismToggle



# Handle messages from ISM clients and pass them on to the appropiate
# callback.

proc ism_msg { param old new } \
{
    global ism_enable 

    set cmd  [lindex $new 0]				;# command name
    set ism  [lindex $new 1]				;# determine ISM name
    set argv [lrange $new 2 end]			;# get args
    set argc [llength $argv]

    switch  $cmd {
    source   { source [lindex $new 1]		}
    alert    { Wexec client [lindex $new 1]	}
    deliver  { ${ism}_msg $argc $argv		}
    info     { ism_info $ism			}
    }
} ; send ism_msg addCallback ism_msg



# Log a client message to the info panel
set ismInfoText " " 

proc ism_info { text } \
{
    global infoMode ismInfoText

    set ismInfoText [format "%s\n%s" $ismInfoText $text]
    if {$infoMode == "infoOptIsm"} {
	send infoText set string $ismInfoText
    }
}

# Initialize the text.
proc ismInitInfoText args \
{
    global ismInfoText

    set ismInfoText ""
    ism_info  "\t    ISM Client Message Logs"
    ism_info  "\t    -----------------------"
    ism_info  " "
} ; ismInitInfoText



################################################
# WPIX module support routines.
################################################

set cur_objid		0
set cur_regid		0
set redraw_compass	0
#set Compass(0)		{ 0.0 1 1 0 X Y }
set Compass(0)		{ 0.0 0.0 1.0 -1.0 0.0 0 X Y }
set Orient(0)		{ 1 1 1 }

set wcspix_debug	0

proc wcspix_msg { argc argv } \
{
    global wcspix_debug

    set arg [string trimleft [ string trimright [lindex $argv 0] ] ]
    set cmd [lindex $arg 0]

    if {$wcspix_debug} { print "wcspix_msg:  $cmd" }


    switch $cmd {
    startup	{ wcspix_startup					}
    shutdown	{ wcspix_shutdown					}
    disable	{ wcspix_disable					}
    capable	{ wcspix_capable					}

    cache	{ wcspix_cache   [lrange $arg 1 end]			}
    uncache	{ wcspix_uncache [lrange $arg 1 end]			}
    wcstran	{ wcspix_wcstran [lrange $arg 1 end]			}
    wcslist	{ wcspix_wcslist [lrange $arg 1 end] 			}

    imghdr	{ send hdrText \
			setText [format "<pre>%s\n%s</pre>" \
			[string trimright [send hdrText getText simple] "\n"]\
			[lindex $arg 1] ]
		}
    wcshdr	{ send hdrKGText append [lindex $arg 1]			}

    wcsinfo	{ send hdrIGText append [lindex $arg 1]		
		  send hdrText   gotoId 0
		  send hdrKGText set insertPosition 0
		}

    compass	{ wcspix_compass [lrange $arg 1 end] ; drawCompass 	}
    orient	{ wcspix_orient  [lrange $arg 1 end] 			}
    wcstype	{ wcspix_wcstype [lindex $arg 1] [lindex $arg 2]	}
    wcsfmt	{ set num [lindex $arg 2]
		  send fmtWcs$num set label [lindex $arg 1] 		}

    wcspix_cmd	{ send wcsIsmCmd set string [lindex $arg 1]		}

    pixtab	{ set tab  [lindex [lindex $arg 1] 0]
		  set col  [lindex [lindex $arg 1] 1]
		  set row  [lindex [lindex $arg 1] 2]
		  set stat [lindex [lindex $arg 1] 3]
		  wcspix_pixtab $tab $col $row $stat
		}
    }
}


# Startup and initialize the wcspix module with the GUI state.
proc wcspix_startup args \
{
    global ism_enable frame
    global up_todo psize pixtab_up frameCache

    set ism_enable 1
    send ismToggle set on True
    send imageHeader setSensitive True
    send woptBPM setSensitive True
    setCoordPanelSensitivity
    ismInitInfoText

    if {$up_todo < 3} {
        resizeCoordsBox $up_todo
    }
    updateCoordsBox
    drawCompass

    # Initialize the frame cache.
    foreach c [array names frameCache] {
	if {$c != "0"} { unset frameCache($c) }
    }

    catch { 
	if {$pixtab_up} { send wcspix set psize $psize }
	for {set i 1} {$i <= 4} {incr i} {
    	    send wcspix set wcs    [send sysWcs$i get label] $i
    	    send wcspix set format [send fmtWcs$i get label] $i
	}
    }

    if { [send infoOptClients get on] } {
	send client info clients
    }
}

# Shutdown the WPIX module.
proc wcspix_shutdown args \
{
    global ism_enable frame

    set ism_enable 0
    send ismToggle set on False
    send imageHeader setSensitive False
    send woptBPM set on False
    send woptBPM setSensitive False
    setCoordPanelSensitivity
    setCoordPanelHeight

    wcsFmtIValue "N/A"
    wcsFmtBValue "0"

    resizeCoordsBox 0
    drawCompass

    if { [send infoOptClients get on] } {
	send client info clients
    }
}


# Disable the WPIX module.  We are only called when a display client has
# indicated it doesn't use the new mapping facilities and having the WPIX
# ISM visible will only confuse the user.

proc wcspix_disable args \
{
    global ism_enable ism_capable rulerWCS

    if {$ism_enable} {
        send wcspix quit
    }
    set ism_capable 0
    set rulerWCS 0
    wcspix_shutdown
    send ismToggle setSensitive False
}


# Client connected is capable of using the ISM, but don't necessarily turn
# it on at this point.

proc wcspix_capable args \
{
    global ism_capable

    set ism_capable 1
    wcsFmtIValue "0"
    wcsFmtBValue "0"
    send ismToggle setSensitive True
}


# Cache an image in the GUI.
proc wcspix_cache { argv } \
{
    global frameCache redraw_compass cur_objid

    set name	[lindex $argv 0]
    set frame	[lindex $argv 1]
    set id	[lindex $argv 2]

    # Store the image name and id in a local cache.
    lappend frameCache($frame) $name $id
    send hdrObjMenu set label $name

    # Automatically get the header.
    regsub -all {[\[]} $name  "\\\[" image
    catch { getHeader $image $id }

    setHdrObjMenu $frame

    set cur_objid $id
    set redraw_compass 1
}

# Uncache an image in the GUI.
proc wcspix_uncache { argv } \
{
    global frameCache Compass

    set id [lindex $argv 0]
    foreach c [array names frameCache] {
	set i1 0
	set i2 1
	set new { }
	while { $i2 < [llength $frameCache($c)] } {
            if {[lindex $frameCache($c) $i2] != $id} { 
		lappend new [lindex $frameCache($c) $i1]
		lappend new [lindex $frameCache($c) $i2]
            }
	    incr i1 2
	    incr i2 2
        }
	set frameCache($c) $new
    }
    if [info exists Compass($id)] {
        unset Compass($id)
    }
}

# Format the results of the WCSTRAN method.
proc wcspix_wcstran { argv } \
{
    global frameCache cur_objid cur_regid redraw_compass

    set objid  [ lindex [lindex $argv 0] 1]
    set regid  [ lindex [lindex $argv 1] 1]
    set pixval [ lindex [lindex $argv 2] 1]
    set bpmval [ lindex [lindex $argv 3] 1]

    wcsFmtIValue $pixval
    wcsFmtBValue $bpmval

    set args [lrange $argv 4 end]
    set nargs [llength $args]
    for {set i 0} {$i < 4} {incr i} {
	set coord   [lindex $args $i]
	set wcsname [lindex $coord 1]
	set xval    [lindex $coord 2]
	set yval    [lindex $coord 3]
	set xunits  [lindex $coord 4]
	set yunits  [lindex $coord 5]
	wcsFmtWcs [expr ($i + 1)] $wcsname $xval $yval $xunits $yunits
    }

    set cur_objid $objid
    set cur_regid $regid

    if {$redraw_compass} \
	drawCompass
}

# Save the object compass information.
proc wcspix_compass { argv } \
{
    global Compass

    set objid     [lindex $argv 0]
    set angle     [lindex $argv 1]

    #set xflip     [lindex $argv 2]
    #set yflip     [lindex $argv 3]
    #set transpose [lindex $argv 4]
    #set xlab      [lindex $argv 5]
    #set ylab      [lindex $argv 6]

    set north_x   [lindex $argv 2]
    set north_y   [lindex $argv 3]
    set east_x    [lindex $argv 4]
    set east_y    [lindex $argv 5]
    set transpose [lindex $argv 6]
    set xlab      [lindex $argv 7]
    set ylab      [lindex $argv 8]

    #lappend Compass($objid) $angle $xflip $yflip $transpose $xlab $ylab
    #set Compass($objid) [list $angle $xflip $yflip $transpose $xlab $ylab]
    set Compass($objid) [list $angle $north_x $north_y $east_x $east_y \
	$transpose $xlab $ylab]
    drawCompass
}

# Save the image display orientation information.
proc wcspix_orient { argv } \
{
    global Orient

    set objid [lindex $argv 0]
    set frame [lindex $argv 1]
    set xflip [lindex $argv 2]
    set yflip [lindex $argv 3]

    set Orient($objid) [list $frame $xflip $yflip]
}


# Handle the list of WCSs available for the current image.
proc wcspix_wcslist { argv } \
{
    set wcslist [lindex $argv 0]

    for {set i 1} {$i <= 4} {incr i} {
        set items {}
	foreach nam $wcslist {
	    if {[string tolower $nam] == "line"} {
	        lappend items "f.dblline"
	    } else {
	        lappend items "\"$nam\" f.exec \{setWcsSys $nam $i\}"
	    }
	}
	editMenu sysMenu$i sysWcs$i $items
    }
}

# Set the type of the WCS.
proc wcspix_wcstype { label num } \
{
    global up_todo

    send sysWcs$num set label $label
    if {[string tolower $label] == "none"} {
	send wpWcs$num "set on False ; setSensitive False"
	send wiWcs$num "set on False ; setSensitive False"
	send fmtWcs$num setSensitive False
    	set hght 4
    } else {
        send wpWcs$num "set on True  ; setSensitive True"
        send wiWcs$num setSensitive True
	send fmtWcs$num setSensitive True
    	set hght 17
    }
    send wtWcs$num set height $hght
    setCoordPanelHeight

    #resizeCoordsBox $up_todo
    updateCoordsBox
}

# Display the pixel table.
proc wcspix_pixtab { tab col row stat } \
{
    global psize

    # Update the pixel table.
    send pixtab setList [lindex $tab 1]
    send pixtab highlight [expr int(($psize * $psize) / 2)]

    # Update the table labels.
    set c [ expr int($psize / 2) ]
    send ptColLabs setList $col ; send ptColLabs highlight $c
    send ptRowLabs setList $row ; send ptRowLabs highlight $c

    # Update the pixtab stats.
    send meanValue set label [ format "%10.2f" [lindex $stat 0] ]
    send sigValue  set label [ format "%10.4f" [lindex $stat 1] ]
}


