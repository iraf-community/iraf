
################################################################################
# Image and objects list display and section
################################################################################

proc filesTextHighlight { widget event args } \
{
    if { $event == "enterNotify" } {
	send $widget set displayCaret True
    } elseif { $event == "leaveNotify" } {
	send $widget set displayCaret False
    }
}
foreach w {imtemplateText fnameText z1Value z2Value} {
    send $w addEventHandler filesTextHighlight enterWindowMask
    send $w addEventHandler filesTextHighlight leaveWindowMask
}

send rootButton     addCallback "send client setLoadOption root"
send homeButton     addCallback "send client setLoadOption home"
send upButton       addCallback "send client setLoadOption up"
send rescanButton   addCallback "send client setLoadOption rescan"

set label \
    [format "%-35.35s %6s   %12s  %12s"\
         "              Image" "Bitpix" "  Size" "          Title"]
send imlistLabel set label $label
send imlistLabel "set height 0 ; unmap"


# Create the Frames menu on the load panel window.
set loadItems { "Current f.exec \{send frameFrame set label Current\}"}
for {set i 1} {$i <= $MAX_FRAMES} {incr i} {
    lappend loadItems "\"\ \ $i\ \ \" f.exec \{send frameFrame set label $i\}"
}; createMenu loadFrames frameFrame $loadItems





# Filename pattern callback.
proc setPattern { widget mode pattern args } \
{
    send client setLoadOption pattern $pattern
} ; send imtemplateText addCallback setPattern


# Load options parameter callback.
proc doLoadOptions { param old new } \
{
    global warnings panel_up

    set val     [ join [lrange $new 1 end] " " ]

    switch [lindex $new 0] {
    pattern  	{ send imtemplateText set string [format "%s" $val ] }
    curdir   	{ send dirLabel set label [format "  Directory:    %s" $val ] }
    newfile  	{ send fnameText set string $val }
    status  	{ send filesStatus set label $val
	          if { ! $panel_up } { send imageTitle set label $val }
	          send server synchronize
	    	}
    warning 	{ if {$warnings} { Wexec server $val } 		}
    gray	{ send grayscale set on $val 			}
    zscale	{ send zscale set on $val
    		  if {$val} {
		      send zrange setSensitive False
		  } else {
		      send zrange setSensitive True
		  }
		}
    zrange	{ send zrange set on $val
    		  if {$val} {
		      foreach w {z1Label z1Value z2Label z2Value} {
		          send $w setSensitive False
		      }
		  } else {
		      foreach w {z1Label z1Value z2Label z2Value} {
		          send $w setSensitive True
		      }
		  }
		}
    z1		{ send z1Value    set string [format "%s" $val ] }
    z2		{ send z2Value    set string [format "%s" $val ] }
    nsample	{ send nsampValue set string [format "%s" $val ] }
    }
} ; send loadOptions addCallback doLoadOptions

# Option utility routines.
proc toggleGraymap args \
{
    send client setLoadOption gray [ send grayscale get on ]
} ; send grayscale addCallback toggleGraymap

proc toggleHeaders args \
{
    if { [send browseHdrs get on] } {
        send client setLoadOption headers 
	send rootButton   setSensitive False
	send homeButton   setSensitive False
	send upButton     setSensitive False
	send rescanButton setSensitive False
	send imlistLabel "set height 10 ; map"
    } else {
        send client setLoadOption rescan 
	send rootButton   setSensitive True
	send homeButton   setSensitive True
	send upButton     setSensitive True
	send rescanButton setSensitive True
	send imlistLabel "set height 0 ; unmap"
    }
} ; send browseHdrs addCallback toggleHeaders

proc toggleZscale args \
{
    send client setLoadOption zscale [send zscale get on]
} ; send zscale addCallback toggleZscale

proc toggleZrange args \
{
    send client setLoadOption zrange [send zrange get on]
} ; send zrange addCallback toggleZrange

proc setZ1 { widget mode pattern args } \
{
    send client setLoadOption z1 $pattern
} ; send z1Value addCallback setZ1

proc setZ2 { widget mode pattern args } \
{
    send client setLoadOption z2 $pattern
} ; send z2Value addCallback setZ2

proc setNsamp { widget mode pattern args } \
{
    send client setLoadOption nsample $pattern
} ; send nsampValue addCallback setNsamp

proc fileLoad { widget mode fname args } \
{
    loadImage $fname
} ; send fnameText addCallback fileLoad



# Image list selection routines.

set fileList	{}

proc setFileList { param old new } \
{
    global fileList
    set fileList $new

    # Get the max length of the strings so we can set
    # the list columns appropriately.
    set max 0
    foreach n $new {
	set len [string length $n]
	if {$len > $max} { set max $len }
    }

    # Optimize the number of columns for the file list.
    if {$max > 35}  { 
	send imageList set defaultColumns 1
    } elseif {$max > 21} { 
	send imageList set defaultColumns 2
    } elseif {$max > 12} { 
	send imageList set defaultColumns 3
    } elseif {$max > 5} { 
	send imageList set defaultColumns 4
    } else { 
	send imageList set defaultColumns 5
    }

    send imageList setList $new resize
}; send filelist addCallback setFileList

proc flResize args \
{
    global fileList
    send imageList setList $fileList resize
}; send imageList addEventHandler flResize ResizeRedirectMask

proc flSelectPrint { widget cbtype selections args } \
{
    if {! [send autoload get on]} \
	return

    foreach selection $selections { 
        if { [send zrange get on] } {
	    loadImage [lindex $selection 0]
        } else {
	    loadImage $selection
        }
    }
}; send imageList addCallback flSelectPrint


# Load the named image in the display.
proc loadImage { name } \
{
    global frame

    set fr [send frameFrame get label]

    if { $fr == "Current" } { set fr $frame }

    if { [fileSetOptions] >= 0 } {
        send imagewin setCursorType busy
        send client load $name $fr
        send imagewin setCursorType idle
        send fnameText set string $name
    }
}

# Load button callback.
proc fileLoadB args \
{
    set fname [send fnameText get string]
    if {$fname == ""} {
	Wexec client "No image name specified"
    } else {
	loadImage $fname
    }
} ; send filesLoadButton addCallback fileLoadB


# Send and selected options to the client before loading the image.
proc fileSetOptions args \
{
    if { ![send zscale get on] && ![send zrange get on] } {
        set z1 [send z1Value get string]
        set z2 [send z2Value get string]

        if {$z1 == $z2} {
	    Wexec client "z1/z2 values are not set properly"
	    return -1
	} else {
            send client setLoadOption z1 $z1
            send client setLoadOption z2 $z2
	}
    }

    return 0
}

