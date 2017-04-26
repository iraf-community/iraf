
################################################################################
# Print Panel Functions.
################################################################################

# Global variables needed for the print setup panel

set printColor	prGrayButton
set orientation	epsPortButton
set page_size	epsLetterButton
set imageScale	100

set epsWidgets { epsPageGroup epsOrientLabel epsSizeLabel 
	epsPortButton epsLandButton epsLetterButton epsLegalButton epsA4Button
	ScaleFrame SCdecrease SCtext SCincrease 
}


proc psetup_init args \
{
    global printColor orientation page_size imageScale #format

    set_printer toPrinter callback 1
    send SCtext set label [ format "%d %%" $imageScale ]
    send $printColor set on true
    send $orientation set on true
    send $page_size set on true
}


proc doPrintOptions { param old new } \
{
    global imageScale page_size orientation imageScale printColor
    global warnings

    send printStatus set label {}
    set val [join [lrange $new 1 end] " "]

    # print [format "doPrintOptions %s = %s" [lindex $new 0] $val]
    switch [lindex $new 0] {
    autoscale	    { 	if { $val == "True" } {
			    send epsscaleButton set on true
			} elseif { $val == "False" } {
			    send epsscaleButton set on false
			}
		    }
    autorotate	    { 	if { $val == "True" } {
			    send autorotateButton set on true
			} elseif { $val == "False" } {
			    send autorotateButton set on false
			}
		    }
    maxaspect	    { 	if { $val == "True" } {
			    send aspectButton set on true
			} elseif { $val == "False" } {
			    send aspectButton set on false
			}
		    }
    annotate	    { 	if { $val == "True" } {
			    send annotateButton set on true
		            send titleButton setSensitive true
		            send colorbarButton setSensitive true
		            send bordersButton setSensitive true
		            send titleLabel setSensitive true
		            send titleString setSensitive true
			} elseif { $val == "False" } {
			    send annotateButton set on false
		            send titleButton setSensitive false
		            send colorbarButton setSensitive false
		            send bordersButton setSensitive false
		            send titleLabel setSensitive false
		            send titleString setSensitive false
			}
		    }
    compress	    { 	if { $val == "True" } {
			    send compressButton set on true
			} elseif { $val == "False" } {
			    send compressButton set on false
			}
		    }
    orientation	    { 	send $orientation set on false
			if { $val == "portrait" } {
			    send epsPortButton set on true
			    set orientation epsPortButton
			} elseif { $val == "landscape" } {
			    send epsPortButton set on false
			    set orientation epsLandButton
			}
		    }
    papersize	    { 	send $page_size set on false
			if { $val == "letter" } {
			    send epsLetterButton set on true
			    set page_size epsLetterButton
			} elseif { $val == "legal" } {
			    send epsLegalButton set on true
			    set page_size epsLegalButton
			} elseif { $val == "A4" } {
			    send epsA4Button set on true
			    set page_size epsA4Button
			} elseif { $val == "B5" } {
			    send epsB5Button set on true
			    set page_size epsB5Button
			}
		    }
    imscale	    { 	set imageScale $val
    			send SCtext set label [ format "%d %%" $imageScale ]
		    }
    colortype	    { 	send $printColor set on false
			if { $val == "gray" } {
			    send prGrayButton set on true
			    set printColor prGrayButton
			} elseif { $val == "pseudo" } {
			    send prPseudoButton set on true
			    set printColor prPseudoButton
			} elseif { $val == "rgb" } {
			    send prRGBButton set on true
			    set printColor prRGBButton
			}
		    }
    printerName	    {	if [send toPrinter get on] {
			    send printlist highlight $val
			}
		    }
    printCmd	    {	if [send toPrinter get on] {
			    send printcmd set string $val
			}
		    }
    printFile	    {	if [send toFile get on] {
			    send printcmd set string $val
			}
		    }
    deviceType      {   if { $val == "Printer" } {
			    send printerLabel set label "Print Command:"
			    send toPrinter set on true
			    send toFile set on false
		        } elseif { $val == "File" } {
			    send printerLabel set label "File Name:"
			    send toPrinter set on false
			    send toFile set on true
			    send printlist unhighlight
		        }
		    }
    dotitle         {   if { $val == "True" } {
		            send titleButton set on true
		        } elseif { $val == "False" } {
		            send titleButton set on false
		        }
		    }
    doborders       {   if { $val == "True" } {
		            send bordersButton set on true
		        } elseif { $val == "False" } {
		            send bordersButton set on false
		        }
		    }
    docolorbar      {   if { $val == "True" } {
		            send colorbarButton set on true
		        } elseif { $val == "False" } {
		            send colorbarButton set on false
		        }
		    }
    title           {   send titleString set string $val
		    }

    status	    {   send printStatus set label $val
			send server synchronize
	 	    }
    warning	    {   if {$warnings} { Wexec server $val }
		    }
    }
}; send printOptions addCallback doPrintOptions


# Print options procedures.
# -------------------------------

set prOptsWidgets { 
    toPrinter toFile
    prGrayButton prPseudoButton prRGBButton
    epsLandButton epsPortButton
    epsLetterButton epsLegalButton epsA4Button epsB5Button
    SCincrease SCdecrease
}
set prSimpleOptions { 
    epsscaleButton autorotateButton aspectButton annotateButton compressButton 
    titleButton bordersButton colorbarButton
}

proc prPrintCommand { widget cbtype args } \
{
    if [send toFile get on] {
	send client setPrintOption printfile $args
	send printStatus set label [format "output file set to %s" $args]
    } else {
	send client setPrintOption printcmd $args
	send printStatus set label [format "print command set to %s" $args]
    }
}; send printcmd addCallback prPrintCommand

proc prTitleString { widget cbtype args } \
{
    send client setPrintOption title $args
}; send titleString addCallback prTitleString

proc prOptionToggle { widget cbtype args } \
{
    global imageScale

    # Handle the image scale widgets first.
    switch $widget {
    SCincrease	{ set scale [expr $imageScale + 5] 
  		  send client setPrintOption imscale $scale
		  return
		}
    SCdecrease	{ set scale [ expr $imageScale - 5 ]
		  send client setPrintOption imscale $scale
		  return
		}
    }

    # If it's not one of those it must be one of the radio toggles.
    set val [ send $widget get on ]
    #print [ format "prOptionToggle %s = %s" $widget $val ]
    if { $val == 1 } {
	switch $widget {
	toPrinter  	{ send client setPrintOption devicetype printer }
	toFile  	{ send client setPrintOption devicetype file }

	epsLandButton   { send client setPrintOption orientation landscape }
	epsPortButton   { send client setPrintOption orientation portrait }

	epsLetterButton { send client setPrintOption papersize letter }
	epsLegalButton  { send client setPrintOption papersize legal }
	epsA4Button     { send client setPrintOption papersize A4 }
	epsB5Button     { send client setPrintOption papersize B5 }

	prGrayButton    { send client setPrintOption colortype gray }
	prPseudoButton  { send client setPrintOption colortype pseudo }
	prRGBButton     { send client setPrintOption colortype rgb }
	}
    } else {
	send $widget set on true
    }

} ; foreach w $prOptsWidgets { send $w addCallback prOptionToggle }

proc prSimpleOptionToggle { widget args } \
{
    set val [ send $widget get on ]
    #print [ format "prSimpleOptionToggle %s = %s" $widget $val ]

    switch $widget {
    epsscaleButton   { send client setPrintOption autoscale  $val }
    autorotateButton { send client setPrintOption autorotate $val }
    aspectButton     { send client setPrintOption maxaspect  $val }
    annotateButton   { send client setPrintOption annotate   $val }
    compressButton   { send client setPrintOption compress   $val }

    titleButton      { send client setPrintOption dotitle    $val }
    bordersButton    { send client setPrintOption doborders  $val }
    colorbarButton   { send client setPrintOption docolorbar $val }
    }
} ; foreach w $prSimpleOptions { send $w addCallback prSimpleOptionToggle }



# Printer display and selection.
# -------------------------------
set lprList	{}

proc psSetPrintList {param old new} \
{
    global lprList
    set lprList $new
    send printlist setList $new resize
    send printlist highlight 0
}; send printerList addCallback psSetPrintList

proc lprResize args \
{
    global lprList
    send printlist setList $lprList resize
    send printlist highlight 0
}; send printlist addEventHandler lprResize ResizeRedirectMask

proc psSelectPrint {widget cbtype selections indices} \
{
    global printerlist
    foreach selection $selections {
	send client setPrintOption printername $selection
    }
}; send printlist addCallback psSelectPrint


proc Print args \
{
    global winWidth winHeight
    send imagewin setCursorType busy

    # Get the print command or file template if not previous reset.
    set val [ send printcmd get string ]
    if [send toFile get on] {
	send client setPrintOption printfile $val
    } else {
	send client setPrintOption printcmd $val
    }
    set val [ send titleString get string ]
    send client setPrintOption title $val
    setPrintCorners 0 [expr $winWidth - 1] [expr $winHeight - 1] 0

    send client print
    send imagewin setCursorType idle
} ; send okayPrint addCallback Print



# setPrintCorners -- Tell the client the WCS of the image being printed.

proc setPrintCorners { lx ly ux uy args } \
{
    global winWidth winHeight

    # Convert raw corner screen coordinates to frame buffer raster coords.
    send imagewin unmapPixel $lx $ly raster llx lly
    set llx [expr "int ($llx)"]
    set lly [expr "int ($lly)"]
    set str  [send client encodewcs $llx $lly]
    scan $str "%g %g %g" llx_r lly_r z
    set llx_i [expr "int ($llx_r)"]
    set lly_i [expr "int ($lly_r)"]

    send imagewin unmapPixel $ux $uy raster urx ury
    set urx [expr "int ($urx)"]
    set ury [expr "int ($ury)"]
    set str [send client encodewcs $urx $ury]
    scan $str "%g %g %g" urx_r ury_r z
    set urx_i [expr "int ($urx_r)"]
    set ury_i [expr "int ($ury_r)"]

    send client setPrintOption corners $llx_i $lly_i $urx_i $ury_i
}


