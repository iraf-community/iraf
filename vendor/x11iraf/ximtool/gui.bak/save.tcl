
################################################################################
# Save panel functions.
################################################################################

# Global variables needed for the save setup panel

set format	rasButton
set saveColor	svPseudoButton

set fileFmtButtons {
    rasButton gifButton jpegButton tiffButton 
    fitsButton x11Button epsButton rawButton 
}


# Format group procedures.
#---------------------------------

# Select a format.

proc set_format { widget func state args } \
{
    global format saveColor

    send $format set on false
    if {$widget == $format} {
	send $widget set on true
    } else {
	switch $widget  { 
	rasButton	{ send client setSaveOption format ras  } 
	gifButton	{ send client setSaveOption format gif  }
	jpegButton	{ send client setSaveOption format jpeg }
	tiffButton	{ send client setSaveOption format tiff }
	fitsButton	{ send client setSaveOption format fits }
	x11Button	{ send client setSaveOption format x11  }
	epsButton	{ send client setSaveOption format eps  }
	rawButton	{ send client setSaveOption format raw  }
	}
    }
} ; foreach fmt $fileFmtButtons { send $fmt addCallback set_format }

proc setSaveFile { widget cbtype args } \
{
	send client setSaveOption fname $args
	send saveStatus set label [format "output file set to %s" $args]
}; send saveFile addCallback setSaveFile


proc doSaveOptions { param old new } \
{
    global format saveColor
    global warnings

    send saveStatus set label {}
    set val [join [lrange $new 1 end] " "]

    #print [format "doSaveOptions %s = %s" [lindex $new 0] $val]
    switch [lindex $new 0] {
    format  {   
		# Now (de)sensitize the color options depending on the format,
		# force the color choice when needed.
    		send $format set on false
		switch [lindex $val 0] {
		ras       { #send svRGBButton setSensitive false
		            send svPseudoButton setSensitive true
			    set format rasButton
		          }
		gif       { send svRGBButton setSensitive false
		            send svPseudoButton setSensitive true
		            if {$saveColor == "svRGBButton"} {
		                send $saveColor set on false
		                send svPseudoButton set on true
		                set saveColor svPseudoButton
		            }
			    set format gifButton
		          }
		jpeg      { send svRGBButton setSensitive true
		            send svPseudoButton setSensitive true
			    set format jpegButton
		          }
		tiff      { send svRGBButton setSensitive false
		            send svPseudoButton setSensitive true
			    set format tiffButton
		          }
		fits      { send svRGBButton setSensitive false
		            send svPseudoButton setSensitive false
		            send $saveColor set on false
		            send svGrayButton set on true
		            set saveColor svGrayButton
			    set format fitsButton
		          }
		x11       { send svRGBButton setSensitive true
		            send svPseudoButton setSensitive true
			    set format x11Button
		          }
		eps       { send svRGBButton setSensitive true
		            send svPseudoButton setSensitive true
			    set format epsButton
		          }
		raw       { send svRGBButton setSensitive true
		       	    send svPseudoButton setSensitive true
			    set format rawButton
		          }
		}
    		send $format set on true
	    }
    color   { send $saveColor set on false
	      switch [lindex $val 0] {
	      grayscale   { send svGrayButton set on true
     			    set saveColor svGrayButton
			  }
	      pseudocolor { send svPseudoButton set on true
     			    set saveColor svPseudoButton
			  }
	      rgb	  { send svRGBButton set on true
     			    set saveColor svRGBButton
			  }
	      }
	    }
    fname   { send saveFile set string $val
	    }
    status  { send saveStatus set label $val
	      send server synchronize
	    }
    text    { send saveData set label $val
	    }
    warning { if {$warnings} { Wexec server $val }
	    }
    }
} ; send saveOptions addCallback doSaveOptions


# Color group procedures.
#---------------------------------
send svGrayButton   addCallback "send client setSaveOption color grayscale"
send svPseudoButton addCallback "send client setSaveOption color pseudocolor"
send svRGBButton    addCallback "send client setSaveOption color rgb"


proc Save args \
{
    global panel_up

    send imagewin setCursorType busy

    # Get the print command or file template if not previous reset.
    set val [ send saveFile get string ]
    send client setSaveOption fname $val
    send saveStatus set label [format "output file set to %s" $args]

    send client save
    send imagewin setCursorType idle
} ; send okaySave addCallback Save


