
################################################################################
# INFO box.
################################################################################

set	infoMode	infoOptFr


# Current Frame information.
proc infoFrameUpdate args \
{
    global version frame nframes
    global frameWidth frameHeight frameDepth
    global enhancement
    global cpXcen cpYcen cpXoff cpYoff
    global cpXmag cpYmag cpXscale cpYscale

    if { [send infoOptWCS get on] } {
	send client info wcs
	return
    } elseif {! [send infoOptFr get on]} \
	return


    if {$frame == 0} {
       send infoText set string "initializing display..."
    } else {
       set line1 $version
       set line2 [format "Image:\t\t%s" [send imageTitle get label]]
       set line3 [format "Frame %d of %d:\t%d x %d" \
	    		$frame $nframes $frameWidth $frameHeight]
       set cmap  [lindex $enhancement($frame) 0]
       set brt   [lindex $enhancement($frame) 1]
       set con   [lindex $enhancement($frame) 2]
       set line4 [format "Colormap:\t%s" $cmap]
       set line5 [format "Enhancement:\tBrt=%s  Cont=%s" $brt $con]

       set cntr  [format "X: %0.1f\n\t\tY: %0.1f" $cpXcen $cpYcen]
       set line6 [format "Center:\t\t%s" $cntr]

       set line7 [format "Zoom:\t\tX: %0.2f\n\t\tY: %0.2f" $cpXmag $cpYmag]
       set line8 [format "Scale:\t\tX: %0.2f\n\t\tY: %0.2f" $cpXscale $cpYscale]
       set line9 [format "Offset:\t\tX: %0.2f\n\t\tY: %0.2f" $cpXoff $cpYoff]

       send infoText set string [
	    format "%s\n\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n\n" \
		$line1 $line2 $line3 $line4 $line5 $line6 $line7 $line8 $line9
       ]
    }
}
set params {frame frameView nframes frameSize frameTitle enhancement}
foreach p $params { send $p addCallback infoFrameUpdate }


# Server Program State information.
proc infoOptToggle { widget type state args } \
{
    global infoMode ismInfoText

    if {$state} { set not False } else { set not True }

    send $infoMode set on $not
    set infoMode $widget

    switch $widget {
    infoOptFr	   { infoFrameUpdate				}
    infoOptSvr	   { set bp [send imagewin get basePixel] 
		     set mc [send imagewin get maxColors]
		     send client info server $bp $mc
		   }
    infoOptIsm     { send infoText set string $ismInfoText	}
    infoOptClients { send client info clients			}
    infoOptWCS     { send client info wcs			}
    infoOptFB	   { send client info imtoolrc			}
    }
}
set iopts {infoOptFr infoOptSvr infoOptIsm infoOptClients infoOptWCS infoOptFB }
foreach w $iopts { send $w addCallback infoOptToggle }


proc infoSetText { param old new } \
{
   send infoText set string $new
} ; send info addCallback infoSetText


