
################################################################################
#  Pixel Table Callbacks.
################################################################################

set psize	5
set pixtab_up	0
set hdr_up	0

createMenu pixtabMenu pixtabSize {
    {   "3x3"          f.exec { pixtabSetSize 3 } }
    {   "5x5"          f.exec { pixtabSetSize 5 } }
    {   "7x7"          f.exec { pixtabSetSize 7 } }
    {   "9x9"          f.exec { pixtabSetSize 9 } }
}


proc updatePixelTable { cx cy wx wy args } \
{
    global psize pixtab_up ism_enable

    if {! $pixtab_up} \
	return
    if {$ism_enable} \
	return

    set delta [expr int($psize / 2) ]
    set x1 [expr ($wx - $delta) ]
    set x2 [expr ($wx + $delta) ]
    set y1 [expr ($wy - $delta) ]
    set y2 [expr ($wy + $delta) ]
    set c [ expr int($psize / 2) ]


    # Update the table labels.
    set x $x1 ;      set xl {}
    set y $y2 ;      set yl {}
    for {set i 0} {$i < $psize} {incr i} {
	lappend xl  [format " %10.1f " $x] ; set x [ expr ($x + 1.) ]
	lappend yl  [format " %10.1f " $y] ; set y [ expr ($y - 1.) ]
    }
    send ptColLabs setList $xl ; send ptColLabs highlight $c
    send ptRowLabs setList $yl ; send ptRowLabs highlight $c

    # Update the pixel table itself.
    #set pix [ send client getPixels $cx $cy $psize True ]

    set x0  [ expr int($cx - $psize / 2. + 0.5)]
    set y0  [ expr int($cy - $psize / 2. + 0.5)]
    set pix [ send client getPixels $x0 $y0 $psize $psize ]
    send pixtab setList [ lrange $pix 4 end ]
    set c [ expr int(($psize * $psize) / 2) ]
    send pixtab highlight $c

    # Update the pixtab stats.
    set sum   0.0
    set sum2  0.0
    set npix  [ expr ($psize * $psize) ]
    set nend  [ expr ($psize * $psize) + 4 ]
    for {set i 4} {$i < $nend} {incr i} {
	set val  [lindex $pix $i]
	catch {
	    set sum  [ expr ($sum + $val) ]
	    set sum2 [ expr ($sum2 + $val * $val) ]
	}
    }

    set mean [ expr ($sum / ($npix * 1.0)) ]
    set var  [ expr (($sum2 - $sum * $mean) / ($npix - 1)) ]
    if {$var <= 0.0} {
	set stdev 0.0
    } else {
	set stdev  [ expr sqrt ($var) ]
    }
    send meanValue set label [ format "%10.2f" $mean ]
    send sigValue set label  [ format "%10.4f" $stdev ]
}


proc pixtabClose args \
{
    global pixtab_up

    send pixel_panel unmap
    send pixelTable set on False
    set pixtab_up 0
    catch { send wcspix set psize 0 }
} ; send pixtabClose addCallback pixtabClose


proc pixtabSetSize { size args } \
{
    global psize ism_enable

    set c [ expr int(($psize * $psize) / 2) ]
    send pixtab highlight $c

    # Now reset the window size.
    switch $size {
    3	{ send pixel_panel "resize 265 175"
	  send pixtab "set width 180 ; set height 60"
	}
    5	{ send pixel_panel "resize 375 215"
	  send pixtab "set width 290 ; set height 105"
	}
    7	{ send pixel_panel "resize 495 265"
	  send pixtab "set width 410 ; set height 150"
	}
    9	{ send pixel_panel "resize 610 310"
	  send pixtab "set width 525 ; set height 195"
	}
    }

    send pixtab    set defaultColumns $size
    send ptColLabs set defaultColumns $size
    set psize $size

    # Notify the ISM we've changed size.
    if ($ism_enable) {
	catch { send wcspix set psize $psize }
    }

    send imagewin getCursorPos xc yc
    updatePixelTable $xc $yc $xc $yc

} ; pixtabSetSize $psize


proc ptPixelTable {widget type state args} \
{
    global pixtab_up psize

    set pixtab_up $state
    if {$pixtab_up == 1} {
	send pixelTable set on True
	send pixel_panel map
	catch { send wcspix set psize $psize }
    } else {
	send pixelTable set on False
	send pixel_panel unmap
	catch { send wcspix set psize 0 }
    }
}; send pixelTable addCallback ptPixelTable

