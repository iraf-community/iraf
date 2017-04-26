
################################################################################
# TILE GEOMETRY
################################################################################

set tileSel	2
set tileOpt	0
set tileNcols	2
set tileNrows	1
set tile_frames {}

proc selectTileOrientation { widget type select args } \
{
    global tileSel tileOpt

    set w { nrowLab ncolLab nrdecrease nrincrease ncdecrease ncincrease }
    if {$select == "Manual"} {
	foreach p $w { send $p setSensitive True }
    } elseif {$select != "none"} {
	foreach p $w { send $p setSensitive False }
    }

    set w { byCols bottomUp labelFrames labelImname labelTitles }
    if {$select == "Disabled"} {
	foreach p $w { send $p setSensitive False }
    } else {
	foreach p $w { send $p setSensitive True }
    }

    switch $select {
    Disabled	 { set tileSel 0 }
    Manual	 { set tileSel 1
		   set nx [send nctext get label]
    		   set ny [send nrtext get label]
		   set select [format "%dx%d" $nx $ny]
		 }
    Best	 { set tileSel 2 }
    Square	 { set tileSel 3 }
    Horizontal	 { set tileSel 4 }
    Vertical	 { set tileSel 5 }
    "One Row"	 { set tileSel 6 }
    "One Column" { set tileSel 7 }
    none	 { send tileMode set selection $tileSel	; return }
    }

    # Reset the button in case we're called directly from elsewhere.
    send tileMode set selection $tileSel

    # Send the option to the client.
    if {$tileSel > 0} {
        setTileFrames
        send client setOption tileFrames "True"
	set tileOpt $tileSel
    } else {
        send client setOption tileFrames "False"
    }

} ; send tileMode addCallback selectTileOrientation

proc tileSelToLabel { selection args } \
{
    switch $selection {
    0    { return "Disabled"	}
    1    { return "Manual"	}
    2    { return "Best"	}
    3    { return "Square"	}
    4    { return "Horizontal"	}
    5    { return "Vertical"	}
    6    { return "One Row"	}
    7    { return "One Column"	}
    }
    return "Best"
}

# Callback for the fill style options.
proc selectFillStyle { widget type state args } \
{
    if {$state} { set not False } else { set not True }

    switch $widget {
    byCols   	{ send client setOption tileByRows $not  }
    bottomUp 	{ send client setOption tileTopDown $not }
    }
} ; foreach w {byCols bottomUp} { send $w addCallback selectFillStyle }

# Callback for the tile labelling options.
proc selectTileLabels { widget type state args } \
{
    if {$state} { 
        switch $widget {
        labelFrames { send client setOption tileLabels 1 }
        labelImname { send client setOption tileLabels 2 }
        labelTitles { send client setOption tileLabels 3 }
        }
    } else {
	send client setOption tileLabels 0
    }
    send $widget set on $state
}
foreach w {labelFrames labelImname labelTitles} {
   send $w addCallback selectTileLabels
}

# Callback for the Tile Frame selection toggles.
proc setTileFrames args \
{
    global tile_frames tileSel tileNcols tileNrows

    # No-op if tiling isn't enabled.
    if {$tileSel == 0} \
	return

    # Get the new tile frames list.
    set tile_frames {}
    for {set i 1} {$i <= 16} {incr i} {
	if {[send tFrame$i get state]} {
	    lappend tile_frames $i
	}
    }

    # Reset the geometry.
    set geom [format "%dx%d" $tileNcols $tileNrows]
    switch $tileSel {
    1   { send client setOption tileGeom $geom      \{ $tile_frames \} }
    2   { send client setOption tileGeom Best       \{ $tile_frames \} }
    3   { send client setOption tileGeom Square     \{ $tile_frames \} }
    4   { send client setOption tileGeom Horizontal \{ $tile_frames \} }
    5   { send client setOption tileGeom Vertical   \{ $tile_frames \} }
    6   { send client setOption tileGeom Row        \{ $tile_frames \} }
    7   { send client setOption tileGeom Column     \{ $tile_frames \} }
    }
}; for {set i 1} {$i <= 16} {incr i} {send tFrame$i addCallback setTileFrames}

proc setAllTileFrames args \
{
    global tileNcols tileNrows
    for {set i 1} {$i <= 16} {incr i} {
	if {[send tFrame$i get sensitive]} {
	    send tFrame$i set state True
	}
    }
    setTileFrames
} ; send tAll addCallback setAllTileFrames

proc setNoTileFrames args \
{
    global tileNcols tileNrows
    for {set i 1} {$i <= 16} {incr i} {
	if {[send tFrame$i get sensitive]} {
	    send tFrame$i set state False
	}
    }
    setTileFrames
} ; send tNone addCallback setNoTileFrames

proc tileOptions { param old new } \
{
    global tileNcols tileNrows

    set tileNcols [lindex $new 0]
    set tileNrows [lindex $new 1]

    # Set the geometry.
    send nctext set label $tileNcols
    send nrtext set label $tileNrows
    send tileGeometry set label [format \
    	"Tile Geometry:  %-2dx%2d" $tileNcols $tileNrows]

} ; send tileOptions addCallback tileOptions


proc tileSetRows { widget args } \
{
    global nframes tile_frames

    set nx [send nctext get label]
    set ny [send nrtext get label]

    if {$widget == "nrdecrease" && [expr ($ny-1)] > 0} {
	incr ny -1
    } elseif {$widget == "nrincrease" && [expr ($ny+1)] <= $nframes} {
	incr ny 1
    } else {
	return
    }
    set geom [format "%dx%d" $nx $ny]
    send client setOption tileGeom $geom $tile_frames
} ; foreach w { nrdecrease nrincrease } { send $w addCallback tileSetRows }

proc tileSetCols { widget args } \
{
    global nframes tile_frames

    set nx [send nctext get label]
    set ny [send nrtext get label]

    if {$widget == "ncdecrease" && [expr ($nx-1)] > 0} {
	incr nx -1
    } elseif {$widget == "ncincrease" && [expr ($nx+1)] <= $nframes} {
	incr nx 1
    } else {
	return
    }
    set geom [format "%dx%d" $nx $ny]
    send client setOption tileGeom $geom $tile_frames
} ; foreach w { ncdecrease ncincrease } { send $w addCallback tileSetCols }


# Initialize the frame tiling.
setAllTileFrames
selectTileOrientation junk junk Disabled
selectFillStyle byRows  junk True
selectFillStyle topDown junk True
selectTileLabels labelImname junk False
