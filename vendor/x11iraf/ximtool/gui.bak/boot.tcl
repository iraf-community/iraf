

################################################################################
#  GUI Bootstrap Procedures
################################################################################

# Initialize the widget tree.
proc InitWidgetTree args \
{
    global Objects Resources Version


    # Add a new objects description for each of the panels found so we can
    # create them by name later rather that with the defaults.

    set guiResources ""
    foreach obj [array names Objects] {
	set guiResources \
		[ format "%s\n\n*%s_objects:%s\n" \
		    $guiResources $obj $Objects($obj) ]
    }

    # Now append all the Resource strings, changing any version strings as
    # needed.

    foreach res [array names Resources] {
	regsub -all XIMTOOL_VERSION $Resources($res) $Version ver
	set guiResources [ format "%s\n\n%s\n\n" $guiResources $ver ]
    }

    # Define all of the GUI objects and resources.
    appInitialize ximtool XImtool $guiResources
}


# Realize a window module, i.e. create it's objects.
proc Realize { module args } \
{
    global Objects

    # Create any widgets for the module.  We only do this once and set a
    # flag to indicate the objects have been created so we don't do it on
    # subsequent realizations.
    if { [info exists Objects($module)] } {
	createObjects [format "%s_objects" $module]
	reset-server
    }
}


# Bootstrap up the GUI.
InitWidgetTree
Realize ximtool
Realize parameters
Realize panelShell
Realize tcl_panel
Realize pixel_table
Realize hdr_panel
Realize blink_panel
Realize help_panel
Realize xpan_panel
Realize xmag_panel
Realize warning

reset-server

# Set the gterm widget focus.
send colorbar setGterm ; send colorbar activate
send imagewin setGterm ; send imagewin activate

# Crank it up.
activate


