#!/bin/csh -f
#
# XIMTOOL-ALT --  Script wrapper to start XImtool using the alternate GUI.
# The GUI file is contained is this script which is created when the system
# is built, it may be used to run any alternate GUI by simply replacing the
# Tcl script making up the GUI at the end of this script or by using the
# "-gui" command line flag.  The only configurable item is the path to the
# XImtool binary to be used, by default the one found in the user's path will
# be used.
#	Arguments specific to this GUI include:
#
#	    -showToolBar <bool>		show toolbar on startup
#	    -showPanelBar <bool>	show panelbar on startup
#
#----------------------------------------------------------------------------

# Configurable parameters
set	XIMTOOL		= ximtool	# Path to default ximtool binary


#------------------------------------------------------------------
#--------------- Do not modify below this line --------------------
#------------------------------------------------------------------
unset 	noclobber
onintr	cleanup

set	SKIP		= 99		# offset to GUI file

# Dump the GUI from this script file.
tail +$SKIP $0 > /tmp/_gui.$$

# Check for no arguments.
set q	= '"'
set cmd = "-gui /tmp/_gui.$$ -title $q XImtool V1.3 - Alternative GUI $q"

# Process the script arguments, quoting args when necessary.
if ($#argv > 0) then
    while ("$1" != "")
        if ("$1" == "-xrm") then
	    if ("$2" != "") then
	        shift
	    else
	        echo "missing argument to '-xrm <resource>' switch"
	        exit 1
	    endif
	    set cmd = "$cmd -xrm $q$1$q"
        else if ("$1" == "-help") then
	     $XIMTOOL -help
	     exit 0
        else if ("$1" == "-defgui") then
	    tail +$SKIP $0
	    exit 0
        else if ("$1" == "-showToolBar") then
	    if ("$2" != "") then
	        shift
	    else
	        echo "missing argument to '-showToolBar <bool>' switch"
	        exit 1
	    endif
	    set cmd = "$cmd -xrm $q XImtool.showToolBar:$1$q"
        else if ("$1" == "-showPanelBar") then
	    if ("$2" != "") then
	        shift
	    else
	        echo "missing argument to '-showPanelBar <bool>' switch"
	        exit 1
	    endif
	    set cmd = "$cmd -xrm $q XImtool.showPanelBar:$1$q"
        else
	    set cmd = "$cmd $1"
        endif

        if ("$2" == "") then
	    break
	else
	    shift
	endif
    end
endif

# Run the command.
echo  "$XIMTOOL $cmd ; /bin/rm -f /tmp/_gui*.$$" > /tmp/_gui.cmds.$$
sh /tmp/_gui.cmds.$$
exit 0

cleanup:
	/bin/rm -f /tmp/_gui*.$$
	exit 0

#--------------------------------------------------------------------------
#-------------------------- XIMTOOL-ALT.GUI -------------------------------
#------								     ------
#------  To change the GUI run by this script just delete everything ------
#------  below here and replace with the new GUI Tcl script.         ------
#------								     ------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------

