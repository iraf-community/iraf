
################################################
# Define some TCL debug procedures.
################################################

set tcl_up 0

proc tclCommandClear {widget args}   { send tclEntry set string "" }
proc tclCommandExecute {widget args} { send server [send tclEntry {get string}]
}
proc tclCommand {widget mode command args} { send server $command }
proc tclClose {widget args}                { tclPanel }
proc tclOpen args \
{ 
    global tcl_up
    send tcl_panel map 
    set tcl_up 1
}

proc tclPanel args \
{
    global tcl_up
    if {$tcl_up} {
	send tcl_panel unmap
	set tcl_up 0
    } else {
	send tcl_panel map
	set tcl_up 1
    }
}

send tclClear   addCallback tclCommandClear
send tclExecute addCallback tclCommandExecute
send tclEntry   addCallback tclCommand
send tclDismiss addCallback tclClose

