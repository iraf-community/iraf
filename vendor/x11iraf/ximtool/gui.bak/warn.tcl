
################################################################################
# Warning dialog.  This pops up a dialog box with the given warning message,
# and executes the given command if the user pushes OK.
#
# Usage:	Wexec object message [ok_action [cancel_action]]
#
# The message text is displayed in a popup and the user hits the ok or
# cancel button to close the popup.  If an action has been posted for the
# button selected then it is sent to the named object.  Only one alert can
# be in effect at a time; posting another alert before the first has
# completed causes the new alert to override the first.
################################################################################

set W_object ""
set W_ok_cmd ""
set W_cancel_cmd ""

proc Wexec {object msg args} \
{
    global W_object W_ok_cmd W_cancel_cmd
    set W_object $object
    set W_ok_cmd [lindex $args 0]
    set W_cancel_cmd [lindex $args 1]
    send warnText set label $msg
    send warning map
}

proc Wbutton {widget args} \
{
    global W_object W_ok_cmd W_cancel_cmd
    switch $widget {
    warnOk	{ if [llength $W_ok_cmd] { send $W_object $W_ok_cmd }
		}
    warnCancel	{ if [llength $W_cancel_cmd] { send $W_object $W_cancel_cmd }
		}
    }
    send warning unmap
}
send warnOk     addCallback Wbutton
send warnCancel addCallback Wbutton

# The parameter "alert" is used to forward alerts from the client.
proc setAlert {param old new} \
{
    Wexec client [lindex $new 0] [lindex $new 1]
}; send alert addCallback setAlert


