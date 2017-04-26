set Resources(tcl_panel) { \

    !--------------------------------
    ! Define a debug Tcl shell.
    !--------------------------------
    *tcl_panel.width:                           550
    *tcl_panel.height:                          180
    *tcl_panel.title:                           Debug TCL Command Entry
    *tclLayout*borderWidth:                     0
    *tclLayout*Frame.frameType:                 sunken
    *tclLayout*Frame.frameWidth:                2
    *tclLayout.layout:  vertical { \
        0 < +0 -0 > \
        tclCmdGroup   < +inf -inf * > \
        tclFrame < +inf -inf * +inf -inf> \
        0 < +0 -0 > \
    }
    *tclEntry*foreground:                       black
    *tclEntry*editType:                         edit
    *tclEntry*type:                             string
    *tclEntry*font:				7x13
    *tclEntry*scrollVertical:                   Always
    *tclEntry*scrollHorizontal:                 whenNeeded

    *tclCmdGroup.label:         
    *tclCmdGroup.outerOffset:                   0
    *tclCmdGroup.innerOffset:                   0
    *tclCmd.layout: vertical { \
        5 \
        horizontal { \
            5 \
            tclClear   3 \
            tclExecute   \
            10 < +inf -10>    \
            tclDismiss   \
            5 \
        } \
        5 \
    }
    *tclClear.label:                            Clear
    *tclExecute.label:                          Execute
    *tclDismiss.label:                          Dismiss
}

