
set Resources(help_panel) { \

    !----------------------
    ! Help panel resources.
    !----------------------
    *help_panel.title:				XImtool	Help Summary
    *help_panel.width:				500
    *help_panel.height:				550
    *helpLayout*borderWidth:			0
    *helpLayout*Command.internalHeight:		4
    *helpLayout*Command.internalWidth:		12
    *helpLayout*HTML*shadowWidth:		1
    *helpLayout*helpText*Scrollbar.shadowWidth:		1

    *helpMenuGroup.label:               
    *helpMenuGroup.height:                      45
    *helpMenuGroup.outerOffset:                 0
    *helpMenuGroup.innerOffset:                 0
    *helpMenuGroup.frameType:			raised
    *helpMenubar.layout: vertical { \
	5 < -5 > \
	horizontal { 20 < +inf -20 > helpClose 7 } \
	5 < -5 > \
    }
    *helpBack.label:				Back
    *helpBack.sensitive:			False
    *helpForward.label:				Forward
    *helpForward.sensitive:			False
    *helpHome.label:				Home
    *helpClose.label:				Dismiss


    *hfEntry*editType:                          edit
    *hfEntry*font:                              7x13
    *hfEntry*displayCaret:                      True
    *hfFrame.frameWidth:			1
    *hfFrame.frameType:				sunken
    *hfFrame.width:				250
    *hfFind.label:                              Search
    *hfFind.shadowWidth:                        1
    *hfClear.label:                             Clear
    *hfClear.shadowWidth:                       1

    *helpLayout.layout:	vertical { \
	-1 \
	horizontal { helpMenuGroup < +inf -inf * > 	     } \
	5 \
        horizontal { \
            5 \
	    helpBack 2 helpForward 2 helpHome \
	    20 < +inf -20 > \
            hfFrame < +inf -inf * > 3 hfFind 1 hfClear \
            5 \
        } \
	5 \
	horizontal { helpTextFrame < +inf -inf * +inf -inf > } \
	horizontal { helpInfoLayout < +inf -inf	* > 	     } \
	-1 \
    }
    *helpTextFrame.frameWidth:			1
    *helpTextFrame.frameType:			sunken
    *helpText.width:				600
    *helpText.height:				500
    *helpText.anchorUnderlines:			1
    *helpText.visitedAnchorUnderlines:		1
    *helpText.verticalScrollOnRight:		true
    *helpText*Scrollbar.shadowWidth:		1
    *helpText.plainFont:       -adobe-courier-medium-r-normal-*-12-*-*-*-*-*-*-*
    *helpText.plainboldFont:   -adobe-courier-bold-r-normal-*-12-*-*-*-*-*-*-*
    *helpText.plainitalicFont: -adobe-courier-medium-o-normal-*-12-*-*-*-*-*-*-*
!   *helpText.boldFont:				6x12bold


    ! Contact info at the bottom of the panel.
    *helpInfoLayout*Label.justify:		center
    *helpInfoLayout*Label.internalHeight:	0
    *helpInfoLayout.layout: horizontal { \
	5 \
	vertical { 5 helpIRAFLogo 5 } \
	1 < +inf > \
	vertical { \
	    5 \
	    helpInfo1 < +inf -inf * +inf -inf > \
	    helpInfo2 < +inf -inf * +inf -inf > \
	    helpInfo3 < +inf -inf * +inf -inf > \
	    5 \
	} \
	1 < +inf > \
	vertical { 5 helpNOAOLogo 5 } \
	5 \
    }
    *helpInfo1.label:		XIMTOOL_VERSION
    *helpInfo2.label:		iraf@noao.edu     (520) 318-8160
    *helpInfo3.label: \
	NOAO is operated by AURA under cooperative agreement with the NSF

    *helpInfoLayout*helpInfo1.font: -*-helvetica-medium-r-normal-*-12-*-*-*
    *helpInfoLayout*helpInfo2.font: -*-helvetica-medium-r-normal-*-12-*-*-*
    *helpInfoLayout*helpInfo3.font: -*-helvetica-medium-r-normal-*-10-*-*-*

    *helpInfoLayout.helpIRAFLogo.internalWidth:		0
    *helpInfoLayout.helpIRAFLogo.internalHeight:	0
    *helpInfoLayout.helpIRAFLogo.foreground:		SteelBlue
    *helpInfoLayout.helpIRAFLogo.background:		white
    *helpInfoLayout.helpNOAOLogo.internalWidth:		0
    *helpInfoLayout.helpNOAOLogo.internalHeight:	0
    *helpInfoLayout.helpNOAOLogo.foreground:		SteelBlue
    *helpInfoLayout.helpNOAOLogo.background:		white
}

