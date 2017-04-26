
set Resources(info_panel) { \

    !--------------------
    ! Information Panel
    !--------------------
    *infoLayout*borderWidth:			0
    *infoLayout*Frame.frameType:		sunken
    *infoLayout*Frame.frameWidth:		1
    *infoLayout*Command.internalWidth:		12
    *infoLayout*Text*editType:			read
    *infoLayout*Text*scrollVertical:		whenNeeded
    *infoLayout*Text*scrollHorizontal:		whenNeeded
    *infoLayout*Text*displayCaret:		False
    *infoLayout*Scrollbar.background:		gray77
    *infoLayout*Scrollbar.width:		17
    *infoLayout*Scrollbar.height:		17

    *infoText.height:				240
    *infoText*font:      			6x13
    *infoLayout.layout:	vertical { \
	infoBox  < +inf -inf * > \
    }

    *infoBox.label:
    *infoBox.outerOffset:			0
    *infoBox.innerOffset:			3
    *infoBoxL*TextToggle.frameType:		raised
    *infoBoxL*TextToggle.frameWidth:		1
    *infoBoxL*TextToggle*outerOffset:		0
    *infoBoxL*TextToggle*innerOffset:		1
    *infoBoxL*TextToggle.location:		0 0 100 25
    *infoBoxL*TextToggle*onIcon:		square1s
    *infoBoxL*TextToggle*offIcon:		square0s
    *infoBoxL*TextToggle*highlightColor:	cyan
    *infoBoxL.layout: vertical { \
	infoFrame < +inf -inf * +inf -inf > 1 \
	4 \
	horizontal { \
	    infoOptFr	< +inf -inf * > 1 \
	    infoOptSvr  < +inf -inf * > 1 \
	    infoOptClients  < +inf -inf * > 1 \
	    infoOptWCS  < +inf -inf * > 1 \
	    infoOptIsm  < +inf -inf * > 1 \
	    infoOptFB	< +inf -inf * >   \
	}\
 	2 \
    }
    *infoOptFr.label:				Frame
    *infoOptFr.on:				True
    *infoOptSvr.label:				Server
    *infoOptWCS.label:				WCS
    *infoOptIsm.label:				ISM
    *infoOptClients.label:			Clients
    *infoOptFB.label:				Imtoolrc
}


