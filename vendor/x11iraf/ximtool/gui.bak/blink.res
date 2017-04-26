
set Resources(blink_panel) { \

    *blink_panel.geometry:			425x200
    *blink_panel.title:				Blink/Register Frames

    *brMenuBar*Command.internalHeight:	       	4
    *brMenuBar*Command.internalWidth:	       	12
    *brMenuBar*Command.height:			27
    *brMenuBar*borderWidth:			0

    *brMenuBar*Label.font:	  7x13bold
    *brLayout*Command.font:	  -*-helvetica-medium-r-normal-*-12-*-iso8859-1
    *brRegButton.font:		  -*-helvetica-medium-r-normal-*-12-*-iso8859-1
    *brReset.font:  		  -*-helvetica-medium-r-normal-*-12-*-iso8859-1
    *brMatchButton.font:  	  -*-helvetica-medium-r-normal-*-12-*-iso8859-1
    *brBlinkButton.font:  	  -*-helvetica-medium-r-normal-*-12-*-iso8859-1
    *brAregButton.font:		  -*-helvetica-medium-r-normal-*-12-*-iso8859-1


    *brMenuBar.layout: vertical	{ \
	5 < -5	> \
	horizontal { 10	< +inf -10> brClose 5 } \
	5 < -5	> \
    }
    *brClose.label:			       	Dismiss

    *brMenuFrame.frameType:		       	raised
    *brMenuFrame.frameWidth:		       	2
    *bpLayout.layout: vertical { \
	brMenuFrame  <	+inf -inf * > \
	1 < -1	> \
	brFrame < +inf	-inf * +inf -inf > \
    }

    *brFrame.frameType:			       	chiseled
    *brFrame.frameWidth:		       	2
    *brFrame.outerOffset:			4
    *brFrame.innerOffset:			4

    *brLayout.borderWidth:			0
    *brLayout.layout: vertical { \
	3 < -3	> \
	horizontal { -3 brFramesG < +inf -inf * > -3 } \
	-5  \
	horizontal { -3 brCmdG	< +inf -inf * +inf -inf	> -3 } \
	-5 \
    }

    *brCmdG.label:
    *brCmdG.outerOffset:			7
    *brCmdG.innerOffset:			7
    *brCmdG*borderWidth:			0
    *brCmd.layout: horizontal {	\
	2 \
	vertical { \
	    2 \
	    horizontal	{ \
		2 \
		brRateLabel \
		2 \
		brBRframe < +inf -inf	* >\
		4 \
		brReset \
		2 \
	    } \
	    8 < -8 > \
	    horizontal	{ \
		4 \
		brMatchButton	< +inf -inf * >	\
		5 < -5 > \
		brRegButton <	+inf -inf * > \
		2 \
	    } \
	    1 < +inf >	\
	} \
	10 \
	vertical { \
	    2 \
	    brBlinkButton < +inf * > \
	    10	< -10 >	\
	    brAregButton < +inf * > \
	    1 < +inf >	\
	} \
	2 \
    }

    *brFramesG.label:				Frames List
    *brFramesG.outerOffset:			7
    *brFramesG.innerOffset:			7
    *brFramesG*SimpleMenu.borderColor:		black
    *brFramesG*SimpleMenu.borderWidth:		1
    *brFramesG*SimpleMenu.foreground:           White
    *brFramesG*SimpleMenu.background:           SteelBlue

    *brFrames.borderWidth:			0
    *brFrames.layout: horizontal { \
	5 < +inf > \
	brFrame1  < +inf * > 1 \
	brFrame2  < +inf * > 1 \
	brFrame3  < +inf * > 1 \
	brFrame4  < +inf * > 1 \
	brFrame5  < +inf * > 1 \
	brFrame6  < +inf * > 1 \
	brFrame7  < +inf * > 1 \
	brFrame8  < +inf * > 1 \
	brFrame9  < +inf * > 1 \
	brFrame10 < +inf * > 1 \
	brFrame11 < +inf * > 1 \
	brFrame12 < +inf * > 1 \
	brFrame13 < +inf * > 1 \
	brFrame14 < +inf * > 1 \
	brFrame15 < +inf * > 1 \
	brFrame16 < +inf * >   \
	5 < +inf > \
    }

    *brBRlayout.layout:	horizontal { \
	brBRdecrease \
	brBRtext < +inf -100% * > \
	brBRincrease \
    }

    *blink_panel*TextToggle.location:		0 0 110	23
    *blink_panel*TextToggle.frameWidth:		1
    *blink_panel*TextToggle.highlightColor:	yellow
    *blink_panel*TextToggle.offIcon:		square0s
    *blink_panel*TextToggle.onIcon:		square1s
    *blink_panel*TextToggle.outerOffset:	0
    *blink_panel*TextToggle.innerOffset:	2
    *blink_panel*Command.height:		23
    *blink_panel*Arrow.width:			16
    *blink_panel*Arrow.height:			20


    *brFramesLabel.label:		     	Frames:
    *brFrame1.menuName:				frame1Menu
    *brFrame2.menuName:				frame2Menu
    *brFrame3.menuName:				frame3Menu
    *brFrame4.menuName:				frame4Menu
    *brFrame5.menuName:				frame5Menu
    *brFrame6.menuName:				frame6Menu
    *brFrame7.menuName:				frame7Menu
    *brFrame8.menuName:				frame8Menu
    *brFrame9.menuName:				frame9Menu
    *brFrame10.menuName:			frame10Menu
    *brFrame11.menuName:			frame11Menu
    *brFrame12.menuName:			frame12Menu
    *brFrame13.menuName:			frame13Menu
    *brFrame14.menuName:			frame14Menu
    *brFrame15.menuName:			frame15Menu
    *brFrame16.menuName:			frame16Menu
    *brReset.label:			     	Reset

    *brRateLabel.label:			     	Rate:
    *brBRframe.frameType:			sunken
    *brBRframe.frameWidth:			1
    *brBRtext.width:				40
    *brBRtext.height:				23
    *brBRdecrease.direction:			left
    *brBRincrease.direction:			right
    *brRegButton.label:			     	Register
    *brMatchButton.label:			Match LUTs
    *brBlinkButton.label:			Blink
    *brAregButton.label:		       	Auto-Register
}

