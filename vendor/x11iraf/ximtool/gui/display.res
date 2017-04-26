
set Resources(display_panel) { \

    !================================
    ! Main Display Control Panel.
    !================================
    *controlPanel*internalWidth:		0
    *controlPanel*borderWidth:			0

    *TextBox.font:	7x13bold
    *TextToggle.font:	-adobe-times-medium-r-normal-*-12-*-*-*-*-*-iso8859-1
    *Command.font:	-adobe-times-bold-i-normal-*-12-*-*-*-*-*-iso8859-1
    *Toggle.font:	-adobe-times-medium-r-normal-*-12-*-*-*-*-*-iso8859-1
    *Label.font:	-*-helvetica-medium-r-normal-*-12-*-iso8859-1
    *MultiList.font:	-adobe-times-medium-r-normal-*-12-*-*-*-*-*-iso8859-1
    *toggleZoom.font:	-*-helvetica-medium-r-normal-*-12-*-iso8859-1
    *centerFrame.font:	-*-helvetica-medium-r-normal-*-12-*-iso8859-1
    *zoom*Command.font:	7x13bold

    *blinkFrame1.font:		-adobe-times-medium-r-normal-*-12-*-iso8859-1
    *blinkFrame2.font:		-adobe-times-medium-r-normal-*-12-*-iso8859-1
    *blinkFrame3.font:		-adobe-times-medium-r-normal-*-12-*-iso8859-1
    *blinkFrame4.font:		-adobe-times-medium-r-normal-*-12-*-iso8859-1
    *matchButton.font:		-adobe-times-medium-r-normal-*-12-*-iso8859-1
    *registerButton.font:	-adobe-times-medium-r-normal-*-12-*-iso8859-1
    *blinkButton.font:		-adobe-times-medium-r-normal-*-12-*-iso8859-1
    *autoregButton.font: 	-adobe-times-medium-r-normal-*-12-*-iso8859-1

    *controlPanel.layout: vertical { \
	5 < -5	> \
	horizontal { \
	    -1	\
	    viewBox < +inf -inf * > \
	    -1	\
	} \
	5 < -5	> \
	horizontal { \
	    -1	\
	    enhancementBox < +inf -inf	* +inf -inf > \
	    -5	\
	    vertical {	\
		-1 \
		blinkBox < * +inf - inf > \
		 1 \
		optionsBox < * +inff -inff > \
		-1 \
	    } \
	    -1	\
	} \
	-5 \
	controlBox < +inf * > \
	-5 \
    }

    ! VIEW
    ! ------------------
    *viewBox.label:				View
    *viewBox.location:				0 0 410 0
    *viewBox.shrinkToFit:			True
    *viewBox.outerOffset:			7

    *view.layout: vertical { \
	5 < +inf -5 > \
	horizontal { \
	    -1	\
	    frameSelect \
	    vertical {	\
		3 < -3 > \
		frameDataBox < +inff -100% * +inff -100% > \
		3 < -3 > \
	    } \
	    zoomBox \
	    -1	\
	} \
	1 < +inf > \
	viewButtons < +inf -inf * +inf	-inf > \
	5 < +inf -5 > \
    }

    *frameDataBox.frameType:			sunken
    *frameDataBox.frameWidth:			1
    *frameData.width:				150
    *frameData.height:				50

    *frameSelect.location:			0 0 72 0
    *frameSelect.shrinkToFit:			True
    *frameSelect.outerOffset:			7
    *frameSelect.innerOffset:			2
    *frameSelect.frameWidth:			2
    *frameSelect*offIcon:			diamond0s
    *frameSelect*onIcon:			diamond1s
    *frameSelect*highlightColor:		blue
    *frameSelect.label:				Frame

    *frameBox.layout: vertical { \
	3 \
	horizontal { -2	frlistBox < * +inff -inff > } \
	3 < +inf -3 > \
	horizontal { prevFrame < +inf -inf * > 4 nextFrame < +inf -inf * > } \
    }
    *framePort.allowVert:			True
    *framePort.allowHoriz:			False
    *framePort.useRight:			True
    *framePort.height:				80

    *frameBox*alignment:			left
    *frameBox*frameWidth:			0
    *frameBox*frame1.label:			1\ \ 
    *frameBox*frame2.label:			2\ \ 
    *frameBox*frame3.label:			3\ \ 
    *frameBox*frame4.label:			4\ \ 

    *frameBox*frame5.label:			5\ \ 
    *frameBox*frame6.label:			6\ \ 
    *frameBox*frame7.label:			7\ \ 
    *frameBox*frame8.label:			8\ \ 
    *frameBox*frame9.label:			9\ \ 
    *frameBox*frame10.label:			10\ 
    *frameBox*frame11.label:			11\ 
    *frameBox*frame12.label:			12\ 
    *frameBox*frame13.label:			13\ 
    *frameBox*frame14.label:			14\ 
    *frameBox*frame15.label:			15\ 
    *frameBox*frame16.label:			16\ 
    *frameBox*Command.width:			24
    *frameBox*prevFrame.label:			xxx
    *frameBox*nextFrame.label:			xxx

    *frameList*location:			0 0 50 20
    *frameList.layout: vertical	{ \
	frame1	< +inf * >\
	frame2	< +inf * >\
	frame3	< +inf * >\
	frame4	< +inf * >\
	frame5	< +inf * >\
	frame6	< +inf * >\
	frame7	< +inf * >\
	frame8	< +inf * >\
	frame9	< +inf * >\
	frame10 < +inf * >\
	frame11 < +inf * >\
	frame12 < +inf * >\
	frame13 < +inf * >\
	frame14 < +inf * >\
	frame15 < +inf * >\
	frame16 < +inf * >\
    }


    *zoomBox.label:				Zoom:
    *zoomBox.location:				0 0 160 127
    *zoomBox.outerOffset:			7
    *zoomBox.shrinkToFit:			True
    *zoomBox*TextButton.frameWidth:		1
    *zoomBox*TextButton.outerOffset:		0

    *controlPanel*zoom*internalWidth:		4
    *zoom.layout: vertical { \
	space = ((50% of width	zoom) -	(50% of	width z5)) \
	1 < +inf > \
	horizontal { \
	    vertical { toggleZoom < +inf * +inf > 1 } \
	    1 \
	    vertical { 1 < +inf > z5 1 < +inf > z3 0 < +inf > } \
	    1 \
	    vertical { zoomIn < +inf	* +inf > 1 } \
	} \
	1 \
	horizontal { \
	    1 < +inf >	\
	    d8	1 d4 1 d2 1 x1 1 z2 1 z4 1 z8 \
	    1 < +inf >	\
	} \
	1 \
	horizontal { \
	    vertical { 1 zoomOut < +inf * +inf	> } \
	    1 \
	    vertical { 0 < +inf > d3 1 < +inf > d5 1 < +inf > } \
	    1 \
	    vertical { 1 centerFrame <	+inf * +inf > } \
	} \
	1 < +inf > \
    }

    *toggleZoom.label:				Toggle\nZoom
    *toggleZoom.outerOffset:			2
    *toggleZoom.width:				30
    *toggleZoom.height:				25

    *zoomIn.label:				Zoom\nIn
    *zoomIn.outerOffset:			2
    *zoomIn.width:				30
    *zoomIn.height:				25

    *x1.label:					1
    *z2.label:					2
    *z3.label:					3
    *z4.label:					4
    *z5.label:					5
    *z8.label:					8

    *controlPanel*zoomIn.foreground:		royalBlue3
    *controlPanel*z4.foreground:		royalBlue3
    *controlPanel*z5.foreground:		royalBlue3
    *controlPanel*z8.foreground:		royalBlue3
    *controlPanel*z2.foreground:		royalBlue3
    *controlPanel*z3.foreground:		royalBlue3

    *zoomOut.label:				Zoom\nOut
    *zoomOut.outerOffset:			2
    *zoomOut.width:				30
    *zoomOut.height:				25

    *centerFrame.label:				Center
    *centerFrame.outerOffset:			2
    *centerFrame.width:				30
    *centerFrame.height:			25

    *d2.label:					2
    *d3.label:					3
    *d4.label:					4
    *d5.label:					5
    *d8.label:					8

    *controlPanel*zoomOut.foreground:		mediumVioletRed
    *controlPanel*d2.foreground:		mediumVioletRed
    *controlPanel*d3.foreground:		mediumVioletRed
    *controlPanel*d4.foreground:		mediumVioletRed
    *controlPanel*d5.foreground:		mediumVioletRed
    *controlPanel*d8.foreground:		mediumVioletRed

    *viewButtons.location:			0 0 100 80
    *viewButtons.layout: horizontal { \
	2 < -2 > \
	aspect     < +inf * > 2 \
	flipX      < +inf * > 2 \
	flipY      < +inf * > 2 \
	flipXY     < +inf * > 2 \
	clearFrame < +inf * > 2 \
	fitFrame   < +inf * >   \
	2 < -2 > \
    }
    *nextFrame.label:				Next Frame
    *prevFrame.label:				Previous Frame
    *fitFrame.label:				Fit Frame
    *aspect.label:				Aspect
    *clearFrame.label:				Clear Frame
    *flipX.label:				Flip X
    *flipY.label:				Flip Y
    *flipXY.label:				Flip XY


    ! ENHANCEMENT
    ! ------------------
    *enhancementBox.label:			Enhancement
    *enhancementBox.location:			0 0 110	0
    *enhancementBox.shrinkToFit:		True
    *enhancementBox.outerOffset:		7

    *enhance*Viewport.allowVert:		True
    *enhance*Viewport.allowHoriz:		False
    *enhance*Viewport.useRight:			False
    *enhance*Viewport.resizeable:		True
    *enhance*Scrollbar.width:			17
    *enhance*Scrollbar.minimumThumb:		10
    *enhance.layout: vertical {	\
	3 < -3	> \
	horizontal { \
	    2 < -2 > \
	    colorlistFrame < +inf -inf	* +inff	-inff >	\
	    2 < -2 > \
	} \
	2 \
	horizontal { \
	    2 < -2 > \
	    colordataFrame < +inf -inf	* +inf -inf > \
	    2 < -2 > \
	} \
	5 < -5	> \
	horizontal { \
	    2 < -2 > \
	    vertical { -1 contrastLabel 3 < -3 > brightnessLabel -1 } \
	    3 < -3 > \
	    vertical {	\
		-1 \
		contrastSlider < +inf -inf * > \
		3 < -3 > \
		brightnessSlider < +inf -inf * > \
		-1 \
	    } \
	    2 < -2 > \
	} \
	5 < -5	> \
	horizontal { \
	    3 < -3 > \
	    invertButton < +inf -inf *	> \
	    5 < -5 > \
	    optimizeButton < +inf -inf	* > \
	    3 < -3 > \
	} \
	3 < -3	> \
    }

    *enhance*frameType:				sunken
    *enhance*frameWidth:			1
    *enhance*BorderWidth:			0
    *enhance*Label.ShadowWidth:			0

    *colorlist.width:				100
    *colorlist.height:				98
    *colordata.width:				100
    *colordata.height:				45
    *enhance*colordata.frameWidth:		0
    *contrastLabel.label:			x
    *contrastSlider.location:			0 0 100	17
    *brightnessLabel.label:			x
    *brightnessSlider.location:			0 0 100 17
    *invertButton.label:			Invert
    *optimizeButton.label:			Optimize


    ! ---------------------
    ! BLINK/REGISTER
    ! ---------------------
    *blinkBox.label:				Blink/Register
    *blinkBox.location:				0 0 235 0
    *blinkBox.shrinkToFit:			True
    *blinkBox.outerOffset:			7
    *blinkBox*TextToggle.frameWidth:		1

    *blink.layout: vertical { \
	space = (width	blinkFramesLabel - width blinkRateLabel) \
	3 < -3	> \
	horizontal { \
	    0 \
	    blinkFramesLabel \
	    3 < +inf >	\
	    blinkFrame1 < -50%	* > \
	    blinkFrame2 < -50%	* > \
	    blinkFrame3 < -50%	* > \
	    blinkFrame4 < -50%	* > \
	    4 < +inf >	\
	    blinkPanel	 < -50%	* > \
	    blinkReset	\
	    2 \
	} \
	5 < -5	> \
	horizontal { \
	    $space \
	    blinkRateLabel \
	    2	\
	    BRframe < +inf * >	\
	} \
	5 < +inf -100%	> \
	horizontal { \
	    vertical {	\
		2 matchButton	< +inf * > 4 < -4 > registerButton < +inf * > \
	    } \
	    5 < -5 > \
	    vertical {	\
		1 blinkButton	< +inf * > 4 < -4 > autoregButton < +inf * > \
	    } \
	} \
    }

    *BRlayout.layout: horizontal { \
	BRdecrease \
	BRtext	< +inf -100% * > \
	BRincrease \
    }

    *blink.Label.borderWidth:			0
    *blink.Label.shadowWidth:			0
    *controlPanel*blink*internalWidth:		4
    *blink.TextToggle.location:			0 0 102 23
    *blink.TextToggle.frameWidth:		1
    *blink*TextToggle.highlightColor:		yellow
    *blink*TextToggle.offIcon:			square0s
    *blink*TextToggle.onIcon:			square1s
    *blink*TextToggle.outerOffset:		1
    *blink*Command.height:			27
    *blink*Arrow.width:				16
    *blink*Arrow.height:			25


    *blinkFramesLabel.label:			Frames List:
    *blinkFrame1.label:				1
    *blinkFrame2.label:				2
    *blinkFrame3.label:				3
    *blinkFrame4.label:				4
    *blinkPanel.label:				x
    *blinkPanel.width:				20
    *blinkReset.label:				Reset

    *blinkRateLabel.label:			Blink Rate:
    *BRframe.frameType:				sunken
    *BRframe.frameWidth:			1
    *BRtext.width:				40
    *BRtext.height:				23
    *BRdecrease.direction:			left
    *BRincrease.direction:			right
    *registerButton.label:			Register
    *matchButton.label:				Match LUTs
    *blinkButton.label:				Blink
    *autoregButton.label:			Auto-Register

    ! OPTIONS
    ! ---------------------
    *optionsBox.label:				Options
    *optionsBox.location:			0 0 220 0
    *optionsBox.shrinkToFit:			False
    *optionsBox.outerOffset:			7
    *optionsBox*offIcon:			square0s
    *optionsBox*onIcon:				square1s
    *optionsBox*selectionStyle:			multi
    *optionsBox*highlightColor:			yellow
    *optionsBox.TextToggle.location:		0 0 102 20
    *optionsBox.TextToggle.frameWidth:		0
    *optionsBox.TextToggle.highlightThickness:	0
    *optionsBox*alignment:			left

    *pannerButton.label:			Panner
    *coordsBoxButton.label:			Coords Box
    *autoscaleButton.label:			Autoscale
    *antialiasButton.label:			Antialias
    *tileFramesButton.label:			Tile Frames
    *magnifierButton.label:			Magnifier
    *warningsButton.label:			Warnings
    *peakupButton.label:			Centroid Peaks
    *peakupButton.on:				True

    ! CONTROL
    ! ----------------------
    *controlBox.frameType:			chiseled
    *controlBox.frameWidth:			2
    *controlBox.outerOffset:			7
    *controlBox.innerOffset:			5
    *controlBox.height:				30

    *control.layout: horizontal	{ \
	1 \
	initializeButton < +inf * > \
	5 < -5	> \
	normalizeButton < +inf	* > \
	80 < +inf -100% > \
    }

    *initializeButton.label:			Initialize
    *normalizeButton.label:			Normalize
}

