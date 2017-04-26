
set Resources(ximtool) { \

    ! Main image window	resources.
    ! -------------------------------
    *allowShellResize:				True
    *beNiceToColormap:				False
    *menuLabel.foreground:			Gold
    *markerMenu.foreground:			White
    *rulerMenu.foreground:			White
    *display.imagewin.markerMenu.foreground:	White
    *display.imagewin.rulerMenu.foreground:	White
    *markerMenu.background:			SteelBlue
    *markerMenu*SimpleMenu.foreground:		White
    *markerMenu*SimpleMenu.background:		SteelBlue
    *rulerMenu.background:			SteelBlue
    *rulerMenu*SimpleMenu.foreground:		White
    *rulerMenu*SimpleMenu.background:		SteelBlue
    *rulerColor.SmeBSB.leftMargin:		16
    *magzoomMenu.foreground:			White
    *magzoomMenu.background:			SteelBlue
    *magzoomMenu*SimpleMenu.foreground:		White
    *magzoomMenu*SimpleMenu.background:		SteelBlue
    *markerColor.SmeBSB.leftMargin:		64
    *markerColor.SmeBSB.rightMargin:		0
    *markerColor.menuLabel.leftMargin:		5
    *markerColor.menuLabel.rightMargin:		5
    *rulerColor.SmeBSB.leftMargin:		64
    *rulerColor.SmeBSB.rightMargin:		0
    *rulerColor.menuLabel.leftMargin:		5
    *rulerColor.menuLabel.rightMargin:		5

    *ximtool.title:				XIMTOOL_VERSION

    *display.borderWidth:			2
    *display.layout: horizontal	{ \
	-0 \
	vertical { \
	    menuGroup  < +inf -inf * > \
	    horizontal { 1 toolbar < +inf -inf * > 1 } \
	    1 < -1 > \
	    horizontal { 1 panelbar < +inf -inf * > 1 } \
	    1 < -1 > \
	    vertical { \
	        horizontal { \
	            imageFrame < +inf -inf * +inf -inf > \
		    -2 \
	            vcutFrame  < * +inf -inf > \
	        } \
		-3 \
	        horizontal { \
	            hcutFrame  < +inf -inf * > 1 \
		    vertical { 4 horizontal { -4 plotOpts}  -4 } \
	        } \
		-3 \
	        horizontal { \
	            cbarFrame  < +inf -inf * > \
	            focusSlider 2 hcut 2 vcut 15 \
	        } \
	    } \
	} \
	-0 \
    }
    *hcut.label:				H
    *hcut.font:					*lucida-bold-r*10*
    *vcut.label:				V
    *vcut.font:					*lucida-bold-r*10*
    *focusSlider.location:			0 0 100 15

    *hcutFrame.frameType:                       sunken
    *hcutFrame.frameWidth:                      1
    *hcutFrame.innerOffset:             	1
    *hcutFrame.outerOffset:             	1
    //*hcutPlot.color0:                           #c4c4c4
    //*hcutPlot.color1:                           #000000
    *hcutPlot.color0:                           #000000
    *hcutPlot.color1:                           #ffffff
    *hcutPlot.crosshairCursorColor:		red
    *hcutPlot.width:                            512
    *hcutPlot.height:                           6
    *hcutPlot.borderWidth:			0
    *hcutPlot.alphaFont1:			6x10
    *hcutPlot.alphaFont2:			6x10
    *hcutPlot.alphaFont3:			6x10
    *hcutPlot.alphaFont4:			6x10
    *hcutPlot.alphaFont5:			6x10
    *hcutPlot.alphaFont6:			6x10
    *hcutPlot.alphaFont7:			6x10
    *hcutPlot.alphaFont8:			6x10
    *hcutPlot.translations:	\
	 !Ctrl	<Key>1:	call(cpZoom,1,1,fixed) 		\n\
	 !Ctrl	<Key>2:	call(cpZoom,2,2,fixed) 		\n\
	 !Ctrl	<Key>3:	call(cpZoom,3,3,fixed) 		\n\
	 !Ctrl	<Key>4:	call(cpZoom,4,4,fixed) 		\n\
	 !Ctrl	<Key>5:	call(cpZoom,5,5,fixed) 		\n\
	 !Ctrl	<Key>6:	call(cpZoom,6,6,fixed) 		\n\
	 !Ctrl	<Key>7:	call(cpZoom,7,7,fixed) 		\n\
	 !Ctrl	<Key>8:	call(cpZoom,8,8,fixed) 		\n\
	 !Ctrl	<Key>9:	call(cpZoom,9,9,fixed) 		\n\
	 !Ctrl	<Key>b:	call(prevFrame,$name)		\n\
	 !Ctrl	<Key>f:	call(nextFrame,$name)		\n\
	 !Ctrl	<Key>i:	call(cpInvert)		       	\n\
	 !Ctrl	<Key>m:	call(toggleMagnifier)		\n\
	 !Ctrl	<Key>n:	call(normalize)			\n\
	 !Ctrl	<Key>p:	call(togglePanner)		\n\
      !Ctrl Alt <Key>q: call(Quit)			\n\
	 !Ctrl	<Key>r:	call(cpRegisterFrames)		\n\
	 !Ctrl	<Key>s:	call(cpMatchFrames)		\n\
	 !Ctrl	<Key>u:	call(cpZoom,1,1,fixed) 		\n\
	  Ctrl	<Key>+:	call(cpZoom,2.0,2.0,relative)	\n\
	  Ctrl	<Key>-:	call(cpZoom,0.5,0.5,relative)	\n\
	  Ctrl <Key>\<:	call(cpSetBlinkRate,BRdecrease)	\n\
	  Ctrl <Key>\>:	call(cpSetBlinkRate,BRincrease)	\n\
	  !Alt	<Key>b:	call(toggleBlink)	     	\n\
	  !Alt	<Key>c:	call(displayPanel)		\n\
	  !Alt	<Key>h:	call(Help)		 	\n\
	  !Alt	<Key>i:	call(infoPanel)		      	\n\
      !Ctrl Alt <Key>f: call(fitFrame)		 	\n\
	  !Alt	<Key>l:	call(loadPanel)		      	\n\
	  !Alt	<Key>p:	call(printPanel)	       	\n\
	  !Alt	<Key>s:	call(savePanel)		      	\n\
	  !Alt	<Key>t:	call(tclPanel)		     	\n\
	   !<Btn1Down>:	call(makeMarker,$name,$x,$y) m_create()	\n\
	 <EnterWindow>:	enter-window()			\n\
	 <LeaveWindow>:	leave-window()			\n\
	    <KeyPress>:	graphics-input()		\n\
         !Alt <Motion>:	call(curtrack_msg,$x,$y)	\n\
	      <Motion>:	track-cursor() call(hcutWCSUpdate,$x,$y)


    *vcutFrame.frameType:                       sunken
    *vcutFrame.frameWidth:                      1
    *vcutFrame.innerOffset:             	0
    *vcutFrame.outerOffset:             	0
    //*vcutPlot.color0:                           #c4c4c4
    //*vcutPlot.color1:                           black
    *vcutPlot.color0:                           #000000
    *vcutPlot.color1:                           #ffffff
    *vcutPlot.crosshairCursorColor:		red
    *vcutPlot.width:                            6
    *vcutPlot.height:                           512
    *vcutPlot.borderWidth:			0
    *vcutPlot.alphaFont1:			6x10
    *vcutPlot.alphaFont2:			6x10
    *vcutPlot.alphaFont3:			6x10
    *vcutPlot.alphaFont4:			6x10
    *vcutPlot.alphaFont5:			6x10
    *vcutPlot.alphaFont6:			6x10
    *vcutPlot.alphaFont7:			6x10
    *vcutPlot.alphaFont8:			6x10
    *vcutPlot.translations:	\
	 !Ctrl	<Key>1:	call(cpZoom,1,1,fixed) 		\n\
	 !Ctrl	<Key>2:	call(cpZoom,2,2,fixed) 		\n\
	 !Ctrl	<Key>3:	call(cpZoom,3,3,fixed) 		\n\
	 !Ctrl	<Key>4:	call(cpZoom,4,4,fixed) 		\n\
	 !Ctrl	<Key>5:	call(cpZoom,5,5,fixed) 		\n\
	 !Ctrl	<Key>6:	call(cpZoom,6,6,fixed) 		\n\
	 !Ctrl	<Key>7:	call(cpZoom,7,7,fixed) 		\n\
	 !Ctrl	<Key>8:	call(cpZoom,8,8,fixed) 		\n\
	 !Ctrl	<Key>9:	call(cpZoom,9,9,fixed) 		\n\
	 !Ctrl	<Key>b:	call(prevFrame,$name)		\n\
	 !Ctrl	<Key>f:	call(nextFrame,$name)		\n\
	 !Ctrl	<Key>i:	call(cpInvert)		       	\n\
	 !Ctrl	<Key>m:	call(toggleMagnifier)		\n\
	 !Ctrl	<Key>n:	call(normalize)			\n\
	 !Ctrl	<Key>p:	call(togglePanner)		\n\
      !Ctrl Alt <Key>q: call(Quit)			\n\
	 !Ctrl	<Key>r:	call(cpRegisterFrames)		\n\
	 !Ctrl	<Key>s:	call(cpMatchFrames)		\n\
	 !Ctrl	<Key>u:	call(cpZoom,1,1,fixed) 		\n\
	  Ctrl	<Key>+:	call(cpZoom,2.0,2.0,relative)	\n\
	  Ctrl	<Key>-:	call(cpZoom,0.5,0.5,relative)	\n\
	  Ctrl <Key>\<:	call(cpSetBlinkRate,BRdecrease)	\n\
	  Ctrl <Key>\>:	call(cpSetBlinkRate,BRincrease)	\n\
	  !Alt	<Key>b:	call(toggleBlink)	     	\n\
	  !Alt	<Key>c:	call(displayPanel)		\n\
	  !Alt	<Key>h:	call(Help)		 	\n\
	  !Alt	<Key>i:	call(infoPanel)		      	\n\
      !Ctrl Alt <Key>f: call(fitFrame)		 	\n\
	  !Alt	<Key>l:	call(loadPanel)		      	\n\
	  !Alt	<Key>p:	call(printPanel)	       	\n\
	  !Alt	<Key>s:	call(savePanel)		      	\n\
	  !Alt	<Key>t:	call(tclPanel)		     	\n\
	   !<Btn1Down>:	call(makeMarker,$name,$x,$y) m_create()	\n\
	 <EnterWindow>:	enter-window()			\n\
	 <LeaveWindow>:	leave-window()			\n\
	    <KeyPress>:	graphics-input()		\n\
         !Alt <Motion>:	call(curtrack_msg,$x,$y)	\n\
	      <Motion>:	track-cursor() call(vcutWCSUpdate,$x,$y)


    *plotOpts.label:                            Options
    !*plotOpts*location:                         0 0 105 18
    *plotOpts*location:                         0 0 1 18
    *plotOpts.shrinkToFit:                      True
    *plotOpts.outerOffset:                      5
    *plotOpts.innerOffset:                      3
    *plotOpts.frameWidth:                       2
    *plotOpts*TextToggle.offIcon:               square0s
    *plotOpts*TextToggle.onIcon:                square1s
    *plotOpts*TextToggle.highlightColor:        yellow
    *plotOpts*TextToggle.outerOffset:           0
    *plotOpts*TextToggle.frameWidth:            0
    *plotOpts*TextToggle.selectionStyle:	multi
    *plotOpts*TextToggle.leftMargin:		3
    *plotOpts*TextToggle.alignment:		left

    *poptsLayout.borderWidth:			0
    *poptsLayout.layout: vertical { \
	-1 \
        plotSpeed plotAccurate plotImgPix \
	2 < -2 > \
        horizontal { -4 optLine < +inf -inf * > -4 } \
	2 < -2 > \
        curJump curSmooth curTrack \
	-1 \
     }
    *plotSpeed.label:				Better Speed
    *plotSpeed.on:			 	True
    *plotAccurate.label:			Better Accuracy
    *plotAccurate.on:				False
    *plotImgPix.label:				Image Pixels
    *plotImgPix.on:				False
    *plotImgPix.sensitive:			False
    *optLine.height:                            2
    *optLine.width:				120
    *optLine.outerOffset:			0
    *optLine.innerOffset:			0
    *optLine.frameWidth:                        2
    *optLine.frameType:                         chiseled
    *curJump.label:				Jump Cursor
    *curJump.on:				True
    *curSmooth.label:				Smooth Cursor
    *curSmooth.on:				False
    *curTrack.label:				Graphics Cursors
    *curTrack.on:				True


    *menuGroup.label:				
    *menuGroup.height:				38
    *menuGroup.width:				512
    *menuGroup.frameType:			raised
    *menuGroup.frameWidth:			2
    *menubar.layout: horizontal	{ \
	2 < -2 > \
	fileButton 1 < -1 > viewButton	1 < -1 > optionsButton \
	1 < -1	> \
	imageTitle < +inff -inff * > \
	1 < -1	> \
	mXflipButton 1	< -1 > mYflipButton \
	1 < -1	> \
	mPrevButton 1 < -1 > mFrameButton 1 < -1 > mNextButton	\
	1 < -1	> \
	toolButton 1 <	-1 > panelButton \
	2 < -2 > \
    }

    *menubar*SimpleMenu.foreground:		Black
    *menubar*SimpleMenu.borderWidth:		1
    *menubar*SimpleMenu.foreground:		White
    *menubar*SimpleMenu.background:		SteelBlue
    *SmeBSB.vertSpace:				10

    *menubar.width:				512
    *menubar*borderWidth:			0
    *menubar*Command.internalWidth:		0
    *menubar*Command.borderWidth:		0
    *menubar*Toggle.label:			x
    *menubar*Toggle.internalWidth:		0
    *menubar*Toggle.borderWidth:		0

    *toolbar.layout: horizontal	{ \
	tbZoomIn    < +inf -inf * > 1 < -1 > \
	tbZoom0     < +inf -inf * > 1 < -1 > \
	tbZoomOut   < +inf -inf * > 1 < -1 > \
	tbInvert    < +inf -inf * > 1 < -1 > \
	tbNormalize < +inf -inf * > 1 < -1 > \
	tbMatchLUT  < +inf -inf * > 1 < -1 > \
	tbAutoReg   < +inf -inf * > 1 < -1 > \
	tbRegister  < +inf -inf * > 1 < -1 > \
	tbCenter    < +inf -inf * > 1 < -1 > \
	tbBlinkDec  < +inf -inf * > 1 < -1 > \
	tbBlink	    < +inf -inf * > 1 < -1 > \
	tbBlinkInc  < +inf -inf * > 1 < -1 > \
	xflipButton < +inf -inf * > 1 < -1 > \
	yflipButton < +inf -inf * > 1 < -1 > \
	tbCompass   < +inf -inf * > 1 < -1 > \
	tbTile      < +inf -inf * > 1 < -1 > \
	prevButton  < +inf -inf * > 1 < -1 > \
	frameButton < +inf -inf * > 1 < -1 > \
	nextButton  < +inf -inf * > 1 < -1 > \
    }
    *toolbar.width:				512
    *toolbar.height:				25
    *toolbar*borderWidth:			0
    *toolbar*Command.internalWidth:		3
    *toolbar*Command.borderWidth:		0
    *toolbar*Toggle.internalWidth:		2
    *toolbar*Toggle.borderWidth:		0

    *panelbar.layout: horizontal { \
        pbDisplayP < +inf -inf * > 1 < -1 > \
	pbPrintP   < +inf -inf * > 1 < -1 > \
	pbLoadP    < +inf -inf * > 1 < -1 > \
	pbSaveP    < +inf -inf * > 1 < -1 > \
	pbInfoP    < +inf -inf * > 1 < -1 > \
	pbTileP    < +inf -inf * > 1 < -1 > \
	pbCoordP   < +inf -inf * > 1 < -1 > \
        pbPanM	   < +inf -inf * > 1 < -1 > \
	pbMagM     < +inf -inf * > 1 < -1 > \
	pbWcsM     < +inf -inf * > 1 < -1 > \
	pbIsm      < +inf -inf * > 1 < -1 > \
        helpButton < +inf -inf * > 1 < -1 > \
	pbQuit     < +inf -inf * > 1 < -1 > \
    }
    *panelbar*SimpleMenu.borderColor:		 Black
    *panelbar*SimpleMenu.borderWidth:		 1
    *SmeBSB.vertSpace:				 10

    *panelbar.width:				 12
    *panelbar.height:				 25
    *panelbar*borderWidth:			 0
    *panelbar*Label.internalWidth:		 4
    *panelbar*Label.borderWidth:		 0
    *panelbar*Command.internalWidth:		 4
    *panelbar*Command.borderWidth:		 0
    *panelbar*Toggle.internalWidth:		 3
    *panelbar*Toggle.borderWidth:		 0

    *pbLoadP.label:				Load
    *pbDisplayP.label:				Display
    *pbSaveP.label:				Save
    *pbPrintP.label:				Print
    *pbInfoP.label:				Info
    *pbTileP.label:				Tile
    *pbCoordP.label:				Coords
    *pbPanM.label:				Pan
    *pbPanM.state:				1
    *pbMagM.label:				Mag
    *pbMagM.state:				0
    *pbWcsM.label:				WCS
    *pbWcsM.state:				1
    *pbIsm.label:				ISM
    *pbIsm.state:				0
    *helpButton.label:				

    *fileButton.label:				File
    *fileButton.menuName:			fileMenu
    *viewButton.label:				View
    *viewButton.menuName:			viewMenu
    *optionsButton.label:			Options
    *optionsButton.menuName:			optionsMenu
    *toolButton.label:			
    *toolButton.state:				0
    *panelButton.label:			
    *panelButton.state:				0
    *imageTitle*font:				*times-bold-r*12*
    *imageTitle.width:				40
    *imageTitle.height:				20
    *frameButton.menuName:			frameMenu
    *frameButton.label:				1
    *frameButton.width:				20
    *mFrameButton.menuName:			mFrameMenu
    *mFrameButton.label:			1
    *mFrameButton.width:			20
    *pbQuit.label:				Quit
    *tbZoom0.label:				Mag
    *tbBlink.label:				Blink
    *tbBlink.internalWidth:			3
    *tbCenter.label:				Cntr
    *tbAutoReg.label:				AReg
    *tbRegister.label:				Reg
    *tbInvert.label:				Inv
    *tbNormalize.label:				Norm
    *tbMatchLUT.label:				Match

    *SimpleMenu*font:     -adobe-times-bold-r-normal-*-12-*-*-*-*-*-iso8859-1
    *fileButton.font:     -adobe-times-bold-i-normal-*-12-*-*-*-*-*-iso8859-1
    *optionsButton.font:  -adobe-times-bold-i-normal-*-12-*-*-*-*-*-iso8859-1
    *viewButton.font:     -adobe-times-bold-i-normal-*-12-*-*-*-*-*-iso8859-1
    *panelbar*font:	  -adobe-times-bold-i-normal-*-12-*-*-*-*-*-iso8859-1
    *toolbar*font:	  -adobe-times-bold-i-normal-*-12-*-*-*-*-*-iso8859-1

    *Gterm.cmapName:				image
    *Gterm.basePixel:				64
    *imageFrame.frameType:			sunken
    *imageFrame.frameWidth:			1
    *imageFrame.outerOffset:			0
    *imageFrame.innerOffset:			0
    *imageFrame.width:                          512
    *imageFrame.height:                         512
    *cbarFrame.frameType:			sunken
    *cbarFrame.frameWidth:			1
    *cbarFrame.outerOffset:			0
    *cbarFrame.innerOffset:			0
    *cbarFrame.width:                           512
    *imagewin.warpCursor:			true
    *imagewin.raiseWindow:			true
    *imagewin.deiconifyWindow:			true
    *imagewin.idleCursor:			crosshair
    *imagewin.ginmodeCursor:			circle
    *imagewin.ginmodeBlinkInterval:		500
    *imagewin.resizable:			true
    *imagewin.copyOnResize:			false
    *imagewin.width:				512
    *imagewin.height:				512
    *imagewin.maxMappings:			64
    *imagewin.borderWidth:			0

    *imagewin.translations:	\
	 None<Key>Left:	call(move_cursor,-1,0)		\n\
	 None<Key>Down:	call(move_cursor,0,1)		\n\
	   None<Key>Up:	call(move_cursor,0,-1)		\n\
        None<Key>Right: call(move_cursor,1,0)		\n\
      !Shift <Key>Left: call(move_cursor,-10,0)		\n\
      !Shift <Key>Down: call(move_cursor,0,10)		\n\
        !Shift <Key>Up: call(move_cursor,0,-10)		\n\
     !Shift <Key>Right: call(move_cursor,10,0)		\n\
	 !Ctrl	<Key>h:	call(move_cursor,-1,0)		\n\
	 !Ctrl	<Key>j:	call(move_cursor,0,1)		\n\
	 !Ctrl	<Key>k:	call(move_cursor,0,-1)		\n\
	 !Ctrl	<Key>l:	call(move_cursor,1,0)		\n\
    !Ctrl Shift <Key>h: call(move_cursor,-10,0)		\n\
    !Ctrl Shift <Key>j: call(move_cursor,0,10)		\n\
    !Ctrl Shift <Key>k: call(move_cursor,0,-10)		\n\
    !Ctrl Shift <Key>l: call(move_cursor,10,0)		\n\
       !Ctrl <Key>Left: call(moveFrame,-1,0)		\n\
       !Ctrl <Key>Down: call(moveFrame,0,1)		\n\
	 !Ctrl <Key>Up:	call(moveFrame,0,-1)		\n\
      !Ctrl <Key>Right: call(moveFrame,1,0)		\n\
   !Ctrl Alt <Key>Left: call(moveFrame,-0.5,0)		\n\
   !Ctrl Alt <Key>Down: call(moveFrame,0,0.5)		\n\
     !Ctrl Alt <Key>Up: call(moveFrame,0,-0.5)		\n\
  !Ctrl Alt <Key>Right: call(moveFrame,0.5,0)		\n\
	  !Alt	<Key>1:	call(cpSetFrame,frame1)		\n\
	  !Alt	<Key>2:	call(cpSetFrame,frame2)		\n\
	  !Alt	<Key>3:	call(cpSetFrame,frame3)		\n\
	  !Alt	<Key>4:	call(cpSetFrame,frame4)		\n\
	 !Ctrl	<Key>1:	call(cpZoom,1,1,fixed) 		\n\
	 !Ctrl	<Key>2:	call(cpZoom,2,2,fixed) 		\n\
	 !Ctrl	<Key>3:	call(cpZoom,3,3,fixed) 		\n\
	 !Ctrl	<Key>4:	call(cpZoom,4,4,fixed) 		\n\
	 !Ctrl	<Key>5:	call(cpZoom,5,5,fixed) 		\n\
	 !Ctrl	<Key>6:	call(cpZoom,6,6,fixed) 		\n\
	 !Ctrl	<Key>7:	call(cpZoom,7,7,fixed) 		\n\
	 !Ctrl	<Key>8:	call(cpZoom,8,8,fixed) 		\n\
	 !Ctrl	<Key>9:	call(cpZoom,9,9,fixed) 		\n\
	 !Ctrl	<Key>a:	call(tbToggleAutoReg)		\n\
	 !Ctrl	<Key>b:	call(prevFrame,$name)		\n\
	 !Ctrl	<Key>c:	call(cpZoomAction,centerFrame)	\n\
	 !Ctrl	<Key>f:	call(nextFrame,$name)		\n\
	 !Ctrl	<Key>i:	call(cpInvert)		       	\n\
	 !Ctrl	<Key>m:	call(toggleMagnifier)		\n\
	 !Ctrl	<Key>n:	call(normalize)			\n\
	 !Ctrl	<Key>o:	call(offset,$x,$y)		\n\
	 !Ctrl	<Key>p:	call(togglePanner)		\n\
      !Ctrl Alt <Key>q: call(Quit)			\n\
	 !Ctrl	<Key>r:	call(cpRegisterFrames)		\n\
	 !Ctrl	<Key>s:	call(cpMatchFrames)		\n\
	 !Ctrl	<Key>t:	call(tbTileFramesToggle)	\n\
	 !Ctrl	<Key>u:	call(cpZoom,1,1,fixed) 		\n\
	 !Ctrl	<Key>x:	call(cpFrameAction,flipX)	\n\
	 !Ctrl	<Key>y:	call(cpFrameAction,flipY)	\n\
	  Ctrl	<Key>+:	call(cpZoom,2.0,2.0,relative)	\n\
	  Ctrl	<Key>-:	call(cpZoom,0.5,0.5,relative)	\n\
      !Ctrl Alt <Key>=: call(Print)			\n\
	  Ctrl <Key>\<:	call(cpSetBlinkRate,BRdecrease)	\n\
	  Ctrl <Key>\>:	call(cpSetBlinkRate,BRincrease)	\n\
	  Ctrl <Key>\[:	call(setCtrBoxSize,$x,$y,-1)	\n\
	  Ctrl <Key>\]:	call(setCtrBoxSize,$x,$y,1)	\n\
	 !Ctrl	<Key>0:	call(centroid,$x,$y,peak) 	\n\
      !Ctrl Alt <Key>0:	call(centroid,$x,$y,min) 	\n\
	  !Alt	<Key>b:	call(toggleBlink)	     	\n\
	  !Alt	<Key>c:	call(displayPanel)		\n\
    !Ctrl Shift <Key>c:	call(displayPanel)		\n\
      !Ctrl Alt <Key>f: call(fitFrame)		 	\n\
	  !Alt	<Key>h:	call(Help)		 	\n\
	  !Alt	<Key>i:	call(infoPanel)		      	\n\
	  !Alt	<Key>l:	call(loadPanel)		      	\n\
	  !Alt	<Key>p:	call(printPanel)	       	\n\
	  !Alt	<Key>s:	call(savePanel)		      	\n\
	  !Alt	<Key>t:	call(tclPanel)		     	\n\
    !Ctrl Shift <Key>t:	call(tclPanel)		     	\n\
      !Shift<Btn1Down>: call(setDynamicMagnifier,1)	\n\
        !Shift<Btn1Up>: call(setDynamicMagnifier,0)	\n\
	   !<Btn1Down>:	call(makeMarker,$name,$x,$y) m_create()	\n\
     !Shift <Btn2Down>: crosshair(on)			\n\
   !Shift <Btn2Motion>: crosshair(on)			\n\
       !Shift <Btn2Up>: crosshair(off)			\n\
 	     !<Btn2Up>:	crosshair(off)			\n\
	   !<Btn2Down>:	call(zoom,$x,$y)		\n\
	   !<Btn3Down>:	call(windowColormap,$x,$y)	\n\
	     !<Btn3Up>:	call(updateColormap,$x,$y)	\n\
	 !<Btn3Motion>:	call(windowColormap,$x,$y)	\n\
      !Ctrl <Btn1Down>:	call(makeRuler,$name,$x,$y) 	\n\
    !Ctrl <Btn1Motion>:	track-cursor() call(wcsUpdate,$x,$y) call(magnifierMapImage,$x,$y) call(resizeRuler,$x,$y,0) 	\n\
        !Ctrl <Btn1Up>:	call(deleteRuler,$x,$y) 	\n\
	 <EnterWindow>:	enter-window()			\n\
	 <LeaveWindow>:	leave-window()			\n\
	    <KeyPress>:	graphics-input()		\n\
         !Alt <Motion>:	call(curtrack_msg,$x,$y)	\n\
	      <Motion>:	track-cursor() call(wcsUpdate,$x,$y) call(magnifierMapImage,$x,$y)

! The following	translations can be used to enable windowing of	the
! individual RGB components of the colormap.  It's not very useful but
! included here	for those that may wish	to use it.
!--------------------------------------------------------------------------
!     !Ctrl <Btn1Down>:	call(windowRGB,1,$x,$y,0)	\n\
!   !Ctrl <Btn1Motion>:	call(windowRGB,1,$x,$y,0)	\n\
!	!Ctrl <Btn1Up>:	call(windowRGB,1,$x,$y,1)	\n\
!     !Ctrl <Btn2Down>:	call(windowRGB,2,$x,$y,0)	\n\
!   !Ctrl <Btn2Motion>:	call(windowRGB,2,$x,$y,0)	\n\
!	!Ctrl <Btn2Up>:	call(windowRGB,2,$x,$y,1)	\n\
!     !Ctrl <Btn3Down>:	call(windowRGB,3,$x,$y,0)	\n\
!   !Ctrl <Btn3Motion>:	call(windowRGB,3,$x,$y,0)	\n\
!	!Ctrl <Btn3Up>:	call(windowRGB,3,$x,$y,1)	\n\


    *colorbar.maxRasters:			1
    *colorbar.maxMappings:			1
    *colorbar.height:				17
    *colorbar.width:				50
    *colorbar.borderWidth:			0
}

