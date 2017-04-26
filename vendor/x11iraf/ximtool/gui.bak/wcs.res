
set Resources(wcs_panel) { \

    !--------------------
    ! WCS Readout Panel
    !--------------------
    *wcsGroup*TextToggle.offIcon:               square0s
    *wcsGroup*TextToggle.onIcon:                square1s

    !*wcsLayout*TextToggle.location:             0 0 160 23
    *wcsLayout*TextToggle.height:		23
    *wcsLayout*TextToggle.outerOffset:          0
    *wcsLayout*Layout.borderWidth:		0
    *wcsLayout.borderWidth:			0
    *wcsLayout.layout: vertical { \
	0 < +0 -0 > \
	wcsGroup < +inf -inf * > \
	5 \
	horizontal { -1 wcsOptGroup < +inf -inf * > -1 } \
	-2 \
    }

    *wcsOptGroup.label:
    *wcsOptGroup.outerOffset:                   0
    *wcsOptGroup.innerOffset:                   5
    *wcsOptGroup.frameType:                     chiseled
    *wcsOptGroup.frameWidth:                    0
    *wcsOptLayout.layout: vertical { \
	horizontal { -9 wcsCoords   < +inf -inf * > -9 } \
	-3 \
	horizontal { -5 wcsOpts     < +inf -inf * > -5 } \
	horizontal { -5 wcsIsmGroup < +inf -inf * > -5 } \
	1 \
    }

    *wcsOpts.label:
    *wcsOpts.width:				265
    *wcsOpts.height:				40
    *wcsOpts.outerOffset:			3
    *wcsOpts.innerOffset:			3
    *woLayout*TextToggle.frameWidth:		0
    *woLayout*TextToggle.onIcon:		square1s
    *woLayout*TextToggle.offIcon:		square0s
    *woLayout*TextToggle.highlightColor:	cyan
    *woLayout*TextToggle.alignment:		left
    *woLayout*TextToggle.leftMargin:		5
    *woLayout.layout: horizontal { \
	3 \
	woptLabels < +inf -inf * > 2 \
	woptTitles < +inf -inf * > 2 \
	woptFBinfo < +inf -inf * > 2 \
	woptBPM    < +inf -inf * >   \
	3 \
    }
    *woptLabels.label:				WCS Labels
    *woptLabels.on:				True
    *woptTitles.label:				Image Titles
    *woptTitles.on:				True
    *woptFBinfo.label:				Frame Buffer Info
    *woptFBinfo.on:				True
    *woptBPM.label:				BPM Data
    *woptBPM.on:				False
    *woptLabels.location:			0 0 150 21
    *woptTitles.location:			0 0 150 21
    *woptFBinfo.location:			0 0 175 21
    *woptBPM.location:				0 0 120 21


    *wcsCoords.label:				Readout Values
    *wcsCoords.width:				265
    *wcsCoords.height:				135
    *wcsCoords.outerOffset:			7
    *wcsCoords.innerOffset:			3
    *wcLayout*TextToggle.offIcon:           	diamond0s
    *wcLayout*TextToggle.onIcon:            	diamond1s
    *wcLayout*TextToggle.highlightColor:    	yellow2
    *wcLayout*TextToggle.shrinkToFit:		True
    *wcLayout*TextToggle.frameWidth:		0
    *wcLayout*TextToggle.label:
    *wcLayout*TextToggle.on:			True
    *wcLayout*Label.justify:			right
    *wcLayout*Label.font:			7x13bold
    *wcLayout*SimpleMenu.borderColor:           black
    *wcLayout*SimpleMenu.borderWidth:           1
    *wcLayout*SimpleMenu.foreground:            White
    *wcLayout*SimpleMenu.background:            SteelBlue
    *wcLayout*MenuButton.shadowWidth:		1
    *wcLayout*MenuButton.resize:		False
    *wcLayout.layout: vertical {\
	1 \
	horizontal { 20 < -20 > wcTitle < +inf -inf * > 20 < -20 > }\
	1 \
	horizontal { 5 wcLine < +inf -inf * > 5 } \
	5 \
	horizontal { \
	    10 \
	    vertical { 2 wlWcs1  2 wlWcs2  2 wlWcs3  2 wlWcs4  2 }  5 \
	    vertical { 1 sysWcs1 1 sysWcs2 1 sysWcs3 1 sysWcs4 1 }  5 \
	    vertical { 1 fmtWcs1 1 fmtWcs2 1 fmtWcs3 1 fmtWcs4 1 } 20 \
	    vertical { 1 wpWcs1  3 wpWcs2  3 wpWcs3  3 wpWcs4  3 } 20 \
	    vertical { 1 wiWcs1  3 wiWcs2  3 wiWcs3  3 wiWcs4  3 } 15 \
	} \
	3 \
    }
    *wcTitle.label:    Type\ \ \ \ \ \ \ \ \ \ Format\ \ \ \ \ \ Panel\ ImgWin

    *wcLine.height:				2
    *wcLine.frameWidth:				2
    *wcLine.frameType:				ledged

    *wlWcs1.label:				First WCS
    *wlWcs2.label:				Second WCS
    *wlWcs3.label:				Third WCS
    *wlWcs4.label:				Fourth WCS
    *sysWcs1.label:				\ Image Display\ 
    *sysWcs1.menuName:				sysMenu1
    *sysWcs2.label:				None
    *sysWcs2.menuName:				sysMenu2
    *sysWcs3.label:				None
    *sysWcs3.menuName:				sysMenu3
    *sysWcs4.label:				None
    *sysWcs4.menuName:				sysMenu4
    *fmtWcs1.label:				\ Sexigesimal\ 
    *fmtWcs1.menuName:				fmtMenu1
    *fmtWcs2.label:				None
    *fmtWcs2.menuName:				fmtMenu2
    *fmtWcs3.label:				None
    *fmtWcs3.menuName:				fmtMenu3
    *fmtWcs4.label:				None
    *fmtWcs4.menuName:				fmtMenu4

    *editMenu fmtWcsMenu$i fmtWcs$i $items

    *wcsIsmGroup.label:
    *wcsIsmGroup.width:				395
    *wcsIsmGroup.height:			50
    *wcsIsmGroup.outerOffset:			3
    *wcsIsmGroup.innerOffset:			5
    *wcsIsmGroup*borderWidth:			0
    *wcsIsmLayout.layout: horizontal { \
	wcsIsmLabel 1 wcsIsmFrame < +inf -inf * > 3 wcsIsmInit 1 \
     }
    *wcsIsmLabel.label:				ISM Command
    *wcsIsmFrame.frameType:			sunken
    *wcsIsmFrame.frameWidth:			1
    *wcsIsmFrame.outerOffset:			1
    *wcsIsmFrame*height:			23
    *wcsIsmCmd*editType:			edit
    *wcsIsmCmd.displayCaret:			True
    *wcsIsmInit.label:				Initialize
    *wcsIsmInit.internalWidth:			7

    *wcsBox*borderWidth:			0
    *wcsBox*TextToggle.frameType:		raised
    *wcsBox*TextToggle.frameWidth:		1
    *wcsBox*TextToggle.leftMargin:		2
    *wcsBox*borderWidth:			0
    *wcsBox.layout: vertical { \
	wcsFrame < +inf -inf * > \
	2 \
	horizontal { \
	    2 \
	    ismToggle   < +inf -inf * > 2 \
	    pixelTable  < +inf -inf * > 2 \
	    imageHeader < +inf -inf * > 2 \
	    compass     < +inf -inf * > 2 \
	    wcsOptions  < +inf -inf * >   \
	    2 \
	} \
    }
    *ismToggle.label:				ISM Mod
    *ismToggle.label:				WCS/Pix
    *pixelTable.label:				Pix Table
    *imageHeader.label:				Header
    *imageHeader.sensitive:			False
    *compass.label:				Compass
    *wcsOptions.label:				Options

    *wcsGroup.label:
    *wcsGroup.outerOffset:                      0
    *wcsGroup.innerOffset:                      5
    *wcsGroup.frameType:                        chiseled
    *wcsGroup.frameWidth:                       2
    *wcsGroup*Text*width:			260
    *wcsGroup*Text*height:			17
    *wcsGroup*Text*font:			7x13
    *wcsGroup*Text*displayCaret:		False
    *wcsGroup*Text*editType:			read
    *wcsGroup*Text*background:			black
    *wcsGroup*Text*foreground:			yellow2
    *wcsGroup*TextToggle.highlightColor:        cyan
    *wcsFrame.frameType:			sunken
    *wcsFrame.frameWidth:			1
    *wcsText*background:			yellow4
    *wcsText.layout: vertical { \
	wtName    < +inf -inf * > -3 \
	wtTitle   < +inf -inf * > -3 \
	wtFBCfg   < +inf -inf * >    \
	1 < -1 > \
	wtWcs1  < +inf -inf * > -3 \
	wtWcs2  < +inf -inf * > -3 \
	wtWcs3  < +inf -inf * > -3 \
	wtWcs4  < +inf -inf * >    \
	1 < -1 > \
	horizontal { \
	    wtIPixval < +inf -inf * +inf > 1 \
	    wtSPixval < +inf -inf * +inf > 1 \
	    wtBPixval < +inf -inf * +inf > \
	} \
    }
}

