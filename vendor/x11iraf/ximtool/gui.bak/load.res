
set Resources(load_panel) { \

    !-------------------------------
    ! File Load	Control	Panel.
    !-------------------------------
    *filesLayout*borderWidth:			0
    *filesLayout*Group.shrinkToFit:		True
    *filesLayout*Group.frameType:		chiseled
    *filesLayout*Frame*frameType:		sunken
    *filesLayout*Frame*frameWidth:		1
    *filesLayout*TextToggle.frameWidth:		0
    *filesLayout*TextToggle.height:		28
    *filesLayout*TextToggle.alignment:		left
    *filesLayout*TextToggle.ledtMargin:		3
    *filesLayout*SimpleMenu.borderWidth:        1
    *filesLayout*SimpleMenu.borderColor:        black
    *filesLayout*SimpleMenu.foreground:         White
    *filesLayout*SimpleMenu.background:         SteelBlue
    *filesLayout*Label.borderWidth:		0
    *filesLayout*Label.shadowWidth:		0
    *filesLayout.layout: vertical { \
	imlistGroup   < +inf -inf * > \
	3 \
	horizontal { -5 imoptsGroup < +inf -inf * > -5 } \
	-1 \
	loadCmdGroup < +inf -inf * > \
    }

    *imlistGroup.label:
    *imlistGroup.frameWidth:			2
    *imlistGroup.outerOffset:			2
    *imlistLayout*Label.shadowWidth:		0
    *imlistLayout*Label.justify:		left
    *imlistLayout*Command.width:		90
    *imlistLayout*Command.height:		23
    *imlistLayout*Command.shadowWidth:		1
    *imlistLayout*Viewport.allowVert:           True
    *imlistLayout*Viewport.allowHoriz           True
    *imlistLayout*Viewport.useRight:            True
    *imlistLayout*Viewport.useBottom:           True
    *imlistLayout*Viewport.resizeable:		True
    *imlistLayout*Scrollbar.height:		17
    *imlistLayout*Scrollbar.width:		17
    *imlistLayout.layout: vertical { \
	3 \
	horizontal { \
	    3 \
	    rootButton	 < +inf -inf * > 1 \
	    homeButton	 < +inf -inf * > 1 \
	    upButton     < +inf -inf * > 1 \
	    rescanButton < +inf -inf * >   \
	    3 \
	    imtemplateLabel 3 imtemplateFrame < +inf -inf  * > \
	    3 \
	} \
	5 \
	horizontal { 1 imlistLabel < +inf -inf * > 1 } \
	imlistFrame < +inf -inf * +inf -inf > \
	3 \
	dirLabel < +inf -inf * > \
	3 \
	horizontal { 5 fnameLabel 5 filnamFrame < +inf -inf * > } \
	3 \
    }
    *imtemplateLabel.label:			Filter:
    *imtemplateText*editType:			edit
    *imtemplateText*height:			23
    *imtemplateText*font:			7x13
    !*imageList.width:				100
    *imageList*height:				140
    *imageList.shadeSurplus:			False
    *imageList.defaultColumns:			3
    *imageList.font:				7x13
    *imlistView.resizeable:			True
    *imlistLabel.height:			0
    *imlistLabel.label:				xxx
    *imlistLabel.justify:			left
    *imlistLabel.font:				*lucida-bold-r*10*
    *upButton.label:				Up
    *rootButton.label:				Root
    *homeButton.label:				Home
    *rescanButton.label:			Rescan
    *dirLabel.label:				\ \ Directory:
    *dirLabel.alignment:			left
    *fnameLabel.label:				Load File:
    *fnameText*editType:			edit
    *fnameText.height:				22

    *imoptsGroup.label:				Options
    *imoptsGroup.frameWidth:			2
    *imoptsGroup.outerOffset:			7
    *imoptsGroup*offIcon:			square0s
    *imoptsGroup*onIcon:			square1s
    *imoptsGroup*highlightColor:		yellow
    *imoptsLayout*Label.shadowWidth:		0
    *imoptsLayout*Label.justify:		left
    *imoptsLayout.layout: vertical { \
	3 \
	horizontal { \
	    3 \
	    autoload 6 grayscale \
	    3 < +inf > \
	    browseHdrs \
	    3 < +inf > \
	    frameLabel 2 frameFrame \
	    3 \
	} \
	6 \
	horizontal { \
	    3 \
	    zscale 6 zrange \
	    3 \
	    z1Label z1Frame < +inf * >  2 \
	    z2Label z2Frame < +inf * >  2 \
	    nsampLabel nsampFrame < +inf * > \
	    3 \
	} \
	3 \
    }
    *grayscale.label:				Auto Grayscale
    *grayscale.location:			0 0 100 22
    *autoload.label:				Auto Load
    *autoload.location:				0 0 80 22
    *autoload.on:				True
    *browseHdrs.label:				List Image Headers
    *browseHdrs.location:			0 0 120 22
    *browseHdrs.on:				False
    *zscale.label:				Zscale
    *zscale.location:				0 0 60 22
    *zrange.label:				Zrange
    *zrange.location:				0 0 60 22
    *z1Label.label:				z1
    *z1Value*width:				60
    *z1Value*height:				22
    *z1Value*editType:				edit
    *z2Label.label:				z2
    *z2Value*width:				60
    *z2Value*height:				22
    *z2Value*editType:				edit
    *nsampLabel.label:				Nsample
    *nsampValue*width:				60
    *nsampValue*height:				22
    *nsampValue*editType:			edit
    *frameLabel.label:				Frame:\ 
    *frameFrame.width:				50
    *frameFrame.resize:				False
    *frameFrame.label:				Current
    *frameFrame.font:				6x13
    *frameFrame.menuName:			loadFrames


    *loadCmdGroup.label:
    *loadCmdGroup*frameWidth:			2
    *loadCmdGroup*outerOffset:			5
    *loadCmdGroup.outerOffset:			2
    *loadCmdGroup*innerOffset:			5
    *loadCmdGroup.frameType:			sunken
    *loadCmdGroup.label:
    *loadCmdGroup.location:                     0 0 400 45
    *loadCmdLayout*Command.internalWidth:	12
    *loadCmdLayout.layout: horizontal { \
	2 \
	filesLoadButton \
	1 < +inf > \
	filesStatus \
	2 \
    }
    *filesLoadButton.label:			Load
    *filesStatus.label:
}


