
set Resources(save_panel) { \

    !=====================================
    !  Save Setup Panel	resources.	!
    !=====================================
    *save_panel*TextToggle.alignment:		left
    *save_panel*TextToggle.frameWidth:		0
    *save_panel*TextToggle.height:		20
    *save_panel*Label.borderWidth:		0
    *save_panel*Label.shadowWidth:		0
    *save_panel*TextButton.width:		80

    *saveLayout.borderWidth:			0
    *saveLayout.layout:	vertical { \
	saveNameGroup < +inf *	> \
	saveOptGroup <	+inf -inf * > -3 \
	saveCmdGroup <	+inf * > \
    }

    ! Save Name	Group resources.
    !----------------------------------
    *saveNameGroup.borderWidth:			0
    *saveNameGroup.outerOffset:			5
    *saveNameGroup.label:
    *saveNameGroup.location:			0 0 400 50
    *saveNameGroup*offIcon:			diamond0s
    *saveNameGroup*onIcon:			diamond1s
    *saveNameGroup*highlightColor:		cyan
    *saveNameGroup*Frame.frameType:		sunken
    *saveNameGroup*Frame.frameWidth:		1
    *saveNameGroup*Label.justify:		right
    *saveNameGroup*Text*editType:		edit
    *saveNameGroup*shadowWidth:			0
    *saveNameGroup*borderWidth:			0
    *saveNameLayout.borderWidth:		0
    *saveNameLayout.layout: vertical { \
	2 \
	horizontal { 5 saveLabel 5 fnameFrame < +inf -inf * > 5 } \
	2 \
    }
    *saveLabel.label:				File Name:
    *saveFile.height:				22

    !  Main options groups layout resources.
    !---------------------------------------
    *saveOptGroup.frameWidth:			2
    *saveOptGroup.frameType:			chiseled
    *saveOptGroup.label:
    *saveOptGroup.location:			0 0 400 140
    *saveOptGroup.outerOffset:			5
    *saveOptGroup.innerOffset:			0
    *saveOptLayout*borderWidth:			0
    *saveOptLayout.layout: horizontal {	\
	-1 \
	vertical { 5 <	-5 > fmtGroup <	+inf * +inf > -1 } \
	-1 \
	vertical { \
	    10	< -10 >	\
	    saveDataBox < +inff -inff * +inff -inff > \
	    5 < -5 > \
	} \
	-1 \
	vertical { 5 <	-5 > saveColorGroup < +inf * +inf > -1 } \
	-1 \
    }

    ! Output color box resources.
    ! ------------------------------
    *saveColorGroup.location:			0 0 140	120
    *saveColorGroup.outerOffset:		7
    *saveColorGroup.frameWidth:			2
    *saveColorGroup*offIcon:			diamond0s
    *saveColorGroup*onIcon:			diamond1s
    *saveColorGroup*highlightColor:		cyan
    *saveColorGroup.innerOffset:		5
    *saveColorGroup.label:			Output Color
    *saveColorGroup*TextToggle.width:		110
    *saveColor.frameWidth:			2
    *saveColor.layout: horizontal { \
	3 \
	vertical { 5 svGrayButton 2 svPseudoButton 2 svRGBButton -1 } \
	-1 \
    }
    *svGrayButton.label:			Grayscale
    *svPseudoButton.label:			PseudoColor
    *svRGBButton.label:				RGB

    *saveDataBox.frameType:			sunken
    *saveDataBox.frameWidth:			1


    ! Output format box	resources.
    ! -----------------------------------
    *fmtGroup.location:				0 0 140 120
    *fmtGroup.outerOffset:			7
    *fmtGroup.frameWidth:			2
    *fmtGroup*offIcon:				diamond0s
    *fmtGroup*onIcon:				diamond1s
    *fmtGroup*TextToggle.width:			55
    *fmtGroup*highlightColor:			cyan
    *fmtGroup.label:				File Format
    *formats.layout: horizontal	{ \
	3 \
	vertical { 5 fitsButton 2 gifButton 2 tiffButton 2 rawButton  1 } \
	2 < -2	> \
	vertical { 5 epsButton	2 rasButton 2 x11Button  2 jpegButton 1 } \
	-1 \
    }
    *rasButton.label:				RAS
    *gifButton.label:				GIF
    *jpegButton.label:				JPEG
    *tiffButton.label:				TIFF
    *fitsButton.label:				FITS
    *x11Button.label:				X11
    *epsButton.label:				EPS
    *rawButton.label:				Raw

    ! Change the sensitivity once these	formats	are implemented. !
    !-------------------------------------------------------------
    *jpegButton.sensitive:			False
    *x11Button.sensitive:			False
    *rawButton.sensitive:			False


    ! Panel command resources.
    ! ------------------------------
    *saveCmdLayout.borderWidth:			0
    *saveCmdGroup.frameType:			chiseled
    *saveCmdGroup.frameWidth:			2
    *saveCmdGroup.outerOffset:			5
    *saveCmdGroup.innerOffset:			5
    *saveCmdGroup.label:
    *saveCmdGroup.location:			0 0 400 50
    *saveCmdLayout*Command.internalWidth:	12
    *saveCmdLayout.layout: horizontal {	\
	2 \
	okaySave 1 < +inf -1 >	saveStatus < +inf -inf * > \
	2 \
    }
    *okaySave.label:				Save
}

