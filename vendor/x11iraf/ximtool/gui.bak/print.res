
set Resources(print_panel) { \

    !=====================================
    !  Print Setup Panel resources.	!
    !=====================================
    *print_panel*TextToggle.alignment:		left
    *print_panel*Arrow.width:			16
    *print_panel*Arrow.height:			20
    *print_panel*TextToggle.frameWidth:		0
    *print_panel*TextToggle.height:		20
    *print_panel*Label.borderWidth:		0
    *print_panel*Label.shadowWidth:		0
    *print_panel*TextButton.width:		40
    *print_panel*TextButton.height:		25

    *printLayout.borderWidth:			0
    *printLayout.layout: vertical { \
	-1 \
	printCmdGroup < +inf *	> \
	-1 \
	optGroup < +inf -inf *	+inf -inf > \
	-3 \
	cmdGroup < +inf * > \
	-1\
    }


    ! Print Group resources.
    !----------------------------------
    *printCmdGroup.borderWidth:			0
    *printCmdGroup.outerOffset:			5
    *printCmdGroup.label:			
    *printCmdGroup.location:			0 0 400 80
    *printCmdGroup*offIcon:			diamond0s
    *printCmdGroup*onIcon:			diamond1s
    *printCmdGroup*highlightColor:		cyan
    *printCmdGroup*Frame.frameType:		sunken
    *printCmdGroup*Frame.frameWidth:		1
    *printCmdGroup*Frame.width:			300
    *printCmdGroup*Label.justify:		right
    *printCmdGroup*Text*editType:		edit
    *printCmdGroup*Text*height:			22
    *printCmdGroup*TextToggle.width:		70
    *printCmdGroup*shadowWidth:			0
    *printCmdGroup*borderWidth:			0
    *printCmdLayout.borderWidth:		0
    *printCmdLayout.layout: horizontal { \
	labelLayout 5 inputLayout < +inf -inf * > \
    }
    *labelLayout.borderWidth:			0
    *labelLayout.layout: vertical { 5 toLabel 7 printerLabel }
    *printerLabel.label:			Print Command:
    *toLabel.label:				Print To:
    *toPrinter.label:				Printer
    *toPrinter.on:				True
    *toFile.label:				File

    *inputLayout.borderWidth:			0
    *inputLayout.layout: horizontal { \
	3 \
	vertical { \
	    5 \
	    horizontal { 5 toPrinter 5 toFile 5 < +inf -inf > } \
	    5 \
	    printcmdFrame < +inf -inf * > \
	    5 \
	} \
	3 \
    }
    *printcmd*string:				lpr
    *printcmd*height:				22
    *printcmd*Text*editType:			edit


    !  Main options groups layout resources.
    !---------------------------------------
    *optGroup.frameWidth:			2
    *optGroup.frameType:			chiseled
    *optGroup.label:			
    *optGroup.location:				0 0 400 330
    *optGroup.outerOffset:			5
    *optGroup.innerOffset:			2
    *optLayout*borderWidth:			0
    *optLayout.layout: vertical	{ \
	-1 \
	horizontal { \
	    vertical {	\
		5 \
		epsPageGroup < +inf -inf * >	\
		optionsGroup < +inf -inf * +inf -inf > \
		-1 \
	    } \
	    vertical {	\
		5 \
		printColorGroup  < +inf -inf * > \
		printerGroup	< +inf -inf * >	\
		-1 \
	    } \
	    -1	\
	} \
	5 \
	horizontal { annOptsGroup < +inf * +inf	> -1 } \
	-1 \
    }


    ! Postscript Options group resources.
    ! -----------------------------------
    *epsPageGroup.label:			Postscript Options
    *epsPageGroup.outerOffset:			7
    *epsPageGroup.innerOffset:			5
    *epsPageGroup.location:			0 0 250 150
    *epsPageGroup*offIcon:			diamond0s
    *epsPageGroup*onIcon:			diamond1s
    *epsPageGroup*highlightColor:		cyan
    *epsPage*Label.justify:			left
    *epsPage.layout: vertical {	\
	-1 \
	epsOrientLabel	4 < -4 > \
	horizontal { 25 epsPortButton epsLandButton epsSquareButton -1 } \
	4 \
	epsSizeLabel -1 \
	horizontal { \
	    25 epsLetterButton epsLegalButton epsA4Button epsB5Button -1 \
	} \
	10 \
	horizontal { 5 epsScaleLabel 4 ScaleFrame < +inf -inf * > } \
	-1 \
    }


    ! Page Layout resources.
    ! -------------------------------
    *epsOrientLabel.label:			Orientation:
    *epsPortButton.label:			Portrait
    *epsPortButton.width:			65
    *epsLandButton.label:			Landscape
    *epsLandButton.width:			85
    *epsSquareButton.label:			Square
    *epsSquareButton.width:			70
    *epsSquareButton.sensitive:			False

    *epsSizeLabel.label:			Paper Size:
    *epsLetterButton.label:			Letter
    *epsLetterButton.width:			60
    *epsLegalButton.label:			Legal
    *epsLegalButton.width:			60
    *epsA4Button.label:				A4
    *epsA4Button.width:				50
    *epsB5Button.label:				B5
    *epsB5Button.width:				50

    ! Image scale box resources.
    ! -------------------------------
    *epsScaleLabel.label:			Output  Image  Scale:
    *epsScaleLabel.justify:			right
    *ScaleFrame.frameType:			sunken
    *ScaleFrame.frameWidth:			1
    *ScaleFrame*shadowWidth:			0
    *ScaleLayout.location:			0 0 100 35
    *ScaleLayout.label:			 
    *ScaleLayout.layout: horizontal { \
	SCdecrease SCtext < +inf -100% * > SCincrease \
    }
    *SCdecrease.direction:			left
    *SCtext.width:				75
    *SCtext.height:				22
    *SCtext.label:				100 %
    *SCincrease.direction:			right


    ! Miscellaneous print options box resources.
    ! ------------------------------------
    *optionsGroup.outerOffset:			7
    *optionsGroup.innerOffset:			5
    *optionsGroup*onIcon:			square1s
    *optionsGroup*offIcon:			square0s
    *optionsGroup.label:			Processing Options
    *optionsGroup*TextToggle.width:		125
    *optionsGroup*TextToggle.highlightColor:	yellow
    *options.location:				0 0 250 60
    *options.frameWidth:			2
    *options.layout: horizontal	{ \
	5 \
	vertical { -1 epsscaleButton 2	autorotateButton -1 } \
	3 \
	vertical { -1 aspectButton   2	compressButton	 -1 } \
	-1 \
    }
    *epsscaleButton.label:			Auto Scale
    *autorotateButton.label:			Auto Rotate
    *aspectButton.label:			Max Aspect
    *compressButton.label:			RLE Compress
    *compressButton.sensitive:			False


    ! Annotation options box resources.
    ! ------------------------------------
    *annOptsGroup.outerOffset:			7
    *annOptsGroup.innerOffset:			5
    *annOptsGroup*onIcon:			square1s
    *annOptsGroup*offIcon:			square0s
    *annOptsGroup.label:			Annotation Options
    *annOptsGroup*TextToggle.width:		90
    *annOptsGroup*TextToggle.highlightColor:	yellow
    *annOptsGroup*Frame.frameType:		sunken
    *annOptsGroup*Frame.frameWidth:		1
    *annOptsGroup*Frame.width:			300
    *annOptsGroup*Text*editType:		edit
    *annOpts.frameWidth:			2
    *annOpts.location:				0 0 400 70
    *annOpts.layout: vertical {	\
	2 \
	horizontal { \
	    5 annotateButton 2 titleButton 2 bordersButton 2 colorbarButton 5 \
	} \
	3 \
	horizontal { -1 titleLabel 2 titleFrame < +inf -inf * > -1 } \
	-1 \
    }
    *annotateButton.label:			\ Annotate
    *titleButton.label:				\ Title
    *bordersButton.label:			\ Borders
    *colorbarButton.label:			\ Colorbar
    *titleLabel.label:				Title	String
    *titleString*string:			imtitle
    *titleString*height:			23
    *titleString*Text*editType:			edit


    ! Output color box resources.
    ! ------------------------------
    *printColorGroup.location:			0 0 150 90
    *printColorGroup.outerOffset:		7
    *printColorGroup.frameWidth:		2
    *printColorGroup*offIcon:			diamond0s
    *printColorGroup*onIcon:			diamond1s
    *printColorGroup*highlightColor:		cyan
    *printColorGroup.innerOffset:		5
    *printColorGroup.label:			Output Color
    *printColorGroup*TextToggle.width:		110
    *printColor.frameWidth:			2
    *printColor.location:			0 0 250 75
    *printColor.layout:	horizontal { \
	1 \
	vertical { -1 prGrayButton 2 prPseudoButton 2 prRGBButton -1 }	\
	-1 \
    }
    *prGrayButton.label:			Grayscale
    *prPseudoButton.label:			PseudoColor
    *prRGBButton.label:				RGB

    ! Printer Selection.
    ! --------------------------
    *printerGroup.label:			Printers
    *printerGroup.location:			0 0 110 130
    *printerGroup.shrinkToFit:			True
    *printerGroup.outerOffset:			7

    *printers*Viewport.allowVert:		True
    *printers*Viewport.allowHoriz:		False
    *printers*Viewport.useRight:		True
    *printers*Viewport.resizeable:		True
    *printers*Scrollbar.width:			17
    *printers*Scrollbar.minimumThumb:		10
    *printers.layout: vertical { \
	3 < -3	> \
	horizontal { \
	    2 < -2 > \
	    printlistFrame < +inf -inf	* +inff	-inff >	\
	    2 < -2 > \
	} \
	3 < -3	> \
    }

    *printers*frameType:			sunken
    *printers*frameWidth:			1
    *printers*BorderWidth:			0
    *printers*Label.ShadowWidth:		0

    *printlist.width:				100
    *printlist.height:				78


    ! Panel command resources.
    ! ------------------------------
    *cmdGroup.frameType:			chiseled
    *cmdGroup.frameWidth:			2
    *cmdGroup.outerOffset:			5
    *cmdGroup.innerOffset:			5
    *cmdGroup.label:
    *cmdGroup.location:				0 0 150 50
    *cmdLayout.borderWidth:			0
    *cmdLayout*Command.internalWidth:       	12
    *cmdLayout.layout: horizontal { \
	2 \
	okayPrint 1 < +inf > printStatus < +inf -inf *	+inf -inf > \
	2 \
    }
    *cmdGroup*TextButton*location:		0 0 80 0
    *okayPrint.label:				Print
}

