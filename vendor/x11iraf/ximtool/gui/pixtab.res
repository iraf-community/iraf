
set Resources(pixel_table) { \

    *pixel_panel.title:				Image Pixel Table
    *pixel_panel*SimpleMenu.borderWidth:	1
    *pixel_panel*SimpleMenu.borderColor:	black
    *pixel_panel*SimpleMenu.foreground:         White
    *pixel_panel*SimpleMenu.background:         SteelBlue

    *pixtabMenuBar*borderWidth:			0
    *pixtabMenuBar*Command.internalHeight:	4
    *pixtabMenuBar*Command.internalWidth:	12
    *pixtabMenuBar.layout: vertical { \
	5 \
	horizontal { \
	    5 \
	    pixtabSize 10 < +inf -10> pixtabHelp 5 pixtabClose \
	    7 \
	} \
	5 \
    }
    *pixtabSize.label:				Size
    *pixtabSize.menuName:			pixtabMenu
    *pixtabHelp.label:				Help
    *pixtabHelp.sensitive:			False
    *pixtabClose.label:				Dismiss

    *pixtabMenuFrame.frameType:			raised
    *pixtabMenuFrame.frameWidth:		2
    *pixel_table.layout: vertical { \
	1 \
	pixtabMenuFrame  < +inf -inf * > \
	1 \
	pixtabFrame < +inf -inf * +inf -inf > \
	1 \
    }
    

    *pixtabFrame.frameType:			raised
    *pixtabFrame.frameWidth:			0
    *pixtabFrame*borderWidth:			0
    *pixtabFrame*font:                          6x10
    *pixtabFrame*MultiList.font: 	-*-helvetica-medium-r-normal-*-10-*
    *pixtabFrame*Label.font: 		-*-helvetica-medium-r-normal-*-10-*
    *pixtabFrame*TextToggle.font: 	-*-helvetica-medium-r-normal-*-10-*

    *pixtabFrame*MultiList.forceColumns:	True
    *pixtabFrame*MultiList.defaultColumns:	5
    *pixtabFrame*MultiList.shadeSurplus:	False
    *pixtabFrame*MultiList.borderWidth:		0
    *pixtabFrame*MultiList.rowHeight:		25
    *pixtabFrame*MultiList.rowSpacing:		7
    *pixtabFrame*MultiList.internalWidth:	7
    *pixtabFrame*MultiList.internalHeight:	4
    *pixtabFrame*MultiList.width:		410
    *pixtabFrame*MultiList.height:		160
    *pixtabFrame*MultiList.columnWidth:		50
    *pixtabFrame*MultiList.columnSpacing:	2
    *pixtabFrame*MultiList.maxSelectable:	1
    *pixtabFrame*MultiList.highlightForeground:	red
    *pixtabFrame*MultiList.highlightBackground:	#c4c4c4
    *pixtabFrame*MultiList.background:		#c4c4c4
    *pixtabFrame*TextToggle.background:		#c4c4c4

    *pixtabFrame*Label.width:			00
    !*pixtabFrame*Label.resize:			False

    *meanFrame.frameWidth:			2
    *meanFrame.frameType:			chiseled
    *meanFrame.outerOffset:			0
    *meanFrame.width:				120
    *meanLabel.label:				Mean:
    *meanValue.label:
    *meanValue.resize:				False

    *sigFrame.frameWidth:			2
    *sigFrame.frameType:			chiseled
    *sigFrame.outerOffset:			0
    *sigFrame.width:				120
    *sigLabel.label:				Stdev:
    *sigValue.label:
    *sigValue.resize:				False

    *ptColFrame.frameWidth:			0
    *ptColFrame.outerOffset:			0
    *ptRowFrame.frameWidth:			0
    *ptRowFrame.outerOffset:			0

    *pixtabFrame*ptColLabs.width:		410
    *pixtabFrame*ptColLabs.height:		23
    *pixtabFrame*ptColLabs.defaultColumns:	5
    *pixtabFrame*ptColLabs.forceColumns:	True
    *pixtabFrame*ptColLabs.columnWidth:		50
    *pixtabFrame*ptColLabs.columnSpacing:	2
    *pixtabFrame*ptRowLabs.width:		60
    *pixtabFrame*ptRowLabs.height:		110
    *pixtabFrame*ptRowLabs.defaultColumns:	1
    *pixtabFrame*ptRowLabs.forceColumns:	True
    *pixtabFrame*ptRowLabs.verticalList:	True
    *pixtabFrame*ptRowLabs.columnWidth:		50
    *pixtabFrame*ptRowLabs.columnSpacing:	2
    *pixtabFrame*pixtab.verticalList:		True

    *ptFrame.outerOffset:           		0
    *ptFrame.innerOffset:           		0
    *ptFrame.borderWidth:			0
    *ptFrame.frameWidth:            		1
    *ptFrame.frameType:             		sunken
    *ptLayout.layout: vertical { \
        3 \
        horizontal { 65 ptColFrame < +inf -inf * > 5 } \
        1 \
        horizontal { \
          vertical { ptRowFrame< * +inf -inf > 5  } \
	  1 \
	  vertical { ptFrame } \
	  5 \
        } \
	1 \
        horizontal { \
	    2 < +inf > \
	    meanLabel meanFrame < +inf -inf * > 2 \
	    sigLabel  sigFrame  < +inf -inf * >   \
	    10 \
	} \
    }
}

