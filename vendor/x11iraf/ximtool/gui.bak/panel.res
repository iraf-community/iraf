
set Resources(panelShell) { \

    !================================
    !  Main Integrated Control Panel
    !================================
    *panelShell.title:				XImtool Control Panel
    *panelShell.geometry:			480x630
    *panelShell.maxWidth:			480
    *panelShell.minWidth:			480
    *panelTabs.internalHeight:			3
    *panelTabs.internalWidth:			10

    *Text*font:	 	 -*-helvetica-medium-r-normal-*-12-*-iso8859-1
    *TextBox*font:	 -*-helvetica-medium-r-normal-*-12-*-iso8859-1
    *List.font:	 	 -*-helvetica-medium-r-normal-*-12-*-iso8859-1
    *MultiList.font:	 -*-helvetica-medium-r-normal-*-12-*-iso8859-1
    *TextButton.font: 	 -*-helvetica-medium-r-normal-*-12-*-iso8859-1
    *TextToggle.font: 	 -*-helvetica-medium-r-normal-*-12-*-iso8859-1

    *display_panel.tabLabel:			Display
    *display_panel.innerOffset:			5
    *print_panel.tabLabel:			Print
    *print_panel.innerOffset:			5
    *load_panel.tabLabel:			Load
    *load_panel.innerOffset:			5
    *save_panel.tabLabel:			Save
    *save_panel.innerOffset:			5
    *info_panel.tabLabel:			Info
    *info_panel.innerOffset:			5
    *tile_panel.tabLabel:			Tile
    *tile_panel.innerOffset:			5
    *wcs_panel.tabLabel:			Coords
    *wcs_panel.innerOffset:			5

    *panelMenuBar*borderWidth:			0
    *panelMenuBar*Command.internalHeight:	4
    *panelMenuBar*Command.internalWidth:	15
    *panelMenuBar.layout: vertical { \
	5 < -5	> \
	horizontal { \
	    10	< +inf -10> \
	    panelHelp \
	    3 < -3 > \
	    panelClose	\
	    7 < -7 > \
	} \
	5 < -5	> \
    }

    *panelHelp.label:				Help
    *panelClose.label:				Dismiss


    *tabFrame.outerOffset:			3
    *tabFrame.innerOffset:			0
    *tabFrame.frameWidth:			0
    *tabFrame.frameType:			chiseled
    *panelMenuFrame.outerOffset:		0
    *panelMenuFrame.innerOffset:		1
    *panelMenuFrame.frameType:			raised
    *panelMenuFrame.frameWidth:			2
    *panel.layout: vertical { \
	panelMenuFrame	 < +inf	-inf * > \
	3 \
	horizontal { tabFrame < +inf -inf * +inf -inf>	} \
    }
} 

