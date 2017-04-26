
set Resources(hdr_panel) { \

    *hdr_panel.geometry:			550x600
    *hdr_panel.title:				Image Header

    *hdr_panel*SimpleMenu.borderColor:          black
    *hdr_panel*SimpleMenu.borderWidth:          1
    *hdr_panel*SimpleMenu.foreground:           White
    *hdr_panel*SimpleMenu.background:           SteelBlue

    *hdrMenuFrame.frameType:			raised
    *hdrMenuFrame.frameWidth:			2
    *hdrLayout.layout: vertical	{ \
	hdrMenuFrame < +inf -inf * > \
	hdrObjGroup  < +inf -inf * > \
	hdrTabFrame  < +inf -inf * +inf	-inf> \
    }

    *hdrMenuBar*borderWidth:			0
    *hdrMenuBar*Command.internalHeight:		5
    *hdrMenuBar*Command.internalWidth:		12
    *hdrMenuBar.layout:	vertical { \
	5 < -5	> \
	horizontal { 10 < +inf -10> hdrClose 7 } \
	5 < -5	> \
    }
    *hdrClose.label:				Dismiss

    *hdrObjGroup.label:
    *hdrObjGroup.outerOffset:			7
    *hdrObjGroup.innerOffset:			4
    *hdrObjLayout.borderWidth:			0
    *hdrObjLayout.layout: vertical { \
	horizontal { hdrObjLabel 2 hdrObjMenu < +inf -inf * > 2 } \
    }
    *hdrObjLabel.label:				Image Name:
    *hdrObjMenu.label:
    *hdrObjMenu.justify:			left
    *hdrObjMenu.font:				7x13
    *hdrObjMenu.menuName:			objMenu
    *hdrObjMenu.internalWidth:			5
    *hdrObjMenu.internalHeight:			1
    *hdrObjMenu.shadowWidth:			1

    *hdrHdrGroup.tabLabel:			Image Header
    *hdrHdrGroup.label:
    *hdrHdrGroup.outerOffset:			7
    *hdrHdrGroup.innerOffset:			5
    *hdrHdrLayout.borderWidth:			0
    *hdrHdrLayout.layout: vertical { \
	horizontal { \
	    hdrFilter 2 hFindFrame < +inf -inf * > 2 hdrFind 1 hdrClear \
	} \
	5 \
	hdrText < +inf -inf * +inf -inf > \
    }
    *hFindEntry*editType:                       edit
    *hFindEntry*font:                           7x13
    *hFindEntry*displayCaret:                   True
    *hFindEntry*width:                          150
    *hFindFrame.frameWidth:                     1
    *hFindFrame*borderWidth:			0
    *hFindFrame.frameType:                      sunken
    *hFindFrame.width:                          150
    *hFindFrame.height:                         23
    *hdrFilter.label:                           Keyword Filter
    *hdrFind.label:                             Search
    *hdrClear.label:                            Clear

    ! Resources if the header text widget is AsciiText
!   *hdrText*scrollVertical:			Always
!   *hdrText*scrollHorizontal:			Always
!   *hdrText*editType:				edit
!   *hdrText*font:				7x13
!   *hdrText*background:			#c4c4c4
!   *hdrText*displayCaret:			False
!   *hdrText*bottomMargin:			10
!   *hdrText*Scrollbar.width:			15
!   *hdrText*Scrollbar.height:			15

    ! Resources if the header text widget is HTML
    *hdrText.width:                             600
    *hdrText.height:                            500
    *hdrText.anchorUnderlines:                  1
    *hdrText.visitedAnchorUnderlines:           1
    *hdrText.verticalScrollOnRight:             True
    *hdrText.plainFont:                         7x13
    *hdrText.marginWidth:			5
    *hdrText.marginHeight:			5


    *hdrWcsGroup.tabLabel:			Image WCS Info
    *hdrWcsGroup.label:
    *hdrWcsGroup.outerOffset:			5
    *hdrWcsGroup.innerOffset:			5
    *hdrWcsGroup*Text*scrollVertical:		whenNeeded
    *hdrWcsGroup*Text*scrollHorizontal:		whenNeeded
    *hdrWcsGroup*Text*editType:			edit
    *hdrWcsGroup*Text*displayCaret:		False
    *hdrWcsGroup*Text*borderWidth:		0
    *hdrWcsGroup*Text*editType:			edit
    *hdrWcsGroup*Text*font:			7x13
    *hdrWcsGroup*Text*Scrollbar.width:		15
    *hdrWcsGroup*Text*Scrollbar.height:		15
    *hdrWcsLayout.borderWidth:			0
    *hdrWcsLayout.layout: vertical { \
	2 < -2	> \
	hdrInfoGroup <	+inf -inf * > \
	2 < -2	> \
	hdrKeywGroup <	+inf -inf * +inf -inf >	\
	-4 \
    }
    *hdrInfoGroup.label:			Basic WCS Information
    *hdrInfoGroup.outerOffset:			7
    *hdrInfoGroup.innerOffset:			0
    *hdrIGFrame.frameType:			sunken
    *hdrIGFrame.frameWidth:			1
    *hdrIGText.height:				130
!   *hdrIGText*background:			black
!   *hdrIGText*foreground:			yellow2
    *hdrIGText*background:			#adadad
    *hdrIGText*foreground:			black
    *hdrWcsGroup*hdrIGText*scrollVertical:	never
    *hdrWcsGroup*hdrIGText*scrollHorizontal:	never
    *hdrWcsGroup*hdrIGText*font:  		7x13
    *hdrKeywGroup.label:			WCS Header Keywords
    *hdrKeywGroup.outerOffset:			7
    *hdrKeywGroup.innerOffset:			5
    *hdrKGFrame.frameType:			sunken
    *hdrKGFrame.frameWidth:			1
    *hdrKGText*background:			#c4c4c4
    *hdrKGText*bottomMargin:			10

    *hdrIGText*background:			#c4c4c4
    *hdrIGFrame.frameWidth:			0
}

