/****************************************************************************
 * NCSA Mosaic for the X Window System                                      *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 * mosaic@ncsa.uiuc.edu                                                     *
 *                                                                          *
 * Copyright (C) 1993, Board of Trustees of the University of Illinois      *
 *                                                                          *
 * NCSA Mosaic software, both binary and source (hereafter, Software) is    *
 * copyrighted by The Board of Trustees of the University of Illinois       *
 * (UI), and ownership remains with the UI.                                 *
 *                                                                          *
 * The UI grants you (hereafter, Licensee) a license to use the Software    *
 * for academic, research and internal business purposes only, without a    *
 * fee.  Licensee may distribute the binary and source code (if released)   *
 * to third parties provided that the copyright notice and this statement   *
 * appears on all copies and that no charge is associated with such         *
 * copies.                                                                  *
 *                                                                          *
 * Licensee may make derivative works.  However, if Licensee distributes    *
 * any derivative work based on or derived from the Software, then          *
 * Licensee will (1) notify NCSA regarding its distribution of the          *
 * derivative work, and (2) clearly notify users that such derivative       *
 * work is a modified version and not the original NCSA Mosaic              *
 * distributed by the UI.                                                   *
 *                                                                          *
 * Any Licensee wishing to make commercial use of the Software should       *
 * contact the UI, c/o NCSA, to negotiate an appropriate license for such   *
 * commercial use.  Commercial use includes (1) integration of all or       *
 * part of the source code into a product for sale or license by or on      *
 * behalf of Licensee to third parties, or (2) distribution of the binary   *
 * code or source code to third parties that need it to utilize a           *
 * commercial product sold or licensed by or on behalf of Licensee.         *
 *                                                                          *
 * UI MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THIS SOFTWARE FOR   *
 * ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED          *
 * WARRANTY.  THE UI SHALL NOT BE LIABLE FOR ANY DAMAGES SUFFERED BY THE    *
 * USERS OF THIS SOFTWARE.                                                  *
 *                                                                          *
 * By using or copying this Software, Licensee agrees to abide by the       *
 * copyright law and all other applicable laws of the U.S. including, but   *
 * not limited to, export control laws, and the terms of this license.      *
 * UI shall have the right to terminate this license immediately by         *
 * written notice upon Licensee's breach of, or non-compliance with, any    *
 * of its terms.  Licensee may be held legally responsible for any          *
 * copyright infringement that is caused or encouraged by Licensee's        *
 * failure to abide by the terms of this license.                           *
 *                                                                          *
 * Comments and questions are welcome and can be sent to                    *
 * mosaic-x@ncsa.uiuc.edu.                                                  *
 ****************************************************************************/

#ifndef HTML_H
#define HTML_H

#ifdef MOTIF
#include <Xm/Xm.h>
#if (XmVERSION == 1)&&(XmREVISION >= 2)
#define MOTIF1_2
#endif
#else
#include <X11/Intrinsic.h>
#include <X11/Constraint.h>
#endif /* MOTIF */
#include <X11/StringDefs.h>



typedef int (*visitTestProc)();
typedef void (*pointerTrackProc)();

typedef struct ele_ref_rec {
	int id, pos;
} ElementRef;

typedef struct link_rec {
	char *href;
	char *role;
} LinkInfo;

/*
 * Public functions
 */
#ifdef _NO_PROTO
extern char *HTMLGetText ();
extern char *HTMLGetTextAndSelection ();
extern char **HTMLGetHRefs ();
extern char **HTMLGetImageSrcs ();
extern void *HTMLGetWidgetInfo ();
extern void HTMLFreeWidgetInfo ();
extern void HTMLFreeImageInfo ();
extern LinkInfo *HTMLGetLinks ();
extern int HTMLPositionToId ();
extern int HTMLIdToPosition ();
extern int HTMLAnchorToPosition ();
extern int HTMLAnchorToId ();
extern void HTMLGotoId ();
extern void HTMLRetestAnchors ();
extern void HTMLClearSelection ();
extern void HTMLSetSelection ();
extern void HTMLSetText ();
extern int HTMLSearchText ();
#else
extern char *HTMLGetText (Widget w, int pretty);
extern char *HTMLGetTextAndSelection (Widget w, char **startp, char **endp,
					char **insertp);
extern char **HTMLGetHRefs (Widget w, int *num_hrefs);
extern char **HTMLGetImageSrcs (Widget w, int *num_srcs);
extern void *HTMLGetWidgetInfo (Widget w);
extern void HTMLFreeWidgetInfo (void *ptr);
extern void HTMLFreeImageInfo (Widget w);
extern LinkInfo *HTMLGetLinks (Widget w, int *num_links);
extern int HTMLPositionToId(Widget w, int x, int y);
extern int HTMLIdToPosition(Widget w, int element_id, int *x, int *y);
extern int HTMLAnchorToPosition(Widget w, char *name, int *x, int *y);
extern int HTMLAnchorToId(Widget w, char *name);
extern void HTMLGotoId(Widget w, int element_id);
extern void HTMLRetestAnchors(Widget w, visitTestProc testFunc,
    XtPointer client_data);
extern void HTMLClearSelection (Widget w);
extern void HTMLSetSelection (Widget w, ElementRef *start, ElementRef *end);
extern void HTMLSetText (Widget w, char *text, char *header_text,
			char *footer_text, int element_id,
			char *target_anchor, void *ptr);
extern int HTMLSearchText (Widget w, char *pattern,
	ElementRef *m_start, ElementRef *m_end, int backward, int caseless);
#endif /* _NO_PROTO */


/*
 * Public Structures
 */
typedef struct acall_rec {
	XEvent *event;
	int element_id;
	char *text;
	char *href;
} WbAnchorCallbackData;


typedef struct fcall_rec {
	XEvent *event;
	char *href;
        char *method;
        char *enctype;
        char *enc_entity;
	int attribute_count;
	char **attribute_names;
	char **attribute_values;
} WbFormCallbackData;


typedef struct form_rec {
	Widget hw;
	char *action;
        char *method;
        char *enctype;
        char *enc_entity;
	int start, end;
	struct form_rec *next;
} FormInfo;


typedef struct image_rec {
	int ismap;
	FormInfo *fptr;
	int internal;
	int delayed;
	int fetched;
	int width, height;
	int num_colors;
	int *reds;
	int *greens;
	int *blues;
	unsigned char *image_data;
	Pixmap image;
	char *text;
} ImageInfo;


typedef struct wid_rec {
	Widget w;
	int type;
	int id;
	int x, y;
	int width, height;
	char *name;
	char *value;
	char *password;
	char **mapping;
	Boolean checked;
	Boolean mapped;
	struct wid_rec *next;
} WidgetInfo;

typedef struct sel_rec {
	Widget hw;
	struct mark_up *mptr;
	int is_value;
	char *retval_buf;
	char *option_buf;
	char **returns;
	char **options;
	int option_cnt;
	char **value;
	int value_cnt;
} SelectInfo;


typedef ImageInfo *(*resolveImageProc)();


/*
 * defines and structures used for the formatted element list
 */

#define E_TEXT		1
#define E_BULLET	2
#define E_LINEFEED	3
#define E_IMAGE		4
#define E_WIDGET	5
#define E_HRULE		6

struct ele_rec {
	int type;
	ImageInfo *pic_data;
	WidgetInfo *widget_data;
	XFontStruct *font;
	int alignment;
	Boolean internal;
	Boolean selected;
	int indent_level;
	int start_pos, end_pos;
	int x, y;
	int y_offset;
	int width;
	int line_number;
	int line_height;
	int ele_id;
	int underline_number;
	Boolean dashed_underline;
	Boolean strikeout;
	unsigned long fg;
	unsigned long bg;
	char *anchorName;
	char *anchorHRef;
	char *edata;
	int edata_len;
	struct ele_rec *next;
	struct ele_rec *prev;
};

struct ref_rec {
	char *anchorHRef;
	struct ref_rec *next;
};

struct delay_rec {
	char *src;
	struct delay_rec *next;
};


/*
 * defines and structures used for the HTML parser, and the
 * parsed object list.
 */

/* Mark types */
#define	M_UNKNOWN	-1
#define	M_NONE		0
#define	M_TITLE		1
#define	M_HEADER_1	2
#define	M_HEADER_2	3
#define	M_HEADER_3	4
#define	M_HEADER_4	5
#define	M_HEADER_5	6
#define	M_HEADER_6	7
#define	M_ANCHOR	8
#define	M_PARAGRAPH	9
#define	M_ADDRESS	10
#define	M_PLAIN_TEXT	11
#define	M_UNUM_LIST	12
#define	M_NUM_LIST	13
#define	M_LIST_ITEM	14
#define	M_DESC_LIST	15
#define	M_DESC_TITLE	16
#define	M_DESC_TEXT	17
#define	M_PREFORMAT	18
#define	M_PLAIN_FILE	19
#define M_LISTING_TEXT	20
#define M_INDEX		21
#define M_MENU		22
#define M_DIRECTORY	23
#define M_IMAGE		24
#define M_FIXED		25
#define M_BOLD		26
#define M_ITALIC	27
#define M_EMPHASIZED	28
#define M_STRONG	29
#define M_CODE		30
#define M_SAMPLE	31
#define M_KEYBOARD	32
#define M_VARIABLE	33
#define M_CITATION	34
#define M_BLOCKQUOTE	35
#define M_STRIKEOUT	36
#define M_INPUT		37
#define M_FORM		38
#define M_HRULE		39
#define M_LINEBREAK	40
#define M_BASE		41
#define M_SELECT	42
#define M_OPTION	43
#define M_TEXTAREA	44

/* syntax of Mark types */
#define	MT_TITLE	"title"
#define	MT_HEADER_1	"h1"
#define	MT_HEADER_2	"h2"
#define	MT_HEADER_3	"h3"
#define	MT_HEADER_4	"h4"
#define	MT_HEADER_5	"h5"
#define	MT_HEADER_6	"h6"
#define	MT_ANCHOR	"a"
#define	MT_PARAGRAPH	"p"
#define	MT_ADDRESS	"address"
#define	MT_PLAIN_TEXT	"xmp"
#define	MT_UNUM_LIST	"ul"
#define	MT_NUM_LIST	"ol"
#define	MT_LIST_ITEM	"li"
#define	MT_DESC_LIST	"dl"
#define	MT_DESC_TITLE	"dt"
#define	MT_DESC_TEXT	"dd"
#define	MT_PREFORMAT	"pre"
#define	MT_PLAIN_FILE	"plaintext"
#define MT_LISTING_TEXT	"listing"
#define MT_INDEX	"isindex"
#define MT_MENU		"menu"
#define MT_DIRECTORY	"dir"
#define MT_IMAGE	"img"
#define MT_FIXED	"tt"
#define MT_BOLD		"b"
#define MT_ITALIC	"i"
#define MT_EMPHASIZED	"em"
#define MT_STRONG	"strong"
#define MT_CODE		"code"
#define MT_SAMPLE	"samp"
#define MT_KEYBOARD	"kbd"
#define MT_VARIABLE	"var"
#define MT_CITATION	"cite"
#define MT_BLOCKQUOTE	"blockquote"
#define MT_STRIKEOUT	"strike"
#define MT_INPUT	"input"
#define MT_FORM		"form"
#define MT_HRULE	"hr"
#define MT_LINEBREAK	"br"
#define MT_BASE		"base"
#define MT_SELECT	"select"
#define MT_OPTION	"option"
#define MT_TEXTAREA	"textarea"


/* anchor tags */
#define	AT_NAME		"name"
#define	AT_HREF		"href"


struct mark_up {
	int type;
	int is_end;
	char *start;
	char *text;
	char *end;
	struct mark_up *next;
};


/*
 * New resource names
 */
#define	WbNmarginWidth		"marginWidth"
#define	WbNmarginHeight		"marginHeight"
#define	WbNtext			"text"
#define	WbNheaderText		"headerText"
#define	WbNfooterText		"footerText"
#define	WbNtitleText		"titleText"
#define	WbNanchorUnderlines	"anchorUnderlines"
#define	WbNvisitedAnchorUnderlines	"visitedAnchorUnderlines"
#define	WbNdashedAnchorUnderlines	"dashedAnchorUnderlines"
#define	WbNdashedVisitedAnchorUnderlines	"dashedVisitedAnchorUnderlines"
#define	WbNanchorColor		"anchorColor"
#define	WbNvisitedAnchorColor	"visitedAnchorColor"
#define	WbNactiveAnchorFG	"activeAnchorFG"
#define	WbNactiveAnchorBG	"activeAnchorBG"
#define	WbNfancySelections	"fancySelections"
#define	WbNimageBorders		"imageBorders"
#define	WbNdelayImageLoads	"delayImageLoads"
#define	WbNisIndex		"isIndex"
#define	WbNitalicFont		"italicFont"
#define	WbNboldFont		"boldFont"
#define	WbNfixedFont		"fixedFont"
#define	WbNfixedboldFont	"fixedboldFont"
#define	WbNfixeditalicFont	"fixeditalicFont"
#define	WbNheader1Font		"header1Font"
#define	WbNheader2Font		"header2Font"
#define	WbNheader3Font		"header3Font"
#define	WbNheader4Font		"header4Font"
#define	WbNheader5Font		"header5Font"
#define	WbNheader6Font		"header6Font"
#define	WbNaddressFont		"addressFont"
#define	WbNplainFont		"plainFont"
#define	WbNplainboldFont	"plainboldFont"
#define	WbNplainitalicFont	"plainitalicFont"
#define	WbNlistingFont		"listingFont"
#define	WbNanchorCallback	"anchorCallback"
#define	WbNlinkCallback		"linkCallback"
#define	WbNsubmitFormCallback	"submitFormCallback"
#define	WbNpreviouslyVisitedTestFunction "previouslyVisitedTestFunction"
#define	WbNpreviouslyVisitedTestData "previouslyVisitedTestData"
#define	WbNresolveImageFunction "resolveImageFunction"
#define	WbNresolveDelayedImage "resolveDelayedImage"
#define	WbNpercentVerticalSpace "percentVerticalSpace"
#define WbNpointerMotionCallback "pointerMotionCallback"
#define WbNpointerMotionData	"pointerMotionData"
#define WbNverticalScrollOnRight "verticalScrollOnRight"
#define WbNhorizontalScrollOnTop "horizontalScrollOnTop"
#define WbNview			 "view"
#define WbNverticalScrollBar	 "verticalScrollBar"
#define WbNhorizontalScrollBar	 "horizontalScrollBar"

/*
 * New resource classes
 */
#define	WbCMarginWidth		"MarginWidth"
#define	WbCMarginHeight		"MarginHeight"
#define	WbCText			"Text"
#define	WbCHeaderText		"HeaderText"
#define	WbCFooterText		"FooterText"
#define	WbCTitleText		"TitleText"
#define	WbCAnchorUnderlines	"AnchorUnderlines"
#define	WbCVisitedAnchorUnderlines	"VisitedAnchorUnderlines"
#define	WbCDashedAnchorUnderlines	"DashedAnchorUnderlines"
#define	WbCDashedVisitedAnchorUnderlines	"DashedVisitedAnchorUnderlines"
#define	WbCAnchorColor		"AnchorColor"
#define	WbCVisitedAnchorColor	"VisitedAnchorColor"
#define	WbCActiveAnchorFG	"ActiveAnchorFG"
#define	WbCActiveAnchorBG	"ActiveAnchorBG"
#define	WbCFancySelections	"FancySelections"
#define	WbCImageBorders		"ImageBorders"
#define	WbCDelayImageLoads	"DelayImageLoads"
#define	WbCIsIndex		"IsIndex"
#define	WbCItalicFont		"ItalicFont"
#define	WbCBoldFont		"BoldFont"
#define	WbCFixedFont		"FixedFont"
#define	WbCFixedboldFont	"FixedboldFont"
#define	WbCFixeditalicFont	"FixeditalicFont"
#define	WbCHeader1Font		"Header1Font"
#define	WbCHeader2Font		"Header2Font"
#define	WbCHeader3Font		"Header3Font"
#define	WbCHeader4Font		"Header4Font"
#define	WbCHeader5Font		"Header5Font"
#define	WbCHeader6Font		"Header6Font"
#define	WbCAddressFont		"AddressFont"
#define	WbCPlainFont		"PlainFont"
#define	WbCPlainboldFont	"PlainboldFont"
#define	WbCPlainitalicFont	"PlainitalicFont"
#define	WbCListingFont		"ListingFont"
#define	WbCPreviouslyVisitedTestFunction "PreviouslyVisitedTestFunction"
#define	WbCPreviouslyVisitedTestData "PreviouslyVisitedTestData"
#define	WbCResolveImageFunction "ResolveImageFunction"
#define	WbCResolveDelayedImage "ResolveDelayedImage"
#define	WbCPercentVerticalSpace "PercentVerticalSpace"
#define WbCPointerMotionCallback "PointerMotionCallback"
#define WbCPointerMotionData	"PointerMotionData"
#define WbCVerticalScrollOnRight "VerticalScrollOnRight"
#define WbCHorizontalScrollOnTop "HorizontalScrollOnTop"
#define WbCView			 "View"
#define WbCVerticalScrollBar	 "VerticalScrollBar"
#define WbCHorizontalScrollBar	 "HorizontalScrollBar"

typedef struct _HTMLClassRec *HTMLWidgetClass;
typedef struct _HTMLRec      *HTMLWidget;

extern WidgetClass htmlWidgetClass;


#endif /* HTML_H */

