#ifndef _XawSeparator_h
#define _XawSeparator_h

/***********************************************************************
 *
 * Separator Widget
 *
 ***********************************************************************/

#include "XrawInit.h"

/* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 bitmap		     Pixmap		Pixmap		None
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 cursor		     Cursor		Cursor		None
 cursorName	     Cursor		String		NULL
 destroyCallback     Callback		XtCallbackList	NULL
 foreground	     Foreground		Pixel		XtDefaultForeground
 height		     Height		Dimension	text height
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 width		     Width		Dimension	text width
 x		     Position		Position	0
 y		     Position		Position	0

*/




typedef enum {
  XawSINGLE_LINE = Xraw_SEPARATOR,
  XawDOUBLE_LINE,
  XawSHADOW_ETCHED_IN,
  XawSHADOW_ETCHED_OUT
} XawSeparatorType;


#define XawSingle_Line       "singleline"
#define XawDouble_Line       "doubleline"
#define XawShadow_Etched_In  "shadowetchedin"
#define XawShadow_Etched_Out "shadowetchedout"



#ifndef XtNmargin
#define XtNmargin "margin"
#endif

#ifndef XtCMargin
#define XtCMargin "Margin"
#endif

#ifndef XtNseparatorType
#define XtNseparatorType "separatorType"
#endif

#ifndef XtCSeparatorType
#define XtCSeparatorType "SeparatorType"
#endif

#ifndef XtRSeparatorType
#define XtRSeparatorType "SeparatorType"
#endif

#define XawTextEncoding8bit 0
#define XawTextEncodingChar2b 1

#define XtNleftBitmap "leftBitmap"
#define XtCLeftBitmap "LeftBitmap"
#define XtNencoding "encoding"
#define XtCEncoding "Encoding"

#ifndef _XtStringDefs_h_
#define XtNbitmap "bitmap"
#define XtNforeground "foreground"
#define XtNseparator "separator"
#define XtNfont "font"
#define XtNinternalWidth "internalWidth"
#define XtNinternalHeight "internalHeight"
#define XtNresize "resize"
#define XtCResize "Resize"
#define XtCBitmap "Bitmap"
#endif

/* Class record constants */

extern WidgetClass separatorWidgetClass;

typedef struct _SeparatorClassRec *SeparatorWidgetClass;
typedef struct _SeparatorRec      *SeparatorWidget;

#endif /* _XawSeparator_h */
/* DON'T ADD STUFF AFTER THIS #endif */
