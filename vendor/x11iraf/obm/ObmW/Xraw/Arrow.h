#ifndef _XawArrow_h
#define _XawArrow_h

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
 direction           Direction          XawDirection    XawTop
 foreground	     Foreground		Pixel		XtDefaultBackground
 height		     Height		Dimension	text height
 insensitiveBorder   Insensitive	Pixmap		Gray
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 pointerColor	     Foreground		Pixel		XtDefaultForeground
 pointerColorBackground Background	Pixel		XtDefaultBackground
 sensitive	     Sensitive		Boolean		True
 shadowWidht         ShadowWidht        Dimension       2
 width		     Width		Dimension	8
 x		     Position		Position	0
 y		     Position		Position	0

*/

typedef enum {
  XawLeft,
  XawRight,
  XawTop,
  XawBottom
}XawDirection;


/* new instance and class names */
#ifndef XtNdirection
#define XtNdirection  "direction"
#endif

#ifndef XtCDirection
#define XtCDirection  "Direction"
#endif

#ifndef XtRDirection
#define XtRDirection  "Direction"
#endif

#ifndef XtNforeground
#define XtNforeground "foreground"
#endif
#ifndef XtCForeground
#define XtCForeground "Foreground"
#endif

#ifndef XtRPixel
#define XtRPixel "Pixel"
#endif

#ifndef XtNarrowShadow
#define XtNarrowShadow "arrowShadow"
#endif
#ifndef XtCArrowShadow
#define XtCArrowShadow "ArrowShadow"
#endif
#ifndef XtRDimension
#define XtRDimension "Dimension"
#endif

#ifndef XtNinitialDelay
#define XtNinitialDelay "initialDelay"
#endif
#ifndef XtCInitialDelay
#define XtCInitialDelay "InitialDelay"
#endif
#ifndef XtRCardinal
#define XtRCardinal "Cardinal"
#endif

#ifndef XtNrepeatDelay
#define XtNrepeatDelay "repeatDelay"
#endif
#ifndef XtCRepeatDelay
#define XtCRepeatDelay "RepeatDelay"
#endif
#ifndef XtRCardinal
#define XtRCardinal "Cardinal"
#endif

#ifndef XtNcallback
#define XtNcallback "callback"
#endif
#ifndef XtCCallback
#define XtCCallback "Callback"
#endif
#ifndef XtRCallback
#define XtRCallback "Callback"
#endif
                                        /* external declarations */

typedef struct {
  XawDirection   direction;
  XPoint p1[3];
  XPoint p2[4];
  XPoint p3[4];
  XPoint p4[4];
  int    a;
  int    a2;
  int    a3;
}XawDrawArrowStruct;

extern void XawMakeDrawArrowStruct(
#if NeedFunctionPrototypes
    int                  /* x         */,
    int                  /* y         */,
    unsigned int         /* w         */,
    unsigned int         /* h         */,
    unsigned int         /* thickness */,
    XawDirection            /* direction */,
    XawDrawArrowStruct * /* result    */
#endif
);

extern void XawDrawArrow(
#if NeedFunctionPrototypes
    Widget               /* w           */,
    GC                   /* inner       */,
    GC                   /* top         */,
    GC                   /* bottom      */,
    XawDrawArrowStruct * /* draw struct */
#endif
);

extern WidgetClass arrowWidgetClass;

typedef struct _ArrowClassRec *ArrowWidgetClass;
typedef struct _ArrowRec      *ArrowWidget;


#endif /* _XawArrow_h */


