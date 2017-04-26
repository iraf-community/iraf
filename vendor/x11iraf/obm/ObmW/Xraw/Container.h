#ifndef _XawContainer_h
#define _XawContainer_h


#include <X11/Xraw/XawInit.h>

/***********************************************************************
 *
 * Container Widget (subclass of CompositeClass)
 *
 ***********************************************************************/

/* Parameters:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 destroyCallback     Callback		Pointer		NULL
 hSpace 	     HSpace		Dimension	4
 height		     Height		Dimension	0
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 width		     Width		Dimension	0
 x		     Position		Position	0
 y		     Position		Position	0

*/


/* New fields */
#define XtNshadowWidth "shadowWidth"
#define XtCShadowWidth "ShadowWidth"
#define XtNtopShadowPixel "topShadowPixel"
#define XtCTopShadowPixel "TopShadowPixel"
#define XtNbottomShadowPixel "bottomShadowPixel"
#define XtCBottomShadowPixel "BottomShadowPixel"
#define XtNtopShadowContrast "topShadowContrast"
#define XtCTopShadowContrast "TopShadowContrast"
#define XtNbottomShadowContrast "bottomShadowContrast"
#define XtCBottomShadowContrast "BottomShadowContrast"
#define XtNbeNiceToColormap "beNiceToColormap"
#define XtCBeNiceToColormap "BeNiceToColormap"
#define XtNtopShadowPixmap "topShadowPixmap"
#define XtCTopShadowPixmap "TopShadowPixmap"
#define XtNbottomShadowPixmap "bottomShadowPixmap"
#define XtCBottomShadowPixmap "BottomShadowPixmap"
#define XtNuserData "userData"
#define XtCUserData "UserData"
#define XtNcontainerType "containerType"
#define XtCContainerType "ContainerType"
#define XtRContainerType "ContainerType"

/* Class record constants */

extern WidgetClass containerWidgetClass;

typedef struct _ContainerClassRec *ContainerWidgetClass;
typedef struct _ContainerRec      *ContainerWidget;

#endif /* _XawContainer_h */
