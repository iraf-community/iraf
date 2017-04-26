/***********************************************************************
  
                             Frame widget
		   Copyright by Vladimir T. Romanovski
			 All rights reserved.

This library is designed  for  free,  non-commercial  software  creation.
It is changeable and can be improved. The author would greatly appreciate
any  advice, new  components  and  patches  of  the  existing  programs.
Commercial  usage is  also  possible  with  participation of the author.

                      romsky@hp1.oea.ihep.su (Russia)
                      romsky@munin.ucsf.edu  (USA)
	
*************************************************************************/

#ifndef _XawFrame_h
#define _XawFrame_h



#include <X11/Xraw/3d.h>

/***********************************************************************
 *
 * Frame Widget (subclass of CompositeClass)
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
#define XtNframeType "frameType"
#define XtCFrameType "FrameType"
#define XtRFrameType "FrameType"



#ifndef XtNhSpace             
#define XtNhSpace "hSpace"
#endif

#ifndef XtNvSpace             
#define XtNvSpace "vSpace"
#endif

#ifndef XtCHSpace             
#define XtCHSpace "HSpace"
#endif

#ifndef XtCVSpace             
#define XtCVSpace "VSpace"
#endif

#ifndef XtNtop  
#define XtNtop "top"
#endif


#ifndef XtNbottom        
#define XtNbottom "bottom"
#endif


#ifndef XtNleft   
#define XtNleft "left"
#endif


#ifndef XtNright      
#define XtNright "right"
#endif

#ifndef XtNlayoutPolicy             
#define XtNlayoutPolicy "layoutPolicy"
#endif

#ifndef XtNxFraction             
#define XtNxFraction "xFraction"
#endif

#ifndef XtNyFraction             
#define XtNyFraction "yFraction"
#endif

#ifndef XtCLayoutPolicy
#define XtCLayoutPolicy "LayoutPolicy"
#endif                        

#ifndef XtRLayoutPolicy
#define XtRLayoutPolicy "LayoutPolicy"
#endif                        


#ifndef XtNcaptionOn             
#define XtNcaptionOn "captionOn"
#endif

#ifndef XtCCaptionOn             
#define XtCCaptionOn "CaptionOn"
#endif


#ifndef XtNcaptionLabel             
#define XtNcaptionLabel "captionLabel"
#endif

#ifndef XtCCaptionLabel             
#define XtCCaptionLabel "CaptionLabel"
#endif

/*#########################################################################*/
/*#                                                                       #*/
/*#                           Widget Class Pointer                        #*/
/*#                                                                       #*/
/*#########################################################################*/
extern WidgetClass frameWidgetClass;

typedef struct _FrameClassRec *FrameWidgetClass;
typedef struct _FrameRec      *FrameWidget;

typedef enum {
  XawSINGLE = Xraw_FRAME + 10,
  XawFRACTION,
  XawCENTER
}XawLayoutPolicy;

#endif /* _XawFrame_h */
/* DON'T ADD STUFF AFTER THIS #endif */
