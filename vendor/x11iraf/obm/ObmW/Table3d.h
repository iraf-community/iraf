#ifndef _3d_h_
#define _3d_h_

#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xraw/XawInit.h>

typedef enum {
  XawRAISED = Xraw_3d,
  XawSUNKEN,
  XawCHISELED,
  XawLEDGED,
  XawTACK
} XawFrameType;

#define TOP    (1)
#define BOTTOM (2)

extern GC AllocGCFromPixmap		Xraw_PROTO((Widget  , Pixmap ));
extern GC AllocGCFromPixel		Xraw_PROTO((Widget  , Pixel ));

extern void RGBtoHLS			Xraw_PROTO((double  , 
						    double  ,
						    double  ,
						    double *  ,
						    double *  ,
						    double * ));
     
extern void HLStoRGB			Xraw_PROTO((double *  ,
						    double *  ,
						    double *  ,
						    double  ,
						    double  ,
						    double ));
						
extern Boolean TopShadowColor		Xraw_PROTO((Widget /* self */,
						    Pixel  /* base */,
						    Pixel* /* result */));
     
extern Boolean BottomShadowColor	Xraw_PROTO((Widget /* self */,
						    Pixel  /* base */,
						    Pixel* /* result */));
     
extern Boolean ArmedColor		Xraw_PROTO((Widget /* self */,
						    Pixel  /* base */,
						    Pixel* /* result */));
     
extern GC MakeTopShadowGC		Xraw_PROTO((Widget  , Pixel ));
extern GC MakeBottomShadowGC		Xraw_PROTO((Widget  , Pixel ));
extern GC MakeArmedGC			Xraw_PROTO((Widget  , Pixel ));
		
		   
extern GC AllocGCFromPixmap		Xraw_PROTO((Widget  , Pixmap ));
		
		   
extern GC AllocGCFromPixel		Xraw_PROTO((Widget  , Pixel ));
		
		   
extern Pixmap CreateShadowPixmap	Xraw_PROTO((Widget  ,
						     Pixel  ,
						     int ));
		
		   
extern Boolean XrawAllocShadowPixel	 Xraw_PROTO((Widget  ,
						     Pixel  ,
						     int  ,
						     Pixel * ));
     
		   
extern GC MakeGC			Xraw_PROTO((Widget  ,
						     Pixel  ,
						     int  ,
						     Boolean  ,
						     int ));
     
		   
extern GC MakeTopShadowGC		Xraw_PROTO((Widget  , Pixel ));
		
		   
extern GC MakeBottomShadowGC		Xraw_PROTO((Widget  , Pixel ));
		
		   
extern GC MakeArmedGC			Xraw_PROTO((Widget  , Pixel ));
		
		   
extern void XawDrawFrame		Xraw_PROTO((Widget  ,
						     Position  ,
						     Position  ,
						     Dimension  ,
						     Dimension  ,
						     XawFrameType  ,
						     Dimension  ,
						     GC  ,
						     GC ));
		
		   
extern void RGBtoHLS			Xraw_PROTO((double  ,
						     double  ,
						     double  ,
						     double *  ,
						     double *  ,
						     double * ));
						
extern void HLStoRGB			Xraw_PROTO((double *  ,
						     double *  ,
						     double *  ,
						     double  ,
						     double  ,
						     double ));
						
extern Boolean BottomShadowColor	Xraw_PROTO((Widget  ,
						     Pixel  ,
						     Pixel * ));
     
		   
extern Boolean TopShadowColor		Xraw_PROTO((Widget  ,
						     Pixel  ,
						     Pixel * ));
		
		   
extern Boolean ArmedColor		Xraw_PROTO((Widget  ,
						     Pixel  ,
						     Pixel * ));
		
		   
extern void DrawRhombus		        Xraw_PROTO((Widget  ,
						     short  ,
						     short  ,
						     short  ,
						     short  ,
						     GC  ,
						     GC  ,
						     GC  ,
						     Boolean ));
     
extern Boolean FetchPixel               Xraw_PROTO((Widget  ,
						     String name ,
						     Pixel* ));

#endif /* _3d_h_ */




