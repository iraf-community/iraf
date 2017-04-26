/* $Header: /users/romsky/Xraw/last/Xraw/RCS/color.h,v 1.4 1995/12/14 00:13:07 romsky Exp romsky $ */

/* 
 * color.h - color definitions
 * 
 * Author:	Christopher A. Kent
 * 		Western Research Laboratory
 * 		Digital Equipment Corporation
 * Date:	Sun Dec 13 1987
 * Copyright (c) 1987 Christopher A. Kent
 */

/*
 * $Log: color.h,v $
 * Revision 1.4  1995/12/14  00:13:07  romsky
 * Panner and MenuButton
 *
 * Revision 1.3  1995/11/24  03:35:16  romsky
 * version 1.1.1
 *
 * Revision 1.3  1995/11/24  03:35:16  romsky
 * version 1.1.1
 *
 * Revision 1.1  1995/11/24  03:28:25  romsky
 * version 1.1.1
 *
 * Revision 1.2  1995/11/21  02:40:53  romsky
 * *** empty log message ***
 *
 * Revision 1.1  1995/11/06  05:48:05  romsky
 * Initial revision
 *
 * Revision 1.1  1995/11/06  05:48:05  romsky
 * Initial revision
 *
 * Revision 1.1  1995/10/08  11:32:36  romsky
 * Initial revision
 *
 * Revision 1.2  90/06/30  14:33:12  rlh2
 * patchlevel 1
 * 
 * Revision 1.1  90/05/10  11:16:54  rlh2
 * Initial revision
 * 
 * Revision 1.2  88/06/30  09:58:56  mikey
 * Handles CMY also.
 * 
 * Revision 1.1  88/06/30  09:10:53  mikey
 * Initial revision
 * 
 */

#ifndef _color_h_
#define _color_h_

#include <X11/Xraw/XawInit.h>

typedef	struct _RGB {
	unsigned short r, g, b;
} RGB;

typedef	struct _HSV {
	float	h, s, v;	/* [0.0, 1.0] */
} HSV;

typedef struct _CMY {
	unsigned short c, m, y;
} CMY;

extern void HSVToRGB Xraw_PROTO((HSV *, RGB *));
extern void RGBToHSV Xraw_PROTO((RGB *, HSV *));

#endif /* _color_h_ */
