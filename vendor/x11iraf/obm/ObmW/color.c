/*LINTLIBRARY*/
/* $Header: /users/romsky/Xraw/last/Xraw/RCS/color.c,v 1.4 1995/12/09 08:03:23 romsky Exp $ */

/* 
 * color.c - color helper routines
 * 
 * Author:	Christopher A. Kent
 * 		Western Research Laboratory
 * 		Digital Equipment Corporation
 * Date:	Sun Dec 13 1987
 * Copyright (c) 1987 Christopher A. Kent
 */

/* 
 * See David F. Rogers, "Procedural Elements for Computer Graphics",
 * McGraw-Hill, for the theory behind these routines.
 */

/*
 * $Log: color.c,v $
 * Revision 1.4  1995/12/09  08:03:23  romsky
 * Dec 09
 *
 * Revision 1.4  1995/12/09  08:03:23  romsky
 * Dec 09
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
 * Revision 1.2  1995/10/22  12:32:41  romsky
 * *** empty log message ***
 *
 * Revision 1.1  1995/10/08  11:32:36  romsky
 * Initial revision
 *
 * Revision 1.2  90/06/30  14:32:48  rlh2
 * patchlevel 1
 * 
 * Revision 1.1  90/05/10  11:17:30  rlh2
 * Initial revision
 * 
 * Revision 1.2  88/06/30  09:58:36  mikey
 * Handles CMY also.
 * 
 * Revision 1.1  88/06/30  09:10:32  mikey
 * Initial revision
 * 
 */

static char rcs_ident[] = "$Header: /users/romsky/Xraw/last/Xraw/RCS/color.c,v 1.4 1995/12/09 08:03:23 romsky Exp $";

#include <X11/Xlib.h>
#include <X11/Xraw/color.h>

#define	MAX_INTENSITY	65535			    /* for X11 */

#define	ABS(x)	    ((x)<0?-(x):(x))
#define MIN(a,b) ((a)<(b) ? (a) : (b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))


static RGB RGBWhite = { MAX_INTENSITY, MAX_INTENSITY, MAX_INTENSITY };
static RGB RGBBlack = { 0, 0, 0 };

/*
 * Intensity percentages to RGB.
 */

static void PctToRGB(rgb, rr, gg, bb)
     RGB  *rgb;
     float rr, gg, bb;
{
	if (rr > 1.0)
		rr = 1.0;
	if (gg > 1.0)
		gg = 1.0;
	if (bb > 1.0)
		bb = 1.0;
	
	rgb->r = (int)(0.5 + rr * MAX_INTENSITY);
	rgb->g = (int)(0.5 + gg * MAX_INTENSITY);
	rgb->b = (int)(0.5 + bb * MAX_INTENSITY);
}

/*
 * Intensity percentages to HSV.
 */

static void PctToHSV(hsv, hh, ss, vv)
     HSV  *hsv;
     float hh, ss, vv;
{
	if (hh > 1.0)
		hh = 1.0;
	if (ss > 1.0)
		ss = 1.0;
	if (vv > 1.0)
		vv = 1.0;

	hsv->h = hh;
	hsv->s = ss;
	hsv->v = vv;
}

/*
 * The Manhattan distance between two colors, between 0.0 and 3.0.
 */

static float RGBDist(r, s)
     RGB *r, *s;
{
	return (
	    ABS((float)(r->r - s->r)) +
	    ABS((float)(r->g - s->g)) +
	    ABS((float)(r->b - s->b))) / (float)MAX_INTENSITY;
}

/*
 * Load an XColor with an RGB.
 */

static void RGBToXColor(r, x)
     RGB	*r;
     XColor	*x;
{
	x->red   = r->r;
	x->green = r->g;
	x->blue  = r->b;
	x->flags = DoRed | DoGreen | DoBlue;
}

/*
 * Convert a CMY to RGB.
 */

static void CMYToRGB(rgb, cmy)
     RGB *rgb;
     CMY *cmy;
{
	rgb->r = MAX_INTENSITY - cmy->c;
	rgb->g = MAX_INTENSITY - cmy->m;
	rgb->b = MAX_INTENSITY - cmy->y;
}

/*
 * Convert an RGB to CMY.
 */

static void RGBToCMY(rgb, cmy)
     RGB *rgb;
     CMY *cmy;
{
	cmy->c = MAX_INTENSITY - rgb->r;
	cmy->m = MAX_INTENSITY - rgb->g;
	cmy->y = MAX_INTENSITY - rgb->b;
}

/*
 * Mix two RGBs, with scale factors alpha and beta, in RGB space.
 */

static void MixRGB(r, alpha, s, beta, t)
     RGB  *r;
     float alpha;
     RGB  *s;
     float beta;
     RGB  *t;
{
	t->r = MAX(0, MIN(MAX_INTENSITY, (int)(alpha*(r->r) + beta*(s->r))));
	t->g = MAX(0, MIN(MAX_INTENSITY, (int)(alpha*(r->g) + beta*(s->g))));
	t->b = MAX(0, MIN(MAX_INTENSITY, (int)(alpha*(r->b) + beta*(s->b))));
}

/*
 * Mix two RGBs with scale factors alpha and beta, in HSV space.
 */

static void MixHSV(r, alpha, s, beta, t)
     RGB  *r;
     float alpha;
     RGB  *s;
     float beta;
     RGB  *t;
{
	HSV	rr, ss, tt;

	RGBToHSV(r, &rr);
	RGBToHSV(s, &ss);
	tt.h = alpha*rr.h + beta*ss.h;
	if (ABS(rr.h - ss.h) > 0.5) {
		tt.h = tt.h + 0.5;
		if (tt.h >= 1.0)
			tt.h = tt.h - 1.0;
	}
	tt.s = alpha*rr.s + beta*ss.s;
	tt.v = alpha*rr.v + beta*ss.v;
	HSVToRGB(&tt, t);
}

/*
 * Convert an HSV to an RGB.
 */

void
#ifdef Xraw_NEED_PROTO
HSVToRGB(HSV *hsv, RGB *rgb)
#else
HSVToRGB(hsv, rgb)
     HSV *hsv;
     RGB *rgb;
#endif
{
	float	p, q, t, f;
	float   h = hsv->h;
        float   s = hsv->s;
        float   v = hsv->v;
	int	i;
	
	if (s == 0.0)
		PctToRGB(rgb, v, v, v);
	else {
		if (s > 1.0)
			s = 1.0;
		if (s < 0.0)
			s = 0.0;
		if (v > 1.0)
			v = 1.0;
		if (v < 0.0)
			v = 0.0;
		if (h >= 1.0)
			h = 0.0;

		h = 6.0 * h;
		i = (int) h;
		f = h - (float) i;
		p = v * (1.0 - s);
		q = v * (1.0 - (s * f));
		t = v * (1.0 - (s * (1.0 - f)));

		switch(i) {
		case 0:	PctToRGB(rgb, v, t, p); break;
		case 1:	PctToRGB(rgb, q, v, p); break;
		case 2:	PctToRGB(rgb, p, v, t); break;
		case 3:	PctToRGB(rgb, p, q, v); break;
		case 4:	PctToRGB(rgb, t, p, v); break;
		case 5:	PctToRGB(rgb, v, p, q); break;
		}
	}
}

/*
 * Convert an RGB to HSV.
 */

void
#ifdef Xraw_NEED_PROTO
RGBToHSV(RGB *rgb, HSV *hsv)
#else
RGBToHSV(rgb, hsv)
     RGB *rgb;
     HSV *hsv;
#endif
{
	float	rr, gg, bb;
	float	min, max;
	float	rc, gc, bc;
	
	rr = (float) rgb->r / (float) MAX_INTENSITY;
	gg = (float) rgb->g / (float) MAX_INTENSITY;
	bb = (float) rgb->b / (float) MAX_INTENSITY;
	
	max = MAX(MAX(rr, gg), bb);
	min = MIN(MIN(rr, gg), bb);
	hsv->v = max;
	if (max == 0.0)
		hsv->s = 0.0;
	else
		hsv->s = (max - min) / max;
	if (hsv->s == 0.0)
		hsv->h = 0.0;
	else {
		rc = (max - rr) / (max - min);
		gc = (max - gg) / (max - min);
		bc = (max - bb) / (max - min);
		if (rr == max)
			hsv->h = bc - gc;
		else if (gg == max)
			hsv->h = 2.0 + rc - bc;
		else if (bb == max)
			hsv->h = 4.0 + gc - rc;

		if (hsv->h < 0.0)
			hsv->h += 6.0;
		hsv->h = hsv->h / 6.0;
	}
}

