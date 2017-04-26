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

#include <stdio.h>
#include <ctype.h>
#include "HTMLP.h"
#include "bitmaps/NoImage.xbm"
#include "bitmaps/DelayedImage.xbm"
#include "bitmaps/AnchoredImage.xbm"


#define IMAGE_BORDER	2


ImageInfo no_image;
ImageInfo delayed_image;
ImageInfo anchored_image;

static int allocation_index[256];


/*
 * Free all the colors in the default colormap that we have allocated so far.
 */
void
FreeColors(dsp, colormap)
	Display *dsp;
	Colormap colormap;
{
	int i, j;
	unsigned long pix;

	for (i=0; i<256; i++)
	{
		if (allocation_index[i])
		{
			pix = (unsigned long)i;
			/*
			 * Because X is stupid, we have to Free the color
			 * once for each time we've allocated it.
			 */
			for (j=0; j<allocation_index[i]; j++)
			{
				XFreeColors(dsp, colormap, &pix, 1, 0L);
			}
		}
		allocation_index[i] = 0;
	}
}


/*
 * Free up all the pixmaps allocated for this document.
 */
void
FreeImages(hw)
	HTMLWidget hw;
{
	struct ele_rec *eptr;

	eptr = hw->html.formatted_elements;
	while (eptr != NULL)
	{
		if ((eptr->type == E_IMAGE)&&(eptr->pic_data != NULL))
		{
			/*
			 * Don't free the no_image default image
			 */
			if ((eptr->pic_data->image != (Pixmap)NULL)&&
			    (eptr->pic_data->image != delayed_image.image)&&
			    (eptr->pic_data->image != anchored_image.image)&&
			    (eptr->pic_data->image != no_image.image))
			{
				/*
				 * Don't free internal images
				 */
				if ((eptr->edata != NULL)&&
					(strncmp(eptr->edata, INTERNAL_IMAGE,
					strlen(INTERNAL_IMAGE)) == 0))
				{
				}
				else
				{
					XFreePixmap(XtDisplay(hw),
						eptr->pic_data->image);
					eptr->pic_data->image = (Pixmap)NULL;
				}
			}
		}
		eptr = eptr->next;
	}
}


/*
 * Find the closest color by allocating it, or picking an already allocated
 * color
 */
void
FindColor(dsp, colormap, colr)
	Display *dsp;
	Colormap colormap;
	XColor *colr;
{
	int i, match;
#ifdef MORE_ACCURATE
	double rd, gd, bd, dist, mindist;
#else
	int rd, gd, bd, dist, mindist;
#endif /* MORE_ACCURATE */
	int cindx;
static XColor def_colrs[256];
static int have_colors = 0;
	int NumCells;

	match = XAllocColor(dsp, colormap, colr);
	if (match == 0)
	{
		NumCells = DisplayCells(dsp, DefaultScreen(dsp));
		if (!have_colors)
		{
			for (i=0; i<NumCells; i++)
			{
				def_colrs[i].pixel = i;
			}
			XQueryColors(dsp, colormap, def_colrs, NumCells);
			have_colors = 1;
		}
#ifdef MORE_ACCURATE
		mindist = 196608.0;		/* 256.0 * 256.0 * 3.0 */
		cindx = colr->pixel;
		for (i=0; i<NumCells; i++)
		{
			rd = (def_colrs[i].red - colr->red) / 256.0;
			gd = (def_colrs[i].green - colr->green) / 256.0;
			bd = (def_colrs[i].blue - colr->blue) / 256.0;
			dist = (rd * rd) +
				(gd * gd) +
				(bd * bd);
			if (dist < mindist)
			{
				mindist = dist;
				cindx = def_colrs[i].pixel;
				if (dist == 0.0)
				{
					break;
				}
			}
		}
#else
		mindist = 196608;		/* 256 * 256 * 3 */
		cindx = colr->pixel;
		for (i=0; i<NumCells; i++)
		{
			rd = ((int)(def_colrs[i].red >> 8) -
				(int)(colr->red >> 8));
			gd = ((int)(def_colrs[i].green >> 8) -
				(int)(colr->green >> 8));
			bd = ((int)(def_colrs[i].blue >> 8) -
				(int)(colr->blue >> 8));
			dist = (rd * rd) +
				(gd * gd) +
				(bd * bd);
			if (dist < mindist)
			{
				mindist = dist;
				cindx = def_colrs[i].pixel;
				if (dist == 0)
				{
					break;
				}
			}
		}
#endif /* MORE_ACCURATE */
		colr->pixel = cindx;
		colr->red = def_colrs[cindx].red;
		colr->green = def_colrs[cindx].green;
		colr->blue = def_colrs[cindx].blue;
	}
	else
	{
		/*
		 * Keep a count of how many times we have allocated the
		 * same color, so we can properly free them later.
		 */
		allocation_index[colr->pixel]++;

		/*
		 * If this is a new color, we've actually changed the default
		 * colormap, and may have to re-query it later.
		 */
		if (allocation_index[colr->pixel] == 1)
		{
			have_colors = 0;
		}
	}
}


static int
highbit(ul)
unsigned long ul;
{
	/*
	 * returns position of highest set bit in 'ul' as an integer (0-31),
	 * or -1 if none.
	 */

	int i;
	for (i=31; ((ul&0x80000000) == 0) && i>=0;  i--, ul<<=1);
	return i;
}


/*
 * Make am image of appropriate depth for display from image data.
 */
XImage *
MakeImage(dsp, data, width, height, depth, img_info)
	Display *dsp;
	unsigned char *data;
	int width, height;
	int depth;
	ImageInfo *img_info;
{
	int linepad, shiftnum;
	int shiftstart, shiftstop, shiftinc;
	int bytesperline;
	int temp;
	int w, h;
	XImage *newimage;
	unsigned char *bit_data, *bitp, *datap;
	Visual *theVisual;
	int bmap_order;
	unsigned long c;
	int rshift, gshift, bshift;

	switch(depth)
	{
	    case 6:
	    case 8:
		bit_data = (unsigned char *)malloc(width * height);
		bcopy(data, bit_data, (width * height));
		bytesperline = width;
		newimage = XCreateImage(dsp,
			DefaultVisual(dsp, DefaultScreen(dsp)),
			depth, ZPixmap, 0, (char *)bit_data,
			width, height, 8, bytesperline);
		break;
	    case 1:
	    case 2:
	    case 4:
		if (BitmapBitOrder(dsp) == LSBFirst)
		{
			shiftstart = 0;
			shiftstop = 8;
			shiftinc = depth;
		}
		else
		{
			shiftstart = 8 - depth;
			shiftstop = -depth;
			shiftinc = -depth;
		}
		linepad = 8 - (width % 8);
		bit_data = (unsigned char *)malloc(((width + linepad) * height)
				+ 1);
		bitp = bit_data;
		datap = data;
		*bitp = 0;
		shiftnum = shiftstart;
		for (h=0; h<height; h++)
		{
			for (w=0; w<width; w++)
			{
				temp = *datap++ << shiftnum;
				*bitp = *bitp | temp;
				shiftnum = shiftnum + shiftinc;
				if (shiftnum == shiftstop)
				{
					shiftnum = shiftstart;
					bitp++;
					*bitp = 0;
				}
			}
			for (w=0; w<linepad; w++)
			{
				shiftnum = shiftnum + shiftinc;
				if (shiftnum == shiftstop)
				{
					shiftnum = shiftstart;
					bitp++;
					*bitp = 0;
				}
			}
		}
		bytesperline = (width + linepad) * depth / 8;
		newimage = XCreateImage(dsp,
			DefaultVisual(dsp, DefaultScreen(dsp)),
			depth, ZPixmap, 0, (char *)bit_data,
			(width + linepad), height, 8, bytesperline);
		break;
	    /*
	     * WARNING:  This depth 16 code is donated code for 16 but
	     * TrueColor displays.  I have no access to such displays, so I
	     * can't really test it.
	     * Donated by - andrew@icarus.demon.co.uk
	     */
	    case 16:
		bit_data = (unsigned char *)malloc(width * height * 2);
		bitp = bit_data;
		datap = data;
		for (w = (width * height); w > 0; w--)
		{
			temp = (((img_info->reds[(int)*datap] >> 1)& 0x7c00) |
				((img_info->greens[(int)*datap] >> 6)& 0x03e0) |
				((img_info->blues[(int)*datap] >> 11)& 0x001f));

			if (BitmapBitOrder(dsp) == MSBFirst)
			{
				*bitp++ = (temp >> 8) & 0xff;
				*bitp++ = temp & 0xff;
			}
			else
			{
				*bitp++ = temp & 0xff;
				*bitp++ = (temp >> 8) & 0xff;
			}

			datap++;
		}

		newimage = XCreateImage(dsp,
			DefaultVisual(dsp, DefaultScreen(dsp)),
			depth, ZPixmap, 0, (char *)bit_data,
			width, height, 16, 0);
		break;
	    case 24:
		bit_data = (unsigned char *)malloc(width * height * 4);

		theVisual = DefaultVisual(dsp, DefaultScreen(dsp));
		rshift = highbit(theVisual->red_mask) - 7;
		gshift = highbit(theVisual->green_mask) - 7;
		bshift = highbit(theVisual->blue_mask) - 7;
		bmap_order = BitmapBitOrder(dsp);

		bitp = bit_data;
		datap = data;
		for (w = (width * height); w > 0; w--)
		{
		    c =
		     (((img_info->reds[(int)*datap] >> 8) & 0xff) << rshift) |
		     (((img_info->greens[(int)*datap] >> 8) & 0xff) << gshift) |
		     (((img_info->blues[(int)*datap] >> 8) & 0xff) << bshift);

			datap++;

			if (bmap_order == MSBFirst)
			{
				*bitp++ = (unsigned char)((c >> 24) & 0xff);
				*bitp++ = (unsigned char)((c >> 16) & 0xff);
				*bitp++ = (unsigned char)((c >> 8) & 0xff);
				*bitp++ = (unsigned char)(c & 0xff);
			}
			else
			{
				*bitp++ = (unsigned char)(c & 0xff);
				*bitp++ = (unsigned char)((c >> 8) & 0xff);
				*bitp++ = (unsigned char)((c >> 16) & 0xff);
				*bitp++ = (unsigned char)((c >> 24) & 0xff);
			}
		}

		newimage = XCreateImage(dsp,
			DefaultVisual(dsp, DefaultScreen(dsp)),
			depth, ZPixmap, 0, (char *)bit_data,
			width, height, 32, 0);
		break;
	    default:
		fprintf(stderr, "Don't know how to format image for display of depth %d\n", depth);
		return(NULL);
	}

	return(newimage);
}


int
AnchoredHeight(hw)
	HTMLWidget hw;
{
	return((int)(AnchoredImage_height + IMAGE_BORDER));
}


char *
IsMapForm(hw)
	HTMLWidget hw;
{
	char *str;

	str = (char *)malloc(strlen("ISMAP Form") + 1);
	if (str != NULL)
	{
		strcpy(str, "ISMAP Form");
	}
	return(str);
}


int
IsIsMapForm(hw, href)
	HTMLWidget hw;
	char *href;
{
	if ((href != NULL)&&(strcmp(href, "ISMAP Form") == 0))
	{
		return(1);
	}
	else
	{
		return(0);
	}
}


char *
DelayedHRef(hw)
	HTMLWidget hw;
{
	char *str;

	str = (char *)malloc(strlen("Delayed Image") + 1);
	if (str != NULL)
	{
		strcpy(str, "Delayed Image");
	}
	return(str);
}


int
IsDelayedHRef(hw, href)
	HTMLWidget hw;
	char *href;
{
	if ((href != NULL)&&(strcmp(href, "Delayed Image") == 0))
	{
		return(1);
	}
	else
	{
		return(0);
	}
}


Pixmap
DelayedImage(hw, anchored)
	HTMLWidget hw;
	Boolean anchored;
{
	if (delayed_image.image == (Pixmap)NULL)
	{
		delayed_image.image = XCreatePixmapFromBitmapData(
			XtDisplay(hw->html.view),
			XtWindow(hw->html.view), DelayedImage_bits,
			DelayedImage_width, DelayedImage_height,
/*
 * Without motif we use our own foreground resource instead of
 * using the manager's
 */
#ifdef MOTIF
                        hw->manager.foreground,
#else
                        hw->html.foreground,
#endif /* MOTIF */
			hw->core.background_pixel,
			DefaultDepthOfScreen(XtScreen(hw)));
	}

	if ((anchored == True)&&(anchored_image.image == (Pixmap)NULL))
	{
		Pixmap pix;

		anchored_image.image = XCreatePixmapFromBitmapData(
			XtDisplay(hw->html.view),
			XtWindow(hw->html.view), AnchoredImage_bits,
			AnchoredImage_width, AnchoredImage_height,
/*
 * Without motif we use our own foreground resource instead of
 * using the manager's
 */
#ifdef MOTIF
                        hw->manager.foreground,
#else
                        hw->html.foreground,
#endif /* MOTIF */
			hw->core.background_pixel,
			DefaultDepthOfScreen(XtScreen(hw)));
		pix = XCreatePixmap(
			XtDisplay(hw->html.view),
                        XtWindow(hw->html.view),
			DelayedImage_width,
			(DelayedImage_height + AnchoredImage_height +
				IMAGE_BORDER),
			DefaultDepthOfScreen(XtScreen(hw)));
		XSetForeground(XtDisplay(hw), hw->html.drawGC,
			hw->core.background_pixel);
		XFillRectangle(XtDisplay(hw->html.view), pix,
			hw->html.drawGC, 0, 0,
			DelayedImage_width,
			(DelayedImage_height + AnchoredImage_height +
				IMAGE_BORDER));
		XCopyArea(XtDisplay(hw->html.view),
			anchored_image.image, pix, hw->html.drawGC,
			0, 0, AnchoredImage_width, AnchoredImage_height,
			0, 0);
		XCopyArea(XtDisplay(hw->html.view),
			delayed_image.image, pix, hw->html.drawGC,
			0, 0, DelayedImage_width, DelayedImage_height,
			0, (AnchoredImage_height + IMAGE_BORDER));
		XFreePixmap(XtDisplay(hw->html.view), anchored_image.image);
		anchored_image.image = pix;

		return(anchored_image.image);
	}

	return(delayed_image.image);
}


ImageInfo *
DelayedImageData(hw, anchored)
	HTMLWidget hw;
	Boolean anchored;
{
	delayed_image.delayed = 1;
	delayed_image.internal = 0;
	delayed_image.fetched = 0;
	delayed_image.width = DelayedImage_width;
	delayed_image.height = DelayedImage_height;
	delayed_image.num_colors = 0;
	delayed_image.reds = NULL;
	delayed_image.greens = NULL;
	delayed_image.blues = NULL;
	delayed_image.image_data = NULL;
	delayed_image.image = (Pixmap)NULL;

	if (anchored == True)
	{
		anchored_image.delayed = 0;
		anchored_image.internal = 0;
		anchored_image.fetched = 0;
		anchored_image.width = DelayedImage_width;
		anchored_image.height = DelayedImage_height +
			AnchoredImage_height + IMAGE_BORDER;
		anchored_image.num_colors = 0;
		anchored_image.reds = NULL;
		anchored_image.greens = NULL;
		anchored_image.blues = NULL;
		anchored_image.image_data = NULL;
		anchored_image.image = (Pixmap)NULL;

		return(&anchored_image);
	}

	return(&delayed_image);
}


Pixmap
NoImage(hw)
	HTMLWidget hw;
{
	if (no_image.image == (Pixmap)NULL)
	{
		no_image.image = XCreatePixmapFromBitmapData(
			XtDisplay(hw->html.view),
			XtWindow(hw->html.view), NoImage_bits,
			NoImage_width, NoImage_height,
/*
 * Without motif we use our own foreground resource instead of
 * using the manager's
 */
#ifdef MOTIF
                        hw->manager.foreground,
#else
                        hw->html.foreground,
#endif /* MOTIF */
			hw->core.background_pixel,
			DefaultDepthOfScreen(XtScreen(hw)));
	}
	return(no_image.image);
}


ImageInfo *
NoImageData(hw)
	HTMLWidget hw;
{
	no_image.delayed = 0;
	no_image.internal = 0;
	no_image.fetched = 0;
	no_image.width = NoImage_width;
	no_image.height = NoImage_height;
	no_image.num_colors = 0;
	no_image.reds = NULL;
	no_image.greens = NULL;
	no_image.blues = NULL;
	no_image.image_data = NULL;
	no_image.image = (Pixmap)NULL;

	return(&no_image);
}


Pixmap
InfoToImage(hw, img_info)
	HTMLWidget hw;
	ImageInfo *img_info;
{
	int i, size;
	int delta, not_right_col, not_last_row;
	Pixmap Img;
	XImage *tmpimage;
	XColor tmpcolr;
	int *Mapping;
	unsigned char *tmpdata;
	unsigned char *ptr;
	unsigned char *ptr2;
	int Vclass;
	XVisualInfo vinfo, *vptr;
	Boolean need_to_dither;
	unsigned long black_pixel;
	unsigned long white_pixel;

	/* find the visual class. */
	vinfo.visualid = XVisualIDFromVisual(DefaultVisual(XtDisplay(hw),
		DefaultScreen(XtDisplay(hw))));
	vptr = XGetVisualInfo(XtDisplay(hw), VisualIDMask, &vinfo, &i);
	Vclass = vptr->class;
	if (vptr->depth == 1)
	{
		need_to_dither = True;
		black_pixel = BlackPixel(XtDisplay(hw),
				DefaultScreen(XtDisplay(hw)));
		white_pixel = WhitePixel(XtDisplay(hw),
				DefaultScreen(XtDisplay(hw)));
	}
	else
	{
		need_to_dither = False;
	}
	XFree((char *)vptr);

	Mapping = (int *)malloc(img_info->num_colors * sizeof(int));

	for (i=0; i < img_info->num_colors; i++)
	{
		tmpcolr.red = img_info->reds[i];
		tmpcolr.green = img_info->greens[i];
		tmpcolr.blue = img_info->blues[i];
		tmpcolr.flags = DoRed|DoGreen|DoBlue;
		if ((Vclass == TrueColor) || (Vclass == DirectColor))
		{
			Mapping[i] = i;
		}
		else if (need_to_dither == True)
		{
			Mapping[i] = (int)((tmpcolr.red>>5)*11 +
				(tmpcolr.green>>5)*16 +
				(tmpcolr.blue>>5)*5) / (65504/64);
		}
		else
		{
			FindColor(XtDisplay(hw),
				DefaultColormapOfScreen(XtScreen(hw)),
				&tmpcolr);
			Mapping[i] = tmpcolr.pixel;
		}
	}

	/*
	 * Special case:  For 2 color non-black&white images, instead
	 * of 2 dither patterns, we will always drop them to be
	 * black on white.
	 */
	if ((need_to_dither == True)&&(img_info->num_colors == 2))
	{
		if (Mapping[0] < Mapping[1])
		{
			Mapping[0] = 0;
			Mapping[1] = 64;
		}
		else
		{
			Mapping[0] = 64;
			Mapping[1] = 0;
		}
	}

	size = img_info->width * img_info->height;
	if (size == 0)
	{
		tmpdata = NULL;
	}
	else
	{
		tmpdata = (unsigned char *)malloc(size);
	}
	if (tmpdata == NULL)
	{
		tmpimage = NULL;
		Img = (Pixmap)NULL;
	}
	else
	{
		ptr = img_info->image_data;
		ptr2 = tmpdata;

		if (need_to_dither == True)
		{
			int cx, cy;

			for (ptr2 = tmpdata, ptr = img_info->image_data;
				ptr2 < tmpdata+(size-1); ptr2++, ptr++)
			{
				*ptr2 = Mapping[(int)*ptr];
			}

			ptr2 = tmpdata;
			for (cy=0; cy < img_info->height; cy++)
			{
				for (cx=0; cx < img_info->width; cx++)
				{
					/*
					 * Assume high numbers are
					 * really negative.
					 */
					if (*ptr2 > 128)
					{
						*ptr2 = 0;
					}
					if (*ptr2 > 64)
					{
						*ptr2 = 64;
					}

					/*
					 * Traditional Floyd-Steinberg
					 */
					if (*ptr2 < 32)
					{
						delta = *ptr2;
						*ptr2 = black_pixel;
					}
					else
					{
						delta = *ptr2 - 64;
						*ptr2 = white_pixel;
					}
					if (not_right_col =
						(cx < (img_info->width-1)))
					{
						*(ptr2+1) += delta*7 >> 4;
					}

					if (not_last_row =
						(cy < (img_info->height-1)))
					{
						(*(ptr2+img_info->width)) +=
							delta*5 >> 4;
					}

					if (not_right_col && not_last_row)
					{
						(*(ptr2+img_info->width+1)) +=
							delta >> 4;
					}

					if (cx && not_last_row)
					{
						(*(ptr2+img_info->width-1)) +=
							delta*3 >> 4;
					}
					ptr2++;
				}
			}
		} /* end if (need_to_dither==True) */
		else
		{

			for (i=0; i < size; i++)
			{
				*ptr2++ = (unsigned char)Mapping[(int)*ptr];
				ptr++;
			}
		}

		tmpimage = MakeImage(XtDisplay(hw), tmpdata,
			img_info->width, img_info->height,
			DefaultDepthOfScreen(XtScreen(hw)), img_info);

                /* Caught by Purify; should be OK. */
                free (tmpdata);

		Img = XCreatePixmap(XtDisplay(hw->html.view),
			XtWindow(hw->html.view),
			img_info->width, img_info->height,
			DefaultDepthOfScreen(XtScreen(hw)));
	}

	if ((tmpimage == NULL)||(Img == (Pixmap)NULL))
	{
		if (tmpimage != NULL)
		{
			XDestroyImage(tmpimage);
		}
		if (Img != (Pixmap)NULL)
		{
			XFreePixmap(XtDisplay(hw), Img);
		}
		img_info->width = NoImage_width;
		img_info->height = NoImage_height;
		Img = NoImage(hw);
	}
	else
	{
		XPutImage(XtDisplay(hw), Img, hw->html.drawGC, tmpimage, 0, 0,
			0, 0, img_info->width, img_info->height);
		XDestroyImage(tmpimage);
	}

        /* Caught by Purify; should be OK. */
        free((char *)Mapping);

	return(Img);
}

