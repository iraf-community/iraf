
/* Convenience macros.
*/
#define	GPMtoRPM		gtermPM_to_rasPM	/* Pixmap	*/
#define	RPMtoRPM		rasPM_to_rasPM		/* Pixmap	*/
#define	RPMtoGPM		rasPM_to_gtermPM	/* Pixmap	*/

#define	IMGtoGPM		ximage_to_gtermPM	/* XImage	*/
#define	IMGtoRPM		ximage_to_rasPM		/* XImage	*/


Pixmap   gtermPM_to_rasPM (GtermWidget w, Raster dest);
Pixmap   rasPM_to_rasPM (Raster src, Raster dest);
Pixmap   rasPM_to_gtermPM (Raster src, GtermWidget w);

XImage  *ximage_to_gtermPM (GtermWidget w, XImage *xin, 
	    int sx, int sy, int dnx, int dny);
XImage  *ximage_to_rasPM (GtermWidget w, XImage *xin, Raster dest, 
	    int sx, int sy, int dnx, int dny);






/*  Match the destination pixmap to the Gterm pixmap.  If they are the 
**  same depth we can simply return the gterm pixmap, otherwise return
**  a copy of the Gterm pixmap at the dest raster's depth.
*/

Pixmap gtermPM_to_rasPM (GtermWidget w, Raster dest)
{
    if (DBG_TRACE) 
	fprintf (stderr, "GPMtoRPM: %d <-> %d\n", w->gterm.w_depth,dest->depth);

    /* Should be a no-op in 8-bit Pseudocolor mode. */
    if (w->gterm.w_depth == dest->depth)
	return (w->gterm.pixmap);
}


/*  Match the raster pixmap to another raster pixmap.  If they are the 
**  same depth we can simply return the src pixmap, otherwise render the
**  src pixmap to match the destination depth.
*/

Pixmap rasPM_to_rasPM (Raster src, Raster dest)
{
    if (DBG_TRACE) 
	fprintf (stderr, "RPMtoRPM: %d <-> %d\n", src->depth, dest->depth);

    /* Should be a no-op in 8-bit Pseudocolor mode. */
    if (src->depth == dest->depth)
	return (src->r.pixmap);


    if (src->depth < dest->depth) {
	fprintf (stderr, "RPMtoRPM: %d < %d\n", src->depth, dest->depth);
	;
    } else if (src->depth > dest->depth) {
	fprintf (stderr, "RPMtoRPM: %d > %d\n", src->depth, dest->depth);
	;
    }
}


/*  Match the raster pixmap to the Gterm pixmap.  If they are the 
**  same depth we can simply return the raster pixmap, otherwise render the
**  raster pixmap to match the Gterm pixmap depth.
*/

Pixmap rasPM_to_gtermPM (Raster src, GtermWidget w)
{
    if (DBG_TRACE) 
	fprintf (stderr, "RPMtoGPM: %d <-> %d\n", src->depth, w->gterm.w_depth);


    /* Should be a no-op in 8-bit Pseudocolor mode. */
    if (src->depth == w->gterm.w_depth)
	return (src->r.pixmap);


    if (src->depth < w->gterm.w_depth) {
	fprintf (stderr, "RPMtoRPM: %d < %d\n", src->depth, w->gterm.w_depth);
	;
    } else if (src->depth > w->gterm.w_depth) {
	fprintf (stderr, "RPMtoRPM: %d > %d\n", src->depth, w->gterm.w_depth);
	;
    }
}



/*  Match the input XImage to match the raster pixmap.  If they are the 
**  same depth we can simply return the input ximage, otherwise return an
**  ximage at the proper depth.
*/

XImage *ximage_to_rasPM (GtermWidget w, XImage *xin, Raster dest, 
  	    int sx, int sy, int dnx, int dny)
{
    uchar *renderPixels ();
    XImage *cnvImg  = (XImage *) NULL;

 
    if (DBG_TRACE) {
	fprintf (stderr, "IMGtoRPM: %d <-> %d  [%d,%d]  cnvImg=0x%x\n",
	    xin->depth, dest->depth, xin->width, xin->height, cnvImg);
	fprintf (stderr, "IMGtoRPM: Map: (%d,%d)  : %d  %d\n",
	    sx, sy, dnx, dny);
    }


    /* Should be a no-op in 8-bit Pseudocolor mode. */
    if (xin->depth == dest->depth)
	return (xin);

    if (xin->depth < dest->depth) {		    /* e.g. 8 -> 24	   */

        cnvImg = XCreateImage (w->gterm.display, NULL, RGBDepth,
            ZPixmap, 0, (char *) NULL, xin->width, xin->height, 32, 0);

        cnvImg->data = 
	    (char *) renderPixels (w, xin->data, xin->width, xin->height,
	        dest->depth, sx, sy, dnx, dny, cnvImg->bytes_per_line, 
		cnvImg->bits_per_pixel, cnvImg->byte_order);

	return (cnvImg);

    } else if (xin->depth > dest->depth) {	    /* shouldn't happen... */
        if (DBG_TRACE) 
	    fprintf (stderr, "IMGtoRPM: shouldn't be here...\n");
	;
    }

    return ((XImage *) NULL);
}


/*  Match the destination pixmap to the Gterm pixmap.  If they are the 
**  same depth we can simply return the dest pixmap, otherwise render the
**  gterm pixmap to match the destination.
*/

XImage *ximage_to_gtermPM (GtermWidget w, XImage *xin, 
  	    int sx, int sy, int dnx, int dny)
{
    uchar *renderPixels ();
    XImage *cnvImg  = (XImage *) NULL;

 
    if (DBG_TRACE)  {
	fprintf (stderr, "IMGtoGPM: %d <-> %d  [%d,%d]\n",
	    xin->depth, w->gterm.w_depth, xin->width, xin->height);
	fprintf (stderr, "IMGtoGPM: Map: (%d,%d)  : %d  %d\n",
	    sx, sy, dnx, dny);
    }


    /* Should be a no-op in 8-bit Pseudocolor mode. */
    if (xin->depth == w->gterm.w_depth)
	return (xin);

    if (xin->depth < w->gterm.w_depth) {	    /* e.g. 8 -> 24	   */

        cnvImg = XCreateImage (w->gterm.display, NULL, w->gterm.w_depth,
            ZPixmap, 0, (char *) NULL, xin->width, xin->height, 32, 0);

	if (DBG_TRACE)
	    fprintf (stderr, "IMGtoGPM:  bpl=%d  bpp=%d  border=%d\n",
    	 	cnvImg->bytes_per_line, cnvImg->bits_per_pixel, 
		cnvImg->byte_order);

        cnvImg->data = 
	    (char *) renderPixels (w, xin->data, xin->width, xin->height,
	        w->gterm.w_depth, sx, sy, dnx, dny, cnvImg->bytes_per_line, 
		cnvImg->bits_per_pixel, cnvImg->byte_order);

	return (cnvImg);

    } else if (xin->depth > w->gterm.w_depth) {	    /* shouldn't happen... */
        if (DBG_TRACE) 
	    fprintf (stderr, "IMGtoGPM: shouldn't be here...\n");
    }

    return ((XImage *) NULL);
}


/*  Render the 8-bit image pixels to the specified depth.  We'll allocate
**  the pointer to the output pixels and assume the caller will free it.
*/
uchar *
renderPixels (w, in, width, height, depth, sx, sy, dnx, dny, bpl, bpp, border)
GtermWidget w;
uchar	*in;
int	width, height, depth;
int	sx, sy, dnx, dny;
int     bpl, bpp, border;
{
    register int i, j, npix = (width * height);
    unsigned long  *lp, xcol, lval;
    int    nbytes = ((depth == ColormapDepth) ? 1 : 4);
    uchar  *ip = in, pv;
    uchar  *img = (uchar *) NULL;

    int    min=256, max=0, cmin=256, cmax=0, xmin=0, xmax=0;
    time_t start, end;


    
    if (DBG_TRACE) {
	start = time ((time_t) NULL);
	fprintf (stderr, "renderPix: ENTER\n");
    }

    /* Compute the lookup table for the most recent offset/scale.  If this
    ** hasn't changed, this is a no-op.
    */
    if (init_lut (bpp, border) != OK)
	return ((uchar *) NULL);

    /* Allocate the new image data.  The bytes-per-line might be larger than
    ** the indicated width,  so use that for the size.
    */
    img = (uchar *) calloc ((size_t) 1, (size_t) (bpl * height));


    /* Here we render only the pixels in the src rect, leaving the remainder
    ** of the output XImage blank.  The idea is that we process only what
    ** will be visible on the screen, the input XImage still contains all the
    ** off-screen pixels.
    */
    ip = &in[sy * width + sx];
    lp = (unsigned long *) img;
    lp += sy * width + sx;
    for (i=0; i < dny; i++) {
        for (j=0; j < dnx; j++)
	    *lp++ = global_lut[*ip++];

	ip += (width - dnx);		/* advance to next line of rect	*/
	lp += (width - dnx);
    }

    if (DBG_TRACE) {
	fprintf (stderr,
	    "renderPix: pix %d %d  cmap %d %d  xcol 0x%x 0x%x  bpp = %d\n",
	    min,max, cmin,cmax, xmin,xmax, bpp);
	end = time ((time_t) NULL);
	fprintf (stderr, "renderPix: DONE  time = %.5f  %ld %ld\n",
	    difftime(start,end), (long)start, (long)end);
    }

    return (img);
}


/*  Compute the lookup table for the RGB rendering.
*/
init_lut (bpp, border)
int	bpp;
int	border;
{
    register int i;
    unsigned long  rmask, gmask, bmask, rpix, gpix, bpix, xcol, lval;
    int    rshift, gshift, bshift, nbytes;
    uchar  *op, pv;


    if (valid_lut) 
	return (OK);

    /* Compute various shifting constants that we'll need.
    */
    switch (bpp) {		    /* should really get this from Visual */
    case 32:
    case 24:
        rmask  = 0xff0000; gmask  = 0xff00; bmask  = 0xff;
        rshift = -16;      gshift = -8;     bshift = 0;
	nbytes = 4;

	/* Create the lookup table.
	*/
	bzero (global_lut, sizeof(long) * MAX_SZCMAP);
	for (i=0; i < MAX_SZCMAP; i++) {
	    pv = global_cmap[i + SZ_STATIC_CMAP];

	    /* Get the color components.
	    */
	    rpix = global_color[pv].red   >> 8;
	    gpix = global_color[pv].green >> 8;
	    bpix = global_color[pv].blue  >> 8;

	    /* Shift and mask.
	    */
	    rpix = (rpix << (-rshift)) & rmask;
	    gpix = (gpix << (-gshift)) & gmask;
	    bpix = (bpix << (-bshift)) & bmask;

	    /* Create the pixel and load the LUT.
	    */
	    xcol = rpix | gpix | bpix;

	    op = (unsigned char *) &lval;
	    if (border == MSBFirst) {
		*op++ = (xcol>>24) & 0xff;
		*op++ = (xcol>>16) & 0xff;
		*op++ = (xcol>>8)  & 0xff;
		*op++ =  xcol      & 0xff;
	    } else {
		*op++ =  xcol      & 0xff;
		*op++ = (xcol>>8)  & 0xff;
		*op++ = (xcol>>16) & 0xff;
		*op++ = (xcol>>24) & 0xff;
	    }
	    global_lut[i] = lval;
	}
	break;
    case 16:
        rmask  = 0x1f0000; gmask  = 0x1f00; bmask  = 0x1f;
        rshift = -16;      gshift = -8;     bshift = 0;
	nbytes = 2;

	/*   FIXME   */
	break;
    case 15:
        rmask  = 0x1f0000; gmask  = 0x1f00; bmask  = 0x1f;
        rshift = -10;      gshift = -5;     bshift = 0;
	nbytes = 2;

	/*   FIXME   */
	break;
    case  8:
	/*   FIXME   */
	break;
    default:
	valid_lut = 0;
	return (ERR);
    }

    if (DBG_TRACE && DBG_CM_VERB)
	fprintf (stderr, "init_lut: mask=(0x%x,0x%x,0x%x) shift=(%d,%d,%d)\n",
	    rmask, gmask, bmask, rshift, gshift, bshift);

    valid_lut = 1;
    return (OK);
}

		
/*  Find highest one bit set.
**  Returns bit number + 1 of highest bit that is set, otherwise returns 0.
**  High order bit is 31 (or 63 in _LP64 kernel).
*/
int highbit(unsigned long i)
{
    register int h = 1;
    if (i == 0)
        return (0);
#ifdef _LP64
    if (i & 0xffffffff00000000ul) { h += 32; i >>= 32; }
#endif
    if (i & 0xffff0000) 	  { h += 16; i >>= 16; }
    if (i & 0xff00) 	  	  { h +=  8; i >>=  8; }
    if (i & 0xf0) 	 	  { h +=  4; i >>=  4; }
    if (i & 0xc) 	 	  { h +=  2; i >>=  2; }
    if (i & 0x2) 	 	  { h +=  1; 	       }

    return (h);
} 


/*  Find lowest one bit set.
**  Returns bit number + 1 of lowest bit that is set, otherwise returns 0.
**  Low order bit is 0.
*/
int lowbit(unsigned long i)
{
    register int h = 1;

    if (i == 0)
        return (0);

#ifdef _LP64
    if (!(i & 0xffffffff)) { h += 32; i >>= 32; }
#endif
    if (!(i & 0xffff))     { h += 16; i >>= 16; }
    if (!(i & 0xff))       { h +=  8; i >>=  8;	}
    if (!(i & 0xf))        { h +=  4; i >>=  4;	}
    if (!(i & 0x3))        { h +=  2; i >>=  2;	}
    if (!(i & 0x1))        { h +=  1; 		}

    return (h);
} 


/* Debug Utilities.
*/
bob() { int i = 0; fprintf (stderr, "Hi from Bob\n");  }

dbg_printCmaps (GtermWidget w)
{
    int     i, j, first, nelem, maxelem, nc;
    unsigned short r[256], g[256], b[256];
    unsigned short rs[256], gs[256], bs[256], iomap[256];


    bzero (r,    (256*sizeof(short)));
    bzero (g,    (256*sizeof(short)));
    bzero (b,    (256*sizeof(short)));
    bzero (rs,   (256*sizeof(short)));
    bzero (gs,   (256*sizeof(short)));
    bzero (bs,   (256*sizeof(short)));
    bzero (iomap,(256*sizeof(short)));

    /* Get the current colormap and iomap.
    */
    GtQueryColormap (w, 0, &first, &nelem, &maxelem);
    nc = GtReadColormap (w, 0, 0, MAX_SZCMAP, rs, gs, bs);
    GtReadIomap (w, iomap, 0, MAX_SZCMAP);

    fprintf (stderr, "cmaps: first=%d nelem=%d max=%d  nc=%d\n",
        first, nelem, maxelem, nc);
    fprintf (stderr, "cmaps: basePixel=%d  maxColors=%d ncolors=%d\n",
        w->gterm.base_pixel, w->gterm.maxColors, w->gterm.ncolors);


    for (i=0;  i < nc;  i++) {
        j = iomap[i];
        r[i] = (rs[j] >> 8); g[i] = (gs[j] >> 8); b[i] = (bs[j] >> 8);
    }
    for (i=nc;  i < MAX_SZCMAP;  i++) {
        j = iomap[i];
        r[i] = (rs[j] >> 8); g[i] = (gs[j] >> 8); b[i] = (bs[j] >> 8);
    }

    fprintf (stderr, "cmap: idx iomap  ReadColormap  UserColormap  ");
    fprintf (stderr, "GlobalCmap   cm    in   out\n");
    for (i=0;  i < MAX_SZCMAP;  i++)
     fprintf (stderr, 
     "cmap: %3d  %3d   %3d %3d %3d   %3d %3d %3d  %3d %3d %3d  %3d  %3d  %3d\n",
          i, iomap[i], rs[i]>>8, gs[i]>>8, bs[i]>>8, r[i], g[i], b[i],
	  global_color[i].red>>8, global_color[i].green>>8, 
	  global_color[i].blue>>8, global_cmap[i],
          w->gterm.cmap_in[i], w->gterm.cmap_out[i]);
}

dbg_printMappings (GtermWidget w)
{
    Mapping mp;
    char *scales[] = {"noScale", "zoom", "intZoom", "deZoom"};

    for (mp=w->gterm.mp_head; mp; mp = mp->next) {
	fprintf (stderr, "Map %2d: src=%d  ty=%s  s=(%d,%d)  n=(%d,%d)\n",
	    mp->mapping, mp->src, (mp->st == GtPixel ? "GtPixel" : "GtNDC"), 
	    mp->sx, mp->sy, mp->snx, mp->sny);
	fprintf (stderr, "      : dst=%d  ty=%s  s=(%d,%d)  n=(%d,%d)\n",
	    mp->dst, (mp->dt == GtPixel ? "GtPixel" : "GtNDC"), 
	    mp->dx, mp->dy, mp->dnx, mp->dny);
	fprintf (stderr, "      : rop=%d  refresh=%d  enabled=%d def=%d  %s\n",
	    mp->rop, mp->refresh, mp->enabled, mp->defined,
	    scales[mp->scaling]);
	fprintf (stderr, "      : src=0x%x  dst=0x%x  pixmap=0x%x\n",
	    (int)&w->gterm.rasters[mp->src], (int)&w->gterm.rasters[mp->dst],
	    (int)&w->gterm.rasters[0]);
    }
}


dbg_printRasters (GtermWidget w)
{
    register int i;
    Raster   rp;


    for (i=0; i < w->gterm.nrasters; i++) {
	rp = &w->gterm.rasters[i];

	fprintf (stderr, "Raster %d:  ty=%s  size=(%d,%d,%d)\t",
	    i, (rp->type == 1 ? "ximage" : "pixmap"),
	    rp->width, rp->height, rp->depth);
	fprintf (stderr, ": p=0x%x im=0x%x sh=0x%x\n",
	    rp->r.pixmap, rp->r.ximage, rp->shadow_pixmap);
    }
}


