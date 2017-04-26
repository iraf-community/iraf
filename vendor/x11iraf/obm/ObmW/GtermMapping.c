


/* refresh_source -- Refresh a portion of a mapping given the source rect
 * to be refreshed.  If the given source rect is not within the mapping,
 * this is a no-op.
 */
static
refresh_source (w, mp, x1, y1, nx, ny)
    GtermWidget w;
    register Mapping mp;	/* mapping defining refresh operation */
    int x1, y1, nx, ny;		/* region of source to be refreshed */
{
    int sx1, sx2, sy1, sy2, snx, sny;
    int dx1, dx2, dy1, dy2, dnx, dny;
    int valid;


    /* Do nothing if mapping not defined and enabled.
    */
    if (!(valid = valid_mapping (w, mp))) {
	if (DBG_TRACE)
	    fprintf (stderr, "refresh_source: valid=%d\n", valid);
	return (ERR);
    }
    if (!mp->enabled) {
	if (DBG_TRACE)
	    fprintf (stderr, "ERROR ref_src: mapping %d not enabled\n",
		mp->mapping);
	return (OK);
    }

    /* Compute the intersection of the modified region of the source raster
    ** with the rectangular region of the source raster affected by the given
    ** mapping.
    */
    sx1 = max (x1, mp->sx);
    sy1 = max (y1, mp->sy);
    sx2 = min(x1 + nx, mp->sx + mp->snx) - 1;
    sy2 = min(y1 + ny, mp->sy + mp->sny) - 1;
    snx = sx2 - sx1 + 1;
    sny = sy2 - sy1 + 1;

    /* Do nothing if the rectangles do not intersect.
    */
    if (snx <= 0 || sny <= 0)
	return (OK);

    /* Compute the destination rect affected by the mapped source rect.
    */
    dx1 = mp->x_extent[sx1 - mp->sx].lo;
    dx2 = mp->x_extent[sx2 - mp->sx].hi;
    if (dx1 > dx2) {
	dx1 = mp->x_extent[sx2 - mp->sx].lo;
	dx2 = mp->x_extent[sx1 - mp->sx].hi;
    }

    dy1 = mp->y_extent[sy1 - mp->sy].lo;
    dy2 = mp->y_extent[sy2 - mp->sy].hi;
    if (dy1 > dy2) {
	dy1 = mp->y_extent[sy2 - mp->sy].lo;
	dy2 = mp->y_extent[sy1 - mp->sy].hi;
    }

    dnx = dx2 - dx1 + 1;
    dny = dy2 - dy1 + 1;

    if (CacheXImage)						/* MF004 */
        DestroyCachedXImage();					/* MF004 */

    if (DBG_TRACE) {
	fprintf(stderr, "refresh_source -- refreshing src  (%d,%d) (%d,%d)\n",
	    sx1, sy1, snx, sny);
	fprintf(stderr, "refresh_source -- refreshing dest (%d,%d) (%d,%d)\n",
	    dx1, dy1, dnx, dny);
	dbg_printMappings (w);
    }

    /* Perform the refresh operation. */
    return (refresh_destination (w, mp, dx1, dy1, dnx, dny));
}


/* refresh_destination -- Refresh (redraw) the pixels in the given destination
 * rect.  The mapping operand defines how to generate the value of each output
 * pixel.  This is the routine which does all the real work of a mapping,
 * computing the values of the output pixels and writing to the destination
 * drawable.  Note: the destination rect must be specified in raster pixel
 * coordinates (no NDC).
 */
static
refresh_destination (w, mp, x1, y1, nx, ny)
    GtermWidget w;
    Mapping mp;			/* mapping defining refresh operation */
    int x1, y1, nx, ny;		/* region of destination to be refreshed */
{
    Raster sr, dr, pr;
    Display *display = w->gterm.display;
    int scaling, xflip, yflip, delxin=0, delxout=0;
    int ox, oy, rop, clip, mapping, i, j;
    int src, st, sx, sy, snx, sny;
    int dst, dt, dx, dy, dnx, dny;
    int xoff, yoff, p1, p2, q1, q2;
    float xscale, yscale;
    struct mapping *np, p_mp;
    XImage *xin, *xout;
    int status = OK;
    Pixmap pixmap;						/* MF004 */

    Region clip_region, mask_region;
    uchar *old_xin_lp, *old_xout_lp;
    uchar *xin_lp, *xout_lp, *op;
    int xin_bpl, xout_bpl;
    int *xmap, *ymap, valid;
    XRectangle r;
    XImage *img1 = (XImage *) NULL, 
	   *img2 = (XImage *) NULL, 
	   *img3 = (XImage *) NULL, 
	   *img4 = (XImage *) NULL, 
	   *img5 = (XImage *) NULL;



    if (DBG_TRACE)
	fprintf(stderr, "refresh_destination ENTER[0x%x]: pos=%d,%d sz=%d,%d\n",
	    w, x1,y1,nx,ny);

    if (!w || !XtIsRealized ((Widget)w))
	return (OK);

    /* Do nothing if mapping not defined and enabled. */
    if (!(valid = valid_mapping (w, mp))) {
	if (DBG_TRACE)
	    fprintf (stderr, "refresh_destination: valid=%d\n", valid);
	return (ERR);
    }
    if (!mp->enabled) {
	if (DBG_TRACE)
	    fprintf (stderr, "ERROR ref_dst: mapping %d not enabled\n",
		mp->mapping);
	return (OK);
    }


    if (DBG_CMAPS && DBG_CM_VERB)
	dbg_printCmaps (w);
    if (DBG_TRACE)
	dbg_printRasters (w);


    /* Offsets into the x_*,y_* mapping lookup tables. */
    xoff = x1 - mp->dx;
    yoff = y1 - mp->dy;

    /* Get source and destination rects. */
    dst = mp->dst;
    dx  = x1;	dy  = y1;
    dnx = nx;	dny = ny;

    src = mp->src;
    p1  = mp->x_srcpix[xoff];
    q1  = mp->y_srcpix[yoff];
    p2  = mp->x_srcpix[xoff + nx - 1];
    q2  = mp->y_srcpix[yoff + ny - 1];
    sx  = min (p1, p2);
    sy  = min (q1, q2);
    snx = abs (p2 - p1) + 1;
    sny = abs (q2 - q1) + 1;

    /* Define some local variables. */
    sr = &w->gterm.rasters[src];
    dr = &w->gterm.rasters[dst];
    pr = &w->gterm.rasters[0];		/* display pixmap raster	*/
    mapping  = mp->mapping;
    scaling  = mp->scaling;
    xscale   = mp->xscale;
    yscale   = mp->yscale;
    rop = mp->rop;

    if (DBG_TRACE) {
	 fprintf (stderr, 
	    "refresh_destination: src,dst = %d,%d st,dt = %s[%d],%s[%d]\n",
	        src, dst, 
	        (sr->type == 1 ? "image" : "pixmap"),  sr->depth,
	        (dr->type == 1 ? "image" : "pixmap"),  dr->depth);
	fprintf (stderr, 
	    "refresh: gterm.pixmap=0x%x src.pixmap=0x%x  dest.pixmap=0x%x\n",
	    w->gterm.pixmap, sr->r.pixmap, dr->r.pixmap);
	fprintf (stderr, 
	    "refresh: gterm.pixmap=0x%x src.ximage=0x%x  dest.ximage=0x%x\n",
	    w->gterm.pixmap, sr->r.ximage, dr->r.ximage);
	fprintf (stderr, 
	    "refresh_destination: src=>(%d,%d : %d,%d)  dst=>(%d,%d : %d,%d)\n",
	    sx, sy, snx, sny, dx, dy, dnx, dny);
    }


    /* Basic error checking.
    */
    if (!sr->type || !dr->type) {
	if (DBG_TRACE)
	    fprintf (stderr, "ERROR: invalid src or dest type\n");
	return (ERR);

    } else if (snx <= 0 || sny <= 0 || dnx == 0 || dny == 0) {
	if (DBG_TRACE)
	    fprintf (stderr, "ERROR: negative src position or zero size\n");
	return (ERR);

    } else if (src < 0 || src > w->gterm.maxRasters ||
	dst < 0 || dst > w->gterm.maxRasters) {
	    if (DBG_TRACE)
	        fprintf (stderr, "ERROR:  invalid src or dest value\n");
	    return (ERR);
    }


    /* Do we have a flip in X or Y? */
    xflip = mp->dnx < 0;
    yflip = mp->dny < 0;

    /* Any higher numbered mappings which map to the same destination as the
     * mapping MP will obscure the current mapping.  Construct a clip mask
     * defining the region of the destination we can write to.  This will be
     * the region not obscured by any higher numbered, active mappings.
     */
    clip = False;
    clip_region = XCreateRegion();
    r.x      = dx;  
    r.y      = dy;
    r.width  = dnx;  
    r.height = dny;
#ifdef sun
    /* As far as I can tell the Sun (probably in the OW X server) has an
     * off by one bug affecting clipping in the server.  A clip region is
     * too small by one pixel at the right and bottom, causing these pixels
     * to not be written when refreshing the screen (usually this shows up
     * as black lines on the screen when a region is refreshed).  So far
     * I haven't seen this on any other platform.  The problem is imperfectly
     * understood and the following workaround may not completely workaround
     * the problem.
     */
    r.width++;  r.height++;
#endif
    XUnionRectWithRegion (&r, clip_region, clip_region);

    for (np = mp->next;  np;  np = np->next) {
	struct mapping p_np;

	if (!np->enabled || np->dst != dst)
	    continue;
	get_pixel_mapping (w, np, &p_np, 0);

	r.x = p_np.dx;  r.y = p_np.dy;
	r.width = abs(p_np.dnx);
	r.height = abs(p_np.dny);

	if (XRectInRegion (clip_region,
	    r.x, r.y, r.width, r.height) != RectangleOut) {

	    mask_region = XCreateRegion();
	    XUnionRectWithRegion (&r, mask_region, mask_region);
	    XSubtractRegion (clip_region, mask_region, clip_region);
	    XDestroyRegion (mask_region);
	    clip++;
	}
    }
    if (DBG_TRACE)
	fprintf (stderr,
	    "refresh_destination: xflip=%d  yflip=%d  clip=%d  scaling=%d\n",
	     xflip, yflip, clip, scaling);

    if (clip && dr->type == PixmapRaster)
	XSetRegion (w->gterm.display, w->gterm.exposeGC, clip_region);


    /* Handle the special case of a pixmap to pixmap (or window) copy in
     * the server with no scaling.
     */
    if (!scaling && sr->type == PixmapRaster && dr->type == PixmapRaster) {

        if (DBG_TRACE)
	    fprintf (stderr,
	        "refresh_destination: pixmap-to-pixmap copy  %d <-> %d\n",
	         src, dst);
	    
	if (src == 0 && dst == 0 && w->gterm.pixmap && !(rop & R_Transient)) {

	    if (DBG_TRACE) 
		fprintf (stderr, 
		    "refresh_destination: pixmap copy src=dst=0\n");

	    XCopyArea (display, 
		w->gterm.pixmap, w->gterm.pixmap,
		w->gterm.exposeGC, sx, sy, snx, sny, dx, dy);

	    XCopyArea (display, 
		GPMtoRPM(w, dr), dr->r.pixmap,
		w->gterm.exposeGC, dx, dy, dnx, dny, dx, dy);

	    /* Update the root display shadow pixmap.
	    */
	    XCopyArea (display, 
		pr->shadow_pixmap, pr->shadow_pixmap,
		w->gterm.expose8GC, sx, sy, snx, sny, dx, dy);

	} else {
	    Raster rp = &w->gterm.rasters[src-1];

	    if (DBG_TRACE) {
		fprintf (stderr, 
		    "refresh_destination: pixmap copy src || dst != 0\n");
		fprintf (stderr, "src=%d  dest=%d  shadow=0x%x  ximage=0x%x\n",
		    src, dst, dr->shadow_pixmap, rp->r.ximage);
		dbg_printRasters (w);
		fprintf (stderr,
		    "src=%d,%d %d %d   dest=%d,%d  %d %d  (src=%d  dst=%d)\n",
		    sx, sy, snx, sny, dx, dy, dnx, dny, src, dst);
	    }

	    XCopyArea (display, 
		RPMtoRPM(sr, dr), dr->r.pixmap, 
		w->gterm.exposeGC, sx, sy, snx, sny, dx, dy);


	    /*  Update the shadow pixmap.
	    */
	    XCopyArea (display, 
		sr->shadow_pixmap, dr->shadow_pixmap, 
		w->gterm.expose8GC, sx, sy, snx, sny, dx, dy);

	    if (dst == 0 && w->gterm.pixmap && !(rop & R_Transient))
		XCopyArea (display,
		    RPMtoGPM (sr, w), w->gterm.pixmap,
		    w->gterm.exposeGC, sx, sy, snx, sny, dx, dy);
	}
		

	if (DBG_TRACE)
	    fprintf (stderr,
		"refresh_destination: spec_case: pixmap copy no scaling\n");

	goto done;
    }

    /* Any other case requires working on ximages in the client.  The first
     * step is to get the source ximage.
     */
    if (sr->type == PixmapRaster) {

	if (DBG_TRACE)
	    fprintf (stderr,
		"refresh_destination: source type is PixmapRaster[%d]\n", src);
	    
	/* Source is a pixmap but we need a local copy as either the
	 * destination is not a pixmap, or we need to do some scaling.
	 */
	if (CacheXImage) {					/* MF004 */
            pixmap = (src || !w->gterm.pixmap) ? sr->r.pixmap : w->gterm.pixmap;

	    if (DBG_TRACE) {
		fprintf (stderr, "refresh_destination: xin is cached XImage ");
		fprintf (stderr, " src=%d pix=0x%x  dims=%d,%d\n",
		    src, w->gterm.pixmap, sr->width, sr->height);
	    }

            xin = GetCachedXImage (w, pixmap, sr->width, sr->height);
            if (xin == NULL) {
                xin = XGetImage (display, pixmap,
                    0, 0, sr->width, sr->height, 0xff, ZPixmap);
                if (xin == NULL) {
                    status = ERR;
                    goto done;
                } else
                    NewCachedXImage (w, xin, pixmap, sr->width, sr->height);
            }

	} else {						/* MF004 */

	    /*
	    */
	    if (w->gterm.w_depth > ColormapDepth) {
		if (DBG_TRACE) {
		    fprintf (stderr,
			"src=%d  dst=%d  d_ras=%d  %d x %d  pr=0x%x  sr=0x%x\n",
		        src, dst, w->gterm.d_raster, sr->width, sr->height, 
		        pr->shadow_pixmap, sr->shadow_pixmap);
		    dbg_printRasters (w);
		}
		    
	        xin = XGetImage (display, sr->shadow_pixmap,
	            0, 0, sr->width, sr->height, 0xff, ZPixmap);

	    } else {
	        xin = XGetImage (display,
	            (src || !w->gterm.pixmap) ? sr->r.pixmap : w->gterm.pixmap,
	            0, 0, sr->width, sr->height, 0xff, ZPixmap);
	    }

	    if (DBG_TRACE) {
		fprintf (stderr, "refresh_destination: xin is drawable '%s'\n",
    		    (src || !w->gterm.pixmap) ? "sr raster" : "gterm pixmap");
		fprintf (stderr,
		    "refresh_destination: xin size=%d,%d  bpp=%d bpl=%d\n",
		    xin->width, xin->height, xin->bits_per_pixel,
		    xin->bytes_per_line);
	    }

	    if (xin == NULL) {
	        status = ERR;
	        goto done;
	    }
   	    delxin++;
	}							/* MF004 */

    } else {
	/* Source is an ximage. */
	if (DBG_TRACE)
	    fprintf (stderr,
		"refresh_destination: src type is XImage (sr->r.ximage)\n");

	xin = sr->r.ximage;
    }

    /* Handle the special case of a copy of an ximage to an output pixmap
     * with no scaling.
     */
    if (!scaling && dr->type == PixmapRaster) {

	if (DBG_TRACE)
	    fprintf (stderr, "refresh_destination: NO scaling, dest=Pixmap\n");

	if (dst == 0 && w->gterm.pixmap && !(rop & R_Transient)) {

	    if (DBG_TRACE)
		fprintf (stderr, "refresh_destination: dest == 0\n");

	    XPutImage (display, 
		w->gterm.pixmap, w->gterm.exposeGC, 
		(img1=IMGtoGPM(w,xin,sx,sy,dnx,dny)),
		sx, sy, dx, dy, dnx, dny);


	    if (DBG_TRACE)
		fprintf (stderr,
		    "\n\nrefresh: gterm.pixmap=0x%x dest.pixmap=0x%x\n\n",
		    w->gterm.pixmap, dr->r.pixmap);

	    XCopyArea (display, 
		GPMtoRPM (w, dr), dr->r.pixmap,
		w->gterm.exposeGC, dx, dy, dnx, dny, dx, dy);

	    if (w->gterm.w_depth > ColormapDepth) {
	        XPutImage (display, pr->shadow_pixmap, w->gterm.expose8GC, xin,
		    sx, sy, dx, dy, dnx, dny);
	    }

	} else {

	    if (DBG_TRACE) {
		fprintf (stderr, "refresh_destination: xin dst > 0\n");
		fprintf (stderr,
		    "refresh_destination: src=(%d,%d) => %d,%d : %d,%d\n",
		    sx, sy, dx, dy, dnx, dny);
		fprintf (stderr, 
		    "refresh_destination: dr->shadow_pixmap = 0x%x\n",
		    dr->shadow_pixmap);
		fprintf (stderr, "refresh_destination: xin => %d x %d x %d\n",
		    xin->width, xin->height, xin->depth);
	    }

	    /* Copy the 'xin' to the 'dr->r.pixmap'
	    */
	    XPutImage (display, 
		dr->r.pixmap, w->gterm.exposeGC, 
		(img2=IMGtoRPM(w,xin,dr,sx,sy,dnx,dny)),
		sx, sy, dx, dy, dnx, dny);


	    if (w->gterm.w_depth > ColormapDepth) {
	        XPutImage (display, dr->r.pixmap, w->gterm.exposeGC, 
		    (img3=IMGtoRPM(w,xin,dr,sx,sy,dnx,dny)),
		    sx, sy, dx, dy, dnx, dny);

		/*  Shadow the pixmap with an 8-bit version for readback.
		*/
		if (DBG_TRACE)
		    fprintf (stderr,
			"updating shadow_pixmap........................0x%x\n",
			dr->shadow_pixmap);
	        XPutImage (display, dr->shadow_pixmap,
		    w->gterm.expose8GC, xin,
		    sx, sy, dx, dy, dnx, dny);
	    }
	}

	if (DBG_TRACE)
	    fprintf (stderr, 
		"refresh_destination: NO scaling, dest=Pixmap -- DONE\n");

	goto done;
    }

    /* Get output ximage. 
    */
    if (dr->type == ImageRaster) {

	if (DBG_TRACE)
	    fprintf (stderr,
		"refresh_destination: dest type is XImage (dr->r.ximage)\n");

	xout = dr->r.ximage;
	ox = dx;  
	oy = dy;

    } else {

	uchar *data = (uchar *) XtCalloc (dnx * dny, sizeof(uchar));


	if (DBG_TRACE)
	    fprintf (stderr,
		"refresh_destination: dest type is Pixmap, create XImage\n");
	    
	if (data == NULL) {
	    status = ERR;
	    goto done;
	}

	if (DBG_TRACE)
	    fprintf (stderr,
		"refresh_destination: creating 8-bit ximage....%d x %d\n",
		dnx, dny);

	xout = XCreateImage (w->gterm.display, NULL, RasterDepth,
	    ZPixmap, 0, (char *)data, dnx, dny, 8, 0);

	if (xout == NULL) {
	    XtFree ((char *)data);
	    status = ERR;
	    goto done;
	}
	ox = 0;  
	oy = 0;
	delxout++;
    }

    xin_lp = (uchar *)xin->data;
    xout_lp = (uchar *)xout->data;
    xin_bpl = xin->bytes_per_line;
    xout_bpl = xout->bytes_per_line;

    /* Map a region of the input ximage XIN to the output ximage XOUT.  Various
     * approaches are used to generate the output data, depending upon what
     * type of scaling we are doing.
     */
    if (DBG_TRACE)
	fprintf (stderr, "refresh: scaling=%d  xin=0x%x  xout=0x%x\n",
	    scaling, xin, xout);

    if (!scaling) {
	/* No scaling.  Copy a region of the ximage xin to xout without
	 * spatially scaling the image data.
	 */
	if (clip && dr->type == ImageRaster)
	    goto zoom;

	xin_lp = (uchar *)xin->data + sy * xin_bpl + sx;
	xout_lp = (uchar *)xout->data + oy * xout_bpl + ox;

	for (j=0;  j < dny;  j++) {
	    memmove (xout_lp, xin_lp, dnx);
	    xin_lp += xin_bpl;
	    xout_lp += xout_bpl;
	}

    } else if (scaling == M_INTZOOM) {
	/* Integer zoom.  The zoom factor is an integer, allowing the zoomed
	 * image to be calculated without using the xmap,ymap lookup tables.
	 */
	if (clip && dr->type == ImageRaster)
	    goto zoom;

	scale_intzoom (xin_lp,xin_bpl, xout_lp,xout_bpl, sx,sy, ox,oy,dnx,dny,
	    xflip,yflip, (int)(xscale + 0.5), (int)(yscale + 0.5));

    } else if (scaling == M_ZOOM) {
	/* We have a zoom, or one-to-many, scaling.  Zoom scaling is always
	 * done with pixel replication.  The [xy]_srcpix arrays in the mapping
	 * descriptor give the source pixel corresponding to each mapped pixel
	 * in the destination raster.
	 */
zoom:
	xmap = &mp->x_srcpix[xoff];
	ymap = &mp->y_srcpix[yoff];

	scale_zoom (xin_lp, xin_bpl, xout_lp, xout_bpl,
	    xmap, ymap, ox, oy, dnx, dny,
	    (clip && dr->type == ImageRaster) ? clip_region : (Region)NULL);

    } else if (scaling == M_DEZOOM) {
	/* We have a dezoom, or many-to-one, scaling.  A block of pixels in
	 * the input raster are combined to generate the value of each output
	 * pixel, using one of several antialias algorithms to compute the
	 * output pixel value.
	 */
	float *x_src = &mp->x_src[xoff];
	float *y_src = &mp->y_src[yoff];
	int near_unitary = (xscale > 0.5 && yscale > 0.5);
	int function;

	/* Get the antialising function to be applied. */
	if (!(function = (mp->rop & R_MFMask)))
	    function = MF_NEAREST;

	/* If the dezoom factor is small and either MF_BILINEAR or
	 * MF_NEAREST is enabled, use the indicated method to sample the
	 * input data.  This uses all the data but minimizes smoothing.
	 */
	if ((function & (MF_BILINEAR|MF_NEAREST)) && near_unitary)
	    function = (function & MF_BILINEAR) ? MF_BILINEAR : MF_NEAREST;
	else if (function != (function & (MF_BILINEAR|MF_NEAREST)))
	    function &= ~(MF_BILINEAR|MF_NEAREST);

filter:
	/* This can take a while so update the display. */
	XFlush (w->gterm.display);

	/* If the filtering operation involves any arithmetic combinations
	 * of pixels we must convert pixel numbers to pixel intensity values
	 * before performing the filtering operation.  This is a case where
	 * we would be better off if frame buffers were maintained using
	 * pixel intensities rather than hardware pixel numbers.
	 */
	if (function != MF_NEAREST) {
            uchar *data = (uchar *) XtMalloc (xin->width * xin->height);
            if (data == NULL) {
                status = ERR;
                goto done;
            }

            mf_getinten (w,
		xin_lp, xin->width, xin->height, xin_bpl, sx,sy,
	        data, xin->width, xin->height, xin_bpl, sx,sy, snx,sny);

            if (!delxin) {
                xin = XCreateImage (w->gterm.display, NULL, RasterDepth,
                    ZPixmap, 0, (char *)data, xin->width, xin->height, 8, 0);
                if (xin == NULL) {
                    XtFree ((char *)data);
                    status = ERR;
                    goto done;
                }
                delxin++;
            } else {
                XtFree ((char *)xin->data);
                xin->data = (char *) data;
            }
            xin_lp = (uchar *)xin->data;
	}

	/* Filter the source rect to the destination. 
	*/
	switch (function) {
	case MF_NEAREST:
	    scale_nearest (
		xin_lp, xin->width, xin->height, xin_bpl,
		xout_lp, xout->width, xout->height, xout_bpl,
		x_src, y_src, ox, oy, dnx, dny,
		(clip && dr->type == ImageRaster) ? clip_region : (Region)NULL
	    );
	    break;
	case MF_BILINEAR:
	    scale_bilinear (
		xin_lp, xin->width, xin->height, xin_bpl,
		xout_lp, xout->width, xout->height, xout_bpl,
		x_src, y_src, ox, oy, dnx, dny,
		(clip && dr->type == ImageRaster) ? clip_region : (Region)NULL
	    );
	    break;
	case MF_BLKAVG:
	    scale_boxcar (
		xin_lp, xin->width, xin->height, xin_bpl,
		xout_lp, xout->width, xout->height, xout_bpl,
		x_src,y_src, sx,sy,snx,sny, ox,oy,dnx,dny,
		xscale,yscale, 0, clip ? clip_region : (Region)NULL
	    );
	    break;
	case MF_BOXCAR:
	    scale_boxcar (
		xin_lp, xin->width, xin->height, xin_bpl,
		xout_lp, xout->width, xout->height, xout_bpl,
		x_src,y_src, sx,sy,snx,sny, ox,oy,dnx,dny,
		xscale,yscale, 1, clip ? clip_region : (Region)NULL
	    );
	    break;
	case MF_LOWPASS:
	    scale_lowpass (
		xin_lp, xin->width, xin->height, xin_bpl,
		xout_lp, xout->width, xout->height, xout_bpl,
		x_src,y_src, sx,sy,snx,sny, ox,oy,dnx,dny,
		xscale,yscale, clip ? clip_region : (Region)NULL
	    );
	    break;
	default:
	    function = MF_BILINEAR;
	    goto filter;
	}

	/* If the operation was performed in intensity space convert back
	 * to pixel number.
	 */
	if (function != MF_NEAREST)
            mf_getpixel (w,
		xout_lp, xout->width, xout->height, xout_bpl,  ox,oy,
	        xout_lp, xout->width, xout->height, xout_bpl,  ox,oy, dnx,dny);

    } else {
	status = ERR;
	goto done;
    }

    /* Copy the scaled ximage to the output pixmap, if any.
     */
    if (dr->type == PixmapRaster) {
	if (DBG_TRACE)
	    fprintf (stderr, "refresh_destination: COPY TO OUTPUT PIXMAP\n");

	if (dst == 0 && w->gterm.pixmap && !(rop & R_Transient)) {

	    if (DBG_TRACE) {
		fprintf (stderr, "refresh_destination: dst=0\n");
		fprintf (stderr,
		    "refresh_destination: src=(%d,%d) => %d,%d : %d,%d\n",
		    ox, oy, dx, dy, dnx, dny);
	    }

	    XPutImage (display, 
		w->gterm.pixmap, w->gterm.exposeGC, 
		(img4=IMGtoGPM(w,xout,ox,oy,dnx,dny)),
		ox, oy, dx, dy, dnx, dny);

	    XCopyArea (display, 
		GPMtoRPM (w, dr), dr->r.pixmap,
		w->gterm.exposeGC, dx, dy, dnx, dny, dx, dy);

	    if (w->gterm.w_depth > ColormapDepth) {
		if (DBG_TRACE) {
		    fprintf (stderr,
			"updating shadow_pixmap........................0x%x\n",
			dr->shadow_pixmap);
		    fprintf (stderr,
			"updating %d,%d  to %d,%d  %d %d\n",
			ox, oy, dx, dy, dnx, dny);
		}
	        XPutImage (display, dr->shadow_pixmap,
		    w->gterm.expose8GC, xout, ox, oy, dx, dy, dnx, dny);
	    }

	} else {

	    if (DBG_TRACE) {
		fprintf (stderr, "refresh_destination: xout dst > 0\n");
		fprintf (stderr,
		    "refresh_destination: src=(%d,%d) => %d,%d : %d,%d\n",
		    ox, oy, dx, dy, dnx, dny);
	    }

	    XPutImage (display, 
		dr->r.pixmap, w->gterm.exposeGC,
		(img5=IMGtoRPM(w,xout,dr,ox,oy,dnx,dny)),
		ox, oy, dx, dy, dnx, dny);

	    if (w->gterm.w_depth > ColormapDepth) {
		/*
	        XPutImage (display, pr->r.pixmap, w->gterm.exposeGC, xout,
		    ox, oy, dx, dy, dnx, dny);
		*/

		/*  Shadow the pixmap with an 8-bit version for readback.
		*/
		if (DBG_TRACE) {
		    fprintf (stderr,
			"updating shadow_pixmap........................0x%x\n",
		        dr->shadow_pixmap);
		    fprintf (stderr,
			"updating %d,%d  to %d,%d  %d %d\n", 
			ox, oy, dx, dy, dnx, dny);
		}
	        XPutImage (display, dr->shadow_pixmap, 
		    w->gterm.expose8GC, xout,
		    ox, oy, dx, dy, dnx, dny);
	    }
	}
    }
    
done:
    /* Clean up.
     */
    if (delxin)  XDestroyImage (xin);
    if (delxout) XDestroyImage (xout);

    if (img1) XDestroyImage (img1);
    if (img2) XDestroyImage (img2);
    if (img3) XDestroyImage (img3);
    if (img4) XDestroyImage (img4);
    if (img5) XDestroyImage (img5);

    XDestroyRegion (clip_region);
    if (clip && dr->type == PixmapRaster)
	XSetClipMask (w->gterm.display, w->gterm.exposeGC, None);

    if (DBG_TRACE)
	fprintf (stderr, "refresh_destination: doing mappings  status=%d\n",
	    status);

    /* Execute any mappings defined on the raster just updated. */
    if (status == OK) {

	GtRefreshPixels (w, dst, GtPixel, dx, dy, dnx, dny);

	if (dst == 0) {
	    Region clip_region = (Region)NULL;
	    XRectangle r;

	    clip_region = XCreateRegion();
	    r.x = dx;  r.y = dy;
	    r.width = dnx;  r.height = dny;
	    XUnionRectWithRegion (&r, clip_region, clip_region);

	    update_transients (w, clip_region);
	    XDestroyRegion (clip_region);
	}
    }

    if (DBG_TRACE)
	fprintf(stderr, "refresh_destination:  LEAVING  status=%d\n", status);

    return (status);
}


/* scale_zoom -- Compute the given destination rect from the input image,
 * using pixel replication and the given x and y dst->scr pixels maps.
 */
static
scale_zoom (idata,ibpl, odata,obpl, xmap,ymap, dx,dy,dnx,dny, clip_region)

    uchar *idata, *odata;		/* input, output data */
    int ibpl, obpl;			/* bytes per line */
    register int *xmap;			/* src coords of each dst pixel */
    int *ymap;				/* src coords of each dst pixel */
    int dx, dy, dnx, dny;		/* destination rect */
    Region clip_region;			/* clip Region or null */
{
    register int i, j;
    register uchar *ip, *op;
    uchar *last_ip = NULL;
    uchar *last_op = NULL;

    for (j=0;  j < dny;  j++) {
	ip = idata + ymap[j] * ibpl;
	op = odata + (j+dy) * obpl + dx;

	if (!clip_region) {
	    if (ip == last_ip)
		memmove (op, last_op, dnx);
	    else {
		for (i=0;  i < dnx;  i++) {
		    op[i] = ip[xmap[i]];
		}
	    }
	    last_ip = ip;
	    last_op = op;
	} else {
	    for (i=0;  i < dnx;  i++)
		if (XPointInRegion (clip_region, i + dx, j + dy))
		    op[i] = ip[xmap[i]];
	}
    }
}


/* scale_intzoom -- Compute the given destination rect from the input image,
 * using pixel replication and integer scaling.  This is functionally
 * equivalent to scale_zoom using the lookup tables, but optimized for the
 * case of integer scaling.
 */
static
scale_intzoom (idata,ibpl,odata,obpl, sx,sy,dx,dy,dnx,dny, xflip,yflip, nx,ny)

    uchar *idata, *odata;		/* input, output data */
    int ibpl, obpl;			/* bytes per line */
    int sx, sy;				/* start coords of src rect */
    int dx, dy, dnx, dny;		/* destination rect */
    int xflip, yflip;			/* set if x or y is flipped */
    int nx, ny;				/* replication factors */
{
    register int n;
    register int pix;
    register uchar *ip, *op;
    uchar *otop, *olast, *lp;
    int i, j, k;

    olast = odata + (dy + dny) * obpl - dnx + dx;

    if (xflip) {
	for (j=0, k=0;  j < dny;  j += ny, k++) {
	    ip = idata + (sy + k) * ibpl + sx;

	    op = odata + (dy + (yflip ? (dny-ny-j) : j)) * obpl + dx + dnx;
	    otop = lp = op - dnx;


	    /* Why are the case statements below necessary, doesn't the
	     * default case do the same thing regardless of what nx is?  MJF
	     */

	    /* Replicate a block of pixels. */
	    switch (nx) {
	    case 2:
		for (n = (dnx/2);  --n >= 0;  ) {
		    pix = *ip++;
		    *--op = pix;  *--op = pix;
		}
		break;
	    case 3:
		for (n = (dnx/3);  --n >= 0;  ) {
		    pix = *ip++;
		    *--op = pix;  *--op = pix;  *--op = pix;
		}
		break;
	    case 4:
		for (n = (dnx/4);  --n >= 0;  ) {
		    pix = *ip++;
		    *--op = pix;  *--op = pix;
		    *--op = pix;  *--op = pix;
		}
		break;
	    case 5:
		for (n = (dnx/5);  --n >= 0;  ) {
		    pix = *ip++;
		    *--op = pix;  *--op = pix;  *--op = pix;
		    *--op = pix;  *--op = pix;
		}
		break;
	    case 6:
		for (n = (dnx/6);  --n >= 0;  ) {
		    pix = *ip++;
		    *--op = pix;  *--op = pix;  *--op = pix;
		    *--op = pix;  *--op = pix;  *--op = pix;
		}
		break;
	    case 7:
		for (n = (dnx/7);  --n >= 0;  ) {
		    pix = *ip++;
		    *--op = pix;  *--op = pix;  *--op = pix;
		    *--op = pix;  *--op = pix;
		    *--op = pix;  *--op = pix;
		}
		break;
	    case 8:
		for (n = (dnx/8);  --n >= 0;  ) {
		    pix = *ip++;
		    *--op = pix;  *--op = pix;
		    *--op = pix;  *--op = pix;
		    *--op = pix;  *--op = pix;
		    *--op = pix;  *--op = pix;
		}
		break;
	    default:
		for (n = (dnx/nx);  --n >= 0;  ) {
		    pix = *ip++;
		    for (i=nx;  --i >= 0;  )
			*--op = pix;
		}
		break;
	    }

	    /* Fill the last partial pixel. */
	    pix = *ip++;
	    while (op > otop)
		*--op = pix;

	    /* Replicate in Y. */
	    for (n=ny, op=lp;  --n > 0;  ) {
		op += obpl;
		if (op <= olast)
		    memmove (op, lp, dnx);
	    }
	}
    } else {
	for (j=0, k=0;  j < dny;  j += ny, k++) {
	    ip = idata + (sy + k) * ibpl + sx;
	    op = lp = odata + (dy + (yflip ? (dny-ny-j) : j)) * obpl + dx;
	    otop = op + dnx;

	    /* Replicate a block of pixels. */
	    switch (nx) {
	    case 2:
		for (n = (dnx/2);  --n >= 0;  ) {
		    pix = *ip++;
		    *op++ = pix;  *op++ = pix;
		}
		break;
	    case 3:
		for (n = (dnx/3);  --n >= 0;  ) {
		    pix = *ip++;
		    *op++ = pix;  *op++ = pix;  *op++ = pix;
		}
		break;
	    case 4:
		for (n = (dnx/4);  --n >= 0;  ) {
		    pix = *ip++;
		    *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;
		}
		break;
	    case 5:
		for (n = (dnx/5);  --n >= 0;  ) {
		    pix = *ip++;
		    *op++ = pix;  *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;
		}
		break;
	    case 6:
		for (n = (dnx/6);  --n >= 0;  ) {
		    pix = *ip++;
		    *op++ = pix;  *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;  *op++ = pix;
		}
		break;
	    case 7:
		for (n = (dnx/7);  --n >= 0;  ) {
		    pix = *ip++;
		    *op++ = pix;  *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;
		}
		break;
	    case 8:
		for (n = (dnx/8);  --n >= 0;  ) {
		    pix = *ip++;
		    *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;
		}
		break;
	    default:
		for (n = (dnx/nx);  --n >= 0;  ) {
		    pix = *ip++;
		    for (i=nx;  --i >= 0;  )
			*op++ = pix;
		}
		break;
	    }

	    /* Fill the last partial pixel. */
	    pix = *ip++;
	    while (op < otop)
		*op++ = pix;

	    /* Replicate in Y. */
	    for (n=ny, op=lp;  --n > 0;  ) {
		op += obpl;
		if (op <= olast)
		    memmove (op, lp, dnx);
	    }
	}
    }
}


/* scale_nearest -- Compute the given destination rect from the input image,
 * using the nearest neighbor technique.
 */
static
scale_nearest (idata,inx,iny,ibpl, odata,onx,ony,obpl,
    x_src,y_src, dx,dy,dnx,dny, clip_region)

    uchar *idata, *odata;		/* input, output data */
    int inx, iny, ibpl;			/* dimensions of input array */
    int onx, ony, obpl;			/* dimensions of output array */
    float *x_src, *y_src;		/* src coords of each dst pixel */
    int dx, dy, dnx, dny;		/* destination rect */
    Region clip_region;			/* clip Region or null */
{
    register int m, n, i, j;
    register uchar *op;

    for (j=0;  j < dny;  j++) {
	op = odata + (j+dy) * obpl + dx;
	n = y_src[j];

	if (!clip_region) {
	    for (i=0;  i < dnx;  i++) {
		m = x_src[i];
		op[i] = idata[n * ibpl + m];
	    }
	} else {
	    for (i=0;  i < dnx;  i++)
		if (XPointInRegion (clip_region, i + dx, j + dy)) {
		    m = x_src[i];
		    op[i] = idata[n * ibpl + m];
		}
	}
    }
}


/* scale_bilinear -- Compute the given destination rect from the input image,
 * using bilinear interpolation.
 */
static
scale_bilinear (idata,inx,iny,ibpl, odata,onx,ony,obpl,
    x_src,y_src, dx,dy,dnx,dny, clip_region)

    uchar *idata, *odata;		/* input, output data */
    int inx, iny, ibpl;			/* dimensions of input array */
    int onx, ony, obpl;			/* dimensions of output array */
    float *x_src, *y_src;		/* src coords of each dst pixel */
    int dx, dy, dnx, dny;		/* destination rect */
    Region clip_region;			/* clip Region or null */
{
    register int i;
    register uchar *op;
    register float *lp, *w1, *w2;
    int buflen, line, *px, pixel, j;
    float lo_w, hi_w, x, y;
    uchar *lo, *hi;
    float *buf;

    buflen = (3 * dnx + 2) * sizeof(float) + dnx * sizeof(int);
    if ((buf = (float *) XtMalloc (buflen)) == NULL)
	return;

    lp = buf + 1;
    w1 = lp + dnx + 1;
    w2 = w1 + dnx;
    px = (int *)(w2 + dnx);

    /* Compute the weight vectors at each point in X. */
    for (i=0;  i < dnx;  i++) {
	x = x_src[i] - 0.5;
	px[i] = (int) x;
	w1[i] = 1.0 - (x - (int)x);
	w2[i] = 1.0 - w1[i];
    }

    /* For each line of the destination rect first interpolate in Y to the
     * y_src coordinate of the output line, then interpolate in X to compute
     * the final output pixels.
     */
    for (j=0;  j < dny;  j++) {
	op = odata + (j+dy) * obpl + dx;
	y = y_src[j] - 0.5;
	line = (int) y;
	lo = idata + line * ibpl;
	hi = idata + min (iny - 1, line + 1) * ibpl;
	lo_w = 1.0 - (y - line);
	hi_w = 1.0 - lo_w;

	/* Interpolate in Y to the line at y_src[j]. */
	for (i=0;  i < dnx;  i++) {
	    pixel = px[i];
	    lp[i] = (float)lo[pixel] * lo_w + (float)hi[pixel] * hi_w;
	}
	lp[-1]  = lp[0];
	lp[dnx] = lp[dnx-1];

	/* Interpolate in X to the final output pixels. */
	if (!clip_region) {
	    for (i=0;  i < dnx;  i++)
		op[i] = lp[i] * w1[i] + lp[i+1] * w2[i];
	} else {
	    for (i=0;  i < dnx;  i++)
		if (XPointInRegion (clip_region, i + dx, j + dy))
		    op[i] = lp[i] * w1[i] + lp[i+1] * w2[i];
	}
    }

    XtFree ((char *)buf);
}


/* scale_lowpass -- Apply a lowpass filter to a region of a 2D data array.
 * The region ox,oy,nx,ny of the output data array is calculated by running
 * a convolution kernel over the region of the input data array at ix,iy.
 * The size of the convolution kernel is adjusted to match the scale factors
 * xscale, yscale.
 */
static
scale_lowpass (idata,inx,iny,ibpl, odata,onx,ony,obpl, x_src,y_src,
    sx,sy,snx,sny, dx,dy,dnx,dny, xscale,yscale, clip_region)

    uchar *idata, *odata;		/* input, output data */
    int inx, iny, ibpl;			/* full input array */
    int onx, ony, obpl;			/* full input array */
    float *x_src, *y_src;		/* src coords of each dst pixel */
    int sx, sy, snx, sny;		/* source rect */
    int dx, dy, dnx, dny;		/* destination rect */
    float xscale, yscale;		/* scale factors */
    Region clip_region;			/* clip Region or null */
{
    uchar *data;
    
    if ((data = (uchar *) XtMalloc (inx * iny)) == NULL)
	return;

    /* Run a lowpass filter over the input rect. */
    lw_convolve (idata,inx,iny,ibpl, sx,sy, data,inx,iny,ibpl, sx,sy,
	snx,sny, xscale,yscale);

    /* Sample the filtered data to generate the output rect. */
    scale_nearest (data,inx,iny,ibpl, odata,onx,ony,obpl, x_src,y_src,
	dx,dy,dnx,dny, clip_region);

    XtFree ((char *)data);
}


/* lw_convolve -- Convolution primitive for scale_lowpass.
 */
static
lw_convolve (idata,inx,iny,ibpl,ix,iy, odata,onx,ony,obpl,ox,oy,
    nx, ny, xscale, yscale)

    uchar *idata, *odata;		/* input, output data */
    int inx, iny, ix, iy;		/* size of input array, start pos */
    int onx, ony, ox, oy;		/* size of output array, start pos */
    int ibpl, obpl;			/* bytes per line */
    int nx, ny;				/* size of output region */
    float xscale, yscale;		/* determines amount of smoothing */
{
    register uchar *ip;
    register int l, m, x, hx, pixval;
    int kx, ky, hy, i, j, y;
    uchar *lp[11], *op;

    /* Determine kernel size (min 3x3, max 7x7). */
    if (xscale < 0.1)
	hx = 3;
    else if (xscale >= 0.5)
	hx = 1;
    else
	hx = ((int)(1.0 / xscale)) / 2;

    if (yscale < 0.1)
	hy = 3;
    else if (yscale >= 0.5)
	hy = 1;
    else
	hy = ((int)(1.0 / yscale)) / 2;

    kx = hx * 2 + 1;
    ky = hy * 2 + 1;

    /* Compute the output data.
     */
    for (j=0;  j < ny;  j++) {
	/* Get line pointers. */
	op = odata + (j+oy) * obpl + ox;
	for (i=0;  i < ky;  i++) {
	    y = iy + j - hy + i;
	    if (y < 0)
		y = 0;
	    else if (y >= iny)
		y = iny - 1;
	    lp[i] = y * ibpl + idata;
	}

	/* Compute a line of output pixels */
	for (i=0;  i < nx;  i++) {
	    x = ix + i;
	    pixval = 0;

	    if (x < hx) {
		/* Near left edge. */
		for (m=0;  m < ky;  m++)
		    for (l=0;  l < kx;  l++)
			pixval += lp[m][max(0,x-hx+l)];
	    } else if (x >= inx - hx) {
		/* Near right edge. */
		for (m=0;  m < ky;  m++)
		    for (l=0;  l < kx;  l++)
			pixval += lp[m][min(inx-1,x-hx+l)];
	    } else {
		/* In central region. */
		for (m=0;  m < ky;  m++) {
		    ip = lp[m] + x - hx;
		    for (l=0;  l < kx;  l++)
			pixval += ip[l];
		}
	    }

	    op[i] = (float)pixval / (float)(kx * ky) + 0.5;
	}
    }
}


/* scale_boxcar -- Apply a boxcar filter to a region of a 2D data array
 * and interpolate the result to the output grid.  The region ox,oy,nx,ny of
 * the output data array is calculated by block averaging the corresponding
 * source rect and then sampling the reduced image using bilinear interpolation
 * to compute the output pixel raster.  This antialiasing technique aims to
 * be as fast as possible but still does a reasonable job of reducing the
 * image.
 */
static
scale_boxcar (idata,inx,iny,ibpl, odata,onx,ony,obpl, x_src,y_src,
    sx,sy,snx,sny, dx,dy,dnx,dny, xscale,yscale, interp, clip_region)

    uchar *idata, *odata;		/* input, output data */
    int inx, iny, ibpl;			/* full input array */
    int onx, ony, obpl;			/* full input array */
    float *x_src, *y_src;		/* src coords of each dst pixel */
    int sx, sy, snx, sny;		/* source rect */
    int dx, dy, dnx, dny;		/* destination rect */
    float xscale, yscale;		/* scale factors */
    int interp;				/* set if interpolation is desired */
    Region clip_region;			/* clip Region or null */
{
    int xblock, yblock;
    int x1, x2, y1, y2, nx, ny;
    float xstep, ystep;
    int xoff, yoff;
    uchar *bp;

    /* Determine blocking factors.   If interpolation is disabled we need
     * to block one step more than for the linear interpolate case in order
     * to ensure that all the data is used.
     */
    xblock = max(1, (int) (1.0 / xscale));
    if (!interp && (1.0 / xscale) - xblock > ZOOM_TOL)
	xblock++;
    yblock = max(1, (int) (1.0 / yscale));
    if (!interp && (1.0 / yscale) - yblock > ZOOM_TOL)
	yblock++;

    /* Align the input region for the given blocking factors. */
    x1 = sx / xblock * xblock;  x2 = (sx + snx - 1) / xblock * xblock;
    y1 = sy / yblock * yblock;  y2 = (sy + sny - 1) / yblock * yblock;
    nx = (x2 - x1) / xblock + 1;  ny = (y2 - y1) / yblock + 1;

    /* Compute the block averaged input rect.  */
    if (xblock > 1 || yblock > 1) {
	if ((bp = (uchar *) XtMalloc (nx * ny)) == NULL)
	    return;
	bx_boxcar (idata,inx,iny,ibpl, x1,y1,x2,y2, bp, xblock, yblock);
	idata = bp;
	inx = ibpl = nx;
	iny = ny;
	xoff = x1;  yoff = y1;
	xstep = 1.0 / xblock;  ystep = 1.0 / yblock;
    } else {
	bp = NULL;
	xoff = yoff = 0;
	xstep = ystep = 1.0;
    }

    /* Interpolate the input rect to the output grid. */
    if (interp &&
	((1.0 / xscale) - xblock) > ZOOM_TOL ||
	((1.0 / yscale) - yblock) > ZOOM_TOL) {

	/* Use bilinear interpolation to compute the output grid. */
	bx_interp (idata,inx,iny,ibpl, odata,onx,ony,obpl,
	    x_src,y_src, xoff,yoff,xstep,ystep, dx,dy,dnx,dny, clip_region);

    } else {
	/* Extract pixels from block averaged input data. */
	bx_extract (idata,inx,iny,ibpl, odata,onx,ony,obpl,
	    x_src,y_src, xoff,yoff,xstep,ystep, dx,dy,dnx,dny, clip_region);
    }

    if (bp)
	XtFree ((char *)bp);
}


/* bx_boxcar -- Block average primitive for scale_boxcar.
 */
static
bx_boxcar (idata,inx,iny,ibpl, x1,y1,x2,y2, obuf, xblock, yblock)
    uchar *idata;			/* input data array */
    int inx, iny, ibpl;			/* array dimensions */
    int x1,y1,x2,y2;			/* region to be block averaged */
    uchar *obuf;			/* output array */
    int xblock, yblock;			/* blocking factors */
{
    register uchar *ip, *op;
    register int count, i, *sp;
    int obpl, block, nxblocks, nyblocks, j, k;
    uchar *lp, *bp;
    int *sb;

    nxblocks = obpl = (x2 - x1) / xblock + 1;
    nyblocks = (y2 - y1) / yblock + 1;
    count = xblock * yblock;

    if ((sb = (int *) XtMalloc (obpl * sizeof(int))) == NULL)
	return;

    /* I don't think the following works for pixel values allocated from the
     * default colormap, as the pixel values are not sequentially allocated.
     */
    for (block = 0;  block < nyblocks;  block++) {
	lp = idata + ibpl * (block * yblock + y1) + x1;
	bp = obuf + block * obpl;

	memset (sb, 0, obpl * sizeof(int));
	for (k=yblock;  --k >= 0;  lp += ibpl)
	    for (j=nxblocks, ip=lp, sp=sb;  --j >= 0;  sp++)
		for (i=xblock;  --i >= 0;  )
		    *sp += *ip++;

	for (i=obpl, sp=sb, op=bp;  --i >= 0;  )
	    *op++ = *sp++ / count;
    }

    XtFree ((char *)sb);
}


/* bx_extract -- Block extract primitive for scale_boxcar.
 */
static
bx_extract (idata,inx,iny,ibpl, odata,onx,ony,obpl,
    x_src,y_src, xoff,yoff,xstep,ystep, dx,dy,dnx,dny, clip_region)

    uchar *idata, *odata;		/* input, output data */
    int inx, iny, ibpl;			/* full input array */
    int onx, ony, obpl;			/* full input array */
    float *x_src, *y_src;		/* src coords of each dst pixel */
    int dx, dy, dnx, dny;		/* destination rect */
    int xoff, yoff;			/* offset of input region */
    float xstep, ystep;			/* scale of input region */
    Region clip_region;			/* clip Region or null */
{
    register int m, n, i;
    register uchar *op;
    int j;

    for (j=0;  j < dny;  j++) {
	op = odata + (j+dy) * obpl + dx;
	n = (y_src[j] - yoff) * ystep;

	if (!clip_region) {
	    for (i=0;  i < dnx;  i++) {
		m = (x_src[i] - xoff) * xstep;
		op[i] = idata[n * ibpl + m];
	    }
	} else {
	    for (i=0;  i < dnx;  i++)
		if (XPointInRegion (clip_region, i + dx, j + dy)) {
		    m = (x_src[i] - xoff) * xstep;
		    op[i] = idata[n * ibpl + m];
		}
	}
    }
}


/* bx_interp -- Bilinear interpolation primitive for scale_boxcar.
 */
static
bx_interp (idata,inx,iny,ibpl, odata,onx,ony,obpl,
    x_src,y_src, xoff,yoff,xstep,ystep, dx,dy,dnx,dny, clip_region)

    uchar *idata, *odata;		/* input, output data */
    int inx, iny, ibpl;			/* dimensions of input array */
    int onx, ony, obpl;			/* dimensions of output array */
    float *x_src, *y_src;		/* src coords of each dst pixel */
    int xoff, yoff;			/* offset of input region */
    float xstep, ystep;			/* scale of input region */
    int dx, dy, dnx, dny;		/* destination rect */
    Region clip_region;			/* clip Region or null */
{
    register int i;
    register uchar *op;
    register float *lp, *w1, *w2;
    int buflen, line, *px, pixel, j;
    float lo_w, hi_w, x, y;
    uchar *lo, *hi;
    float *buf;

    buflen = (3 * dnx + 2) * sizeof(float) + dnx * sizeof(int);
    if ((buf = (float *) XtMalloc (buflen)) == NULL)
	return;

    lp = buf + 1;
    w1 = lp + dnx + 1;
    w2 = w1 + dnx;
    px = (int *)(w2 + dnx);

    /* Compute the weight vectors at each point in X. */
    for (i=0;  i < dnx;  i++) {
	x = ((x_src[i] - xoff) * xstep) - 0.5;
	px[i] = (int) x;
	w1[i] = 1.0 - (x - (int)x);
	w2[i] = 1.0 - w1[i];
    }

    /* For each line of the destination rect first interpolate in Y to the
     * y_src coordinate of the output line, then interpolate in X to compute
     * the final output pixels.
     */
    for (j=0;  j < dny;  j++) {
	op = odata + (j+dy) * obpl + dx;
	y = ((y_src[j] - yoff) * ystep) - 0.5;
	line = (int) y;
	lo = idata + line * ibpl;
	hi = idata + min (iny - 1, line + 1) * ibpl;
	lo_w = 1.0 - (y - line);
	hi_w = 1.0 - lo_w;

	/* Interpolate in Y to the line at y_src[j]. */
	for (i=0;  i < dnx;  i++) {
	    pixel = px[i];
	    lp[i] = (float)lo[pixel] * lo_w + (float)hi[pixel] * hi_w;
	}
	lp[-1]  = lp[0];
	lp[dnx] = lp[dnx-1];

	/* Interpolate in X to the final output pixels. */
	if (!clip_region) {
	    for (i=0;  i < dnx;  i++)
		op[i] = lp[i] * w1[i] + lp[i+1] * w2[i];
	} else {
	    for (i=0;  i < dnx;  i++)
		if (XPointInRegion (clip_region, i + dx, j + dy))
		    op[i] = lp[i] * w1[i] + lp[i+1] * w2[i];
	}
    }

    XtFree ((char *)buf);
}


/* mf_getinten -- Copy the source rect to the destination rect, converting
 * pixel numbers to pixel intensities.
 */
static
mf_getinten (w, idata,inx,iny,ibpl, sx,sy, odata,onx,ony,obpl, dx,dy, nx,ny)

    GtermWidget w;
    uchar *idata, *odata;		/* input, output data */
    int inx, iny, ibpl;			/* dimensions of input array */
    int onx, ony, obpl;			/* dimensions of output array */
    int sx, sy;				/* source offset */
    int dx, dy;				/* destination offset */
    int nx, ny;				/* size of region */
{
    register Pixel *cmap;
    register uchar *ip, *op;
    register int n;
    int j;

    cmap = get_cmap_out (w);
    for (j=0;  j < ny;  j++) {
	ip = idata + ibpl * (sy + j) + sx;
	op = odata + obpl * (dy + j) + dx;
	for (n = nx;  --n >= 0;  )
	    *op++ = (cmap[*ip++]);
    }
}


/* mf_getpixel -- Copy the source rect to the destination rect, converting
 * pixel intensities to pixel numbers.
 */
static
mf_getpixel (w, idata,inx,iny,ibpl, sx,sy, odata,onx,ony,obpl, dx,dy, nx,ny)

    GtermWidget w;
    uchar *idata, *odata;		/* input, output data */
    int inx, iny, ibpl;			/* dimensions of input array */
    int onx, ony, obpl;			/* dimensions of output array */
    int sx, sy;				/* source offset */
    int dx, dy;				/* destination offset */
    int nx, ny;				/* size of region */
{
    register Pixel *cmap;
    register uchar *ip, *op;
    register int n;
    int j;

    cmap = get_cmap_in (w);
    for (j=0;  j < ny;  j++) {
	ip = idata + ibpl * (sy + j) + sx;
	op = odata + obpl * (dy + j) + dx;
	for (n = nx;  --n >= 0;  )
	    *op++ = (cmap[*ip++]);
    }
}


/* get_regions -- For each pixel I in the sequence of DNX pixels starting at DX
 * there is an associated value XMAP[I].  Compare this sequence to an alternate
 * sequence and return a list of subregions {XS,XE,XV} for which the XMAP
 * values are equal (XV=0), not equal (XV=1), or not common to (XV=2) the two
 * regions.  The number of regions output is returned as the function value.
 */
static
get_regions (xs,xe,xv, max_regions, dx, dnx, xmap, alt_dx, alt_dnx, alt_xmap)
    int *xs, *xe, *xv, max_regions;
    int dx, dnx, *xmap;
    int alt_dx, alt_dnx, *alt_xmap;
{
    register int state, current;
    register int nx, i;
    int offset, old_i;

    offset = dx - alt_dx;
    nx = 0;

    for (i=0;  i < dnx;  i++) {
	if (nx >= max_regions-1)
	    return (0);

	/* Determine whether or not this pixel is mapped the same in both
	 * sequences.
	 */
	old_i = i + offset;
	if (alt_dnx <= 0 || old_i < 0 || old_i >= alt_dnx)
	    state = 2;
	else
	    state = (xmap[i] != alt_xmap[old_i]);

	/* When the state boundary is crossed add a range. */
	if (i == 0) {
	    xs[nx] = dx;
	    xv[nx] = current = state;
	}
	if (state != current) {
	    xe[nx] = dx + i - 1;
	    xs[++nx] = dx + i;
	    xv[nx] = current = state;
	}
	if (i == dnx-1)
	    xe[nx++] = dx + i;
    }

    return (nx);
}


/* get_rects -- Combine two lists of regions, one in X and the other in Y,
 * to produce a list of 2D rectangles defining the regions outlined by the
 * region lists.  Only rects for which the given condition is true in either
 * X or Y are selected.  Adjacent rects are combined.
 */
static
get_rects (o_rl, max_rects, xs,xe,xv,nx, ys,ye,yv,ny, xcond,ycond)
    XRectangle *o_rl;		/* receives list of rectangles */
    int max_rects;		/* max rectangles out */
    int *xs, *xe, *xv, nx;	/* X list of regions */
    int *ys, *ye, *yv, ny;	/* Y list of regions */
    int xcond, ycond;		/* X,Y condition bitflags */
{
    register int i, j;
    XRectangle rl[MAX_REGIONS];
    int limit = min (max_rects, MAX_REGIONS);
    int o_nrects=0, nrects=0;
    int i1, i2, j1, j2;

    /* Get initial list of rects matching the given X and Y conditions.
     * Rects which are adjacent in X are combined to form one larger rect.
     */
    for (j=0;  j < ny;  j++) {
	for (i=0;  i < nx;  i++) {
	    if ((xv[i] & xcond) || (yv[j] & ycond)) {
		/* Collapse rects adjacent in X. */
		for (i1 = i2 = i++;  i < nx;  i++) {
		    if ((xv[i] & xcond) || (yv[j] & ycond))
			i2 = i;
		    else
			break;
		}

		rl[nrects].x = xs[i1];
		rl[nrects].y = ys[j];
		rl[nrects].width = xe[i2] - xs[i1] + 1;
		rl[nrects].height = ye[j] - ys[j] + 1;

		if (++nrects >= limit)
		    return (nrects);
	    }
	}
    }

    /* Now scan the rect list and collapse rects which are adjacent in Y
     * into one larger rect.  Find all the rects that are at the same X
     * and write them to the output list, collapsing rects that are adjacent
     * in Y in the process.
     */
    for (i=0;  i < nx;  i++)
	for (j=0;  j < nrects;  ) {
	    /* Find first rect if any. */
	    for (j1=0, j2 = -1;  j < nrects;  j++)
		if (rl[j].x == xs[i]) {
		    j1 = j2 = j++;
		    break;
		}
	    
	    /* Collapse rects adjacent in Y. */
	    for (   ;  j < nrects;  j++)
		if (rl[j].x == xs[i])
		    if (rl[j].y == rl[j2].y + rl[j2].height &&
			    rl[j].width == rl[j1].width)
			j2 = j;
		    else
			break;
	    
	    /* Output the rect. */
	    if (j2 >= j1) {
		o_rl[o_nrects].x = rl[j1].x;
		o_rl[o_nrects].y = rl[j1].y;
		o_rl[o_nrects].width = rl[j1].width;
		o_rl[o_nrects].height = rl[j2].y + rl[j2].height - rl[j1].y;

		if (++o_nrects >= max_rects)
		    return (o_nrects);
	    }
	}

    return (o_nrects);
}


/* rect_intersect -- Compute the intersection of two rects.  The area of
 * the intersection is returned as the function value, i.e., zero is
 * returned if the rects do not intersect.
 */
static
rect_intersect (in, r1, r2)
    register XRectangle *in;
    register XRectangle *r1, *r2;
{
    int x1, y1, x2, y2;

    x1 = max (r1->x, r2->x);
    y1 = max (r1->y, r2->y);
    x2 = min ((int)r1->x + (int)r1->width, (int)r2->x + (int)r2->width) - 1;
    y2 = min ((int)r1->y + (int)r1->height, (int)r2->y + (int)r2->height) - 1;

    in->x = x1;
    in->y = y1;
    in->width = max (0, x2 - x1 + 1);
    in->height = max (0, y2 - y1 + 1);

    return (in->width * in->height);
}


/* save_mapping -- Store a mapping in a mapping descriptor.
 */
static
save_mapping (mp, mapping, rop, src, st, sx,sy,sw,sh, dst, dt, dx,dy,dw,dh)
    register Mapping mp;
    int mapping, rop;
    int src, st, sx,sy,sw,sh;
    int dst, dt, dx,dy,dw,dh;
{
    mp->src = src;  mp->st = st;
	mp->sx = sx; mp->sy = sy; mp->snx = sw; mp->sny = sh;
    mp->dst = dst;  mp->dt = dt;
	mp->dx = dx; mp->dy = dy; mp->dnx = dw; mp->dny = dh;
    mp->defined = mp->enabled = mp->refresh = 1;
    mp->mapping = mapping;
    mp->rop = rop;
}

/* load_mapping -- Load a mapping from a mapping descriptor.
 */
static
load_mapping (mp, mapping, rop, src, st, sx,sy,sw,sh, dst, dt, dx,dy,dw,dh)
    register Mapping mp;
    int *mapping, *rop;
    int *src, *st, *sx,*sy,*sw,*sh;
    int *dst, *dt, *dx,*dy,*dw,*dh;
{
    *src = mp->src;  *st = mp->st;
	*sx = mp->sx; *sy = mp->sy; *sw = mp->snx; *sh = mp->sny;
    *dst = mp->dst;  *dt = mp->dt;
	*dx = mp->dx; *dy = mp->dy; *dw = mp->dnx; *dh = mp->dny;
    *mapping = mp->mapping;
    *rop = mp->rop;
}


/* get_pixel_mapping -- Copy a mapping, converting to pixel coordinates in
 * the process if the mapping is not already in pixel coordinates.
 */
static
get_pixel_mapping (w, mp1, mp2, update)
    GtermWidget w;
    register Mapping mp1;		/* input mapping */
    register Mapping mp2;		/* output mapping */
    int update;				/* update mapping */
{
    float maxndc = (float)MAXNDC;

    mp2->mapping = mp1->mapping;
    mp2->refresh = mp1->refresh;
    mp2->enabled = mp1->enabled;
    mp2->rop = mp1->rop;

    if (!(mp2->defined = mp1->defined))
	return;

    mp2->src = mp1->src;
    if (mp1->st == GtPixel) {
	mp2->st = mp1->st;
	mp2->sx = mp1->sx;	mp2->sy = mp1->sy;
	mp2->snx = mp1->snx;	mp2->sny = mp1->sny;
    } else {
	float width  = w->gterm.rasters[mp1->src].width;
	float height = w->gterm.rasters[mp1->src].height;
	mp2->sx = mp1->sx * width / maxndc;
	mp2->sy = (MAXNDC - (mp1->sy + abs(mp1->sny))) * height / maxndc;
	mp2->snx = mp1->snx * width / maxndc;
	mp2->sny = mp1->sny * height / maxndc;		/* NDC flipped in Y */
	mp2->st = GtPixel;
    }

    mp2->dst = mp1->dst;
    if (mp1->dt == GtPixel) {
	mp2->dt = mp1->dt;
	mp2->dx = mp1->dx;	mp2->dy = mp1->dy;
	mp2->dnx = mp1->dnx;	mp2->dny = mp1->dny;
    } else {
	float width  = w->gterm.rasters[mp1->dst].width;
	float height = w->gterm.rasters[mp1->dst].height;
	mp2->dx = mp1->dx * width / maxndc;
	mp2->dy = (MAXNDC - (mp1->dy + abs(mp1->dny))) * height / maxndc;
	mp2->dnx = mp1->dnx * width / maxndc;
	mp2->dny = mp1->dny * -height / maxndc;		/* NDC flipped in Y */
	mp2->dt = GtPixel;
    }

    /* The lookup tables are already in pixel space, so we can propagate
     * these to the new mapping if the old mapping was updated.
     */
    if (update && mp1->updated) {
	if (mp2->mapdata = (uchar *) XtMalloc (mp1->datalen)) {

	    memmove (mp2->mapdata, mp1->mapdata, mp1->datalen);
	    mp2->datalen = mp1->datalen;
	    mp2->scaling = mp1->scaling;
	    mp2->xscale = mp1->xscale;
	    mp2->yscale = mp1->yscale;

	    mp2->x_extent = (mapExtent *)
		((uchar *)mp1->x_extent - mp1->mapdata + mp2->mapdata);
	    mp2->y_extent = (mapExtent *)
		((uchar *)mp1->y_extent - mp1->mapdata + mp2->mapdata);
	    mp2->x_srcpix = (int *)
		((uchar *)mp1->x_srcpix - mp1->mapdata + mp2->mapdata);
	    mp2->y_srcpix = (int *)
		((uchar *)mp1->y_srcpix - mp1->mapdata + mp2->mapdata);
	    mp2->x_src = (float *)
		((uchar *)mp1->x_src - mp1->mapdata + mp2->mapdata);
	    mp2->y_src = (float *)
		((uchar *)mp1->y_src - mp1->mapdata + mp2->mapdata);

	    mp2->updated = 1;
	}
    } else
	mp2->updated = 0;
}

/* valid_mapping -- Perform some sanity checks on a mapping to verify that
 * it contains something meaningful.
 */
static
valid_mapping (w, mp)
    GtermWidget w;
    register Mapping mp;
{
    register int x, y;
    int snx, sny, dnx, dny;
    int s_width, s_height, d_width, d_height;
    Raster sr, dr;

    if (!mp || !mp->defined)
	return (False);

    if (mp->src < 0 || mp->src >= w->gterm.maxRasters)
	return (False);
    if (mp->dst < 0 || mp->dst >= w->gterm.maxRasters)
	return (False);

    sr = &w->gterm.rasters[mp->src];
    dr = &w->gterm.rasters[mp->dst];
    if (!sr->type || !dr->type)
	return (False);

    switch (mp->st) {
    case GtPixel:
	s_width = sr->width;  s_height = sr->height;
	break;
    case GtNDC:
	s_width = MAXNDC + 1;  s_height = MAXNDC + 1;
	break;
    default:
	return (False);
    }

    switch (mp->dt) {
    case GtPixel:
	d_width = dr->width;  d_height = dr->height;
	break;
    case GtNDC:
	d_width = MAXNDC + 1;  d_height = MAXNDC + 1;
	break;
    default:
	return (False);
    }

    snx = mp->snx;  dnx = abs(mp->dnx);
    sny = mp->sny;  dny = abs(mp->dny);
    if (snx <= 0 || dnx <= 0 || sny <= 0 || dny <= 0)
	return (False);

    x = mp->sx;  y = mp->sy;
    if (x < 0 || x >= s_width || y < 0 || y >= s_height)
	return (False);
    x = mp->sx + snx - 1;  y = mp->sy + sny - 1;
    if (x < 0 || x >= s_width || y < 0 || y >= s_height)
	return (False);

    x = mp->dx;  y = mp->dy;
    if (x < 0 || x >= d_width || y < 0 || y >= d_height)
	return (False);
    x = mp->dx + dnx - 1;  y = mp->dy + dny - 1;
    if (x < 0 || x >= d_width || y < 0 || y >= d_height)
	return (False);

    return (True);
}


/* initialize_mapping -- Initialize the contents of a mapping descriptor.
 */
static
initialize_mapping (mp)
    register Mapping mp;
{
    memset ((char *)mp, 0, sizeof(struct mapping));
}


/* update_mapping -- Update the portion of a mapping descriptor used at
 * runtime to execute the mapping.  This information consists of several
 * lookup tables and other parameters describing how a destination pixel
 * maps back to a source pixel and vice versa.
 */
static
update_mapping (w, mp)
    GtermWidget w;
    register Mapping mp;
{
    register uchar *op;
    register int i, j, k;
    int snx, sny, dnx, dny, sx, sy, dx, dy;
    int xmax, ymax, lo, hi, edge1, edge2;
    int temp, xflip=0, yflip=0;
    struct mapping p_mp;
    float pixwidth, *fp;
    int *ip;

    if (mp->updated)
	return;

    /* The internal lookup tables are in pixel units. */
    initialize_mapping (&p_mp);
    get_pixel_mapping (w, mp, &p_mp, 0);

    if ((snx = p_mp.snx) <= 0 || (sny = p_mp.sny) <= 0)
	return;

    if ((dnx = p_mp.dnx) < 0) {
	dnx = -dnx;
	xflip++;
    }
    if ((dny = p_mp.dny) < 0) {
	dny = -dny;
	yflip++;
    }

    sx   = p_mp.sx;
    sy   = p_mp.sy;
    dx   = p_mp.dx;
    dy   = p_mp.dy;
    xmax = dnx - 1;
    ymax = dny - 1;

    /* Discard the temporary mapping.
    free_mapping (w, &p_mp);
     */

    /* Get scale factors. */
    mp->xscale = (float)dnx / (float)snx;
    mp->yscale = (float)dny / (float)sny;

    /* Determine type of scaling to be used. */
    if (mp->xscale < 1.0 || mp->yscale < 1.0) {
	mp->scaling = M_DEZOOM;
    } else if (mp->xscale > 1.0 || mp->yscale > 1.0) {
	mp->scaling = M_ZOOM;
	if (abs(mp->xscale - (int)(mp->xscale+0.5)) < ZOOM_TOL &&
	    abs(mp->yscale - (int)(mp->yscale+0.5)) < ZOOM_TOL)
	    mp->scaling = M_INTZOOM;
    } else
	mp->scaling = (xflip || yflip) ? M_ZOOM : M_NOSCALING;

    /* Get a data buffer for the lookup tables. */
    mp->datalen =
	snx * sizeof(mapExtent) +		/* xy, extent */
	sny * sizeof(mapExtent) +
	dnx * sizeof(int) +			/* xy, srcpix */
	dny * sizeof(int) +
	dnx * sizeof(float) +			/* xy, src */
	dny * sizeof(float);

    if (mp->mapdata)
	mp->mapdata = (uchar *) XtRealloc ((char *)mp->mapdata, mp->datalen);
    else
	mp->mapdata = (uchar *) XtMalloc (mp->datalen);
    if (mp->mapdata == NULL)
	return;

    /* Set the table pointers. */
    op = mp->mapdata;
    mp->x_extent = (mapExtent *) op;	op += snx * sizeof(mapExtent);
    mp->y_extent = (mapExtent *) op;	op += sny * sizeof(mapExtent);
    mp->x_srcpix = (int *) op;		op += dnx * sizeof(int);
    mp->y_srcpix = (int *) op;		op += dny * sizeof(int);
    mp->x_src    = (float *) op;	op += dnx * sizeof(float);
    mp->y_src    = (float *) op;	op += dny * sizeof(float);

    /* Compute the backpointers to the source raster for each destination
     * pixel center.
     */
    for (i=0, ip = mp->x_srcpix, fp = mp->x_src;  i < dnx;  i++) {
	fp[i] = ((xflip ? xmax - i : i) + 0.5) / mp->xscale + sx;
	ip[i] = fp[i];
    }
    for (i=0, ip = mp->y_srcpix, fp = mp->y_src;  i < dny;  i++) {
	fp[i] = ((yflip ? ymax - i : i) + 0.5) / mp->yscale + sy;
	ip[i] = fp[i];
    }

    /* Compute the extent arrays.  These define the range of destination
     * pixels affected by each source pixel.
     */
    lo = dnx - 1 + dx;
    hi = dx;
    for (i=0;  i < snx;  i++) {
	mp->x_extent[i].lo = lo;
	mp->x_extent[i].hi = hi;
    }
    lo = dny - 1 + dy;
    hi = dy;
    for (i=0;  i < sny;  i++) {
	mp->y_extent[i].lo = lo;
	mp->y_extent[i].hi = hi;
    }

    /* Map the left and right or top and bottom edges of each destination
     * pixel back into the source rect and update the corresponding extent
     * entries to indicate that these source pixels are used to compute the
     * destination pixel.
     */
    pixwidth = 1.0 - ZOOM_TOL;

    for (i=0;  i < dnx;  i++) {
	edge1 = (xflip ? xmax - i : i) / mp->xscale;
	edge2 = (xflip ? xmax - (i-pixwidth) : (i+pixwidth)) / mp->xscale;
	if (edge1 > edge2) {
	    temp = edge1;  edge1 = edge2;  edge2 = temp;
	}
	edge1 = max (0, edge1);
	edge2 = min (snx - 1, edge2);

	for (j=edge1, k = dx + i;  j <= edge2;  j++) {
	    if (mp->x_extent[j].lo > k)
		mp->x_extent[j].lo = k;
	    if (mp->x_extent[j].hi < k)
		mp->x_extent[j].hi = k;
	}
    }

    for (i=0;  i < dny;  i++) {
	edge1 = (yflip ? ymax - i : i) / mp->yscale;
	edge2 = (yflip ? ymax - (i-pixwidth) : (i+pixwidth)) / mp->yscale;
	if (edge1 > edge2) {
	    temp = edge1;  edge1 = edge2;  edge2 = temp;
	}
	edge1 = max (0, edge1);
	edge2 = min (sny - 1, edge2);

	for (j=edge1, k = dy + i;  j <= edge2;  j++) {
	    if (mp->y_extent[j].lo > k)
		mp->y_extent[j].lo = k;
	    if (mp->y_extent[j].hi < k)
		mp->y_extent[j].hi = k;
	}
    }

    mp->updated = 1;
}


/* free_mapping -- Free any storage used internally by a mapping descriptor,
 * and deactivate the mapping.
 */
static
free_mapping (w, mp)
    GtermWidget w;
    register Mapping mp;
{
    mp_unlink (w, mp);
    mp->defined = mp->enabled = mp->updated = 0;
    if (mp->mapdata) {
	XtFree ((char *) mp->mapdata);
	mp->mapdata = NULL;
	mp->datalen = 0;
	mp->x_extent = mp->y_extent = NULL;
	mp->x_srcpix = mp->y_srcpix = NULL;
	mp->x_src = mp->y_src = NULL;
	mp->updated = 0;
    }
}

static void
mp_linkafter (w, mp, ref_mp)
    register GtermWidget w;
    register Mapping mp;
    register Mapping ref_mp;
{
    register Mapping map;

    /* Don't use the reference mapping unless it is already linked or
     * the list is empty.
     */
    if (w->gterm.mp_head) {
	for (map = w->gterm.mp_head;  map && map != ref_mp;  map = map->next)
	    ;
	if (map != ref_mp)
	    ref_mp = NULL;
    }

    mp->prev = ref_mp;
    mp->next = ref_mp ? ref_mp->next : NULL;
    if (ref_mp && ref_mp->next)
	ref_mp->next->prev = mp;
    if (ref_mp)
	ref_mp->next = mp;

    if (!w->gterm.mp_tail || ref_mp == w->gterm.mp_tail)
	w->gterm.mp_tail = mp;
    if (!w->gterm.mp_head)
	w->gterm.mp_head = mp;
}


static void
mp_unlink (w, mp)
    register GtermWidget w;
    register Mapping mp;
{
    if (mp->prev)
	mp->prev->next = mp->next;
    if (mp->next)
	mp->next->prev = mp->prev;
    if (w->gterm.mp_head == mp)
	w->gterm.mp_head = mp->next;
    if (w->gterm.mp_tail == mp)
	w->gterm.mp_tail = mp->prev;

    mp->prev = mp->next = NULL;
}



