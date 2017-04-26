
/*
 * IMAGING routines.
 * -----------------------
 * Our strategy here is to support a range of visuals with pseudocolor being
 * preferred if available.  All imaging is done internally using 8 bit images
 * and a max 256 element colormap.  If the display hardware has a depth less
 * than 8 bits, e.g. for a monochrome display, the image is reduced to the
 * screen depth by some technique before being output to the display.
 * 
 * Images (rasters) are implemented internally in Gterm using either ximages or
 * off screen pixmaps.  Which format is used is decided at raster create time
 * and is controlled by a Gterm resource.  This is transparent to the client
 * application.  Currently only 8 bit rasters are supported.
 *
 *	        GtRasterInit (gt)
 *	      GtAssignRaster (gt, raster, drawable)
 *	      GtCreateRaster (gt, raster, type, width, height, depth)
 *	     GtDestroyRaster (gt, raster)
 *    exists = GtQueryRaster (gt, raster, &type, &width, &height, &depth)
 *     raster = GtNextRaster (gt)
 *	         GtSetRaster (gt, raster)
 *      raster = GtGetRaster (gt)
 *	      n = GtNRasters (gt)
 *
 *	       GtWritePixels (gt, raster, pixels, nbits, x1, y1, nx, ny)
 *	        GtReadPixels (gt, raster, pixels, nbits, x1, y1, nx, ny)
 *		 GtSetPixels (gt, raster, ct, x1, y1, nx, ny, color, rop)
 *	     GtRefreshPixels (gt, raster, ct, x1, y1, nx, ny)
 *  pixmap = GtExtractPixmap (gt, src, ct, x, y, width, height)
 *	      GtInsertPixmap (gt, pixmap, dst, ct, x, y, width, height)
 *
 * colormap = GtNextColormap (gt)
 *            GtFreeColormap (gt, colormap)
 *	     GtWriteColormap (gt, colormap, first, nelem, r, g, b)
 *	      GtReadColormap (gt, colormap, first, nelem, r, g, b)
 *	      GtLoadColormap (gt, colormap, offset, scale)
 *  exists = GtQueryColormap (gt, colormap, &first, &nelem, &maxelem)
 *	        GtWriteIomap (gt, iomap, first, nelem)
 *	         GtReadIomap (gt, iomap, first, nelem)
 *	           GtReadLUT (gt, lut, first, nelem)
 *  pixel = GtGetClientPixel (gt, gterm_pixel)
 *
 *	      GtInitMappings (gt)
 *   mapping = GtNextMapping (gt)
 *             GtFreeMapping (gt, mapping)
 *	      GtRaiseMapping (gt, mapping, ref|NULL)
 *	      GtLowerMapping (gt, mapping, ref|NULL)
 *   int = GtCompareMappings (gt, map1, map2)
 *	     GtEnableMapping (gt, mapping, refresh)
 *          GtDisableMapping (gt, mapping, erase)
 *  active = GtActiveMapping (gt, mapping)
 *	    GtRefreshMapping (gt, mapping)
 *   raster = GtSelectRaster (gt, dras, dt, dx, dy, rt, &rx, &ry, &mapping)
 *	  GtSetDisplayRaster (gt, mapping)
 *
 *	        GtCopyRaster (gt, rop,
 *				  src,st,sx,sy,snx,sny, dst,dt,dx,dy,dnx,dny)
 *	        GtSetMapping (gt, mapping, rop,
 *				  src,st,sx,sy,snx,sny, dst,dt,dx,dy,dnx,dny)
 *	        GtGetMapping (gt, mapping, rop,
 *				  src,st,sx,sy,snx,sny, dst,dt,dx,dy,dnx,dny)
 *
 *               GtMapVector (gt, mapping, dir, pv1, pv2, npts)
 *              GtPixelToNDC (gt, raster, pv1, pv2, npts)
 *              GtNDCToPixel (gt, raster, pv1, pv2, npts)
 *
 *		     GtDebug (gt, fp, what)
 *
 * In the case of CopyRaster or {Set|Get}Mapping, raster coordinates may be
 * specified in either raster pixel (GtPixel) units or in normalized device
 * coordinates (GtNDC) in the range 0-32767.
 * ---------------------------------------------------------------------------
 */

GtRasterInit (w)
    GtermWidget w;
{
    register int i;
    register Raster rp;
    register struct colormap *cm;
    struct colormap *next_cm;


    invalidate_draw_context (w);

    /* Destroy any existing rasters. */
    if (w->gterm.rasters) {
	for (i=1; i < w->gterm.maxRasters; i++) {
	    if (w->gterm.rasters[i].type)
		GtDestroyRaster (w, i);
	}
    }

    /* Allocate the initially empty raster descriptors. */
    XtFree ((char *)w->gterm.rasters);
    w->gterm.rasters = rp =
	(Raster) XtCalloc (w->gterm.maxRasters, sizeof (struct raster));
    w->gterm.nrasters = 0;
    w->gterm.raster   = 0;

    /* Raster 0 is the display window.
    */
    if (DBG_TRACE)
	fprintf (stderr,
	    "GtRasterInit:  Init display PixmapRaster (%dx%d) depth=%d\n",
		w->core.width, w->core.height, w->core.depth);

    rp->type      = PixmapRaster;
    rp->width     = w->core.width;
    rp->height    = w->core.height;
    rp->depth     = w->core.depth;
    rp->r.pixmap  = w->gterm.window;
    rp->delete    = 0;
    rp->shadow_pixmap = XCreatePixmap (w->gterm.display, w->gterm.window,
	    w->core.width, w->core.height, RasterDepth);


    /* Free any previously allocated colormap cells. */
    if (! w->gterm.useGlobalCmap) {
        if (w->gterm.ncolors > SZ_STATIC_CMAP && w->gterm.useDefaultCM) {
	    XFreeColors (w->gterm.display, w->core.colormap,
	        &w->gterm.cmap[SZ_STATIC_CMAP], w->gterm.ncolors-SZ_STATIC_CMAP,
	        0);
	    w->gterm.ncolors = SZ_STATIC_CMAP;
	    invalidate_cmap (w);
        }
    }

    /* Free any client defined colormaps. */
    for (cm = w->gterm.colormaps;  cm;  cm = next_cm) {
	next_cm = cm->next;
	XtFree ((char *)cm);
    }
    w->gterm.colormaps = NULL;

    /* Initialize the mappings. */
    GtInitMappings (w);

    if (DBG_TRACE)
	fprintf (stderr, "GtRasterInit:  After Init Mappings...Returning\n");
}


initialize_shadow_pixmap (GtermWidget w, int dst)
{
    Raster rp =  (Raster) NULL;

    if (w->gterm.rasters) {
        rp = &w->gterm.rasters[dst];

        if (dst == 0 || rp->type == PixmapRaster) {
            XFillRectangle (w->gterm.display, rp->shadow_pixmap,
                w->gterm.clear8GC, 0, 0, rp->width, rp->height);
        }
    }
}


/* GtNextRaster -- Return the index of the next unused raster.
 */
GtNextRaster (w)
    register GtermWidget w;
{
    register int i;

    if (w->gterm.rasters)
        for (i=1;  i < w->gterm.maxRasters;  i++)
            if (!w->gterm.rasters[i].type)
                return (i);

    return (-1);
}


/* GtNRasters -- Return the number of currently defined rasters.
 */
GtNRasters (w)
    GtermWidget w;
{
    return (w->gterm.nrasters);
}


/* GtAssignRaster -- Assign a raster descriptor to an externally created
 * drawable (window or pixmap).  The raster thus created may be mapped or
 * drawn into like the rasters created privately by the imaging code, but
 * this allows use of this code to access other windows, or shared pixmaps.
 */
GtAssignRaster (w, raster, drawable, type)
    GtermWidget w;
    int raster;			/* one-indexed */
    XtPointer drawable;		/* object containing pixel array */
    int type;			/* type of drawable [not used] */
{
    register Raster rp;
    XWindowAttributes wa;

    if (raster <= 0 || raster >= w->gterm.maxRasters)
	return (ERR);
    else
	rp = &w->gterm.rasters[raster];

    if (!w->gterm.wa_defined) {
        if (!XGetWindowAttributes (w->gterm.display, (Window)drawable, &wa))
	    return (ERR);
    } else
	wa = w->gterm.wa;

    rp->type      = PixmapRaster;
    rp->width     = wa.width;
    rp->height    = wa.height;
    rp->depth     = wa.depth;
    rp->r.pixmap  = (Pixmap) drawable;
    rp->delete    = 0;

    return (OK);
}


/* GtCreateRaster -- Create a new raster of the given size.  A server pixmap
 * (GtServer) or  ximage (GtClient) raster will be created depending upon the
 * current value of the cacheRasters resource.
 */
GtCreateRaster (w, raster, type, width, height, depth)
    GtermWidget w;
    int raster;                 /* one-indexed */
    int type;
    int width, height;
    int depth;
{
    register uchar *op;
    register int npix, pixel;
    uchar *data;
    XImage *xp;
    Raster rp;
    int cache;


    if (!XtIsRealized ((Widget)w))
        return (ERR);

    if (DBG_TRACE) {
	fprintf (stderr, "GtCreateRaster:  raster=%d  type=%s  (%dx%dx%d)\n",
  	    raster, ((type == GtClient) ? "GtClient" : "GtServer"), 
  	    width, height, depth);
    }


    /*  Only rasters of depth 8 bits are currently supported.  Eventually
    **  we may want to allow arbitrarily deep frame buffers (e.g. for RGB
    **  composites, overlay planes, etc).
    if (depth && depth != 8) {
	if (DBG_TRACE)
	    fprintf (stderr, 
	        "GtCreateRaster ERROR: attempt to create raster depth=%d\n",
	        depth);
	return (ERR);
    }
    */


    /* Check for a raster number in bounds. */
    if (raster < 0 || raster >= w->gterm.maxRasters) {
	if (DBG_TRACE)
	    fprintf (stderr, 
	        "GtCreateRaster ERROR: invalid raster = %d\n", raster);
	return (ERR);
    } else
	rp = &w->gterm.rasters[raster];


    /* A create on raster 0 (the display window) is treated as an attempt
     * to resize the window.
     */
    if (raster == 0) {
	XWindowAttributes wa;

	invalidate_draw_context (w);

	/* Issue the resize request. */
	XtVaSetValues ((Widget)w,
	    XtNwidth, (XtArgVal)width,
	    XtNheight, (XtArgVal)height,
	    NULL);
	XFlush (w->gterm.display);

	/* The following generates a round trip request to the server and
	 * is an attempt to allow the window system time to process the
	 * resize request before the client can issue a GtQueryRaster to
	 * see if the request has succeeded (hence causing a race condition).
	 * If the window is not the requested size the delay flag is set
	 * to cause graphics input processing to be suspended until the
	 * window is resized or redisplayed.  A dummy expose event is
	 * generated to clear the delay condition in case the resize request
	 * is not granted.
	 */
	if (XGetWindowAttributes (w->gterm.display, w->gterm.window, &wa)) {
	    rp->width = wa.width;
	    rp->height = wa.height;

	    if (rp->width != width || rp->height != height) {
		XExposeEvent ev;
		ev.type = Expose;
		ev.send_event = True;
		ev.display = w->gterm.display;
		ev.window = w->gterm.window;
		ev.x = ev.y = 0;
		ev.width = ev.height = 1;
		ev.count = 0;

		XSendEvent (w->gterm.display, w->gterm.window, False,
		    NoEventMask, (XEvent *)&ev);
		w->gterm.delay = 1;
	    }
	}
	return (OK);
    }

    /* Get rid of any old raster. */
    GtDestroyRaster (w, raster);

    rp->width  = width;
    rp->height = height;
    rp->depth  = depth;
    rp->delete = 1;

    /* Cache the raster? */
    if (strcmp (w->gterm.cacheRasters, "always") == 0)
	cache = 1;
    else if (strcmp (w->gterm.cacheRasters, "never") == 0)
	cache = 0;
    else
	cache = (type == GtServer);


    if (DBG_TRACE) {
	fprintf (stderr,
	    "GtCreateRaster:  cacheRasters = '%s'  type=%s  cache=%d\n",
    	    w->gterm.cacheRasters, (type == GtServer ? "GtServer" : "GtClient"),
    	    cache);
	fprintf (stderr,"GtCreateRaster:  cache=%d  (%dx%dx%d)\n", 
    	    cache, width, height, depth);
	fprintf (stderr,"GtCreateRaster:  Creating %s: %d\n", 
    	    (cache ? "PIXMAPRASTER" : "IMAGERASTER"), raster);
    }

    /* Create new raster. */
    if (cache) {
	/* Create a pixmap.  */
	rp->type      = PixmapRaster;
	rp->depth     = depth;
	rp->r.pixmap  = XCreatePixmap (w->gterm.display, w->gterm.window,
	    width, height, depth);


	if (DBG_TRACE)
	    fprintf (stderr,
		"GtCreateRaster:  [pixmap]  creating shadow pixmap %dx%d\n",
		w->core.width+1, w->core.height+1);

        rp->shadow_pixmap = XCreatePixmap (w->gterm.display, w->gterm.window,
	    w->core.width+1, w->core.height+1, RasterDepth);

	if (rp->r.pixmap == (Pixmap) NULL)
	    goto ximage;

	XFillRectangle (w->gterm.display, rp->r.pixmap, 
	    (depth == RasterDepth ? w->gterm.clear8GC : w->gterm.clearGC),
	    0, 0, width, height);

    } else {
	/* Create an XImage for the raster.
	*/
ximage:
	rp->type      = ImageRaster;
	rp->depth     = depth;

	/* Get pixel storage.  
	*/
	if ((data = (uchar *) XtMalloc((npix=width*height))) == NULL)
	    return (ERR);
	else {
	    for (op=data, pixel=w->gterm.color0;  --npix >= 0;  )
		*op++ = pixel;
	}

	/* The following doesn't yet deal properly with byte and bit ordering
	 * differences between the server and client.
	 */
	rp->r.ximage = xp = XCreateImage (w->gterm.display, NULL, RasterDepth,
	    ZPixmap, 0, (char *)data, width, height, 8, 0);
	if (xp == NULL) {
	    rp->type = 0;
	    return (ERR);
	}


	if (DBG_TRACE)
	    fprintf (stderr,
		"GtCreateRaster:  [ximage]  creating shadow pixmap %dx%d\n",
		w->core.width+1, w->core.height+1);

        rp->shadow_pixmap = XCreatePixmap (w->gterm.display, w->gterm.window,
	    w->core.width+1, w->core.height+1, RasterDepth);
    }

    w->gterm.nrasters++;

    if (DBG_TRACE) {
	int i;

	fprintf (stderr, "GtCreateRaster: LEAVING nraster=%d\n",
	    w->gterm.nrasters);

	for (i=0; i < w->gterm.nrasters; i++) 
	    fprintf (stderr, "GtCreateRaster[%d]: type=%8s  %dx%d  [%d]\n",
		i, 
	        (w->gterm.rasters[i].type==GtClient)?"GtClient":"GtServer", 
		w->gterm.rasters[i].width, w->gterm.rasters[i].height,
		w->gterm.rasters[i].depth);
    }
    return (OK);
}


/* GtDestroyRaster -- Destroy a raster.  Any mappings which reference the
 * raster are deactivated, and all storage associated with the raster is freed.
 */
GtDestroyRaster (w, raster)
    GtermWidget w;
    int raster;
{
    register Raster rp;
    register Mapping mp, next;

    if (raster <= 0)
	return;

    invalidate_draw_context (w);

    /* Disable any mappings that reference this raster. */
    for (mp = w->gterm.mp_head;  mp;  mp = next) {
	next = mp->next;
	if (mp->src == raster || mp->dst == raster)
	    free_mapping (w, mp);
    }

    /* Destroy the raster. */
    rp = &w->gterm.rasters[raster];
    if (rp->type) {
	if (rp->delete) {
	    if (rp->type == ImageRaster)
		XDestroyImage (rp->r.ximage);
	    else if (rp->type == PixmapRaster)
		XFreePixmap (w->gterm.display, rp->r.pixmap);
	}
	w->gterm.nrasters--;
	rp->type = 0;
	rp->delete = 0;
    }
}


/* GtQueryRaster -- Determine whether a raster exists and if so return its
 * size.
 */
GtQueryRaster (w, raster, type, width, height, depth)
    GtermWidget w;
    int raster;			/* one-indexed */
    int *type;
    int *width, *height;
    int *depth;
{
    register Raster rp;


    if (DBG_TRACE && DBG_VERBOSE)
	fprintf (stderr, "GtQueryRaster: raster=%d\n", raster);

    if (raster < 0 || raster > w->gterm.maxRasters)
	return (0);

    rp = &w->gterm.rasters[raster];
    if (rp->type) {
	if (type) {
	    if (rp->type == PixmapRaster)
		*type = GtServer;
	    else
		*type = GtClient;
	}
	if (width)
	    *width = rp->width;
	if (height)
	    *height = rp->height;
	if (depth)
	    *depth = rp->depth;

        if (DBG_TRACE && DBG_VERBOSE)
	    fprintf (stderr, "GtQueryRaster: raster=%d (%s) w=%d  h=%d d=%d\n",
	        raster, 
	        ((*type == PixmapRaster) ? "GtServer" : "GtClient"),
	        *width, *height, *depth);


	return (1);
    } else
	return (0);
}


/* GtWritePixels -- Write to a rectangular region of a raster.  If any
 * mappings are currently defined which reference this raster as the source,
 * and a mapped region is being rewritten, the affected pixels will be
 * refreshed by the mapping.
 */
GtWritePixels (w, raster, pixels, nbits, x1, y1, nx, ny)
    GtermWidget w;
    int raster;
    uchar *pixels;
    int nbits;				/* not used */
    int x1, y1;
    int nx, ny;
{
    register uchar *ip, *op;
    register Pixel *cmap;
    register int i, n, bytes_per_line;
    Mapping mp;
    Raster  rp;
    uchar  *lp;
    XWindowAttributes wa;
    unsigned int *ras = NULL;


    rp = &w->gterm.rasters[raster];

    if (DBG_TRACE) 
	fprintf(stderr, 
	    "GtWritePixels[%s] ENTER: nbits=%d raster=%d type='%s' wa=0x%x\n", 
	    dbg_wSize(w), nbits, raster, 
	    (rp->type == PixmapRaster) ? "PixmapRaster" : "ImageRaster",
	    w->gterm.wa);

	
    if (rp->type == 0)
	return (ERR);

    /* Perform some range checks. */
    if (x1 < 0 || x1 > rp->width || nx <= 0 || x1+nx > rp->width)
	return (ERR);
    if (y1 < 0 || y1 > rp->height || ny <= 0 || y1+ny > rp->height)
	return (ERR);

    if (!w->gterm.wa_defined) {
        if (!XGetWindowAttributes (w->gterm.display, w->gterm.window, &wa)) {
	    fprintf (stderr, "GtWritePixels:  Error getting window attrs\n");
	    return (ERR);
        }
    } else
	wa = w->gterm.wa;

    if (DBG_TRACE)
	fprintf (stderr,
	    "GtWritePixels:  window depth=%d  RasterDepth= %d class=%s\n",
	    wa.depth, RasterDepth, dbg_visStr(wa.visual->class));


    if (rp->type == PixmapRaster) {
	Display *display = w->gterm.display;
	XImage *ximage;
	uchar *data;
	int npix;

	if (DBG_TRACE) 
	    fprintf(stderr, "GtWritePix: Doing PixmapRaster[%s]....depth=%d\n",
		dbg_wSize(w), wa.depth);

	/* Get a data buffer. */
	if ((data = (uchar *)XtMalloc (npix = nx * ny)) == NULL)
	    return (ERR);

	/* Convert the pixel values to colormap indices. */
	cmap = get_cmap_in (w);
	for (ip=pixels, op=data, n=npix;  --n >= 0;  ) {

	    /* In TrueColor mode, we preserve the index values in the 
	    ** XImage and apply the colormap when rendering.
	    */
	    *op++ = (wa.depth == ColormapDepth ? 
		(cmap[*ip++] & 0377) : ((*ip++) & 0377));
	}


	if (DBG_TRACE) 
	    fprintf(stderr, "GtWritePix: Creating 8-bit ximage\n");

	ximage = XCreateImage (w->gterm.display, NULL, RasterDepth,
	    ZPixmap, 0, (char *)data, nx, ny, 8, 0);


	if (raster == 0 && w->gterm.pixmap) {
	    if (DBG_TRACE) 
		fprintf(stderr, "GtWritePix: type = pixmap, raster=0\n");

	    XPutImage (display, w->gterm.pixmap, w->gterm.exposeGC, 
		IMGtoGPM(w,ximage,0,0,nx,ny),
		0, 0, x1, y1, nx, ny);

	    XCopyArea (display, 
		GPMtoRPM(w, rp), rp->r.pixmap,
		w->gterm.exposeGC, x1, y1, nx, ny, x1, y1);

	} else {
	    XPutImage (display, rp->r.pixmap, w->gterm.exposeGC, 
		IMGtoRPM (w,ximage,rp,0,0,nx,ny),
		0, 0, x1, y1, nx, ny);
	}

	XtFree ((char *)data);
	ximage->data = NULL;
	XDestroyImage (ximage);

    } else if (rp->type == ImageRaster) {
	int min=256, max=0;
	int min1=256, max1=0;


	if (DBG_TRACE)
	    fprintf (stderr,
		"GtWritePix: ImageRaster....bytes_per_line=%d  ras depth=%d\n",
    		rp->r.ximage->bytes_per_line, rp->r.ximage->depth);

	cmap = get_cmap_in (w);
	bytes_per_line = rp->r.ximage->bytes_per_line;
	lp = (uchar *)rp->r.ximage->data + y1 * bytes_per_line + x1;
	ip = pixels;

	if (DBG_TRACE)
	    fprintf(stderr,
		"GtWritePix: Doing ColormapDepth....writing to ximage\n");

	/* Copy the data into the ximage data raster, converting input
	 * pixels to Xlib pixel values in the process.
	 *
	 * Possibly this should be done at Pixmap write time rather than
	 * during raster i/o so that the image pixel values are preserved.
	 * Otherwise reading back pixels is difficult and if the color map
	 * is dynamically modified the original pixel values may be lost.
	 * Postponing display pixel value generation would also make it
	 * easy to add support later for image depths other than 8 bit.
	 * Doing the conversion to display pixels here is however simpler
	 * and more efficient so that is how we do it for now.
	 */
	for (i=0;  i < ny;  i++) {
	    for (n=nx, op=lp;  --n >= 0;  ) {
		if (wa.depth == ColormapDepth)
		    *op++ = (cmap[*ip++] & 0xFF);
		else
		    *op++ = (*ip++ & 0xFF);

	 	if (DBG_TRACE) {
		    if (*ip < min) min = *ip; 
		    if (*ip > max) max = *ip; 
		    if (cmap[*ip] < min1) min1 = cmap[*ip];
		    if (cmap[*ip] > max1) max1 = cmap[*ip];
		}
	    }
	    lp += bytes_per_line;
	}

	if (DBG_TRACE)
	    fprintf (stderr,
		"CMAP MINMAX:  %d  %d  --  %d  %d\n", min, max, min1, max1);
    }

    /* Execute any mappings that reference this raster. */
    for (mp = w->gterm.mp_head;  mp;  mp = mp->next) {
	if (wa.depth != ColormapDepth) {		/* FIXME	*/
	    update_mapping (w, mp);
	    refresh_source (w, mp, x1, y1, nx, ny);
	    /* break; */

	} else if (mp->enabled && mp->src == raster) {
	    struct mapping *map=mp, p_mp;
	    if (map->st != GtPixel || map->dt != GtPixel) {
		initialize_mapping (&p_mp);
		get_pixel_mapping (w, map, &p_mp, 1);
		update_mapping (w, map = &p_mp);
	    } else
		update_mapping (w, map);
	    refresh_source (w, map, x1, y1, nx, ny);
	    if (map == &p_mp)
		free_mapping (w, map);
	}
    }

    if (DBG_TRACE)
	fprintf(stderr, "GtWritePixels[%s]  LEAVING....\n", dbg_wSize(w));

    return (OK);
}


/* GtReadPixels -- Read a rectangular region of a raster.
 */
GtReadPixels (w, raster, pixels, nbits, x1, y1, nx, ny)
    GtermWidget w;
    int raster;
    uchar *pixels;
    int nbits;			/* not used */
    int x1, y1;
    int nx, ny;
{
    register uchar *ip, *op;
    register Pixel *cmap;
    register int n;

    int bytes_per_line, i, nskip = 1;
    int x, y, delxin = 0;
    XImage *xin;
    Raster rp;
    uchar *lp;


    if (DBG_TRACE)
	fprintf (stderr, "GtReadPixels:  ras=%d  %d %d %d %d  w->depth=%d\n",
    	    raster, x1, y1, nx, ny, w->gterm.w_depth);

    rp = &w->gterm.rasters[raster];
    if (rp->type == 0)
	return (ERR);

    /* Perform some range checks. */
    if (x1 < 0 || x1 > rp->width || nx <= 0 || x1+nx > rp->width)
	return (ERR);
    if (y1 < 0 || y1 > rp->height || ny <= 0 || y1+ny > rp->height)
	return (ERR);

    /* Get the input ximage.
    if (rp->type == PixmapRaster) {
    */
    if (rp->type == PixmapRaster || (raster == 0 && w->gterm.w_depth > 8)) {

	Display *display = w->gterm.display;


	if (DBG_TRACE)
	    fprintf (stderr, "GtReadPixels:  rp->type == PixmapRaster  [%d]\n",
		raster);

	/* Read the pixmap subraster into an ximage.  If we are reading the
	 * screen (raster == 0) and we have an off-screen backing store pixmap,
	 * use that instead of the screen.
	 */
	if (w->gterm.w_depth > ColormapDepth) {
	    Raster ras = (Raster) NULL;

	    if (raster)
		ras = &w->gterm.rasters[w->gterm.d_raster];
	    else
		ras = &w->gterm.rasters[0];

            xin = XGetImage (display, 
	        (raster == 0 && w->gterm.pixmap) ? 
		    ras->shadow_pixmap : rp->r.pixmap,
                x1, y1, nx, ny, 0xff, ZPixmap);

	} else {
            xin = XGetImage (display,
	        (raster == 0 && w->gterm.pixmap) ? 
		    w->gterm.pixmap : rp->r.pixmap,
                x1, y1, nx, ny, 0xff, ZPixmap);
	}

	if (xin == NULL)
	    return (ERR);

	delxin++;
	x = y = 0;

    } else {
	xin = rp->r.ximage;
	x = x1;
	y = y1;
    }
    nskip = xin->bits_per_pixel / 8;

    if (DBG_TRACE)
	fprintf (stderr,
	    "GtReadPixels:  xin->bpp=%d  bpl=%d  nskip=%d  %d,%d  %dx%d\n",
	    xin->bits_per_pixel, xin->bytes_per_line, nskip,
	    x1, y1, nx, ny);

    if (w->gterm.w_depth == ColormapDepth)
        cmap = get_cmap_out (w);
    bytes_per_line = xin->bytes_per_line;
    lp = (uchar *)xin->data + (y * bytes_per_line + (nskip * x));
    op = pixels;

    /* Copy the data to the output buffer, converting display pixels to
     * client pixels in the process.
     */
    for (i=0;  i < ny;  i++) {
	for (n=nx, ip=lp;  --n >= 0;  ) {
	    if (w->gterm.w_depth == ColormapDepth) {
	        *op++ = cmap[*ip];
	    } else {
		*op++ = *ip;
	    }
	    ip += nskip;
	}
	lp += bytes_per_line;
    }

    if (delxin)
	XDestroyImage (xin);
    return (OK);
}


/* GtSetPixels -- Set all the raster pixels in a region to a single color.
 * If nx=ny=0 the entire raster will be written.
 */
GtSetPixels (w, raster, ct, x1, y1, nx, ny, color, rop)
    GtermWidget w;
    int raster;
    int ct;
    int x1, y1;
    int nx, ny;
    int color;
    int rop;
{
    register Raster rp;
    Mapping mp;

    /* Get raster pointer. */
    rp = &w->gterm.rasters[raster];
    if (rp->type == 0)
	return (ERR);

    /* Get pixel coordinates. */
    if (ct != GtPixel) {
	struct mapping sv_mp, p_mp;
	initialize_mapping (&sv_mp);				/* MF035 */
	save_mapping (&sv_mp, 0, 0,
	    0, GtPixel, 0,0,0,0,
	    raster, ct, x1,y1,nx,ny);
	get_pixel_mapping (w, &sv_mp, &p_mp, 0);

	x1 = p_mp.dx;
	y1 = p_mp.dy;
	nx = p_mp.dnx;
	ny = p_mp.dny;
    }

    /* Perform some range checks. */
    if (x1 == 0 && y1 == 0 && nx == 0 && ny == 0) {
	nx = rp->width;
	ny = rp->height;
    } else {
	if (x1 < 0 || x1 > rp->width || nx <= 0 || x1+nx > rp->width)
	    return (ERR);
	if (y1 < 0 || y1 > rp->height || ny <= 0 || y1+ny > rp->height)
	    return (ERR);
    }

    /* Set the pixels.
     */
    if (rp->type == PixmapRaster) {
	Display *display = w->gterm.display;
	GC gc = w->gterm.clearGC;
	int use_backing_store;
	Raster sp = &w->gterm.rasters[0];

	use_backing_store =
	    (raster == 0 && w->gterm.pixmap && !(rop & R_Transient));


	XSetForeground (display, gc, get_pixel(w,color));
	XFillRectangle (display, rp->r.pixmap, gc, x1, y1, nx, ny);
	if (use_backing_store) {
	    XFillRectangle (display, w->gterm.pixmap, gc, x1, y1, nx, ny);
	    XFillRectangle (display, sp->shadow_pixmap, w->gterm.clear8GC,
	        x1, y1, nx, ny);
	}
	XSetForeground (display, gc, w->gterm.color0);

    } else {
	register int n, i;
	register uchar *op;
	register Pixel pixel;
	int bytes_per_line;
	uchar *lp;

	pixel = get_pixel (w, color);
	bytes_per_line = rp->r.ximage->bytes_per_line;
	lp = (uchar *)rp->r.ximage->data + y1 * bytes_per_line + x1;

	/* Set all pixels in the indicated region.  */
	for (i=0;  i < ny;  i++) {
	    for (n=nx, op=lp;  --n >= 0;  )
		*op++ = pixel;
	    lp += bytes_per_line;
	}
    }

    /* Execute any mappings that reference this raster. */
    for (mp = w->gterm.mp_head;  mp;  mp = mp->next) {
	if (mp->enabled && mp->src == raster) {
	    struct mapping *map=mp, p_mp;
	    if (map->st != GtPixel || map->dt != GtPixel) {
		initialize_mapping (&p_mp);
		get_pixel_mapping (w, map, &p_mp, 1);
		update_mapping (w, map = &p_mp);
	    } else
		update_mapping (w, map);
	    refresh_source (w, map, x1, y1, nx, ny);
	    if (map == &p_mp)
		free_mapping (w, map);
	}
    }

    return (OK);
}


/* GtRefreshPixels -- Update any mappings defined upon the given region of
 * the given source raster, as if the pixel values had been set with a
 * write pixels call.
 */
GtRefreshPixels (w, raster, ct, x1, y1, nx, ny)
    GtermWidget w;
    int raster;
    int ct;
    int x1, y1;
    int nx, ny;
{
    register Raster rp = &w->gterm.rasters[raster];
    register Mapping mp;

    if (!w || !XtIsRealized ((Widget)w))
	return;

    if (DBG_TRACE)
	fprintf (stderr, "GtRefreshPixels:  ENTER\n");

    /* Get pixel coordinates. 
    */
    if (ct != GtPixel) {
	struct mapping sv_mp, p_mp;
	initialize_mapping (&sv_mp);
	save_mapping (&sv_mp, 0, 0,
	    raster, ct, x1,y1,nx,ny,
	    0, GtPixel, 0,0,0,0);
	get_pixel_mapping (w, &sv_mp, &p_mp, 0);

	x1 = p_mp.sx;
	y1 = p_mp.sy;
	nx = p_mp.snx;
	ny = p_mp.sny;
    }

    /* Execute any mappings that reference this raster. 
    */
    for (mp = w->gterm.mp_head;  mp;  mp = mp->next) {

        if (DBG_TRACE)
	    fprintf (stderr, "GtRefreshPixels: mp=0x%x enabled=%d src=%d/%d\n",
		mp, mp->enabled, mp->src, raster);

	if (mp->enabled && mp->src == raster) {
	    struct mapping *map=mp, p_mp;

	    if (map->st != GtPixel || map->dt != GtPixel) {
		if (DBG_TRACE)
		    fprintf (stderr,"GtRefreshPixels: update pixtype raster\n");

		initialize_mapping (&p_mp);
		get_pixel_mapping (w, map, &p_mp, 1);
		update_mapping (w, map = &p_mp);
	    } else {
		if (DBG_TRACE)
		    fprintf (stderr,"GtRefreshPixels: update non-pix raster\n");

		update_mapping (w, map);
	    }

	    refresh_source (w, map, x1, y1, nx, ny);
	    if (map == &p_mp)
		free_mapping (w, map);
	}
    }

    if (DBG_TRACE)
	fprintf (stderr, "GtRefreshPixels:  LEAVING\n");
}


/* GtExtractPixmap -- Extract a rectangular region of a raster and return
 * as a pixmap.  The caller is responsible for later deleting this pixmap.
 */
Pixmap
GtExtractPixmap (w, src, ctype, x, y, width, height)
    GtermWidget w;
    int src;
    int ctype;
    int x, y;
    int width, height;
{
    register Raster rp;
    int x1, y1, nx, ny;
    String cache;
    int i;

    rp = &w->gterm.rasters[src];
    if (!rp->type)
	return ((Pixmap)NULL);

    /* If width and height are zero, return the full raster. */
    if (width <= 0)
	width = rp->width;
    if (height <= 0)
	height = rp->height;

    /* Get pixel coordinates. */
    if (ctype != GtPixel) {
	struct mapping sv_mp, p_mp;
	initialize_mapping (&sv_mp);				/* MF035 */
	save_mapping (&sv_mp, 0, 0,
	    0, GtPixel, 0,0,0,0,
	    src, ctype, x,y,width,height);
	get_pixel_mapping (w, &sv_mp, &p_mp, 0);

	x1 = p_mp.dx;
	y1 = p_mp.dy;
	nx = p_mp.dnx;
	ny = p_mp.dny;

    } else {
	x1 = x;
	y1 = y;
	nx = width;
	ny = height;
    }
 
    /* Find any empty raster slot and use it to generate the output pixmap.
     */
    for (i=0;  i < w->gterm.maxRasters;  i++) {
	rp = &w->gterm.rasters[i];
	if (!rp->type) {
	    cache = w->gterm.cacheRasters;
	    w->gterm.cacheRasters = "always";

	    if (GtCreateRaster (w, i, GtServer, nx, ny, 	/* MF006 */
		RasterDepth) == ERR) {
		    w->gterm.cacheRasters = cache;
		    return ((Pixmap)NULL);

	    } else if (rp->type != PixmapRaster)
		goto err;

	    if (GtCopyRaster (w, 0,
		src,0, x1,y1,nx,ny, i,0, 0,0,nx,ny) == ERR) {
err:
		GtDestroyRaster (w, i);				/* MF005 */
		w->gterm.cacheRasters = cache;
		return ((Pixmap)NULL);
	    }

	    rp->type = 0;
	    w->gterm.nrasters--;
	    w->gterm.cacheRasters = cache;

	    return (rp->r.pixmap);
	}
    }

    return ((Pixmap)NULL);
}


/* GtInsertPixmap -- Insert the contents of the given pixmap into a raster
 * at the indicated coordinates.
 */
GtInsertPixmap (w, pixmap, dst, ctype, x, y, width, height)
    GtermWidget w;
    Pixmap pixmap;
    int dst;
    int ctype;
    int x, y;
    int width, height;
{
    register Raster rp;
    XWindowAttributes wa;
    int x1, y1, nx, ny;
    int i;

    /* Check that the pixmap exists. */
    if (!XGetWindowAttributes (w->gterm.display, pixmap, &wa))
	return (ERR);

    /* Default to full dimensions of pixmap if no dimensions given. */
    if (width <= 0)
	width = wa.width;
    if (height <= 0)
	height = wa.height;

    /* Get pixel coordinates. */
    if (ctype != GtPixel) {
	struct mapping sv_mp, p_mp;
	initialize_mapping (&sv_mp);				/* MF035 */
	save_mapping (&sv_mp, 0, 0,
	    0, GtPixel, 0, 0, 0, 0,
	    dst, ctype, x, y, width, height);
	get_pixel_mapping (w, &sv_mp, &p_mp, 0);

	x1 = p_mp.dx;
	y1 = p_mp.dy;
	nx = p_mp.dnx;
	ny = p_mp.dny;

    } else {
	x1 = x;
	y1 = y;
	nx = width;
	ny = height;
    }

    /* Create the destination raster if none exists. */
    if (!w->gterm.rasters[dst].type)
	if (GtCreateRaster (w, dst, GtDefault, nx, ny,		/* MF006 */
	    RasterDepth) == ERR)
	        return (ERR);
    
    /* Find an empty raster slot and use it to build a fake source raster
     * using the supplied pixmap.
     */
    for (i=0;  i < w->gterm.maxRasters;  i++) {
	rp = &w->gterm.rasters[i];
	if (!rp->type) {
	    rp->type = PixmapRaster;
	    rp->width = nx;
	    rp->height = ny;
	    rp->r.pixmap = pixmap;

	    if (GtCopyRaster (w, 0,
		    i,0, 0,0,nx,ny, dst,0, x1,y1,nx,ny) < 0)
		return (ERR);

	    rp->type = 0;
	    return (OK);
	}
    }

    return (ERR);
}


/* GtWriteColormap -- Allocate or modify colormap cells.  The Gterm widget
 * colormap consists of a fixed number of preassigned, read-only color cells,
 * plus a variable number of dynamically allocated application defined color
 * cells.  The application sees only the preassigned pixel space 0-N where
 * N is the maximum pixel value.  These values are mapped to Xlib pixel values
 * by the CMAP vector in the main Gterm widget descriptor.  The server
 * colormap does the final mapping to RGB triplets.
 *
 * Our strategy here is as follows.  If we have a monochrome screen set up a
 * one-to-one fake colormap so that we preserve the input pixel values and
 * render a one-bit image later.  If we have a StaticGray or StaticColor
 * visual allocate read-only color cells to allow X to choose the closest
 * colors to what we request.  If we have a GrayScale or PseudoColor visual
 * allocate private read-write colors.  The TrueColor and DirectColor
 * visuals should not be seen here as we should have been able to set up the
 * PseudoColor visual on screens that support these visuals, but if they are
 * seen use a one-to-one mapping to preserve the 8 bit pixel values.
 */
GtWriteColormap (w, map, first, nelem, r, g, b)
    GtermWidget w;
    int map;
    int first;
    int nelem;
    ushort *r, *g, *b;
{
    XWindowAttributes wa;
    register XColor *cp;
    register int i, j, v, n, use_wa = 1;
    unsigned long plane_masks[1];
    int req, need, ncolors;
	    
    Colormap colormap;


    if (!w || !XtIsRealized ((Widget)w))
	return (ERR);

    if (DBG_TRACE) {
        fprintf (stderr, "GtWriteColormap: ENTER.....\n");
        fprintf (stderr, 
	    "GtWriteColormap: map=%d first=%d nelem=%d gt.ncols=%d\n", 
    	    map, first, nelem, w->gterm.ncolors);
    }

    if (map > 0) {
	/* Create or modify a colormap descriptor.  The display colormap
	 * is not affected.
	 */
	register struct colormap *cm;
	struct colormap *last_cm;
	register XColor *cp;
	register int i;



        if (DBG_TRACE)
            fprintf (stderr, "GtWriteColormap: create/modify map = %d\n", map);

	/* See if the colormap already exists.
	*/
	for (cm = w->gterm.colormaps, last_cm = NULL;  cm;  cm = cm->next) {
	    last_cm = cm;
	    if (cm->map == map)
		break;
	}

	/* If not, create it.
	*/
	if (!cm) {
	    if (!(cm = (struct colormap *)XtCalloc(1,sizeof(struct colormap))))
		return (ERR);
	    if (last_cm)
		last_cm->next = cm;
	    else
		w->gterm.colormaps = cm;

	    /* Initialize static part of colormap. */
	    for (i=0;  i < SZ_STATIC_CMAP;  i++) {
		cp = &w->gterm.color[i];
		cm->r[i] = cp->red;
		cm->g[i] = cp->green;
		cm->b[i] = cp->blue;
	    }
	}

	cm->map = map;
	cm->ncells = max (cm->ncells, first + nelem);

	/* Ignore attempts to overwrite static part of colormap.
	*/
	for (  ;  first < SZ_STATIC_CMAP;  first++, nelem--) {
	    r++;  g++;  b++;
	}

        if (nelem >= 0) {
	    memmove (&cm->r[first], r, nelem * sizeof (ushort));
	    memmove (&cm->g[first], g, nelem * sizeof (ushort));
	    memmove (&cm->b[first], b, nelem * sizeof (ushort));
        }


	if (DBG_TRACE)
	    fprintf (stderr, "GtWriteColormap: map=%d -- RETURNING\n",map);

	return (OK);
    }

    /* Write to the display colormap.
     */
    if (first < SZ_STATIC_CMAP || first + nelem > MAX_SZCMAP)
	return (ERR);

    /* Invalidate the cmap cache.
    */
    invalidate_cmap (w);

    /* Get the window attributes.  We need to do this to determine the type
     * of visual used for the window, which determines our color allocation
     * strategy.  This is only done once since presumably the visual and
     * window depth will not change after the widget has been around long
     * enough to receive a GtWriteColormap call.
     */
    if (w->gterm.w_depth == 0 && w->gterm.w_visual_class == 0) {
	if (!XGetWindowAttributes (w->gterm.display, w->gterm.window, &wa))
	    return (ERR);
	w->gterm.wa = wa;
	w->gterm.wa_defined++;

        if (wa.depth == 1)
	    goto unitary;
    } else
	use_wa = 0;
    

    switch ((use_wa ? wa.visual->class : w->gterm.w_visual_class)) {
    case StaticGray:
    case StaticColor:
	/* Allocate "best-match" colors. */
	for (i=first;  i < first+nelem;  i++) {
	    cp = &w->gterm.color[i];
	    cp->red   = r[i-first];
	    cp->green = g[i-first];
	    cp->blue  = b[i-first];
	    cp->flags = (DoRed | DoGreen | DoBlue);
	    if (XAllocColor (w->gterm.display, wa.colormap, cp)) {
		w->gterm.cmap[i] = cp->pixel;
	    } else {
		w->gterm.cmap[i] = cp->pixel =
		    BlackPixelOfScreen (w->gterm.screen);
	    }
	}
	break;

    case GrayScale:
    case PseudoColor:
	if (DBG_TRACE)
	    fprintf (stderr,
		"GtWriteColormap: PSEUDOCOLOR vis, defCM=%d, gt.ncols=%d\n",
		w->gterm.useDefaultCM, w->gterm.ncolors);

	if (w->gterm.useDefaultCM) {
usedef:	    /* Allocate private r/w colors from default colormap. */
	    need = first + nelem - w->gterm.ncolors;

	    /* Allocate new color cells if needed.  If we can't allocate all
	     * the requested cells the unallocated pixel values are set to
	     * black.
	     */
	    if ((n = need) > 0) {
		/* Get the colormap cells. */
		req = min(w->gterm.maxColors, first + nelem - SZ_STATIC_CMAP) -
		    (w->gterm.ncolors - SZ_STATIC_CMAP);
		for (n=0;  req > 0 && n < need;  )
		    if (XAllocColorCells (w->gterm.display, wa.colormap,
			    False, plane_masks, 0,
			    &w->gterm.cmap[w->gterm.ncolors+n], req)) {
			n += req;
		    } else
			req /= 2;

		/* Initialize the color descriptors. */
		for (i = w->gterm.ncolors;  i < first+nelem;  i++) {
		    cp = &w->gterm.color[i];
		    if (i < w->gterm.ncolors + n) {
			cp->pixel = w->gterm.cmap[i];
			cp->flags = (DoRed | DoGreen | DoBlue);
		    } else {
			w->gterm.cmap[i] = cp->pixel =
			    BlackPixelOfScreen (w->gterm.screen);
			cp->flags = 0;
		    }
		}
		w->gterm.ncolors += n;
	    }

	    if (DBG_TRACE)
	        fprintf (stderr,
		    "GtWriteColormap: PSEUDOCOLOR defCM, need=%d gt.ncols=%d\n",
		    need, w->gterm.ncolors);

	    /* Assign RGB colors to the referenced cells. */
	    for (i=0;  i < nelem;  i++) {
		cp = &w->gterm.color[first+i];
		cp->red   = r[i];
		cp->green = g[i];
		cp->blue  = b[i];
	    }

	    n = w->gterm.ncolors - first;
	    if (n > 0)
		XStoreColors (w->gterm.display, wa.colormap,
		    &w->gterm.color[first], n);

	} else {
	    /* Allocate colors in a custom colormap.  If the named colormap
	     * does not yet exist we create one.  Multiple gterm widget
	     * instances may share the same colormap.
	     */
	    long timeval, time();
	    int shadow;


	    /* get_colormap will direct us to the default colormap if the
	     * custom colormap cannot be accessed or created.
	     */
	    colormap = get_colormap (w);
	    if (w->gterm.useDefaultCM)
		goto usedef;

	    /* Assign RGB colors to the referenced cells. */
	    ncolors = min (w->gterm.maxColors, nelem);
	    cp = &w->gterm.color[first];

	    if (DBG_TRACE) {
	        fprintf (stderr, "GtWriteColormap: PSEUDOCOLOR custom cmap\n");
		fprintf (stderr,
    		  "GtWriteColormap: Pseudo: first=%d nelem=%d -> ncols=%d/%d\n",
		  first, nelem, ncolors, w->gterm.ncolors);
	    }

	    for (i=0;  i < ncolors;  i++, cp++) {
		cp->red   = r[i];
		cp->green = g[i];
		cp->blue  = b[i];
		cp->flags = (DoRed | DoGreen | DoBlue);

	        if (DBG_TRACE && DBG_CM_VERB)
		    fprintf (stderr,
		        "GtWriteColormap: Pseudo: %3d (%3d %3d %3d) %d / %d\n",
		        i, r[i]>>8,g[i]>>8,b[i]>>8, w->gterm.ncolors, ncolors);
	    }

	    if (DBG_TRACE)
	        fprintf (stderr,
		    "GtWriteColormap: first=%d nelem=%d ncol=%d %d\n",
		        first, nelem, ncolors, w->gterm.ncolors);

	    /* Store the new colors. */
	    if (ncolors > 0)
		XStoreColors (w->gterm.display,
		    colormap, &w->gterm.color[first], ncolors);

	    /* Also attempt to store the colors in the default colortable,
	     * by allocating, writing, and then deallocating cells.  This
	     * helps keeps the gterm window visible when the default
	     * colormap is loaded.  To avoid excessive server queries the
	     * default colormap is only updated every so often.  Updating is
	     * disabled if cmapShadow is set to zero.  If shadowing is
	     * enabled the update is always performed if the WriteColormap
	     * occurs when the pointer is not in the window (e.g., when a
	     * button elsewhere in the GUI is pressed) as otherwise the
	     * change will not be visible as the private colormap will not
	     * be loaded by the window manager.
	     */
	    shadow = w->gterm.cmapShadow;
	    timeval = time((long *)NULL);

	    if (shadow && (!w->gterm.in_window ||
		    (timeval - w->gterm.cmapLastShadow > shadow * 1000))) {
		update_default_colormap (w);
		w->gterm.cmapLastShadow = timeval;
	    }
	}

	if (DBG_TRACE)
	    fprintf (stderr, "GtWriteColormap: PSEUDOCOLOR DONE\n");
	break;

    default:
	/* Set up a unitary, or one-to-one mapping, to preserve the input
	 * pixel values so that we can render them later.
	 */
	if (DBG_TRACE)
	    fprintf (stderr, "GtWriteColormap: visual default case\n");

unitary:
	colormap = get_colormap (w);

	if ((first+nelem) > MAX_SZCMAP)
	    break;

	if (w->gterm.useGlobalCmap) 
	    break;

	first = SZ_STATIC_CMAP;
	ncolors = min (w->gterm.maxColors, nelem);
	cp = &w->gterm.color[first];

	if (DBG_TRACE)
	    fprintf (stderr,
    	       "GtWriteColormap: unitary: first=%d nelem=%d -> ncolors=%d/%d\n",
		   first, nelem, ncolors, w->gterm.ncolors);

	for (i = 0;  i < ncolors;  i++, cp++) {
	    w->gterm.cmap[i] = i;
	    cp->pixel = i;
	    cp->red   = r[i];
	    cp->green = g[i];
	    cp->blue  = b[i];
	    cp->flags = (DoRed | DoGreen | DoBlue);

	    if (DBG_TRACE && DBG_CM_VERB)
	 	fprintf (stderr,
		    "GtWriteColormap: True: %3d:  (%3d, %3d, %3d)  %d / %d\n",
		    i, r[i]>>8, g[i]>>8, b[i]>>8, w->gterm.ncolors, ncolors);

	    if (i+1 > w->gterm.ncolors)
		w->gterm.ncolors = i + 1;
	}

	if (DBG_TRACE) {
	    fprintf (stderr, 
		"GtWriteColormap: map=%d first=%d nelem=%d ncol=%d %d\n",
		map, first, nelem, ncolors, w->gterm.ncolors);
	    fprintf (stderr, "GtWriteColormap: TRUECOLOR DONE\n");
	}

	break;
    }


    if (DBG_TRACE) {
	dbg_printCmaps (w);
	fprintf (stderr, "GtWriteColormap:  LEAVING\n");
    }

    return (OK);
}


/* GtReadColormap -- Return the color assignments for a region of the named
 * colormap.
 */
GtReadColormap (w, map, first, nelem, r, g, b)
    GtermWidget w;
    int map;
    int first;
    int nelem;
    ushort *r, *g, *b;
{
    register int i;
	
	
    if (DBG_TRACE)
	fprintf (stderr, "GtReadColormap: ENTER  map=%d first=%d nelem=%d\n",
 	    map, first, nelem);

    if (w->gterm.useGlobalCmap) {
	for (i=0; i < MAX_SZCMAP; i++) {
	    r[i] = global_color[i].red;
	    g[i] = global_color[i].green;
	    b[i] = global_color[i].blue;
	}
	return (SZ_STATIC_CMAP + SZ_DYNAMIC_CMAP + SZ_OVERLAY_CMAP);
    }


    /* Clear the output colormap.
    */
    for (i=0; i < MAX_SZCMAP; i++) r[i] = g[i] = b[i] = 0;

    if (map > 0) {
	/* Read from a colormap descriptor.
	 */
	register struct colormap *cm;
	register int i, j;

	/* Locate colormap. */
	for (cm = w->gterm.colormaps;  cm;  cm = cm->next)
	    if (cm->map == map)
		break;
	if (!cm)
	    return (0);

	/* Return RGB values. */
	for (i=0;  i < nelem;  i++) {
	    j = first + i;
	    if (j < cm->ncells) {
		r[i] = cm->r[j];
		g[i] = cm->g[j];
		b[i] = cm->b[j];
	    } else
		break;
	}

	return (i);

    } else {
	/* Read the display colormap.
	 */
	register XColor *cp;

	/* Return RGB values. */
	for (i=0;  i < nelem;  i++) {
	    if (first+i < w->gterm.ncolors) {

	    if (DBG_TRACE && DBG_CM_VERB)
		fprintf (stderr, "GtReadColormap: %3d    %3d %3d %3d\t",
    		    first+i, r[i]>>8, g[i]>>8, b[i]>>8);

		cp = &w->gterm.color[first+i];
		r[i] = (ushort) cp->red;
		g[i] = (ushort) cp->green;
		b[i] = (ushort) cp->blue;
	    } else
		break;
	    
	    if (DBG_TRACE && DBG_CM_VERB)
		fprintf (stderr,"%3d\t%3d %3d %3d\n",i,r[i]>>8,g[i]>>8,b[i]>>8);
	}

        if (DBG_TRACE)
	    fprintf (stderr, "GtReadColormap: LEAVING  ncolors=%d\n", i);
	return (i);
    }
}


/* GtLoadColormap -- Load a colormap into the display, optionally scaling
 * the colormap via a linear transformation in the process.  The colormap is
 * unaffected if offset=0.5, scale=1.0.  A negative scale inverts the image.
 * If map=0 the linear transformation is applied directly to the display
 * colormap.
 *
 * The offset refers to the center of the mapped region of the transfer
 * function, which is why the center value is at 0.5.  For example, if the
 * range of raster pixel intensities is normalized to the range 0.0 to 1.0,
 * then a transfer function of [offset=0.3,slope=3.0] will display the region
 * of intenstities centered around the normalized intenstity of 0.3, with a
 * contrast of 3.0 (the screen intensity changes 3 units for a unit change in
 * raster pixel intensity).  The transfer function [offset=0.3,slope=-3.0]
 * will display the same range of pixel intensitites, but with a negative
 * contrast.  The transfer function [offset=0.5,slope=1.0] has intercepts
 * of [0,0] and [1,1] hence it displays the full range of raster pixel
 * intensities - the input colormap is used as is, without resampling.
 */
GtLoadColormap (w, map, offset, slope)
    GtermWidget w;
    int map;
    float offset, slope;
{
    register int i;
    register XColor *cp;
    register struct colormap *cm;
    struct colormap d_cmap, o_cmap;
    int noscale, nelem, c1, c2;
    float x, y, z, frac;
    ushort r, g, b;


    if (DBG_TRACE)
	fprintf (stderr, "GtLoadColormap: map=%d offset=%f slope=%f\n",
	    map, offset, slope);

    /* Get the colormap to be loaded.
    */
    if (map == 0) {
	/* Create a dummy colormap struct from the screen colormap.
	*/
dummy:
	cm = &d_cmap;
	cm->map = 0;
	cm->next = NULL;
	cm->ncells = w->gterm.ncolors;
	for (i=0;  i < cm->ncells;  i++) {
	    cp = &w->gterm.color[i];
	    cm->r[i] = cp->red;
	    cm->g[i] = cp->green;
	    cm->b[i] = cp->blue;
	}

    } else {
	/* Locate colormap.
	*/
	for (cm = w->gterm.colormaps;  cm;  cm = cm->next)
	    if (cm->map == map)
		break;

        if (DBG_TRACE)
	    fprintf (stderr, "GtLoadColormap: map=%d/%d cm=0x%x ncells=%d\n", 
		map, cm->map, cm, cm->ncells);

	if (!cm)
	    return (ERR);
    }

    /* Compute the scaled colormap.  Only the dynamic part of the colormap
    ** is scaled, the static cells are not affected.
    */
    o_cmap.map = 0;
    o_cmap.ncells = cm->ncells;
    if (w->gterm.useGlobalCmap)
	nelem = SZ_DYNAMIC_CMAP;
    else
	nelem = cm->ncells - SZ_STATIC_CMAP;
    noscale = (abs(offset - 0.5) < 0.0001 && abs(slope - 1.0) < 0.0001);

    if (noscale) {
	for (i=0;  i < nelem;  i++) {
	    o_cmap.r[i] = cm->r[SZ_STATIC_CMAP+i];
	    o_cmap.g[i] = cm->g[SZ_STATIC_CMAP+i];
	    o_cmap.b[i] = cm->b[SZ_STATIC_CMAP+i];
	}
    } else {
        if (DBG_TRACE)
	    fprintf (stderr,"GtLoadColormap: scaling cmap; nelem=%d\n", nelem);

	for (i=0;  i < nelem;  i++) {
	    x = (float)i / (float)(nelem - 1);
	    y = (x - offset) * slope + 0.5;

	    if (y <= 0.0) {
		r = cm->r[SZ_STATIC_CMAP];
		g = cm->g[SZ_STATIC_CMAP];
		b = cm->b[SZ_STATIC_CMAP];
	    } else if (y >= 1.0) {
		r = cm->r[cm->ncells-1];
		g = cm->g[cm->ncells-1];
		b = cm->b[cm->ncells-1];
	    } else {
		z = y * (nelem - 1) + SZ_STATIC_CMAP;
		if (w->gterm.cmapInterpolate) {
		    c1 = (int)z;
		    c2 = min (cm->ncells-1, c1 + 1);
		    frac = z - c1;
		    r = cm->r[c1] * (1.0 - frac) + cm->r[c2] * frac;
		    g = cm->g[c1] * (1.0 - frac) + cm->g[c2] * frac;
		    b = cm->b[c1] * (1.0 - frac) + cm->b[c2] * frac;
		} else {
		    c1 = (int)z;
		    r = cm->r[c1];
		    g = cm->g[c1];
		    b = cm->b[c1];
		}
	    }

	    o_cmap.r[i] = r;
	    o_cmap.g[i] = g;
	    o_cmap.b[i] = b;
	}
    }

    if (w->gterm.useGlobalCmap) {
	for (i=0; i < nelem; i++) {
	    global_color[i+SZ_STATIC_CMAP].red   = o_cmap.r[i];
	    global_color[i+SZ_STATIC_CMAP].green = o_cmap.g[i];
	    global_color[i+SZ_STATIC_CMAP].blue  = o_cmap.b[i];
	}
	global_color[i+SZ_STATIC_CMAP-1].red   = o_cmap.r[i] = o_cmap.r[i-2];
	global_color[i+SZ_STATIC_CMAP-1].green = o_cmap.g[i] = o_cmap.g[i-2];
	global_color[i+SZ_STATIC_CMAP-1].blue  = o_cmap.b[i] = o_cmap.b[i-2];
	nelem = SZ_DYNAMIC_CMAP;
	valid_lut = 0;
    } 

    /* Load the colormap into the display. 
    */
    if (DBG_TRACE)
	fprintf (stderr, "GtLoadColormap: loading colormap into display\n");
    GtWriteColormap (w, 0, SZ_STATIC_CMAP, nelem,
	o_cmap.r, o_cmap.g, o_cmap.b);


    /* If the colormap we loaded to the display was the display colormap,
    ** restore the original unscaled colors in the gterm descriptor so that
    ** we won't be scaling a scaled colormap in the next operation.
    */
    if (map == 0) {
	for (i=SZ_STATIC_CMAP;  i < cm->ncells;  i++) {
	    cp = &w->gterm.color[i];
	    cp->red   = cm->r[i];
	    cp->green = cm->g[i];
	    cp->blue  = cm->b[i];
	}
    }

    if (w->gterm.useGlobalCmap) {
	Mapping mp;

        /* Execute any mappings that reference this raster.
	*/
        for (mp = w->gterm.mp_head;  mp;  mp = mp->next) {
            if (mp->enabled) {
        	struct mapping *map=mp, p_mp;

                if (map->st != GtPixel || map->dt != GtPixel) {
                    initialize_mapping (&p_mp);
                    get_pixel_mapping (w, map, &p_mp, 1);
                    update_mapping (w, map = &p_mp);
                } else
                    update_mapping (w, map);

		if (colormap_focus > 0 && colormap_focus < mp->snx) {
		    /* If we're doing a focused update, refresh only the
		    ** central part of the main display.
		    */
		    int half = colormap_focus;
		    int full = ((half * 2 > 512) ? 512 : half * 2);

		    /* refresh_source (w, map, 
		        mp->sx+(mp->snx/2 - 64), mp->sy+(mp->sny/2 - 64), 
		        128, 128); */

		    refresh_source (w, map, 
		        mp->sx+(mp->snx/2 - half), mp->sy+(mp->sny/2 - half), 
		        full, full);
		} else {
		    refresh_source (w, map, mp->sx, mp->sy, mp->snx, mp->sny);
		}

                if (map == &p_mp)
                    free_mapping (w, map);

		/* if (colormap_focus) break; */
            }
        }
    }

    return (OK);
}

GtSetColormapFocus (int box_size)
{
    if (box_size != 0)
        colormap_focus = ((box_size > 0 && box_size < 64) ? 64 : box_size);
}


/* GtQueryColormap -- Return information on the size and state of a colormap.
 */
GtQueryColormap (w, map, first, nelem, maxelem)
    register GtermWidget w;
    int map;
    int *first, *nelem, *maxelem;
{
    register struct colormap *cm;
    int nitems;

    if (first)
	*first = SZ_STATIC_CMAP;
    if (nelem)
	*nelem = 0;
    if (maxelem)
	*maxelem = min (w->gterm.maxColors, MAX_SZCMAP) - SZ_STATIC_CMAP;

    if (w->gterm.useGlobalCmap) {
	*first  = SZ_STATIC_CMAP;
	*nelem  = SZ_STATIC_CMAP + SZ_DYNAMIC_CMAP + SZ_OVERLAY_CMAP;
	*maxelem = SZ_STATIC_CMAP + SZ_DYNAMIC_CMAP + SZ_OVERLAY_CMAP;

    } else if (map > 0) {
	for (cm = w->gterm.colormaps;  cm;  cm = cm->next)
	    if (cm->map == map)
		break;
	if (!cm)
	    return (0);

	if (nelem)
	    *nelem = cm->ncells - SZ_STATIC_CMAP;

    } else {
	if (nelem)
	    *nelem = w->gterm.ncolors - SZ_STATIC_CMAP;
	if (maxelem) {
	    nitems = min (MAX_SZCMAP, CellsOfScreen(w->gterm.screen));
	    *maxelem = min (nitems,
		min (w->gterm.maxColors, MAX_SZCMAP - w->gterm.base_pixel));
	}	

    }
    if (DBG_TRACE)
	fprintf (stderr,
	    "GtQueryColormap: map=%d -> first=%d nelem=%d max=%d\n",
		map, *first, *nelem, *maxelem);

    return (1);
}


/* GtNextColormap -- Return a unique colormap number.
 */
GtNextColormap (w)
    register GtermWidget w;
{
    register struct colormap *cm;
    register int mapno = 0;

    /* Get the next map number. */
    for (cm = w->gterm.colormaps;  cm;  cm = cm->next)
	if (cm->map > mapno)
	    mapno = cm->map;

    return (mapno + 1);
}


/* GtFreeColormap -- Free a colormap descriptor.
 */
GtFreeColormap (w, colormap)
    register GtermWidget w;
    int colormap;
{
    register struct colormap *p_cm, *cm;

    /* Find the colormap and free it. */
    for (p_cm = NULL, cm = w->gterm.colormaps;  cm;  p_cm = cm, cm = cm->next)
	if (cm->map == colormap) {
	    if (p_cm)
		p_cm->next = cm->next;
	    else
		w->gterm.colormaps = cm->next;
	    XtFree ((char *)cm);
	    return;
	}
}


/* GtWriteIomap -- An iomap is an optional lookup table used to isolate the
 * client application from the color model used within the Gterm widget.
 * To simplify color allocation the Gterm widget defines a logical color
 * space where color 0 is the background, 1 the foreground, 2-N are statically
 * allocated standard colors, and colors N+1 and above are dynamically
 * allocated by the graphics application.  Less-demanding applications use
 * only the statically allocated, shared colors.  The widget internally maps
 * these logical colors to whatever the window system requires, but providing
 * a well-defined logical color space isolates the client from the details of
 * color allocation in the underlying window system.
 *
 * An iomap can be used to define a mapping between the color model of the
 * client application and the Gterm color model (when we say color model here
 * we mean color allocation schemes for 8 bit pseudocolor).  By default the
 * iomap is one-to-one.  The use of an iomap frees the client from having to
 * worry about color index translations, and allows color tables to be
 * combined in the widget for greater efficiency when color tables are serially
 * applied.  The iomap applies to all color indices or pixel values passed
 * in i/o operations between the client and the Gterm widget.
 */
GtWriteIomap (w, iomap, first, nelem)
    register GtermWidget w;
    ushort *iomap;
    int first, nelem;
{
    register int c1, c2;

    if (w->gterm.useGlobalCmap)
	return;

    c1 = max(0, min(MAX_SZCMAP-1, first));
    c2 = max(0, min(MAX_SZCMAP-1, first + nelem - 1));

    if (DBG_TRACE)
	fprintf (stderr,"GtWriteIomap: first=%d nelem=%d -> c1=%d c2=%d (%d)\n",
	    first, nelem, c1, c2, (c2-c1+1));

    nelem = c2 - c1 + 1;

    memmove (&w->gterm.iomap[c1], iomap, nelem * sizeof(ushort));
    invalidate_cmap (w);

    if (DBG_IOMAP) {
	int i;
 	for (i=0; i < MAX_SZCMAP; i++) 
    	    fprintf (stderr, "iomap[%3d] = %d\n", i, w->gterm.iomap[i]);
    }
}


/* GtReadIomap -- Read back the contents of the iomap.
 */
GtReadIomap (w, iomap, first, nelem)
    register GtermWidget w;
    uchar *iomap;
    int first, nelem;
{
    register int c1, c2;

    c1 = max(0, min(MAX_SZCMAP-1, first));
    c2 = max(0, min(MAX_SZCMAP-1, first + nelem - 1));
    nelem = c2 - c1 + 1;

    memmove (iomap, &w->gterm.iomap[c1], nelem * sizeof(ushort));
}


/* GtReadLUT -- Read back the contents of the global LUT.
 */
GtReadLUT (w, lut, first, nelem)
    register GtermWidget w;
    unsigned long *lut;
    int first, nelem;
{
    register int c1, c2;

    c1 = max(0, min(MAX_SZCMAP-1, first));
    c2 = max(0, min(MAX_SZCMAP-1, first + nelem - 1));
    nelem = c2 - c1 + 1;

    memmove (lut, &global_lut[c1], nelem * sizeof(unsigned long));
}


/* init_iomap -- Initialize the iomap and the cmap cache.
 */
static void
init_iomap (w)
    GtermWidget w;
{
    register ushort *iomap = w->gterm.iomap;
    register int i;

    for (i=0;  i < MAX_SZCMAP;  i++)
	iomap[i] = i;
    invalidate_cmap (w);
}

/* init_global_map -- Initialize the global cmap;
 */
static void
init_global_cmap ()
{
    register int i;

    for (i=0;  i < MAX_SZCMAP;  i++)
	global_cmap[i] = (Pixel) i;
}

/* invalidate_cmap -- Invalidate the cmap cache.
 */
static void
invalidate_cmap (w)
    register GtermWidget w;
{
    w->gterm.cmap_in_valid = w->gterm.cmap_out_valid = 0;
}


/* get_cmap_in -- Get the combined input colormap, used to transform color
 * values received from the client to window system color indices.
 */
static Pixel *
get_cmap_in (w)
    register GtermWidget w;
{
    register Pixel *cmap, *cmap_in = w->gterm.cmap_in;
    register ushort *iomap;
    register int i, j;
    int ncolors;


    if (w->gterm.useGlobalCmap)
	return (global_cmap);

    if (DBG_TRACE && DBG_CM_VERB)
	fprintf (stderr, "get_cmap_in: valid=%d  ncolors=%d\n", 
	    w->gterm.cmap_in_valid, (w->gterm.ncolors - SZ_STATIC_CMAP));

    if (w->gterm.cmap_in_valid)
	return (cmap_in);

    cmap    = w->gterm.cmap;
    iomap   = w->gterm.iomap;
    ncolors = w->gterm.ncolors - SZ_STATIC_CMAP;

    /* If ncolors is small wrap around so that pixel values stay within
     * the mapped range of output pixels.
     */
    for (i=0;  i < MAX_SZCMAP;  i++) {
	j = iomap[i];
	if (j > SZ_STATIC_CMAP && ncolors)
	    j = ((j - SZ_STATIC_CMAP) % ncolors) + SZ_STATIC_CMAP;
	cmap_in[i] = cmap[j];
    }

    w->gterm.cmap_in_valid++;
    return (cmap_in);
}


/* get_cmap_out -- Get the combined output colormap, used to transform window
 * system color indices to the color system of the client.  Note that this is
 * not necessarily a uniquely defined invertible transformation.
 */
static Pixel *
get_cmap_out (w)
    GtermWidget w;
{
    register Pixel *cmap;
    register ushort *iomap;
    Pixel *cmap_out = w->gterm.cmap_out;
    register int pixel, i;
    int j;


    if (w->gterm.useGlobalCmap)
	return (global_cmap);

    if (DBG_TRACE && DBG_CM_VERB)
	fprintf (stderr, "get_cmap_out: valid=%d  ncolors=%d\n", 
	    w->gterm.cmap_out_valid, (w->gterm.ncolors - SZ_STATIC_CMAP));

    if (w->gterm.cmap_out_valid)
	return (cmap_out);

    cmap  = w->gterm.cmap;
    iomap = w->gterm.iomap;

    /* Invert the two colormaps.  This is not very efficient, but we don't
     * have to do this very often (a GtReadPixels call is about the only
     * case, and even then the map is cached).
     */
    for (j=0;  j < MAX_SZCMAP;  j++) {
	pixel = j;

	/* Lookup display pixel in cmap. */
	for (i=0;  i < MAX_SZCMAP;  i++)
	    if (cmap[i] == pixel) {
		pixel = i;
		break;
	    }
	if (i >= MAX_SZCMAP) {
	    cmap_out[j] = 0;
	    continue;
	}

	/* Lookup cmap pixel in iomap. */
	if (iomap[pixel] != pixel) {
	    for (i=0;  i < MAX_SZCMAP;  i++)
		if (iomap[i] == pixel) {
		    pixel = i;
		    break;
		}
	    if (i >= MAX_SZCMAP) {
		cmap_out[j] = 0;
		continue;
	    }
	}

	cmap_out[j] = pixel;
    }

    w->gterm.cmap_out_valid++;
    return (cmap_out);
}


/* get_pixel -- Convert a client color index into a display pixel.
 */
static Pixel
get_pixel (w, client_pixel)
    register GtermWidget w;
    register int client_pixel;
{
    register Pixel *cmap = get_cmap_in (w);

    if (client_pixel < 0 || client_pixel >= MAX_SZCMAP)
	return (w->gterm.cmap[1]);
    else
	return (cmap[client_pixel]);
}


/* GtGetClientPixel -- Convert a gterm pixel into a client pixel.
 */
GtGetClientPixel (w, pixel)
    GtermWidget w;
    register int pixel;
{
    register int i;
    register ushort *iomap;
    int client_pixel = 0;

    get_cmap_in (w);
    iomap = w->gterm.iomap;

    for (i=0;  i < MAX_SZCMAP;  i++)
	if (iomap[i] == pixel) {
	    client_pixel = i;
	    break;
	}

    return (client_pixel);
}


/* GtInitMappings -- Delete all mappings and initialize the mapping subsystem.
 */
GtInitMappings (w)
    register GtermWidget w;
{
    register Mapping mp;
    register int i;

    invalidate_draw_context (w);

    /* Free any mapping storage. */
    if (w->gterm.mappings) {
	for (i=0;  i < w->gterm.maxMappings;  i++) {
	    mp = &w->gterm.mappings[i];
	    if (mp->defined)
		free_mapping (w, mp);
	}
	XtFree ((char *)w->gterm.mappings);
	w->gterm.mp_head = NULL;
	w->gterm.mp_tail = NULL;
    }

    /* Allocate the initially empty mapping descriptors. */
    w->gterm.mappings =
	(Mapping) XtCalloc (w->gterm.maxMappings, sizeof (struct mapping));

    for (i=0;  i < w->gterm.maxMappings;  i++) {
	mp = &w->gterm.mappings[i];
	mp->mapping = i;
    }

    w->gterm.nmappings = 0;
}


/* GtNextMapping -- Return the index of the next available mapping descriptor.
 * This routine always returns a mapping index of 1 or higher.
 */
GtNextMapping (w)
    register GtermWidget w;
{
    register Mapping mp;
    register int i;

    for (i=1;  i < w->gterm.maxMappings;  i++) {
	mp = &w->gterm.mappings[i];
	if (!mp->defined)
	    return (i);
    }

    return (-1);
}


/* GtFreeMapping -- Free a mapping descriptor.
 */
GtFreeMapping (w, mapping)
    register GtermWidget w;
    int mapping;
{
    free_mapping (w, &w->gterm.mappings[mapping]);
}


/* GtRaiseMapping -- Set the stacking order of a mapping to one level
 * higher than the reference mapping.  If no reference mapping is given
 * the mapping is raised to the top of the stacking order.
 */
GtRaiseMapping (w, mapping, reference)
    register GtermWidget w;
    int mapping, reference;
{
    register Mapping mp, ref_mp;

    mp = &w->gterm.mappings[mapping];
    if (!mp->defined)
	return;

    if (reference <= 0 || reference >= w->gterm.maxMappings)
	ref_mp = w->gterm.mp_tail;
    else
	ref_mp = &w->gterm.mappings[reference];

    /* Already on top? */
    if (mp == w->gterm.mp_tail)
	return;

    mp_unlink (w, mp);
    mp_linkafter (w, mp, ref_mp);
}


/* GtLowerMapping -- Change the stacking order of a mapping relative to another
 * mapping, causing the first mapping to be drawn below the second.
 */
GtLowerMapping (w, mapping, reference)
    register GtermWidget w;
    int mapping, reference;
{
    register Mapping mp, ref_mp;

    mp = &w->gterm.mappings[mapping];
    if (!mp->defined)
	return;

    if (reference <= 0 || reference >= w->gterm.maxMappings)
	ref_mp = NULL;
    else
	ref_mp = &w->gterm.mappings[reference];

    /* Already lowered? */
    if (mp == w->gterm.mp_head)
	return;

    /* Lower it. */
    mp_unlink (w, mp);
    if (ref_mp && ref_mp->prev)
	mp_linkafter (w, mp, ref_mp->prev);
    else {
	mp->next = w->gterm.mp_head;
	w->gterm.mp_head = mp;
	if (mp->next)
	    mp->next->prev = mp;
	if (!w->gterm.mp_tail)
	    w->gterm.mp_tail = mp;
    }
}


/* GtCompareMappings -- Compare the stacking order of two mappings.  A
 * negative value is returned if the m1 < m2, zero is returned if the
 * mappings are the same, and a positive value is returned if m1 > m2.
 */
GtCompareMappings (w, map1, map2)
    register GtermWidget w;
    int map1, map2;
{
    register Mapping mp, mp1, mp2;

    if (map1 == map2)
	return (0);

    mp1 = &w->gterm.mappings[map1];
    mp2 = &w->gterm.mappings[map2];

    for (mp = w->gterm.mp_head;  mp;  mp = mp->next)
	if (mp == mp1)
	    return (-1);
	else if (mp == mp2)
	    return (1);

    return (0);
}


/* GtSelectRaster -- Select the raster which maps to the given raster pixel,
 * and transform the coordinates back to the source raster.  The raster number
 * and the raster coordinates of the source raster are returned.  If no raster
 * maps to the given pixel, raster=src and source raster coordinates are
 * returned.
 *
 * The raster pixel coordinate system is best explained by an example.
 * Suppose we have a 10x10 raster mapped into a 500x500 window.  The
 * window pixel 0 on an axis has raster pixel coordinate 0.0; pixel 500
 * (which is outside the window) has raster pixel coordinate 10.0.  The
 * coordinates correspond to the edge of the pixel as displayed in the
 * window, i.e., the left edge of the (nonflipped) window is at x=0.0, and
 * the right edge at x=10.0.  Due to the pixelization of the screen, the
 * maximum value is a limit which is only approached as the magnification
 * increases.
 *
 * The client application may have a different coordinate system than the
 * above.  For example, if the client wants an integer pixel value to be
 * at the center of a pixel, the first pixel has the coordinate [1,1], and
 * the raster is 10 pixels wide, the client coordinate system would range
 * from 0.5 to 10.5 at the edges of the NDC space.
 */
GtSelectRaster (w, dras, dt, dx, dy, rt, rx, ry, rmap)
    GtermWidget w;
    int dras;		/* display raster */
    int dt;		/* coordinate type of input coords */
    int dx, dy;		/* display raster coordinates */
    int rt;		/* coordinate type for output */
    int *rx, *ry;	/* raster coordinates (output) */
    int *rmap;		/* mapping selected */
{
    register Mapping mp;
    float x, y, x2, y2;
    int raster, mapping;

    /* Get display raster pixel coordinates. */
    if (dt != GtPixel) {
	struct mapping sv_mp, p_mp;
	initialize_mapping (&sv_mp);				/* MF035 */
	save_mapping (&sv_mp, 0, 0,
	    0,  0, 0,0,0,0,
	    0, dt, dx,dy,0,0);
	get_pixel_mapping (w, &sv_mp, &p_mp, 0);

	dx = p_mp.dx;
	dy = p_mp.dy;
    }

    /* Search for a mapping which maps to this pixel.  The mapping we are
     * looking for is the mapping closest to the tail of the mapping list
     * (highest stacking order) which is defined and enabled and which
     * includes the given display raster pixel in its destination rect.
     */
    for (mp = w->gterm.mp_tail, mapping = -1;  mp;  mp = mp->prev) {
	if (mp->defined && mp->enabled && mp->dst == dras) {
	    struct mapping *map, p_mp;
	    int dnx, dny;

	    get_pixel_mapping (w, mp, &p_mp, 0);
	    map = &p_mp;

	    if ((dnx = map->dnx) < 0)
		dnx = -dnx;
	    if ((dny = map->dny) < 0)
		dny = -dny;

	    /* Is display raster pixel in destination rect for this mapping?
	     */
	    if (dnx > 0 && dx >= map->dx && dx < map->dx + dnx &&
		dny > 0 && dy >= map->dy && dy < map->dy + dny) {

		/* Compute offset into destination rect and apply axis flip
		 * if any from mapping.
		 */
		x = dx - map->dx + 0.5;
		if (map->dnx < 0)
		    x = dnx - x;
		y = dy - map->dy + 0.5;
		if (map->dny < 0)
		    y = dny - y;

		/* Compute the source raster coordinates corresponding to
		 * the given display pixel.  This is done in floating point
		 * to permit fractional pixel resolution if the mapping
		 * zooms the raster.
		 */
		x = x * (float)map->snx / (float)dnx + map->sx;
		y = y * (float)map->sny / (float)dny + map->sy;

		mapping = map->mapping;
		raster = map->src;
		x2 = w->gterm.rasters[raster].width;
		y2 = w->gterm.rasters[raster].height;

		break;
	    }
	}
    }

    /* Return display raster coordinates if no mapped raster was found.
     */
    if (mapping < 0) {
	x = dx;  y = dy;
	x2 = (int)w->core.width - 1;
	y2 = (int)w->core.height - 1;
	raster = dras;
    }

    /* Output coordinates of the requested type.  The increased resolution
     * of NDC coordinates allows fractional pixel coordinates to be returned
     * (e.g. 1/32 of a pixel for a 1K raster).
     */
    if (rt == GtPixel) {
	*rx = x;
	*ry = y;
    } else {
	*rx = (     x) / x2 * MAXNDC;
	*ry = (y2 - y) / y2 * MAXNDC;	/* NDC is flipped in Y */
    }

    *rmap = mapping;
    return (raster);
}


/* GtCopyRaster -- Copy a region of the source raster to a region of the
 * destination raster.  If the input and output regions are not the same
 * size the subimage is automatically scaled to fit the destination region.
 * If the destination extent DNX or DNY is negative, the image is flipped in
 * that axis.  The type of spatial scaling performed is determined by the
 * scale factors (zoom, dezoom, or no scaling).  The rasterop argument is
 * used to exercise fine control over how the mapping is performed, e.g., to
 * force a refresh, implement a transient mapping, or in the case of a dezoom
 * (many-to-one) mapping, select the antialiasing technique to be used.
 */
GtCopyRaster (w, rop, src,st,sx,sy,snx,sny, dst,dt,dx,dy,dnx,dny)
    GtermWidget w;
    int rop;			/* rasterop */
    int src;			/* 0=window, >0 = raster number */
    int st;			/* coordinate type for source raster */
    int sx,sy,snx,sny;		/* source raster */
    int dst;			/* 0=window, >0 = raster number */
    int dt;			/* coordinate type for destination raster */
    int dx,dy,dnx,dny;		/* destination raster */
{
    struct mapping sv_mp, p_mp;				/* MF007 */
    int status;


    if (DBG_TRACE)
	fprintf(stderr, "GtCopyRaster() -- ENTER\n");

    if (!w || !XtIsRealized ((Widget)w))
	return (OK);

    /* Construct a temporary mapping describing the desired raster copy. */
    initialize_mapping (&sv_mp);				/* MF035 */
    save_mapping (&sv_mp, w->gterm.maxMappings, 0,
	src,st,sx,sy,snx,sny, dst,dt,dx,dy,dnx,dny);
    initialize_mapping (&p_mp);
    get_pixel_mapping (w, &sv_mp, &p_mp, 1);
    update_mapping (w, &p_mp);

    /* Refresh the destination pixels. */
    status = refresh_destination (w, &p_mp, dx, dy, abs(dnx), abs(dny));

    /* Discard the temporary mapping. */
    free_mapping (w, &p_mp);


    if (DBG_TRACE)
	fprintf(stderr, "GtCopyRaster() -- RETURNING\n");

    return (status);
}


/* GtSetMapping -- Define a new mapping function, or modify an old one.
 * If a new mapping is defined it is merely enabled, and no refreshing
 * of the screen takes place until either some mapped data is written
 * to or the mapping is explicitly refreshed.  If an existing mapping is
 * modified the old and new mappings are examined and only those portions
 * of the destination rect for which the mapping changed are updated.
 * This permits minor changes to a mapping (e.g. moving an edge) without
 * having to redraw the entire region.  Regions of the destination drawable
 * which were previously covered by the mapping but which were exposed by
 * modifying the mapping are redrawn.
 */
GtSetMapping (w, mapping, rop, src,st,sx,sy,snx,sny, dst,dt,dx,dy,dnx,dny)
    GtermWidget w;
    int mapping;		/* mapping number */
    int rop;			/* rasterop */
    int src;			/* 0=window, >0 = raster number */
    int st;			/* coordinate type for source raster */
    int sx,sy,snx,sny;		/* source raster */
    int dst;			/* 0=window, >0 = raster number */
    int dt;			/* coordinate type for source raster */
    int dx,dy,dnx,dny;		/* destination raster */
{
    register int i, j;
    register Mapping mp, o_mp, n_mp;
    struct mapping pix_mp, new_mp;
    int defined, scale_changed, offset, current, state, old_i;
    int nx, xs[MAX_REGIONS], xe[MAX_REGIONS], xv[MAX_REGIONS];
    int ny, ys[MAX_REGIONS], ye[MAX_REGIONS], yv[MAX_REGIONS];
    int n_dnx, n_dny, n_xflip=0, n_yflip=0, i1, i2;
    int o_dnx, o_dny, o_xflip=0, o_yflip=0;
    int *o_xymap, *o_xmap, *o_ymap;
    int *n_xymap, *n_xmap, *n_ymap;
    int dummy_rop;						/* MF011 */
    XRectangle rl[MAX_REGIONS];
    int nrect, buflen, refresh;


    if (DBG_TRACE)
	fprintf (stderr, "GtSetMapping - ENTER\n");

    /* Check mapping number in range. */
    if (mapping < 0 || mapping >= w->gterm.maxMappings)
	return (ERR);
    else
	mp = &w->gterm.mappings[mapping];

    invalidate_draw_context (w);
    initialize_mapping (&pix_mp);
    initialize_mapping (&new_mp);

    /* Get local pixel space copy of old mapping, store new mapping. */
    get_pixel_mapping (w, mp, &pix_mp, 1);
    mp->src = src;  mp->st = st;
	mp->sx = sx;  mp->sy = sy;  mp->snx = snx;  mp->sny = sny;
    mp->dst = dst;  mp->dt = dt;
	mp->dx = dx;  mp->dy = dy;  mp->dnx = dnx;  mp->dny = dny;
    mp->rop = (rop & ~(R_RefreshNone|R_RefreshAll));
    mp->updated = 0;

    /* Newly defined mappings are linked at the tail of the mapping list,
     * i.e. they stack (display) on top of any other mappings.  If the
     * mapping is already defined the stacking order is not changed.
     */
    if (!(defined = mp->defined)) {
	mp_linkafter (w, mp, w->gterm.mp_tail);
	mp->defined = 1;
    }

    if (!valid_mapping (w, mp)) {
	mp_unlink (w, mp);
	mp->defined = 0;
	return (ERR);
    }
    update_mapping (w, mp);

    /* If we are defining a new mapping just define it and quit, without
     * refreshing the window, unless R_RefreshAll is explicitly set in the
     * mapping.  If the mapping is not enabled merely store the new mapping.
     * If the mapping is a null mapping (no pixels) do nothing.  If refresh
     * is disabled in the rasterop merely store the new mapping.  If we are
     * editing an existing mapping which is enabled with the default rasterop,
     * we continue on to compare the old and new mappings and refresh any
     * changed pixels in the destination rect.
     */
    if (!defined || src != mp->src || dst != mp->dst) {
	mp->enabled = mp->defined = 1;
	mp->refresh = 0;
	return (OK);
    } else if (!mp->enabled) {
	return (OK);
    } else if (snx == 0 || sny == 0 || dnx == 0 || dny == 0)
	return (OK);

    if (rop & R_RefreshNone)
	return (OK);

    /* Convert input mappings to pixel coordinates, we deal with only pixel
     * coordinates from here on.
     */
    get_pixel_mapping (w, mp, &new_mp, 1);
    load_mapping (&new_mp, &mapping, &dummy_rop,		/* MF011 */
	&src,&st,&sx,&sy,&snx,&sny, &dst,&dt,&dx,&dy,&dnx,&dny);
    update_mapping (w, n_mp = &new_mp);
    update_mapping (w, o_mp = &pix_mp);

    /*
     * We are editing an existing mapping.  Determine what has changed in
     * the mapping and refresh the changed regions.
     */

    /* Get old XY scaling maps.
     */
    o_xmap = o_mp->x_srcpix;
    o_ymap = o_mp->y_srcpix;

    if ((o_dnx = o_mp->dnx) < 0) {
	o_dnx = -o_dnx;
	o_xflip = 1;
    }
    if ((o_dny = o_mp->dny) < 0) {
	o_dny = -o_dny;
	o_yflip = 1;
    }

    /* Get new XY scaling maps.
     */
    n_xmap = n_mp->x_srcpix;
    n_ymap = n_mp->y_srcpix;

    if (dnx < 0) {
	dnx = -dnx;
	n_xflip = 1;
    }
    if (dny < 0) {
	dny = -dny;
	n_yflip = 1;
    }

    /* Refresh the entire region if the refresh flag is set, a flip has
     * occurred, or we are doing a complex dezoom mapping.
     */
    refresh = (o_mp->refresh || (rop & R_RefreshAll));
    if (n_xflip != o_xflip || n_yflip != o_yflip)
	refresh = 1;
    if (n_mp->scaling == M_DEZOOM)
	refresh = 1;

    /* Has the spatial scale changed? */
    scale_changed =
	abs (o_mp->xscale - n_mp->xscale) > 1.0E-4 ||
	abs (o_mp->yscale - n_mp->yscale) > 1.0E-4;

    /* If any of the above conditions are true refresh the entire mapping,
     * otherwise compare the old and new mappings and refresh any subregions
     * which have changed.
     */
    if (refresh || scale_changed || n_mp->scaling == M_DEZOOM) {
	refresh_destination (w, n_mp, dx, dy, dnx, dny);

    } else {
	/* Compare the old and new mappings to see what needs to be
	 * refreshed.  For speed reasons we only want to refresh the pixels
	 * which have been remapped.  Any destination pixel in the new mapping
	 * which does not map to the same source pixel as in the old mapping
	 * must be refreshed.  We examine each X and Y coordinate in the new
	 * destination rect and see if the source coordinate this maps to is
	 * the same as in the old mapping.  If a given destination pixel [i,j]
	 * maps to the same pixel [i,j] in the source as it did in the
	 * previous mapping, we do not need to refresh that pixel.  We examine
	 * the X and Y axis in turn and build up a list of regions which do or
	 * do not need to be refreshed.
	 */

	/* Get a list of ranges {XS,XE,XV} in X. */
	nx = get_regions (xs,xe,xv, MAX_REGIONS,
	    dx, dnx, n_xmap, o_mp->dx, o_dnx, o_xmap);

	/* Get a list of ranges {YS,YE,YV} in Y. */
	ny = get_regions (ys,ye,yv, MAX_REGIONS,
	    dy, dny, n_ymap, o_mp->dy, o_dny, o_ymap);

	/* The list of ranges in X and Y together define a raster of arbitary
	 * sized subrectangles filling the destination rect.  If the state
	 * value is nonzero (bit 1 set) in either X or Y, the subrectangle
	 * must be refreshed.  The get_rects routine returns a list of the
	 * rectangular subregions matching the given condition (bit 1 set in
	 * either axis).  Adjacent rectangles are merged to minimize the
	 * number of calls to refresh_destination.
	 */
	nrect = get_rects (rl, MAX_REGIONS, xs,xe,xv,nx, ys,ye,yv,ny, 1,1);
	for (i=0;  i < nrect;  i++)
	    refresh_destination (w, n_mp,
		rl[i].x, rl[i].y, rl[i].width, rl[i].height);
    }

    /* Refresh any lower level mappings exposed when the current mapping was
     * modified.  These will be regions of the old rect not present in the
     * new, modified rect for the current mapping.
     */
    nx = get_regions (xs,xe,xv, MAX_REGIONS,
	o_mp->dx, o_dnx, o_xmap, dx, dnx, n_xmap);
    ny = get_regions (ys,ye,yv, MAX_REGIONS,
	o_mp->dy, o_dny, o_ymap, dy, dny, n_ymap);
    nrect = get_rects (rl, MAX_REGIONS, xs,xe,xv,nx, ys,ye,yv,ny, 2,2);

    for (i=0;  i < nrect;  i++) {
	XRectangle r, in;
	Mapping mp;

	/* Clear the uncovered region. */
	GtSetPixels (w, dst, GtPixel, rl[i].x, rl[i].y, rl[i].width,
	    rl[i].height, GtGetClientPixel(w,0), 0);

	/* Refresh any lower level mappings the destination rects of
	 * which intersect the uncovered region.
	 */
	for (mp = w->gterm.mp_head;  mp && mp->mapping != mapping;
	    mp = mp->next) {

	    if (mp->enabled && mp->dst == dst) {
		r.x = mp->dx;
		r.y = mp->dy;
		r.width = mp->dnx;
		r.height = mp->dny;

		if (rect_intersect (&in, &r, &rl[i]))
		    refresh_destination (w, mp,
			in.x, in.y, in.width, in.height);
	    }
	}
    }

    free_mapping (w, n_mp);
    free_mapping (w, o_mp);
    mp = &w->gterm.mappings[mapping];
    mp->refresh = 0;

    if (DBG_TRACE)
	fprintf (stderr, "GtSetMapping - LEAVING\n");

    return (OK);
}


/* GtGetMapping -- Return the external parameters of a mapping.  If the
 * numberd mapping is undefined -1 is returned; 0 is returned if the
 * mapping is defined but not enabled, and 1 is returned if the mapping
 * is active.
 */
GtGetMapping (w, mapping, rop, src,st,sx,sy,snx,sny, dst,dt,dx,dy,dnx,dny)
    GtermWidget w;
    int mapping;		/* mapping number */
    int *rop;			/* rasterop */
    int *src;			/* 0=window, >0 = raster number */
    int *st;			/* coordinate type for source raster */
    int *sx,*sy,*snx,*sny;	/* source raster */
    int *dst;			/* 0=window, >0 = raster number */
    int *dt;			/* coordinate type for source raster */
    int *dx,*dy,*dnx,*dny;	/* destination raster */
{
    register Mapping mp;

    if (mapping < 0 || mapping >= w->gterm.maxMappings)
	return (-1);
    else if (!(mp = &w->gterm.mappings[mapping])->defined)
	return (-1);

    *rop = mp->rop;
    *src = mp->src;  *st = mp->st;
	*sx = mp->sx;  *sy = mp->sy;  *snx = mp->snx;  *sny = mp->sny;
    *dst = mp->dst;  *dt = mp->dt;
	*dx = mp->dx;  *dy = mp->dy;  *dnx = mp->dnx;  *dny = mp->dny;

    return (mp->enabled != 0);
}


/* GtActiveMapping -- Query whether a mapping is active.
 */
GtActiveMapping (w, mapping)
    register GtermWidget w;
    int mapping;		/* mapping number */
{
    register Mapping mp;

    if (mapping < 0 || mapping >= w->gterm.maxMappings)
	return (0);
    else if (!(mp = &w->gterm.mappings[mapping])->defined)
	return (0);

    return (mp->enabled != 0);
}


/* GtEnableMapping -- Enable a mapping.  Enabling a mapping does not
 * update the destination unless the refresh flag is set.  Enabling a
 * mapping activates the mapping so that any changes to the source will
 * be mapped to the destination.
 */
GtEnableMapping (w, mapping, refresh)
    GtermWidget w;
    int mapping;		/* mapping number */
    int refresh;		/* refresh destination */
{
    register Mapping mp;


    if (DBG_TRACE)
	fprintf (stderr, "GtEnableMapping:  mapping=%d  refresh=%d\n",
	    mapping, refresh);

    invalidate_draw_context (w);
    if (mapping >= 0 && mapping < w->gterm.maxMappings) {
	mp = &w->gterm.mappings[mapping];
	if (mp->defined) {
	    if (!mp->enabled) {
		mp->enabled = True;
		if (refresh)
		    GtRefreshMapping (w, mapping);
	    }
	    return (OK);
	}
    }
    return (ERR);
}


/* GtSetDisplayRaster -- Set the currently displayed raster.  On TrueColor
** visuals the XImage for a frame represents the actual pixel values and 
** not the colormapped image.  We need this for pixel readback where the
** display pixmap (i.e. w->gterm.pixmap) is the 24-bits colormapped image
** and we have no way to get back to the pixel values.
** 
*/	 

GtSetDisplayRaster (gt, raster)
    GtermWidget gt;
    int raster;				/* raster number */
{
    if (DBG_TRACE)
	fprintf (stderr, "GtSetDisplayRaster:  raster=%d\n", raster);

    gt->gterm.d_raster = (raster * 2);
}


/* GtDisableMapping -- Disable a mapping.  Disabling a mapping does not
 * affect the mapping definition, hence a disabled mapping may later be
 * reenabled.  If the ERASE flag is set the destination region is redrawn
 * with the mapping disabled.
 */
GtDisableMapping (w, mapping, erase)
    GtermWidget w;
    int mapping;		/* mapping number */
    int erase;			/* erase the destination */
{
    register int i;
    register Mapping mp, dmp;
    XRectangle r, d, in;

    invalidate_draw_context (w);
    if (mapping >= 0 && mapping < w->gterm.maxMappings) {
	mp = &w->gterm.mappings[mapping];
	if (mp->defined) {
	    if (mp->enabled) {
		mp->enabled = False;

		if (erase) {
		    d.x = mp->dx;
		    d.y = mp->dy;
		    d.width = abs(mp->dnx);
		    d.height = abs(mp->dny);

		    /* Clear the uncovered region. */
		    GtSetPixels (w, mp->dst, GtPixel,
			d.x, d.y, d.width, d.height,
			GtGetClientPixel(w,0), 0);

		    /* Refresh any lower level mappings the destination rects of
		     * which intersect the uncovered region.
		     */
		    for (dmp = w->gterm.mp_head;
			dmp && dmp->mapping != mapping;  dmp = dmp->next) {

			if (dmp->enabled && dmp->dst == mp->dst) {
			    r.x = dmp->dx;
			    r.y = dmp->dy;
			    r.width = dmp->dnx;
			    r.height = dmp->dny;

			    if (rect_intersect (&in, &r, &d))
				refresh_destination (w, dmp,
				    in.x, in.y, in.width, in.height);
			}
		    }
		}
	    }
	    return (OK);
	}
    }

    return (ERR);
}


/* GtRefreshMapping -- Refresh the destination region defined by a mapping.
 */
GtRefreshMapping (w, mapping)
    GtermWidget w;
    int mapping;		/* mapping number */
{
    register Mapping mp;
    struct mapping p_mp;


    if (DBG_TRACE)
	fprintf(stderr, "GtRefreshMapping() -- ENTER\n");

    if (mapping >= 0 && mapping < w->gterm.maxMappings) {
	mp = &w->gterm.mappings[mapping];
	if (mp->defined) {
	    if (mp->st != GtPixel || mp->dt != GtPixel) {
		initialize_mapping (&p_mp);
		get_pixel_mapping (w, mp, &p_mp, 1);
		update_mapping (w, mp = &p_mp);
	    } else
		update_mapping (w, mp);

	    if (CacheXImage)					/* MF004 */
	        DestroyCachedXImage();				/* MF004 */

	    refresh_destination (w, mp,
		mp->dx, mp->dy, abs(mp->dnx), abs(mp->dny));
	    if (mp == &p_mp)
		free_mapping (w, mp);
	}
    }

    if (DBG_TRACE)
	fprintf(stderr, "GtRefreshMapping() -- LEAVING\n");
}


/* GtMapVector -- Map a point vector from the coordinate system of one raster
 * to another as defined by the given mapping.  The mapping may be either in
 * the forward direction (dir = GtMap) or the reverse (dir = GtUnmap).  The
 * point vector is maintained in floating point for this operation to avoid
 * loss of precision.  The input and output vectors may be the same vector.
 */
GtMapVector (w, mapping, dir, pv1, pv2, npts)
    GtermWidget w;
    int mapping;
    int dir;			/* GtMap, GtUnmap */
    DPoint *pv1;
    DPoint *pv2;
    int npts;
{
    register DPoint *ip = pv1;
    register DPoint *op = pv2;
    register Mapping mp;
    register int n;

    struct mapping p_mp;
    double xscale, xoffset;
    double yscale, yoffset;
    int sx, sy;

    xscale = yscale = 1.0;
    xoffset = yoffset = 0.0;
    sx = sy = 0;

    if (mapping >= 0 && mapping < w->gterm.maxMappings) {
	mp = &w->gterm.mappings[mapping];
	if (valid_mapping (w, mp)) {
	    /* Determine the transformation coefficients. */
	    get_pixel_mapping (w, mp, &p_mp, 0);
	    mp = &p_mp;

	    xscale = (float)mp->dnx / (float)mp->snx;
	    if (xscale < 0)
		xoffset = mp->dx + abs(mp->dnx) - 1;
	    else
		xoffset = mp->dx;

	    yscale = (float)mp->dny / (float)mp->sny;
	    if (yscale < 0)
		yoffset = mp->dy + abs(mp->dny) - 1;
	    else
		yoffset = mp->dy;

	    sx = mp->sx;
	    sy = mp->sy;
	}
    }

    /* Map the vector. */
    if (dir == GtMap) {
	for (n=npts;  --n >= 0;  ip++, op++) {
	    op->x = (ip->x - sx) * xscale + xoffset;
	    op->y = (ip->y - sy) * yscale + yoffset;
	}
    } else {
	for (n=npts;  --n >= 0;  ip++, op++) {
	    op->x = (ip->x - xoffset) / xscale + sx;
	    op->y = (ip->y - yoffset) / yscale + sy;
	}
    }
}


/* GtPixelToNDC -- Transform a vector from pixel to NDC coordinates in the
 * coordinate system of the given reference raster.  The input and output
 * vectors may be the same vector.
 */
GtPixelToNDC (w, raster, pv1, pv2, npts)
    GtermWidget w;
    int raster;
    DPoint *pv1;
    DPoint *pv2;
    int npts;
{
    register Raster rp = &w->gterm.rasters[raster];
    register DPoint *ip = pv1;
    register DPoint *op = pv2;
    register int n;

    for (n=npts;  --n >= 0;  ip++, op++) {
	op->x = (             ip->x) / rp->width * MAXNDC;
	op->y = (rp->height - ip->y) / rp->height * MAXNDC;
    }
}


/* GtNDCToPixel -- Transform a vector from NDC to pixel coordinates in the
 * coordinate system of the given reference raster.  The input and output
 * vectors may be the same vector.
 */
GtNDCToPixel (w, raster, pv1, pv2, npts)
    GtermWidget w;
    int raster;
    DPoint *pv1;
    DPoint *pv2;
    int npts;
{
    register Raster rp = &w->gterm.rasters[raster];
    register DPoint *ip = pv1;
    register DPoint *op = pv2;
    register int n;

    for (n=npts;  --n >= 0;  ip++, op++) {
	op->x = ip->x / MAXNDC * rp->width;
	op->y = rp->height - (ip->y / MAXNDC * rp->height);
    }
}


/* GtDebug -- Print debug info.  If the file descriptor is NULL output is
 * to the process stdout.  The "what" argument may be used to select the
 * type of output desired.  If what=0 the full output is generated,
 * otherwise bits are used to select what output is to be generated.
 *
 * "what" bitflags:
 *
 *	001	Widget information
 *	002	List rasters
 *	004	List mappings
 *	010	List colormaps
 *	020	List markers
 *
 * This routine is intended only for use during debugging.
 */
GtDebug (w, fp, what)
    GtermWidget w;
    FILE *fp;
    int what;
{
    /* Default is to write everything to the stdout. */
    what = what ? what : 0777;
    fp = fp ? fp : stdout;

    /* Print widget header. */
    if (what & 001) {
	fprintf (fp, "Widget 0x%x (%s) %dx%d raster=%d\n",
	    w, w->core.name, w->core.width, w->core.height, w->gterm.raster);
	fprintf (fp,
	    "--------------------------------------------------------------\n");
    }

    /* Print raster information. */
    if (what & 002) {
	register int i;
	register Raster rp;

	if (w->gterm.rasters) {
	    for (i=0;  i < w->gterm.maxRasters;  i++) {
		rp = &w->gterm.rasters[i];
		if (!rp->type)
		    continue;
		fprintf (fp, "raster %4d type=%s delete=%d size=%dx%d\n",
		    i, rp->type == ImageRaster ? "client" : "server",
		    rp->delete, rp->width, rp->height);
	    }
	} else
	    fprintf (fp, "no rasters\n");
    }

    /* Print mapping information. */
    if (what & 004) {
	register int i;
	register Mapping mp;
	char flags[32];

	if (w->gterm.mappings) {
	    for (i=0;  i < w->gterm.maxMappings;  i++) {
		mp = &w->gterm.mappings[i];
		if (!mp->defined)
		    continue;

		flags[0] = mp->enabled ? 'E' : 'D';
		flags[1] = mp->updated ? 'U' : ' ';
		flags[2] = mp->refresh ? 'R' : ' ';
		flags[3] = '\0';

		fprintf (fp, "mapping %3d %s %8o", i, flags, mp->rop);
		fprintf (fp, "  %2d %s %3d %3d %3d %3d",
		    mp->src, mp->st == GtPixel ? "pix" : "ndc",
		    mp->sx, mp->sy, mp->snx, mp->sny);
		fprintf (fp, "  %2d %s %3d %3d %3d %3d\n",
		    mp->dst, mp->dt == GtPixel ? "pix" : "ndc",
		    mp->dx, mp->dy, mp->dnx, mp->dny);
	    }
	} else
	    fprintf (fp, "no mappings\n");

	fprintf (fp, "mappings from head: ");
	for (mp = w->gterm.mp_head;  mp;  mp = mp->next)
	    fprintf (fp, " %d", mp->mapping);
	fprintf (fp, "\n");

	fprintf (fp, "mappings from tail: ");
	for (mp = w->gterm.mp_tail;  mp;  mp = mp->prev)
	    fprintf (fp, " %d", mp->mapping);
	fprintf (fp, "\n");
    }

    /* Print colormap information. */
    if (what & 010) {
	register struct colormap *cm;

	fprintf (fp, "cmapName=%s ncolors=%d basePixel=%d\n",
	    w->gterm.cmapName, w->gterm.ncolors, w->gterm.base_pixel);
	for (cm = w->gterm.colormaps;  cm;  cm = cm->next)
	    fprintf (fp, "colormap %2d ncells=%d\n", cm->map, cm->ncells);
    }

    /* Print marker information. */
    if (what & 020) {
	register Marker mm;
	char value[256];

	for (mm = w->gterm.gm_head;  mm;  mm = mm->next) {
	    GmGetAttribute (mm, GmType, (XtArgVal)value, XtRString);
	    fprintf (fp, "marker 0x%x: %10s flags=0x%x [%d %d %d %d] %0.5g\n",
		mm, value, mm->flags, mm->x, mm->y, mm->width, mm->height,
		mm->rotangle);
	}
    }
}


