
/*
 * GRAPHICS routines (public functions).
 * --------------------------------------
 */

GtActivate (w)
    GtermWidget w;
{
    w->gterm.interactive = 0;
    w->gterm.save_x = w->gterm.save_y = 0;
}

GtDeactivate (w)
    GtermWidget w;
{
    Display *display = w->gterm.display;
    Window window = w->gterm.window;

    if (w->gterm.interactive) {
	if (w->gterm.save_x > 0 && w->gterm.save_y > 0) {
	    if (w->gterm.warpCursor) {
		/* Workaround X server bug. */
		if (w->gterm.root != w->gterm.save_root)
		    XWarpPointer (display,None,w->gterm.root, 0,0,0,0,
			WidthOfScreen(w->gterm.screen) - 1,
			HeightOfScreen(w->gterm.screen) - 1);

		/* Move pointer to saved position. */
		XWarpPointer (display, None, w->gterm.save_root,
		    0,0,0,0, w->gterm.save_x, w->gterm.save_y);
	    }
	    w->gterm.save_x = 0;
	    w->gterm.save_y = 0;
	}
	w->gterm.interactive = 0;
    }
}

GtReady (w)
    GtermWidget w;
{
    return (w->gterm.delay == 0);
}

GtReset (w)
    GtermWidget w;
{
    invalidate_draw_context (w);
    set_default_color_index (w);
    XSetFunction (w->gterm.display, w->gterm.drawGC, GXcopy);
    XSetLineAttributes (w->gterm.display,
	w->gterm.drawGC, 1, LineSolid, CapButt, JoinMiter);

    /* Set defaults. */
    w->gterm.line_width = 1;
    w->gterm.data_level = GtSet;
    w->gterm.line_style = GtSolid;
    w->gterm.fill_type  = GtSolid;
    w->gterm.raster     = 0;
}

GtTimerInhibit (w, state)
    GtermWidget w;
    Boolean state;
{
    /* This is a kludge to allow a client (xgterm) to disable use of timers
     * if they don't work in a given implementation.
     */
    w->gterm.useTimers = !state;
}

GtAugmentTranslations (w, translations)
    register GtermWidget w;
    char *translations;
{
    register int i;

    if ((i = w->gterm.nauxTrans) < MAX_AUXTRANS) {
	w->gterm.auxTrans[i] =
	    XtParseTranslationTable (translations);
	w->gterm.auxTType[i] = T_augment;
	w->gterm.nauxTrans++;
	XtAugmentTranslations ((Widget)w, w->gterm.auxTrans[i]);
    }
}

GtOverrideTranslations (w, translations)
    register GtermWidget w;
    char *translations;
{
    register int i;

    if ((i = w->gterm.nauxTrans) < MAX_AUXTRANS) {
	w->gterm.auxTrans[i] =
	    XtParseTranslationTable (translations);
	w->gterm.auxTType[i] = T_override;
	w->gterm.nauxTrans++;
	XtOverrideTranslations ((Widget)w, w->gterm.auxTrans[i]);
    }
}

GtFlush (w)
    GtermWidget w;
{
    XFlush (w->gterm.display);
}

GtSetLogRes (w, width, height)
    GtermWidget w;
    int width, height;
{
    w->gterm.xres = width;
    w->gterm.yres = height;
}

GtGetLogRes (w, width, height)
    GtermWidget w;
    int *width, *height;
{
    *width = w->gterm.xres;
    *height = w->gterm.yres;
}

GtGetPhysRes (w, raster, width, height)
    GtermWidget w;
    int raster;				/* zero for screen size */
    int *width, *height;
{
    if (raster) {
	register Raster rp = &w->gterm.rasters[raster];
	*width = rp->width;
	*height = rp->height;
    } else {
	*width = w->core.width;
	*height = w->core.height;
    }
}

GtSetPhysRes (w, raster, width, height)
    GtermWidget w;
    int raster;
    int width, height;
{
    GtCreateRaster (w, raster, GtServer, width, height, RasterDepth);
}

GtSetRaster (w, raster)
    GtermWidget w;
    int raster;
{
    if (raster >= 0 && raster < w->gterm.maxRasters) {
	w->gterm.raster = raster;
	invalidate_draw_context (w);
    }
}

GtGetRaster (w)
    GtermWidget w;
{
    return (w->gterm.raster);
}

/* ARGSUSED */
GtSetTextRes (w, optrows, optcols)
    GtermWidget w;
    int optrows, optcols;
{
    w->gterm.optrows = optrows;
    w->gterm.optcols = optcols;
}

/* ARGSUSED */
GtSetCharSize (w, ival)
    GtermWidget w;
    int ival;
{
}

GtSetDataLevel (w, ival)
    GtermWidget w;
    int ival;
{
    invalidate_draw_context (w);

    switch (ival) {
    case GtSet:
	XSetFunction (w->gterm.display, w->gterm.drawGC, GXcopy);
	if (w->gterm.useGlobalCmap)
	    XSetForeground (w->gterm.display, w->gterm.drawGC,
	        static_colors[w->gterm.color_index].value);
	else
	    XSetForeground (w->gterm.display, w->gterm.drawGC,
	        w->gterm.cmap[w->gterm.color_index]);
	w->gterm.data_level = ival;
	break;

    case GtClear:
	XSetFunction (w->gterm.display, w->gterm.drawGC, GXcopy);
	if (w->gterm.useGlobalCmap)
	    XSetForeground (w->gterm.display, w->gterm.drawGC,
	        static_colors[0].value);
	else
	    XSetForeground (w->gterm.display, w->gterm.drawGC, w->gterm.color0);
	w->gterm.data_level = ival;
	break;

    case GtInvert:
	/* This probably won't work correctly but leave it for now... */
	XSetFunction (w->gterm.display, w->gterm.drawGC, GXxor);
	w->gterm.data_level = ival;
	break;
    }
}


GtSetLineWidth (w, ival)
    GtermWidget w;
    int ival;
{
    w->gterm.line_width = ival;
    GtSetLineStyle (w, w->gterm.line_style);
}

#define	Dashed		"\010\003"
#define	Dotted		"\002\003"
#define	DashDot		"\016\003\001\003"
#define	Dash3Dot	"\024\003\001\003\001\003\001\003"

GtSetLineStyle (w, ival)
    GtermWidget w;
    int ival;
{
    int line_width = w->gterm.line_width;
    int line_style = LineSolid;
    int cap_style = CapButt;
    int join_style = JoinMiter;
    int dash_offset = 0;
    char *dash_list = NULL;
    int dash_list_length = 0;

    switch (ival) {
    case GtSolid:
	w->gterm.line_style = ival;
	break;
    case GtDashed:
	line_style = LineOnOffDash;
	dash_list = (char *)Dashed;
	dash_list_length = strlen(Dashed);
	w->gterm.line_style = ival;
	break;
    case GtDotted:
	line_style = LineOnOffDash;
	dash_list = (char *)Dotted;
	dash_list_length = strlen(Dotted);
	w->gterm.line_style = ival;
	break;
    case GtDashDot:
	line_style = LineOnOffDash;
	dash_list = (char *)DashDot;
	dash_list_length = strlen(DashDot);
	w->gterm.line_style = ival;
	break;
    case GtDash3Dot:
	line_style = LineOnOffDash;
	dash_list = (char *)Dash3Dot;
	dash_list_length = strlen(Dash3Dot);
	w->gterm.line_style = ival;
	break;
    }

    if (dash_list_length)
	XSetDashes (w->gterm.display, w->gterm.drawGC, dash_offset, dash_list,
	    dash_list_length);

    XSetLineAttributes (w->gterm.display,
	w->gterm.drawGC, line_width, line_style, cap_style, join_style);

    invalidate_draw_context (w);
}

GtSetColorIndex (w, ival)
    GtermWidget w;
    int ival;
{
    register int color = w->gterm.iomap[ival];

    if (color >= 0 && color < w->gterm.ncolors) {
	if (w->gterm.useGlobalCmap) {
	    register int i, found = 0;

	    for (i=0; i < num_static_colors; i++) {
		if (ival == static_colors[i].index) {
	    	    XSetForeground (w->gterm.display, w->gterm.drawGC,
	        	static_colors[i].value);
		    found++;
		}
	    }

	    /* If no match found, set the foreground color as the index.
	    */
	    if (!found) {
	        XSetForeground (w->gterm.display, w->gterm.drawGC,
	            static_colors[1].value);
	    }
	} else
	    XSetForeground (w->gterm.display, w->gterm.drawGC,
	        w->gterm.cmap[color]);
	w->gterm.color_index = color;
	invalidate_draw_context (w);
    }
}

GtSetFillType (w, ival)
    GtermWidget w;
    int ival;
{
    switch (ival) {
    case GtSolid:
    case GtOutline:
	w->gterm.fill_type = ival;
	break;
    }
}

GtClearScreen (w)
GtermWidget w;
{
    register Mapping mp;


    if (!w || !XtIsRealized ((Widget)w))
	return;

    if (w->gterm.pixmap)
	XFillRectangle (w->gterm.display, w->gterm.pixmap,
	    w->gterm.clearGC, 0, 0, w->core.width, w->core.height);
    XClearWindow (w->gterm.display, w->gterm.window);

    set_default_color_index (w);
    XSetFunction (w->gterm.display, w->gterm.drawGC, GXcopy);
    if (w->gterm.useGlobalCmap)
	XSetForeground (w->gterm.display, w->gterm.drawGC,
	    static_colors[1].value);
    else
        XSetForeground (w->gterm.display, w->gterm.drawGC, w->gterm.color1);
    XSetLineAttributes (w->gterm.display,
	w->gterm.drawGC, 1, LineSolid, CapRound, JoinRound);

    w->gterm.line_width     = 1;
    w->gterm.line_style     = GtSolid;
    w->gterm.fill_type      = GtSolid;
    w->gterm.data_level     = GtSet;
    w->gterm.preserve_valid = 0;
    w->gterm.gm_redisplay   = 1;
    w->gterm.d_saved        = 0;

    /* Mark any screen mappings to be unconditionally refreshed. */
    for (mp = w->gterm.mp_head;  mp;  mp = mp->next)
	if (mp->enabled && mp->dst == 0)
	    mp->refresh++;

    invalidate_draw_context (w);
    update_transients (w, NULL);

    /* Reinitialize the display pixmap shadow.
    */
    if (w->gterm.rasters)
        initialize_shadow_pixmap (w, 0);
}

GtDrawPolyline (w, pv, npts)
    GtermWidget w;
    XPoint *pv;
    int npts;
{
    XPoint *points, o_pv[MAX_POINTS];
    DrawContext dx = get_draw_context (w);
    register MappingContext mx;
    register int i;

    for (i=0;  i < dx->nmappings;  i++) {
	mx = &dx->mapContext[i];
	points = mx->scale ? mapVector(mx,pv,o_pv,npts) : pv;

	/* Add code to support max display request size. */
	if (mx->use_backing_store)
	    XDrawLines (w->gterm.display, w->gterm.pixmap,
		mx->drawGC, points, npts, CoordModeOrigin);
	XDrawLines (w->gterm.display, mx->pixmap,
	    mx->drawGC, points, npts, CoordModeOrigin);
    }

    update_transients (w, (Region)NULL);
}

GtDrawPolymarker (w, pv, npts)
    GtermWidget w;
    XPoint *pv;
    int npts;
{
    XPoint *points, o_pv[MAX_POINTS];
    DrawContext dx = get_draw_context (w);
    register MappingContext mx;
    register int i;

    for (i=0;  i < dx->nmappings;  i++) {
	mx = &dx->mapContext[i];
	points = mx->scale ? mapVector(mx,pv,o_pv,npts) : pv;

	/* Add code to support max display request size. */
	if (mx->use_backing_store)
	    XDrawPoints (w->gterm.display, w->gterm.pixmap,
		mx->drawGC, points, npts, CoordModeOrigin);
	XDrawPoints (w->gterm.display, mx->pixmap,
	    mx->drawGC, points, npts, CoordModeOrigin);
    }

    update_transients (w, (Region)NULL);
}

GtDrawPolygon (w, pv, npts)
    GtermWidget w;
    XPoint *pv;
    int npts;
{
    XPoint *points, o_pv[MAX_POINTS];
    DrawContext dx = get_draw_context (w);
    register MappingContext mx;
    register int i;

    for (i=0;  i < dx->nmappings;  i++) {
	mx = &dx->mapContext[i];
	points = mx->scale ? mapVector(mx,pv,o_pv,npts) : pv;

	if (w->gterm.fill_type == GtOutline) {
	    /* Draw outline of region.
	     */
	    int first = 0;
	    int last = npts - 1;

	    if (mx->use_backing_store)
		XDrawLines (w->gterm.display, w->gterm.pixmap, mx->drawGC,
		    points, npts, CoordModeOrigin);
	    XDrawLines (w->gterm.display, mx->pixmap, mx->drawGC,
		points, npts, CoordModeOrigin);

	    if (points[last].x != points[first].x ||
		points[last].y != points[first].y) {

		if (mx->use_backing_store)
		    XDrawLine (w->gterm.display, w->gterm.pixmap, mx->drawGC,
			points[last].x, points[last].y,
			points[first].x, points[first].y);
		XDrawLine (w->gterm.display, mx->pixmap, mx->drawGC,
		    points[last].x, points[last].y,
		    points[first].x, points[first].y);
	    }
	} else {
	    /* Fill the outlined area.
	     */
	    if (mx->use_backing_store) {
		XFillPolygon (w->gterm.display, w->gterm.pixmap, mx->drawGC,
		    points, npts, Nonconvex, CoordModeOrigin);
		XDrawLines (w->gterm.display, w->gterm.pixmap, mx->drawGC,
		    points, npts, CoordModeOrigin);
	    }
	    XFillPolygon (w->gterm.display, mx->pixmap, mx->drawGC,
		points, npts, Nonconvex, CoordModeOrigin);
	    XDrawLines (w->gterm.display, mx->pixmap, mx->drawGC,
		points, npts, CoordModeOrigin);
	}
    }

    update_transients (w, (Region)NULL);
}

GtDrawMarker (w, x, y, xsize, ysize, type)
    GtermWidget w;
    int x, y;
    int xsize, ysize;
    int type;
{
}

GtBell (w)
    GtermWidget w;
{
    XBell (w->gterm.display, 0);
}


/* GtSetCursorPos -- Set the cursor position to the given coordinates X,Y.
 * Coordinates are specified in the current graphics coordinate system,
 * defined by the current raster and logical resolution.
 *
 * This routine is a little more complex than one might think due to the
 * complication of mappings.  Screen coordinates are required to set the
 * cursor, but the graphics drawing context may be defined relative to
 * any raster.  In the general case a graphics pipeline defines the series
 * of coordinate transformations required to transform from graphics
 * coordinates to screen coordinates.  Things are further complicated since
 * the pipeline or desired position may not map to the screen, or there
 * may be multiple mappings to the screen.  The first case (no mapping to
 * the screen) is dealt with by ignoring the request to warp the cursor.
 * The second case (one-to-many mapping) is dealt with by a heuristic:
 * the most recent screen coordinates are unmapped back to the raster we
 * are "drawing" into, defining a unique path through the mappings which
 * we can use to map back to the screen.
 *
 * The simplest case occurs when we are drawing directly into the screen.
 * In this case (raster=0) there may still be a logical to physical
 * coordinate transformation, but there are no mappings to complicate things.
 */
GtSetCursorPos (w, x, y)
    GtermWidget w;
    int x, y;
{
    register MappingContext mx;
    register DrawContext dx;
    register Mapping mp;

    Window window = w->gterm.window;
    int sv_raster = w->gterm.raster;
    int sv_xres = w->gterm.xres, sv_yres = w->gterm.yres;
    int rasters[256], mappings[256], nmap=0, ntrans=0;
    int rx, ry, src, dst, map, i, npts = 1;
    int raster = w->gterm.raster;
    XPoint pv1[1], pv2[2];
    XPoint *points, pv[1];
    Raster rp;

    if (!w || !XtIsRealized ((Widget)w))
	return;

    /* Follow the current cursor position back to the source raster if
     * possible.  This gives us a default pipeline to follow in the reverse
     * direction to map raster coordinates to screen coordinates, and is
     * necessary to find the right mappings when multiple mappings are
     * defined on a single source.
     */
    rx = w->gterm.last_x;
    ry = w->gterm.last_y;
    src = 0;
    do {
	src = GtSelectRaster (w, dst=src, GtPixel,rx,ry, GtPixel,&rx,&ry,&map);
	if (src != dst) {
	    rasters[nmap] = src;
	    mappings[nmap++] = map;
	}
    } while (src != dst && src != raster);

    /* Ray trace the point through all of the mappings to the screen.
     * This isn't fully general, but gives us the capability to follow
     * most graphics pipelines to a point on the screen.
     */
    do {
	GtSetRaster (w, raster);
	if (ntrans++ || raster == 0) {				/* MF041 */
	    /* After the first transformation we have raster coordinates,
	     * so set the logical resolution to the raster dimensions.
	     */
	    rp = &w->gterm.rasters[raster];
	    GtSetLogRes (w, rp->width, rp->height);
	}

	dx = get_draw_context (w);
	if (!dx->nmappings)
	    return;

	/* Try to find the next mapping. */
	if (nmap && rasters[nmap-1] == raster)
	    for (i=0;  i < dx->nmappings;  i++) {
		mx = &dx->mapContext[i];
		if (mx->mapping == mappings[nmap-1]) {
		    mp = mx->mp;
		    nmap--;
		    goto havemap;
		}
	    }
	for (i=0;  i < dx->nmappings;  i++) {
	    mx = &dx->mapContext[i];
	    mp = mx->mp;
	    if (mp && mp->dst == 0)
		break;
	}
	if (i >= dx->nmappings) {
	    mx = &dx->mapContext[0];
	    mp = mx->mp;
	}

havemap:
	rp = &w->gterm.rasters[mp ? mp->dst : raster];

	/* Compute the coordinates points[0].{x,y} of the point x,y in the
	 * destination raster.
	 */
	if (mx->scale) {
	    /* Scaling is in effect.  The following subterfuge is used to
	     * compute the coordinates of the center of the raster pixel (x,y)
	     * when the image is zoomed.  We want to set the cursor to the
	     * center of the selected pixel, not the edge.
	     */
	    pv[0].x = x;  pv[0].y = y;
	    mapVector (mx, pv, pv1, npts);
	    pv[0].x = x + 1;  pv[0].y = y + 1;
	    mapVector (mx, pv, pv2, npts);

	    pv[0].x = (pv1[0].x + pv2[0].x) / 2.0;
	    pv[0].y = (pv1[0].y + pv2[0].y) / 2.0;
	    points = pv;

	} else {
	    /* No scaling. */
	    pv[0].x = x;  pv[0].y = y;
	    points = pv;
	}

	/* Clip to the bounds of the destination raster and generate the
	 * new x,y.
	 */
	x = max(0, min(rp->width-1,  points[0].x));
	y = max(0, min(rp->height-1, points[0].y));

    } while (mp && (raster = mp->dst));

    XWarpPointer (w->gterm.display, window, window, 0,0,0,0, x,y);

    w->gterm.last_x = w->gterm.cur_x = x;
    w->gterm.last_y = w->gterm.cur_y = y;

    GtSetRaster (w, sv_raster);
    GtSetLogRes (w, sv_xres, sv_yres);
}


GtGetCursorPos (w, x, y)
    GtermWidget w;
    int *x, *y;
{
    *x = w->gterm.last_x;
    *y = w->gterm.last_y;
}

GtSetCursorType (w, type)
    GtermWidget w;
    int type;
{
    static XtIntervalId id = (XtIntervalId) NULL;
    Display *display = w->gterm.display;
    Cursor cursor;
    int interval;
    Widget pw;

    if (!w || !XtIsRealized ((Widget)w))
	return;
    if (w->gterm.cursor_type == type)
	return;

    switch (w->gterm.cursor_type = type) {
    case GtNoCursor:
    case GtIdleCursor:
	erase_crosshair (w);
	cursor = w->gterm.idle_cursor;
	break;

    case GtGinmodeCursor:
	/* Begin graphics cursor mode.
	 */

	/* If a screen clear or drawing operation has caused the redisplay
	 * flag to be set, redisplay any marker overlays.
	 */
	if (w->gterm.gm_redisplay) {
	    GmRedisplay (w, (Region)NULL);
	    w->gterm.gm_redisplay = False;
	}

	/* Make sure the window is visible.
	 */
	if (w->gterm.raiseWindow || w->gterm.deiconifyWindow)
	    for (pw = (Widget)w;  pw;  pw = XtParent(pw))
		if (XtIsShell(pw)) {
		    if (w->gterm.deiconifyWindow)
			XMapWindow (display, XtWindow(pw));
		    if (w->gterm.raiseWindow)
			XRaiseWindow (display, XtWindow(pw));
		    XSync (display, False);
		}

	/* The first time this is done after a GtActivate causes the cursor
	 * to be warped into the graphics window.  The interactive flag is set
	 * to cause GtDeactivate to restore the cursor to its original position
	 * after the graphics interaction finishes.
	 */
	if (w->gterm.warpCursor) {
	    int root_x, root_y, win_x, win_y;
	    int xoff, yoff, width, height, x, y;
	    Window gtermwin, root, child;
	    unsigned int keys;
	    int in_window = 0;

	    width = w->core.width;
	    height = w->core.height;
	    gtermwin = w->gterm.window;
	    XTranslateCoordinates (display, gtermwin, w->gterm.root,
		w->core.x, w->core.y, &xoff, &yoff, &child);

	    if (XQueryPointer (display, w->gterm.root, &root, &child,
		&root_x, &root_y, &win_x, &win_y, &keys)) {

		/* Already in gterm window? */
		if ((root_x >= xoff && root_x < xoff+width) &&
		    (root_y >= yoff && root_y < yoff+height)) {

		    if (!w->gterm.interactive) {
			w->gterm.save_x = 0;
			w->gterm.save_y = 0;
			w->gterm.interactive++;
		    }
		    x = root_x - xoff;   y = root_y - yoff;
		    in_window++;

		} else {
		    if (!w->gterm.interactive) {
			w->gterm.save_x = root_x;
			w->gterm.save_y = root_y;
			w->gterm.save_root = root;
			w->gterm.interactive++;
		    }
		    x = w->gterm.cur_x;  y = w->gterm.cur_y;
		}
	    } else {
		/* Pointer not on the current screen.
		 */
		if (!w->gterm.interactive) {
		    w->gterm.save_x = root_x;
		    w->gterm.save_y = root_y;
		    w->gterm.save_root = root;
		    w->gterm.interactive++;
		}
		x = w->gterm.cur_x;  y = w->gterm.cur_y;
	    }

	    if ((x < 5 || x > width-5) || (y < 5 || y > height-5)) {
		x = width / 2;
		y = height / 2;
	    }

	    if (!in_window) {
		erase_crosshair (w);
		if (w->gterm.warpCursor) {
		    XWindowAttributes wa;
		    if (XGetWindowAttributes (display, gtermwin, &wa) &&
			    wa.map_state == IsViewable) {

			/* The following should not be necessary but is needed
			 * to workaround an X server bug.  When warping to a
			 * different screen the pointer is not erased on the
			 * old screen.  It is hard to erase it, but we can
			 * at least move it to the corner of the screen.
			 */
			if (root != w->gterm.root) {
			    Screen *screen = w->gterm.screen;
			    if (XGetWindowAttributes (display, root, &wa))
				screen = wa.screen;
			    XWarpPointer (display,None,root, 0,0,0,0,
				WidthOfScreen(screen) - 1,
				HeightOfScreen(screen) - 1);
			}

			/* Now warp into the gterm window. */
			XWarpPointer (display, None, gtermwin, 0,0,0,0, x,y);
		    }
		    if (w->gterm.full_crosshair)
			draw_crosshair (w, x, y);
		}

	    } else if (w->gterm.full_crosshair) {
		erase_crosshair (w);
		draw_crosshair (w, x, y);
	    }
	} else
	    update_cursor (w);

	cursor = w->gterm.ginmode_cursor;
	if (interval = w->gterm.ginmodeBlinkInterval) {
	    XtAppContext appcon = XtWidgetToApplicationContext ((Widget) w);
	    id = XtAppAddTimeOut (appcon,
		interval, blink_cursor, (XtPointer)w);
	} else
	    id = (XtIntervalId) NULL;
	break;

    case GtBusyCursor:
	/* Exit graphics cursor mode.
	 */
	erase_crosshair (w);
	cursor = w->gterm.busy_cursor;
	break;
    }

    if (w->core.visible)
	XDefineCursor (w->gterm.display, w->gterm.window,
	    w->gterm.cursor = cursor);
    if (id && w->gterm.cursor_type != GtGinmodeCursor) {
	XtRemoveTimeOut (id);
	id = (XtIntervalId) NULL;
    }
}

static void
blink_cursor (w, id)
    GtermWidget w;
    XtIntervalId *id;
{
    XtAppContext app_context;
    XColor bg, fg;
    int interval;

    if (w->gterm.cursor_type != GtGinmodeCursor) 		/* MF032 */
	return;

    bg = w->gterm.ginmodeColors[1];
    fg = w->gterm.ginmodeColors[0];

    app_context = XtWidgetToApplicationContext ((Widget) w);
    XRecolorCursor (w->gterm.display, w->gterm.ginmode_cursor, &fg, &bg);
    XFlush (w->gterm.display);

    w->gterm.ginmodeColors[0] = bg;
    w->gterm.ginmodeColors[1] = fg;

    if (interval = w->gterm.ginmodeBlinkInterval)
	XtAppAddTimeOut (app_context,
	    interval, (XtTimerCallbackProc) blink_cursor, (XtPointer)w);
}

GtPostInputProc (w, userfcn, client_data)
    GtermWidget w;
    GtCallbackProc userfcn;
    XtPointer client_data;
{
    register GtCallback *cb, *new;

    new = (GtCallback *) XtMalloc (sizeof (GtCallback));
    new->proc = userfcn;
    new->client_data = client_data;
    new->next = NULL;

    for (cb = w->gterm.inputCallback;  cb && cb->next;  cb = cb->next)
	;
    if (cb)
	cb->next = new;
    else
	w->gterm.inputCallback = new;
}

GtDeleteInputProc (w, userfcn, client_data)
    GtermWidget w;
    GtCallbackProc userfcn;
    XtPointer client_data;
{
    register GtCallback *cb, *prev;

    for (prev=NULL, cb = w->gterm.inputCallback;  cb;  cb = cb->next)
	if (cb->proc == userfcn && cb->client_data == client_data) {
	    if (prev)
		prev->next = cb->next;
	    else
		w->gterm.inputCallback = cb->next;
	    XtFree ((char *)cb);
	    break;
	} else
	    prev = cb;
}

GtPostResetProc (w, userfcn, client_data)
    GtermWidget w;
    GtCallbackProc userfcn;
    XtPointer client_data;
{
    register GtCallback *cb, *new;

    new = (GtCallback *) XtMalloc (sizeof (GtCallback));
    new->proc = userfcn;
    new->client_data = client_data;
    new->next = NULL;

    for (cb = w->gterm.resetCallback;  cb && cb->next;  cb = cb->next)
	;
    if (cb)
	cb->next = new;
    else
	w->gterm.resetCallback = new;
}

GtDeleteResetProc (w, userfcn, client_data)
    GtermWidget w;
    GtCallbackProc userfcn;
    XtPointer client_data;
{
    register GtCallback *cb, *prev;

    for (prev=NULL, cb = w->gterm.resetCallback;  cb;  cb = cb->next)
	if (cb->proc == userfcn && cb->client_data == client_data) {
	    if (prev)
		prev->next = cb->next;
	    else
		w->gterm.resetCallback = cb->next;
	    XtFree ((char *)cb);
	    break;
	} else
	    prev = cb;
}

GtPostResizeProc (w, userfcn, client_data)
    GtermWidget w;
    GtCallbackProc userfcn;
    XtPointer client_data;
{
    register GtCallback *cb, *new;

    new = (GtCallback *) XtMalloc (sizeof (GtCallback));
    new->proc = userfcn;
    new->client_data = client_data;
    new->next = NULL;

    for (cb = w->gterm.resizeCallback;  cb && cb->next;  cb = cb->next)
	;
    if (cb)
	cb->next = new;
    else
	w->gterm.resizeCallback = new;
}

GtDeleteResizeProc (w, userfcn, client_data)
    GtermWidget w;
    GtCallbackProc userfcn;
    XtPointer client_data;
{
    register GtCallback *cb, *prev;

    for (prev=NULL, cb = w->gterm.resizeCallback;  cb;  cb = cb->next)
	if (cb->proc == userfcn && cb->client_data == client_data) {
	    if (prev)
		prev->next = cb->next;
	    else
		w->gterm.resizeCallback = cb->next;
	    XtFree ((char *)cb);
	    break;
	} else
	    prev = cb;
}

GtDrawAlphaText (w, x, y, text)
    GtermWidget w;
    int x, y;
    char *text;
{
    XPoint *points, pv[1], o_pv[1];
    DrawContext dx = get_draw_context (w);
    register MappingContext mx;
    register int npts, i;

    pv[0].x = x;
    pv[0].y = y;
    npts = 1;

    for (i=0;  i < dx->nmappings;  i++) {
	mx = &dx->mapContext[i];
	points = mx->scale ? mapVector(mx,pv,o_pv,npts) : pv;
	x = points[0].x;  y = points[0].y;

	if (mx->use_backing_store)
	    XDrawString (w->gterm.display, w->gterm.pixmap,
		mx->drawGC, x, y, text, strlen(text));
	XDrawString (w->gterm.display, mx->pixmap,
	    mx->drawGC, x, y, text, strlen(text));
    }

    update_transients (w, (Region)NULL);
}

GtGetAlphaTextSize (w, string, width, height, base)
    GtermWidget w;
    char *string;
    int *width, *height, *base;
{
    XFontStruct *fp;

    fp = w->gterm.alpha_fonts[w->gterm.alpha_font];
    if (string)
	*width = XTextWidth (fp, string, strlen(string));
    else
	*width = fp->max_bounds.width;

    *height = fp->max_bounds.ascent + fp->max_bounds.descent;
    *base = fp->max_bounds.ascent;
}

GtWriteAlphaCursor (w, x, y)
    GtermWidget w;
    int x, y;
{
}

GtEraseAlphaCursor (w)
    GtermWidget w;
{
}

GtStartDialog (w)
    GtermWidget w;
{
    if (DBG_TRACE)
	fprintf (stderr, "GtStartDialog:  ENTER  d_pixmap=0x%x d_saved=%d\n",
	    w->gterm.d_pixmap, w->gterm.d_saved);

    if (w->gterm.d_pixmap) {
	if (w->gterm.d_saved) {
	    GtEraseDialog (w);
	} else {
	    XSetClipMask (w->gterm.display, w->gterm.exposeGC, None);
	    XCopyArea (w->gterm.display,
		w->gterm.pixmap ? w->gterm.pixmap : w->gterm.window,
		w->gterm.d_pixmap, w->gterm.exposeGC,
		0, w->gterm.d_yoff, w->core.width, w->gterm.d_height, 0, 0);
	    w->gterm.d_saved = 1;
	}
    }

    if (DBG_TRACE)
	fprintf (stderr, "GtStartDialog:  LEAVING\n");
}

GtEndDialog (w)
    GtermWidget w;
{
    GtEraseDialog (w);
    w->gterm.d_saved = 0;
}

GtEraseDialog (w)
    GtermWidget w;
{
    if (DBG_TRACE)
	fprintf (stderr, "GtEraseDialog:  ENTER  d_pixmap=0x%x d_saved=%d\n",
	    w->gterm.d_pixmap, w->gterm.d_saved);

    if (w->gterm.d_pixmap && w->gterm.d_saved) {
	XSetClipMask (w->gterm.display, w->gterm.exposeGC, None);
	XCopyArea (w->gterm.display,
	    w->gterm.d_pixmap, w->gterm.window, w->gterm.exposeGC,
	    0, 0, w->core.width, w->gterm.d_height, 0, w->gterm.d_yoff);
	if (w->gterm.pixmap)
	    XCopyArea (w->gterm.display,
		w->gterm.d_pixmap, w->gterm.pixmap, w->gterm.exposeGC,
		0, 0, w->core.width, w->gterm.d_height, 0, w->gterm.d_yoff);
	update_transients (w, (Region)NULL);
    }

    if (DBG_TRACE)
	fprintf (stderr, "GtEraseDialog:  LEAVING\n");
}

GtDrawDialogText (w, x, y, text)
    GtermWidget w;
    int x, y;
    char *text;
{
    int xpos = w->gterm.d_xoff + x;
    int ypos = w->gterm.d_yoff + y;

    if (w->gterm.pixmap)
	XDrawImageString (w->gterm.display, w->gterm.pixmap,
	    w->gterm.dialogGC, xpos, ypos, text, strlen(text));
    XDrawImageString (w->gterm.display, w->gterm.window,
	w->gterm.dialogGC, xpos, ypos, text, strlen(text));
}

GtGetDialogTextSize (w, string, width, height, base)
    GtermWidget w;
    char *string;
    int *width, *height, *base;
{
    XFontStruct *fp;

    fp = w->gterm.dialog_fonts[w->gterm.dialog_font];
    if (string)
	*width = XTextWidth (fp, string, strlen(string));
    else
	*width = fp->max_bounds.width;

    *height = fp->max_bounds.ascent + fp->max_bounds.descent;
    *base = fp->max_bounds.ascent;
}


/*
 * Internal functions for above code.
 * ----------------------------------------
 */

static
set_default_color_index (w)
    GtermWidget w;
{
    /* The default color index is 1, corresponding to the foreground
     * drawing color color1.  Index zero is the background drawing color
     * color0.  The remaining NColors color table entries are the optional
     * drawing colors corresponding to resources "color2" through "colorN".
     * These are used only if explicitly selected by the client application.
     */
    if (w->gterm.useGlobalCmap)
        XSetForeground (w->gterm.display, w->gterm.drawGC,
	    static_colors[1].value);
    else
        XSetForeground (w->gterm.display, w->gterm.drawGC, w->gterm.cmap[1]);
    w->gterm.color_index = 1;
    invalidate_draw_context (w);
}


static
draw_crosshair (w, x, y)
    GtermWidget w;
    int x, y;
{
    if (!w || !XtIsRealized ((Widget)w))
	return;

    if (w->gterm.pixmap) {
	/* The preserve_screen flag is set if we need to preserve the
	 * exact display window contents, rather than merely refresh from
	 * the backing store pixmap.
	 */
	if (w->gterm.preserve_screen) {
	    if (!w->gterm.preserve_valid || y != w->gterm.cur_y)
		XCopyArea (w->gterm.display,
		    w->gterm.window, w->gterm.pixmap, w->gterm.exposeGC,
		    0, y, w->core.width, 1, 0, w->core.height);
	    if (!w->gterm.preserve_valid || x != w->gterm.cur_x)
		XCopyArea (w->gterm.display,
		    w->gterm.window, w->gterm.pixmap, w->gterm.exposeGC,
		    x, 0, 1, w->core.height, w->core.width, 0);
	    w->gterm.preserve_valid = 1;
	}

	XDrawLine (w->gterm.display, w->gterm.window, w->gterm.cursorGC,
	    0, y, w->core.width, y);
	XDrawLine (w->gterm.display, w->gterm.window, w->gterm.cursorGC,
	    x, 0, x, w->core.height);

	XFlush (w->gterm.display);
	w->gterm.cursor_drawn++;
    }

    w->gterm.cur_x = x;
    w->gterm.cur_y = y;
}


static
erase_crosshair (w)
    GtermWidget w;
{
    if (!w || !XtIsRealized ((Widget)w))
	return;

    if (w->gterm.cursor_drawn) {
	register int x = w->gterm.cur_x;
	register int y = w->gterm.cur_y;

	XSetClipMask (w->gterm.display, w->gterm.exposeGC, None);
	if (w->gterm.pixmap) {
	    if (w->gterm.preserve_screen && w->gterm.preserve_valid) {
		XCopyArea (w->gterm.display,
		    w->gterm.pixmap, w->gterm.window, w->gterm.exposeGC,
		    0, w->core.height, w->core.width, 1, 0, y);
		XCopyArea (w->gterm.display,
		    w->gterm.pixmap, w->gterm.window, w->gterm.exposeGC,
		    w->core.width, 0, 1, w->core.height, x, 0);
	    } else {
		XCopyArea (w->gterm.display,
		    w->gterm.pixmap, w->gterm.window, w->gterm.exposeGC,
		    0, y, w->core.width, 1, 0, y);
		XCopyArea (w->gterm.display,
		    w->gterm.pixmap, w->gterm.window, w->gterm.exposeGC,
		    x, 0, 1, w->core.height, x, 0);
	    }
	}

	w->gterm.cursor_drawn = 0;
	w->gterm.preserve_valid = 0;
    }
}


static
update_transients (w, region)
    GtermWidget w;
    Region region;
{
    /* If an explicit region is given redisplay any markers in it immediately,
     * otherwise set the redisplay flag to cause a full screen redisplay when
     * drawing finishes and the widget is ready for input.
     */
    if ((char *)region)
	GmRedisplay (w, region);
    else
	w->gterm.gm_redisplay = True;

    /* Update the crosshair cursor if GIN mode is in effect. */
    update_cursor (w);
}


static
update_cursor (w)
    GtermWidget w;
{
    if (w->gterm.cursor_type == GtGinmodeCursor && w->gterm.full_crosshair) {
	register int x = w->gterm.cur_x;
	register int y = w->gterm.cur_y;

	if (x || y)
	    draw_crosshair (w, x, y);
    }
}

static Cursor
get_cursor (w, cursor_name)
    GtermWidget w;
    String cursor_name;
{
    XrmValue from, to;
    Cursor cursor;

    from.size = strlen (cursor_name) + 1;
    from.addr = cursor_name;

    to.addr = (caddr_t) &cursor;
    to.size = sizeof(cursor);

    if (!XtConvertAndStore ((Widget)w, XtRString, &from, XtRCursor, &to))
	cursor = XCreateFontCursor (w->gterm.display, XC_crosshair);

    return (cursor);
}


static DrawContext
get_draw_context (w)
    GtermWidget w;
{
    DrawContext dx = &w->gterm.draw;

    if (!dx->valid) {
	int raster = w->gterm.raster;
	Raster rp = &w->gterm.rasters[raster];
	register MappingContext mx = &dx->mapContext[0];
	Region clip_region, mask_region;
	struct mapping *map, *mp, *np, p_mp;
	int xres = w->gterm.xres;
	int yres = w->gterm.yres;
	float xscale, yscale;
	XRectangle r;
	int i, j;

	dx->raster = w->gterm.raster;
	dx->rp = rp;

	if (raster == 0) {
	    dx->nmappings = 1;
	    mx->mapping = 0;
	    mx->mp = NULL;
	    mx->use_backing_store = (w->gterm.pixmap != (Pixmap)NULL);
	    mx->pixmap = w->gterm.window;
	    mx->drawGC = w->gterm.drawGC;
	    mx->GC_private = 0;

/* (7/16/97) MJF - we don't scale raster 0 since it's already in screen coords.
   Otherwise the cursor movement keystrokes scale incorrectly and quickly move
   to (0,0).
	    mx->xoffset = mx->yoffset = mx->scale = 0;
   [DCT] This doesn't look entirely right as it disables logical coords for
   the screen.  Leave as is until this can be studied more carefully.
 */

	    mx->xoffset = mx->yoffset = 0;
	    if (xres == rp->width && yres == rp->height)
		mx->scale = 0;
	    else {
		mx->xscale = (float)rp->width / (float)xres;
		mx->yscale = (float)rp->height / (float)yres;
		mx->scale = 1;
	    }

	} else {
	    dx->nmappings = 0;
	    for (mp = w->gterm.mp_head;  mp;  mp = mp->next) {
		if (!mp->enabled || mp->src != raster ||
			w->gterm.rasters[mp->dst].type != GtServer)
		    continue;
		if (!valid_mapping (w, mp))
		    continue;

		mx->mp = mp;
		mx->mapping = mp->mapping;
		mx->pixmap = w->gterm.rasters[mp->dst].r.pixmap;
		mx->use_backing_store = (mp->dst == 0 &&
		    w->gterm.pixmap && !(mp->rop & R_Transient));

		/* Determine if any scaling is necessary. */
		get_pixel_mapping (w, mp, &p_mp, 0);
		map = &p_mp;

		/* Compute logical-to-raster scaling. */
		mx->xoffset = mx->yoffset = 0;
		if (xres == rp->width && yres == rp->height) {
		    mx->xscale = mx->yscale = 1.0;
		    mx->scale = 0;
		} else {
		    mx->xscale = (float)rp->width / (float)xres;
		    mx->yscale = (float)rp->height / (float)yres;
		    mx->scale = 1;
		}

		/* Compute overall scale factors by combining logical-to-
		 * raster and raster-to-screen mappings.
		 */
		if (map->snx != map->dnx || map->sny != map->dny ||
		    map->sx  != map->dx  || map->sy  != map->dy) {

		    xscale = (float)map->dnx / (float)map->snx;
		    mx->xscale *= xscale;
		    if (xscale < 0)
			mx->xoffset = map->dx + abs(map->dnx) - 1;
		    else
			mx->xoffset = map->dx;
		    mx->xoffset -= (map->sx * xscale);

		    yscale = (float)map->dny / (float)map->sny;
		    mx->yscale *= yscale;
		    if (yscale < 0)
			mx->yoffset = map->dy + abs(map->dny) - 1;
		    else
			mx->yoffset = map->dy;
		    mx->yoffset -= (map->sy * yscale);

		    mx->scale = 1;
		}

		/* Compute the clip mask which will clip graphics to the
		 * destination rect of the mapping, minus any regions of
		 * this rect covered by other mappings.
		 */
		clip_region = XCreateRegion();
		r.x = map->dx;  r.y = map->dy;
		r.width = abs(map->dnx);
		r.height = abs(map->dny);
		XUnionRectWithRegion (&r, clip_region, clip_region);

		for (np = mp->next;  np;  np = np->next) {
		    struct mapping p_np;

		    if (!np->enabled || np->dst != mp->dst)
			continue;
		    get_pixel_mapping (w, np, &p_np, 0);

		    mask_region = XCreateRegion();
		    r.x = p_np.dx;  r.y = p_np.dy;
		    r.width = abs(p_np.dnx);
		    r.height = abs(p_np.dny);
		    XUnionRectWithRegion (&r, mask_region, mask_region);

		    XSubtractRegion (clip_region, mask_region, clip_region);
		    XDestroyRegion (mask_region);
		}

		/* Create a drawing GC which is a copy of the global drawGC
		 * but using the clip mask computed above.
		 */
		mx->drawGC = XCreateGC (w->gterm.display, w->gterm.root,
		    0, NULL);

                /* Deep Frame */
                mx->drawGC = XCreateGC (w->gterm.display, w->gterm.window,
                    0, NULL);
                /* Deep Frame */

		XCopyGC (w->gterm.display, w->gterm.drawGC, ~0, mx->drawGC);
		XSetRegion (w->gterm.display, mx->drawGC, clip_region);
		XDestroyRegion (clip_region);
		mx->GC_private = 1;

		if (++dx->nmappings >= MAX_DRAW)
		    break;
		else
		    mx++;
	    }
	}

	dx->valid = 1;
    }

    return (dx);
}


static
invalidate_draw_context (w)
    GtermWidget w;
{
    register DrawContext dx = &w->gterm.draw;
    register MappingContext mx;
    register int i;

    if (dx->valid) {
	for (i=0;  i < dx->nmappings;  i++) {
	    mx = &dx->mapContext[i];
	    if (mx->GC_private)
		XFreeGC (w->gterm.display, mx->drawGC);
	}
	dx->valid = 0;
    }
}

static XPoint *
mapVector (mx, pv1, pv2, npts)
    register MappingContext mx;
    XPoint *pv1;
    XPoint *pv2;
    int npts;
{
    register XPoint *ip = pv1;
    register XPoint *op = pv2;
    register int n;

    for (n=npts;  --n >= 0;  ip++, op++) {
	op->x = ip->x * mx->xscale + mx->xoffset;
	op->y = ip->y * mx->yscale + mx->yoffset;
    }

    return (pv2);
}


static void
savepos (w, event)
    GtermWidget w;
    XEvent *event;
{
    if (event == NULL)
	return;

    switch (event->type) {
    case KeyPress:
    case KeyRelease:
	w->gterm.last_x = event->xkey.x;
	w->gterm.last_y = event->xkey.y;
	break;
    case ButtonPress:
    case ButtonRelease:
	w->gterm.last_x = event->xbutton.x;
	w->gterm.last_y = event->xbutton.y;
	break;
    case MotionNotify:
	w->gterm.last_x = event->xmotion.x;
	w->gterm.last_y = event->xmotion.y;
	break;
    }
}
