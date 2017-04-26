
/*
 * Graphics MARKERS.
 * --------------------
 * A marker is an active graphics object displayed on top of a drawing to
 * mark, annotate, or outline a region.  Markers can respond to events and
 * move, resize, or modify themselves, optionally executing callback
 * procedures when the marker changes state.  Markers are used to
 * interactively define regions with the mouse, to provide a dynamic graphical
 * display which doesn't interfere with the underlying graphics frame, or as a
 * graphical means of command input, using callbacks to perform some operation
 * when the marker is moved or resized by the user.
 * 
 *	       GtMarkerInit (w)
 *	       GtMarkerFree (w)
 *
 *	      gm = GmCreate (w, type, interactive)
 *		GmRedisplay (w, region|NULL)
 *		gm = GmCopy (gm)
 *		  GmDestroy (gm)
 *	      GmAddCallback (gm, events, func, client_data)
 *	   GmDeleteCallback (gm, func, client_data)
 *	      gm = GmSelect (gt, x, y, &what)
 *
 *		  GmMarkpos (gm)
 *		   GmRedraw (gm, func, erase)
 *		    GmRaise (gm, ref_gm|NULL)
 *		    GmLower (gm, ref_gm|NULL)
 *		   GmNotify (gm, events, event, param, nparams)
 *
 *		    GmAddPt (gm, x, y)
 *		 GmDeletePt (gm, x, y)
 *		   GmMovePt (gm, x, y)
 *		     GmMove (gm, x, y)
 *		   GmResize (gm, x, y)
 *		   GmRotate (gm, x, y)
 *
 *          GmSetAttributes (gm, args, nargs, type)
 *          GmGetAttributes (gm, args, nargs, type)
 *	     GmSetAttribute (gm, attribute, value, type)
 *	     GmGetAttribute (gm, attribute, value, type)
 *	      GmSetVertices (gm, points, first, npts)
 *     npts = GmGetVertices (gm, points, first, maxpts)
 *	   GmGetBoundingBox (gm, x, y, width, height)
 *
 *	 type = GmStrToType (marker_type)
 *     event = GmStrToEvent (event_type)
 *   func = GmStrToFunction (drawing_function)
 *
 * Markers operate in screen coordinates (raster 0).  The SelectRaster
 * and MapVector routines may be used to convert to and from raster
 * coordinates if desired.
 *
 *  raster = GtSelectRaster (gt, dras, dt, dx, dy, rt, &rx, &ry, &mp)
 *              GtMapVector (gt, mp, dir, st, sv, dt, dv, npts, clip)
 *
 * The Gm procedures above implement the main functionality of markers.  User
 * interaction is provided at a higher level using action procedures which
 * are bound to pointer and keyboard events via translations (or by the GUI
 * itself directly calling the above procedures).
 */

static void gm_text_init(), gm_line_init(), gm_plin_init(), gm_rect_init();
static void gm_boxx_init(), gm_circ_init(), gm_elip_init(), gm_pgon_init();
static int gm_putint(), gm_putfloat(), gm_do_callbacks(), gm_constraint();
static int gm_getint(), gm_getattribute(), gm_gettype();
static double gm_getfloat();
static char *gm_getstring();

static void gm_markpos(), gm_erase(), gm_redraw(), gm_setCurRect();
static void gm_linkafter(), gm_unlink();
static double gm_niceAngle();
static Pixel gm_getpixel();
static int gm_select();
static int gm_getfillstyle();

static GmVMethod gm_classinit[] = {
    gm_text_init, gm_line_init, gm_plin_init, gm_rect_init,
    gm_boxx_init, gm_circ_init, gm_elip_init, gm_pgon_init
};

static Region null_region;
static XRectangle null_rect = { 0, 0, 0, 0 };
#define NullRect(r)	(!(r)->width || !(r)->height)

#define PI_2		1.57079632679489661923
#define PI_4		0.78539816339744830962
#define BORDER		5

static void M_create(), M_destroy(), M_destroyNull(), M_set(), M_raise();
static void M_lower(), M_notify(), M_markpos(), M_markposAdd(), M_redraw();
static void M_addPt(), M_deletePt(), M_movePt(), M_deleteDestroy();
static void M_move(), M_resize(), M_moveResize(), M_rotate();
static void M_rotateResize(), M_input();
static void gm_focusin(), gm_focusout();

static XtActionsRec markerActionsList[] = {
	{ "m_create",		M_create },
	{ "m_destroy",		M_destroy },
	{ "m_destroyNull",	M_destroyNull },
	{ "m_set",		M_set },
	{ "m_raise",		M_raise },
	{ "m_lower",		M_lower },
	{ "m_notify",		M_notify },
	{ "m_input",		M_input },
	{ "m_markpos",		M_markpos },
	{ "m_markposAdd",	M_markposAdd },
	{ "m_redraw",		M_redraw },
	{ "m_addPt",		M_addPt },
	{ "m_deletePt",		M_deletePt },
	{ "m_movePt",		M_movePt },
	{ "m_deleteDestroy",	M_deleteDestroy },
	{ "m_move",		M_move },
	{ "m_resize",		M_resize },
	{ "m_moveResize",	M_moveResize },
	{ "m_rotate",		M_rotate },
	{ "m_rotateResize",	M_rotateResize },
};


/* GtMarkerInit -- Initialize the marker subsystem.
 */
GtMarkerInit (w)
    GtermWidget w;
{
    register Marker gm, prev;
    XColor fg_color, bg_color;
    Display *display = w->gterm.display;
    int type, i;
    GC gc;

    for (gm = w->gterm.gm_tail;  gm;  gm = prev) {
	prev = gm->prev;
        GmDestroy (gm);
    }

    if (!w->gterm.gm_initialized) {
	/* Register some additional actions for markers. */
	XtAppAddActions (XtWidgetToApplicationContext((Widget)w),
	    markerActionsList, XtNumber(markerActionsList));

	/* Get the gterm widget translations. */
	if ((char *)w->gterm.defTranslations == NULL) {
	    char *translations = NULL;
	    XtTranslations tt;
	    XtResource r;
	    int ttype, i;

	    r.resource_name   = XtNtranslations;
	    r.resource_class  = XtCTranslations;
	    r.resource_type   = XtRString;
	    r.resource_size   = sizeof (char *);
	    r.resource_offset = 0;
	    r.default_type    = XtRString;
	    r.default_addr    = (caddr_t) NULL;

	    XtGetApplicationResources ((Widget)w, &translations, &r, 1,NULL,0);

	    if (translations) {
		if (strncmp (translations, "#augment", 8) == 0)
		    ttype = T_augment;
		else if (strncmp (translations, "#override", 9) == 0)
		    ttype = T_override;
		else
		    ttype = T_replace;

		if (ttype == T_replace) {
		    w->gterm.defTranslations =
			XtParseTranslationTable (translations);
		} else if ((i = w->gterm.nauxTrans) < MAX_AUXTRANS) {
		    w->gterm.defTranslations =
			XtParseTranslationTable (defaultGtermTranslations);
		    w->gterm.auxTrans[i] =
			XtParseTranslationTable (translations);
		    w->gterm.auxTType[i] = ttype;
		    w->gterm.nauxTrans++;
		}

	    } else {
		w->gterm.defTranslations =
		    XtParseTranslationTable (defaultGtermTranslations);
	    }

	    /* Get the marker translations. */
	    if ((char *)w->gterm.gm_defTranslations == NULL)
		w->gterm.gm_defTranslations =
		    XtParseTranslationTable (w->gterm.gm_translations);
	}

	/* Cancel any load translation table interval timer. */
	if (w->gterm.gm_timer_id) {
	    XtRemoveTimeOut (w->gterm.gm_timer_id);
	    w->gterm.gm_timer_id = (XtIntervalId) NULL;
	}

	/* Set the default gterm window translations. */
	gm_load_translations (w, NULL);

	/* Get graphics drawing GC. */
/*	gc = XCreateGC (display, w->gterm.root, 0, NULL); */
	gc = XCreateGC (display, w->gterm.window, 0, NULL);
	XSetBackground (display, gc, w->gterm.color0);
	XSetForeground (display, gc, w->gterm.color1);
	XSetLineAttributes (display, gc,
	    w->gterm.gm_lineWidth, w->gterm.gm_lineStyle, CapButt, JoinMiter);
	w->gterm.gm_drawGC = gc;

	/* Get graphics rubber-band GC. */
/*	gc = XCreateGC (display, w->gterm.root, 0, NULL); */
	gc = XCreateGC (display, w->gterm.window, 0, NULL);
	XSetFunction (display, gc, GXxor);
	XSetFillStyle (display, gc, FillSolid);
	XSetForeground (display, gc, w->gterm.gm_xorFillColor);
	XSetBackground (display, gc, w->gterm.gm_xorFillBgColor);
	XSetLineAttributes (display, gc,
	    0, LineDoubleDash, CapButt, JoinMiter);
	w->gterm.gm_rubberGC = gc;

	fg_color.pixel = w->gterm.gm_cursorFgColor;
	bg_color.pixel = w->gterm.gm_cursorBgColor;
	XQueryColor (display, w->core.colormap, &fg_color);
	XQueryColor (display, w->core.colormap, &bg_color);

	w->gterm.gm_markerCursor = XCreateFontCursor (display, XC_fleur);
	XRecolorCursor (display, w->gterm.gm_markerCursor, &fg_color,&bg_color);
	w->gterm.gm_edgeCursor = XCreateFontCursor (display, XC_dotbox);
	XRecolorCursor (display, w->gterm.gm_edgeCursor, &fg_color,&bg_color);
	w->gterm.gm_pointCursor = XCreateFontCursor (display, XC_sizing);
	XRecolorCursor (display, w->gterm.gm_pointCursor, &fg_color,&bg_color);

	if (!(type = GmStrToType (w->gterm.gm_defaultMarker)))
	    type = Gm_Rectangle;
	w->gterm.gm_defaultType = type;

	if (!null_region)
	    null_region = XCreateRegion();
	w->gterm.gm_initialized++;
    }

    w->gterm.gm_create = NULL;
    w->gterm.gm_active = NULL;
    w->gterm.gm_redisplay = False;
    w->gterm.preserve_screen = 0;
}


/* GtMarkerFree -- Free any marker subsystem resources.
 */
static void
GtMarkerFree (w)
    register GtermWidget w;
{
    register Display *display = w->gterm.display;
    register Marker gm;

    /* Cancel any load translation table interval timer. */
    if (w->gterm.gm_timer_id) {
	XtRemoveTimeOut (w->gterm.gm_timer_id);
	w->gterm.gm_timer_id = (XtIntervalId) NULL;
    }

    /* Set the default gterm window translations. */
    gm_load_translations (w, NULL);

    for (gm = w->gterm.gm_tail;  gm;  gm = gm->prev)
        GmDestroy (gm);

    if (!w->gterm.gm_initialized)
	return;

    XFreeGC (display, w->gterm.gm_drawGC);
    XFreeGC (display, w->gterm.gm_rubberGC);

    /* This call can fail - see comments elsewhere in this file about
     * XFreeCursor.
     *
    XFreeCursor (display, w->gterm.gm_markerCursor);
    XFreeCursor (display, w->gterm.gm_edgeCursor);
    XFreeCursor (display, w->gterm.gm_pointCursor);
     */

    w->gterm.gm_initialized = 0;
}


/* gm_focusin -- Called when gterm window input is directed to a marker.
 */
static void
gm_focusin (w, gm, what)
    register GtermWidget w;
    register Marker gm;
    GmSelection what;
{
    Cursor cursor;
    int erase;
    Marker am;

    if (!w || !XtIsRealized ((Widget)w))
	return;

    if (am = w->gterm.gm_active) {
	if (am != gm)
	    gm_focusout (w, 0);
	else if (what && what->type == w->gterm.gm_selection.type) {
	    /* no change */
	    return;
	}
    }

    if (what) {
	switch (what->type) {
	case Ge_Point:
	    cursor = w->gterm.gm_pointCursor;
	    break;
	case Ge_Edge:
	    cursor = w->gterm.gm_edgeCursor;
	    break;
	default:
	    cursor = w->gterm.gm_markerCursor;
	    break;
	}
    } else
	cursor = w->gterm.gm_markerCursor;

    erase_crosshair (w);
    XDefineCursor (w->gterm.display, w->gterm.window, cursor);
    w->gterm.gm_active = gm;
    w->gterm.gm_selection = *what;

    if (gm && gm != am) {
	gm_request_translations (w, gm);
	GmMarkpos (gm);
	GmRedraw (gm, GXcopy, erase=True);
    }

    gm_do_callbacks (gm, GmEvFocusIn, NULL, NULL, 0);
}


/* gm_focusout -- Called to restore the normal gterm window input when the
 * pointer moves off a marker.
 */
static void
gm_focusout (w, enableSetTrans)
    register GtermWidget w;
    int enableSetTrans;			/* replace translations */
{
    register Display *display = w->gterm.display;
    register Marker gm = w->gterm.gm_active;
    int erase, i;

    if (!w || !XtIsRealized ((Widget)w))
	return;

    /* Restore the default gterm window translations. */
    if (enableSetTrans)
	gm_request_translations (w, NULL);

    XDefineCursor (display, w->gterm.window, w->gterm.cursor);
    w->gterm.gm_active = NULL;

    if (gm) {
	GmMarkpos (gm);
	GmRedraw (gm, GXcopy, erase=True);
    }

    gm_do_callbacks (gm, GmEvFocusOut, NULL, NULL, 0);
}


/* gm_refocus -- Simulate a pointer event to recompute the marker pointer
 * focus.  Called when a software event changes the marker stacking order
 * in some way.
 */
static void 
gm_refocus (w)
    GtermWidget w;
{
    XMotionEvent event;
    int nparams = 0;

    event.type = MotionNotify;					/* MF009 */
    event.x = w->gterm.last_x;
    event.y = w->gterm.last_y;
    HandleTrackCursor ((Widget)w, &event, NULL, &nparams);
}


/* 
 * Translation tables.  The widget's translation table must not be replaced
 * while a translation is executing.  This can be a problem as it is often
 * events and their translations which lead to the translation table getting
 * replaced.  To avoid this problem we merely queue a timer event to load
 * the desired translation table, allowing any existing translation to
 * finish executing before the translation table is swapped out.  If multiple
 * translation table load requests are issued only the final one has any
 * effect.
 */

/* gm_request_translations -- Queue a request to load the translations for the
 * specified marker (or NULL to load the default gterm translations).  If this
 * is the first request and timers are enabled a timer is posted to load the
 * translations when any current event processing is complete.  If a request
 * is already active then the most recent request supercedes any previous one.
 */
static void
gm_request_translations (w, gm)
    register GtermWidget w;
    Marker gm;
{
    w->gterm.gm_reqTranslations = gm;

    if (!w->gterm.useTimers)
	gm_load_translations (w, NULL);
    else if (!w->gterm.gm_timer_id) {
	w->gterm.gm_timer_id =
	    XtAppAddTimeOut (XtWidgetToApplicationContext((Widget)w), 0,
		gm_load_translations, (XtPointer)w);
    }
}


/* gm_load_translations -- Swap out the widget's translation table.  This is
 * a no-op if the requested translation table is already loaded.
 */
static void
gm_load_translations (w, id)
    register GtermWidget w;
    XtIntervalId id;
{
    register Marker am, gm;
    register int i;

    w->gterm.gm_timer_id = (XtIntervalId) NULL;

    am = w->gterm.gm_curTranslations;
    gm = w->gterm.gm_reqTranslations;
    if (am == gm && w->gterm.gm_initialized)
	return;

    if (gm) {
	/* Set the translations for the indicated marker. */
	if (!am || am->translations != gm->translations)
	    XtOverrideTranslations ((Widget)w, gm->translations);
    } else {
	/* Restore the default gterm window translations. */
	XtVaSetValues ((Widget)w,
	    XtNtranslations, (XtArgVal)w->gterm.defTranslations, NULL);
	for (i=0;  i < w->gterm.nauxTrans;  i++) {
	    switch (w->gterm.auxTType[i]) {
	    case T_augment:
		XtAugmentTranslations ((Widget)w, w->gterm.auxTrans[i]);
		break;
	    case T_override:
		XtOverrideTranslations ((Widget)w, w->gterm.auxTrans[i]);
		break;
	    }
	}
    }

    w->gterm.gm_curTranslations = w->gterm.gm_reqTranslations;
}


/* Public marker functions.
 * --------------------------
 */

/* GmCreate -- Create a new marker.
 */
Marker
GmCreate (w, type, interactive)
    GtermWidget w;
    int type;			/* marker type */
    int interactive;		/* use pointer to set position */
{
    register Marker gm;

    /* Allocate descriptor. */
    if (type < 1 || type > Gm_NTypes)
	return (NULL);
    if (!(gm = (Marker) XtCalloc (1, sizeof (struct marker))))
	return (NULL);

    /* Initialize descriptor. */
    gm->w = w;
    gm->type = type;
    gm->flags = interactive ? (Gm_Visible|Gm_Sensitive) : 0;
    gm->translations = w->gterm.gm_defTranslations;
    gm->old_region = XCreateRegion();
    gm->cur_region = XCreateRegion();
    (gm_classinit[type-1]) (gm, interactive);

    /* Link marker to tail of marker list. */
    gm_linkafter (gm, w->gterm.gm_tail);

    /* If marker is being created interactive, set flag to indicate that the
     * next create marker event should finish creating this marker.
     */
    if (w->gterm.gm_create)
	GmDestroy (w->gterm.gm_create);
    w->gterm.gm_create = interactive ? gm : NULL;

    return (gm);
}


/* GmDestroy -- Destroy a marker.
 */
GmDestroy (gm)
    register Marker gm;
{
    register GtermWidget w = gm->w;
    Region old_region, cur_region;

    /* GmDestroy can be called recursively during a destroy operation as a
     * side effect of the destroy callback.  Set the Gm_BeingDestroyed flag
     * to cause these redundant destroy requests to be ignored.
     */
    if (gm->flags & Gm_BeingDestroyed)
	return (OK);
    gm->flags |= Gm_BeingDestroyed;

    /* Release the focus if active marker.   This should be done before
     * proceeding to destroy the marker, i.e. before calling the destroy
     * callbacks.
     */
    if (w->gterm.gm_active == gm) {
	gm_focusout (w, 1);
	w->gterm.gm_active = NULL;
    }

    /* Inform any clients that have registered a callback for this marker
     * that we are about to destroy the marker.
     */
    gm_do_callbacks (gm, GmEvDestroy, NULL, NULL, 0);

    /* Erase the marker from the screen. */
    GmMarkpos (gm);
    gm_erase (gm);

    /* Note marker position. */
    old_region = gm->old_region;
    cur_region = gm->cur_region;

    /* Free all storage and unlink the marker. */
    if (gm->npoints > GM_MAXVERTICES)
	XtFree ((char *)gm->points);
    if (gm->text)
	XtFree ((char *)gm->text);
    if (gm->pgon)
	XtFree ((char *)gm->pgon);

    gm_unlink (gm);
    XtFree ((char *)gm);

    /* Redraw any markers that were obscured by the deleted marker. */
    update_transients (w, old_region);

    XDestroyRegion (old_region);
    XDestroyRegion (cur_region);

    /* Recompute the marker focus. */
    gm_refocus (w);

    return (OK);
}


/* GmCopy -- Copy a marker.
 */
Marker
GmCopy (gm)
    register Marker gm;
{
    register GtermWidget w = gm->w;
    register Marker nm;

    if (!(nm = (Marker) XtCalloc (1, sizeof (struct marker))))
	return (NULL);

    *nm = *gm;
    nm->parent = gm;
    nm->old_region = NULL;
    nm->cur_region = NULL;
    nm->points = NULL;
    nm->pgon = NULL;
    nm->text = NULL;

    /* Copy region descriptors. */
    if ((char *)(nm->old_region = XCreateRegion()) == NULL)
	goto fail;
    if ((char *)(nm->cur_region = XCreateRegion()) == NULL)
	goto fail;
    XUnionRegion (nm->old_region, gm->cur_region, nm->cur_region);

    /* Copy any polypoint data. */
    if (gm->pgon) {
	nm->pgon = (DPoint *) XtMalloc (gm->npoints * sizeof(DPoint));
	if (nm->pgon == NULL)
	    goto fail;
	memmove (nm->pgon, gm->pgon, gm->npoints * sizeof(DPoint));
    }

    /* Copy region polygon. */
    if (gm->npoints > GM_MAXVERTICES) {
	if (!(nm->points = (XPoint *) XtMalloc (gm->npoints * sizeof(XPoint))))
	    goto fail;
	memmove (nm->points, gm->points, gm->npoints * sizeof(XPoint));
    }

    /* Copy any text data. */
    if (gm->text) {
	int nchars = strlen (gm->text);
	if (!(nm->text = XtMalloc (nchars + 1)))
	    goto fail;
	memmove (nm->text, gm->text, nchars + 1);
    }

    gm_linkafter (nm, w->gterm.gm_tail);
    return (nm);

fail:
    if (nm->text)
	XtFree (nm->text);
    if (nm->pgon)
	XtFree ((char *)nm->pgon);
    if (nm->points && nm->points != nm->point_data)
	XtFree ((char *)nm->points);
    if ((char *)nm->cur_region)
	XDestroyRegion (nm->cur_region);
    if ((char *)nm->old_region)
	XDestroyRegion (nm->old_region);

    XtFree ((char *)nm);
    return (NULL);
}


/* GmAddCallback -- Add a callback to a marker.
 */
GmAddCallback (gm, events, func, client_data)
    register Marker gm;
    int events;			/* events callback is to receive */
    GmIMethod func;		/* function to be called */
    XtPointer client_data;	/* client data for above */
{
    register struct markerCallback *cb;
    register int i;

    /* Find an empty callback slot. */
    for (i=0;  i < GM_MAXCALLBACKS;  i++)
	if (!gm->callback[i].events)
	    break;

    /* Register the callback. */
    if (i < GM_MAXCALLBACKS) {
	cb = &gm->callback[i];
	cb->events = events;
	cb->func = func;
	cb->client_data = client_data;
	gm->ncallbacks = max (gm->ncallbacks, i + 1);
    }

    if (events & GmEvConstraint)
	gm->constraints++;
}


/* GmDeleteCallback -- Delete a previously posted callback given the
 * function pointer and client data passed when the callback was registered.
 */
GmDeleteCallback (gm, func, client_data)
    register Marker gm;
    GmIMethod func;		/* callback function */
    XtPointer client_data;	/* client data for above */
{
    register struct markerCallback *cb;
    register int i, n;

    for (i=n=0;  i < GM_MAXCALLBACKS;  i++) {
	cb = &gm->callback[i];

	if (cb->func == func && cb->client_data == client_data) {
	    if (cb->events & GmEvConstraint)
		gm->constraints--;
	    cb->events = (int)NULL;
	    cb->func = (GmIMethod)NULL;
	    cb->client_data = (XtPointer)NULL;
	} else if (cb->events)
	    n = i;
    }

    gm->ncallbacks = n + 1;
}


/* GmSelect -- Scan the marker list to see if the given pointer coordinates
 * are within an active marker.  If so, the marker descriptor is returned as
 * the function value, and the "what" argument is set to indicate what part
 * of the marker was selected.
 */
Marker
GmSelect (w, x, y, what)
    GtermWidget w;
    int x, y;
    GmSelection what;
{
    register int flags = (Gm_Activated|Gm_Visible|Gm_Sensitive);
    register XRectangle *r;
    register Marker gm;

    for (gm = w->gterm.gm_tail;  gm;  gm = gm->prev) {
	if (!((gm->flags & (flags|Gm_BeingDestroyed)) == flags))
	    continue;
	r = &gm->cur_rect;
	if (x < (int)r->x || x >= (int)(r->x + r->width) ||
	    y < (int)r->y || y >= (int)(r->y + r->height))
	    continue;
	if (gm->select (gm, x, y, what))
	    return (gm);
    }

    return (NULL);
}


/* GmMarkpos -- Save the current marker position, e.g., prior to modifying
 * the marker.  This is used to erase the old marker when the modified
 * marker is later redrawn.
 */
GmMarkpos (gm)
    register Marker gm;
{
    gm->markpos (gm);
}


/* GmRedraw -- Redraw a marker using the given drawing function.  If the erase
 * flag is not set (as when in rubber-band mode) the marker is merely drawn
 * to the screen.  Otherwise if the old marker position has been saved the
 * old marker is first erased, then any markers affected by the erase are
 * redrawn, and finally the current marker is redrawn at the new location.
 */
GmRedraw (gm, func, erase)
    Marker gm;
    int func;
    int erase;
{
    register Marker mm;
    register XRectangle *o, *n, *r;
    int flags = (Gm_Activated|Gm_Visible);
    Region clip_region, temp_region, temp;
    GtermWidget w = gm->w;
    int outside;

    /* Recompute marker polygon if any attributes have changed. */
    gm->update (gm);

    clip_region = XCreateRegion();
    temp_region = XCreateRegion();

    /* Erase the previously marked region (old position). */
    if (erase) {
	XUnionRegion (gm->old_region, clip_region, temp_region);
	temp = clip_region; clip_region = temp_region; temp_region = temp;
	XSetRegion (w->gterm.display, w->gterm.exposeGC, clip_region);
	gm_erase (gm);
    }

    if (!erase && func == GXxor)
	gm->redraw (gm, func);
    else {
	/* Draw the marker and any markers it intersects, clipping to the
	 * new marker region.
	 */
	o = &gm->old_rect;
	n = &gm->cur_rect;

	XUnionRegion (gm->cur_region, clip_region, temp_region);
	temp = clip_region; clip_region = temp_region; temp_region = temp;
	XSetRegion (w->gterm.display, w->gterm.gm_drawGC, clip_region);

	for (mm = gm->w->gterm.gm_head;  mm;  mm = mm->next) {
	    if (!((mm->flags & flags) == flags))
		continue;

	    /* Redraw a marker if it intersects either the old rect or the
	     * new rect.
	     */
	    if (mm != gm) {
		r = &mm->cur_rect;
		outside = 0;
		if ((int)r->x >= (int)o->x + (int)o->width  ||
			(int)r->x + (int)r->width <= (int)o->x ||
		    (int)r->y >= (int)o->y + (int)o->height ||
			(int)r->y + (int)r->height <= (int)o->y)
		    outside++;
		if ((int)r->x >= (int)n->x + (int)n->width  ||
			(int)r->x + (int)r->width  <= (int)n->x ||
		    (int)r->y >= (int)n->y + (int)n->height ||
			(int)r->y + (int)r->height <= (int)n->y)
		    outside++;
		if (outside == 2)
		    continue;
	    }
	    mm->redraw (mm, func);
	}

	XSetClipMask (w->gterm.display, w->gterm.gm_drawGC, None);
    }

    if (erase)
	XSetClipMask (w->gterm.display, w->gterm.exposeGC, None);
    XDestroyRegion (clip_region);
    XDestroyRegion (temp_region);

    if (func != GXxor && gm->width > 0 && gm->height > 0) {
	/* Redraw callback. */
	gm_do_callbacks (gm, GmEvRedraw, NULL, NULL, 0);

	/* Generate moveResize callback, if marker was moved or resized.
	 */
	if (gm->old_rect.x != gm->cur_rect.x ||
	    gm->old_rect.y != gm->cur_rect.y ||
	    gm->old_rect.width != gm->cur_rect.width ||
	    gm->old_rect.height != gm->cur_rect.height) {

	    char x[32], y[32];
	    char width[32], height[32];
	    char *argv[5];
	    int argc;

	    /* If the marker was just created (old_rect null) or the marker
	     * moved and we did a full erase and redraw, any old markpos is
	     * obsolete so we may as well update the saved position.
	     */
	    if (erase || !gm->old_rect.width || !gm->old_rect.height)
		GmMarkpos (gm);

	    sprintf (x, "%d", gm->x);
	    sprintf (y, "%d", gm->y);
	    sprintf (width, "%d", gm->width);
	    sprintf (height, "%d", gm->height);
	    argv[0] = x;
	    argv[1] = y;
	    argv[2] = width;
	    argv[3] = height;
	    argv[4] = NULL;
	    argc = 4;

	    gm_do_callbacks (gm, GmEvMoveResize, NULL, argv, argc);
	}
    }
}


/* GmRedisplay -- Redisplay the markers in the given region, or redisplay
 * the entire window if the region is given as (char *)NULL.
 */
GmRedisplay (w, region)
    GtermWidget w;
    Region region;
{
    register int flags = (Gm_Activated|Gm_Visible);
    register XRectangle *r;
    register Marker gm;

    if (!w || !XtIsRealized ((Widget)w))
	return;

    /* Set the clip mask to only draw in the affected region. */
    if (region)
	XSetRegion (w->gterm.display, w->gterm.gm_drawGC, region);

    /* Draw all markers that intersect the target region. */
    for (gm = w->gterm.gm_head;  gm;  gm = gm->next) {
	if (!((gm->flags & flags) == flags))
	    continue;

	if ((char *)region) {
	    gm->update (gm);
	    r = &gm->cur_rect;
	    if (XRectInRegion (region,
		r->x, r->y, r->width, r->height) == RectangleOut)
		continue;
	}

	gm->redraw (gm, GXcopy);
    }

    XSetClipMask (w->gterm.display, w->gterm.gm_drawGC, None);
    w->gterm.gm_redisplay = False;
}


/* GmRaise -- Change the stacking order of a marker relative to another
 * marker, causing the first marker to be drawn above the second.
 */
GmRaise (gm, ref_gm)
    register Marker gm, ref_gm;
{
    register GtermWidget w = gm->w;
    int erase;

    /* Already on top? */
    if (gm == w->gterm.gm_tail || ref_gm && ref_gm->next == gm)
	return;

    /* Raise it. */
    gm_unlink (gm);
    gm_linkafter (gm, ref_gm ? ref_gm : w->gterm.gm_tail);

    GmMarkpos (gm);
    GmRedraw (gm, GXcopy, erase=True);
    gm_refocus (w);
}


/* GmLower -- Change the stacking order of a marker relative to another
 * marker, causing the first marker to be drawn below the second.
 */
GmLower (gm, ref_gm)
    register Marker gm, ref_gm;
{
    register GtermWidget w = gm->w;
    int erase;

    /* Already lowered? */
    if (gm == w->gterm.gm_head || ref_gm && ref_gm->prev == gm)
	return;

    /* Lower it. */
    gm_unlink (gm);
    if (ref_gm && ref_gm->prev)
	gm_linkafter (gm, ref_gm->prev);
    else {
	gm->next = w->gterm.gm_head;
	w->gterm.gm_head = gm;
	if (gm->next)
	    gm->next->prev = gm;
	if (!w->gterm.gm_tail)
	    w->gterm.gm_tail = gm;
    }

    GmMarkpos (gm);
    GmRedraw (gm, GXcopy, erase=True);
    gm_refocus (w);
}


/* GmNotify -- Notify any clients that have registered callbacks that the
 * given marker events have occurred.
 */
GmNotify (gm, events, event, params, nparams)
    register Marker gm;
    int events;
    XEvent *event;
    String *params;
    Cardinal nparams;
{
    gm_do_callbacks (gm, events, event, params, nparams);
}


/* GmAddPt -- Add a point to a marker.
 */
GmAddPt (gm, x, y)
    register Marker gm;
    int x, y;
{
    int erase;
    if (gm->addPt) {
	GmRedraw (gm, GXxor, erase=False);
	gm->addPt (gm, x, y);
	GmRedraw (gm, GXxor, erase=False);
	gm_refocus (gm->w);
    }
}


/* GmDeletePt -- Delete a point from a marker.
 */
GmDeletePt (gm, x, y)
    register Marker gm;
    int x, y;
{
    int erase;
    if (gm->deletePt) {
	GmMarkpos (gm);
	gm->deletePt (gm, x, y);
	GmRedraw (gm, GXcopy, erase=True);
	gm_refocus (gm->w);
    }
}


/* GmMovePt -- Move a point within a marker.
 */
GmMovePt (gm, x, y)
    register Marker gm;
    int x, y;
{
    int erase;
    if (gm->movePt) {
	GmRedraw (gm, GXxor, erase=False);
	gm->movePt (gm, x, y);
	GmRedraw (gm, GXxor, erase=False);
    }
}


/* GmMove -- Move a marker.
 */
GmMove (gm, x, y)
    register Marker gm;
    int x, y;
{
    int erase;
    if (gm->move) {
	GmRedraw (gm, GXxor, erase=False);
	gm->move (gm, x, y);
	GmRedraw (gm, GXxor, erase=False);
    }
}


/* GmResize -- Resize a marker.
 */
GmResize (gm, x, y)
    register Marker gm;
    int x, y;
{
    int erase;
    if (gm->resize) {
	GmRedraw (gm, GXxor, erase=False);
	gm->resize (gm, x, y);
	GmRedraw (gm, GXxor, erase=False);
    }
}


/* GmRotate -- Rotate a marker.
 */
GmRotate (gm, x, y)
    register Marker gm;
    int x, y;
{
    int erase;
    if (gm->rotate) {
	GmRedraw (gm, GXxor, erase=False);
	gm->rotate (gm, x, y);
	GmRedraw (gm, GXxor, erase=False);
    }
}


/* GmSetAttributes -- Set a list of attributes.  Requires that all attribute
 * values be specified in the same type.  Autoredraw, if enabled, is suspended
 * until all attributes have been changed.
 */
GmSetAttributes (gm, args, nargs, argtype)
    register Marker gm;
    ArgList args;
    int nargs;
    char *argtype;
{
    register int i;
    int autoredraw, erase;
    int status = OK;

    if (autoredraw = (gm->flags & Gm_AutoRedraw)) {
	gm->flags &= ~Gm_AutoRedraw;
	GmMarkpos (gm);
    }
    
    for (i=0;  i < nargs;  i++) {
	status |= GmSetAttribute (gm, args[i].name, args[i].value, argtype);
	if (strcmp (args[i].name, GmAutoRedraw) == 0)
	    autoredraw = gm_getint (args[i].value, argtype);
    }

    if (autoredraw) {
	gm->flags |= Gm_AutoRedraw;
	GmRedraw (gm, GXcopy, erase=True);
    }

    return (status ? ERR : OK);
}


/* GmSetAttribute -- Set the value of a marker attribute.
 */
GmSetAttribute (gm, attribute, value, type)
    register Marker gm;
    char *attribute;
    XtArgVal value;
    char *type;
{
    GtermWidget w = gm->w;
    int marker_type, atType;
    int erase, n, i;

    if (gm->flags & Gm_AutoRedraw)
	GmMarkpos (gm);

    switch (atType = gm_getattribute (attribute)) {
    case Ga_Type:
	switch (gm_gettype (type)) {
	case Gt_String:
	    marker_type = GmStrToType ((char *)value);
	    break;
	case Gt_Int:
	    marker_type = gm_getint (value, type);
	    break;
	default:
	    return (ERR);
	}

	marker_type = max(1, min(Gm_NTypes, marker_type));
	(gm_classinit[marker_type-1]) (gm, False);
	gm->flags |= Gm_Modified;
	break;

    case Ga_Activated:
	if (gm_getint (value, type)) {
	    if (!(gm->flags & Gm_Activated)) {
		gm->flags |= Gm_Activated;
		GmRedraw (gm, GXcopy, erase=False);
	    }
	} else {
	    GmMarkpos (gm);
	    gm_erase (gm);
	    gm->flags &= ~Gm_Activated;
	}
	return (OK);

    case Ga_Visible:
	if (gm_getint (value, type)) {
	    if (!(gm->flags & Gm_Visible)) {
		gm->flags |= Gm_Visible;
 		GmRedraw (gm, GXcopy, erase=False); 
	    }
	} else if (gm->flags & Gm_Visible) {
	    GmMarkpos (gm);
	    gm_erase (gm);
	    gm->flags &= ~Gm_Visible;
	}
	return (OK);

    case Ga_Sensitive:
	if (gm_getint (value, type))
	    gm->flags |= Gm_Sensitive;
	else
	    gm->flags &= ~Gm_Sensitive;
	return (OK);

    case Ga_AutoRedraw:
	if (gm_getint (value, type))
	    gm->flags |= Gm_AutoRedraw;
	else
	    gm->flags &= ~Gm_AutoRedraw;
	return (OK);

    case Ga_Translations:
	switch (gm_gettype (type)) {
	case Gt_String:
	    gm->translations = XtParseTranslationTable ((char *)value);
	    break;
	default:
	    return (ERR);
	}
	return (OK);

    case Ga_X:
	gm->x = gm_getint (value, type);
	break;
    case Ga_Y:
	gm->y = gm_getint (value, type);
	break;

    case Ga_Width:
    case Ga_Height:
	/* For a text marker a size can be specified either in integer
	 * pixels or in characters, e.g., "40ch" or "40 chars".
	 */
	if (gm->type == Gm_Text && type == XtRString) {
	    XFontStruct *fp = gm->font;
	    int char_width, char_height;
	    int l_pix, r_pix;
	    char *ip;

	    for (n=0, ip=(char *)value;  *ip && isdigit(*ip);  ip++)
		n = n * 10 + (*ip - '0');

	    while (isspace (*ip))
		ip++;
	    if (ip[0] == 'c' && ip[1] == 'h') {
		char_width  = fp->max_bounds.width;
		char_height = fp->max_bounds.ascent + fp->max_bounds.descent;
		l_pix = (gm->lineWidth + 1) / 2 + gm->textBorder - 1;
		r_pix = (gm->lineWidth + 1) / 2 + gm->textBorder - 1 -
		    (fp->max_bounds.width - fp->max_bounds.rbearing);

		if (atType == Ga_Width)
		    n = n * char_width + l_pix + r_pix;
		else
		    n = n * char_height + l_pix * 2;
	    }
	} else
	    n = gm_getint (value, type);

	if (atType == Ga_Width)
	    gm->width = n;
	else
	    gm->height = n;
	break;

    case Ga_Rotangle:						/* MF022 */
 	gm->rotangle = gm_getfloat (value, type) * (M_PI / (double) 180.0);
	break;

    case Ga_HighlightColor:
	gm->highlightColor = gm_getpixel (w, value, type);
	break;
    case Ga_LineColor:
	gm->lineColor = gm_getpixel (w, value, type);
	break;
    case Ga_LineWidth:
	gm->lineWidth = gm_getint (value, type);
	break;
    case Ga_LineStyle:
	gm->lineStyle = gm_getint (value, type);
	break;

    case Ga_KnotColor:
	gm->knotColor = gm_getpixel (w, value, type);
	break;
    case Ga_KnotSize:
	gm->knotSize = gm_getint (value, type);
	break;

    case Ga_Fill:
	gm->fill = gm_getint (value, type);
	break;
    case Ga_FillColor:
	gm->fillColor = gm_getpixel (w, value, type);
	break;
    case Ga_FillBgColor:
	gm->fillBgColor = gm_getpixel (w, value, type);
	break;
    case Ga_FillStyle:
	switch (gm_gettype (type)) {
	case Gt_String:
	    gm->fillStyle = gm_getfillstyle (w, value, type);
	    break;
	default:
	    break;
	}
	break;
    case Ga_FillPattern:
	switch (gm_gettype (type)) {
	case Gt_Pointer:
	    gm->fillPattern = (Pixmap) (value);
	    break;
	default:
	    return (ERR);
	}
	break;

    case Ga_TextColor:
	gm->textColor = gm_getpixel (w, value, type);
	break;
    case Ga_TextBgColor:
	gm->textBgColor = gm_getpixel (w, value, type);
	break;
    case Ga_TextBorder:
	gm->textBorder = gm_getint (value, type);
	break;
    case Ga_ImageText:
	gm->imageText = gm_getint (value, type);
	break;
    case Ga_Font:
	switch (gm_gettype (type)) {
	case Gt_Int:
	    i = gm_getint (value, type);
	    if (i >= 0 && i < NDialogFonts)
		gm->font = w->gterm.dialog_fonts[i];
	    break;
	case Gt_Pointer:
	    gm->font = (XFontStruct *) (value);
	    break;
	default:
	    return (ERR);
	}
	break;
    case Ga_Text:
	switch (gm_gettype (type)) {
	case Gt_Pointer:
	case Gt_String:
	    if (gm->text)
		XtFree (gm->text);
	    if (!(gm->text = XtMalloc (strlen((char *)value) + 1)))
		return (ERR);
	    strcpy (gm->text, (char *)value);
	    break;
	default:
	    return (ERR);
	}
	break;
    case Ga_RotIndicator:					/* MF020 */
	gm->rotIndicator = gm_getint (value, type);
	break;

    default:
	return (ERR);
    }

    gm->flags |= Gm_Modified;

    if (gm->flags & Gm_AutoRedraw)
	GmRedraw (gm, GXcopy, erase=True);

    /* Notify client that a marker attribute has changed. */
    {   char *argv[2];
	int argc;

	argv[0] = attribute;
	argv[1] = NULL;
	argc = 1;

	gm_do_callbacks (gm, GmEvModify, NULL, argv, argc);
    }

    return (OK);
}


/* GmGetAttributes -- Get a list of attributes.  Requires that all attribute
 * values be specified in the same type.
 */
GmGetAttributes (gm, args, nargs, argtype)
    register Marker gm;
    ArgList args;
    int nargs;
    char *argtype;
{
    register int i;

    for (i=0;  i < nargs;  i++)
	GmGetAttribute (gm, args[i].name, args[i].value, argtype);
}


/* GmGetAttribute -- Get the value of a marker attribute.
 */
GmGetAttribute (gm, attribute, value, type)
    register Marker gm;
    char *attribute;
    XtArgVal value;
    char *type;
{
    GtermWidget w = gm->w;
    int i;

    switch (gm_getattribute (attribute)) {
    case Ga_Type:
	switch (gm_gettype (type)) {
	case Gt_String:
	    switch (gm->type) {
	    case Gm_Text:
		strcpy ((char *)value, GmText);
		break;
	    case Gm_Line:
		strcpy ((char *)value, GmLine);
		break;
	    case Gm_Polyline:
		strcpy ((char *)value, GmPolyline);
		break;
	    case Gm_Rectangle:
		strcpy ((char *)value, GmRectangle);
		break;
	    case Gm_Box:
		strcpy ((char *)value, GmBox);
		break;
	    case Gm_Circle:
		strcpy ((char *)value, GmCircle);
		break;
	    case Gm_Ellipse:
		strcpy ((char *)value, GmEllipse);
		break;
	    case Gm_Polygon:
		strcpy ((char *)value, GmPolygon);
		break;
	    default:
		return (ERR);
	    }
	    break;
	case Gt_Int:
	    if (gm_putint (gm->type, value, type) == ERR)
		return (ERR);
	    break;
	default:
	    return (ERR);
	}
	break;

    case Ga_Activated:
	if (gm_putint ((gm->flags & Gm_Activated) != 0, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_Visible:
	if (gm_putint ((gm->flags & Gm_Visible) != 0, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_Sensitive:
	if (gm_putint ((gm->flags & Gm_Sensitive) != 0, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_AutoRedraw:
	if (gm_putint ((gm->flags & Gm_AutoRedraw) != 0, value, type) == ERR)
	    return (ERR);
	break;

    case Ga_X:
	if (gm_putint (gm->x, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_Y:
	if (gm_putint (gm->y, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_Width:
	if (gm_putint (gm->width, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_Height:
	if (gm_putint (gm->height, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_Rotangle:						/* MF022 */
	if (gm_putfloat(((double)180.0/M_PI)*(gm->rotangle),value,type) == ERR)
	    return (ERR);
	break;

    case Ga_HighlightColor:
	if (gm_putint ((int)gm->highlightColor, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_LineColor:
	if (gm_putint ((int)gm->lineColor, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_LineWidth:
	if (gm_putint (gm->lineWidth, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_LineStyle:
	if (gm_putint (gm->lineStyle, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_KnotColor:
	if (gm_putint ((int)gm->knotColor, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_KnotSize:
	if (gm_putint (gm->knotSize, value, type) == ERR)
	    return (ERR);
	break;

    case Ga_Fill:
	if (gm_putint (gm->fill, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_FillColor:
	if (gm_putint ((int)gm->fillColor, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_FillBgColor:
	if (gm_putint ((int)gm->fillBgColor, value, type) == ERR)
	    return (ERR);
	break;

    case Ga_FillStyle:
	switch (gm_gettype (type)) {
	case Gt_String:
	    switch (gm->fillStyle) {
	    case FillSolid:
		strcpy ((char *)value, "FillSolid");
		break;
	    case FillTiled:
		strcpy ((char *)value, "FillTiled");
		break;
	    case FillStippled:
		strcpy ((char *)value, "FillStippled");
		break;
	    case FillOpaqueStippled:
		strcpy ((char *)value, "FillOpaqueStippled");
		break;
	    default:
		strcpy ((char *)value, "FillSolid");
		break;
	    }
	    break;
	case Gt_Int:
	    if (gm_putint (gm->fillStyle, value, type) == ERR)
		return (ERR);
	    break;
	default:
	    return (ERR);
	}
	break;

    case Ga_FillPattern:
	switch (gm_gettype (type)) {
	case Gt_Pointer:
	    *(Pixmap *)value = gm->fillPattern;
	    break;
	default:
	    return (ERR);
	}
	break;

    case Ga_TextColor:
	if (gm_putint ((int)gm->textColor, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_TextBorder:
	if (gm_putint (gm->textBorder, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_ImageText:
	if (gm_putint (gm->imageText, value, type) == ERR)
	    return (ERR);
	break;
    case Ga_Font:
	switch (gm_gettype (type)) {
	case Gt_Int:
	    for (i=0;  i < NDialogFonts;  i++)
		if (gm->font == w->gterm.dialog_fonts[i]) {
		    if (gm_putint (i, value, type) == ERR)
			return (ERR);
		    break;
		}
	    break;
	case Gt_Pointer:
	    *(XFontStruct **)value = gm->font;
	    break;
	default:
	    return (ERR);
	}
	break;
    case Ga_Text:
	switch (gm_gettype (type)) {
	case Gt_Pointer:
	    *((char **)value) = gm->text;
	    break;
	case Gt_String:
	    strcpy ((char *)value, gm->text);
	    break;
	default:
	    return (ERR);
	}
	break;
    case Ga_RotIndicator:					/* MF020 */
	if (gm_putint (gm->rotIndicator, value, type) == ERR)
	    return (ERR);
	break;


    default:
	return (ERR);
    }

    return (OK);
}


/* GmSetVertices -- Set the vertices of a "poly" type object.
 */
GmSetVertices (gm, points, first, npts)
    Marker gm;
    DPoint *points;		/* input array of points */
    int first;			/* first point to be set */
    int npts;			/* number of points to set */
{
    register DPoint *ip, *pp;
    register XPoint *op;
    register int i;
    int erase;

    /* The point vector is automatically extended if more space is needed.
     * Small vectors are stored directly in the marker descriptor in the
     * point_data array.
     */
    if (first + npts != gm->npoints) {				/* MF013 */
	if (gm->npoints > GM_MAXVERTICES) {
	    if ((gm->points = (XPoint *) XtRealloc ((char *)gm->points,
		    first + npts)) == (XPoint *)NULL)
		return;
	} else if (first + npts > GM_MAXVERTICES) {
	    if ((gm->points = (XPoint *)
		    XtMalloc (first + npts)) == (XPoint *)NULL)
		return;
	} else if (!gm->points)
	    gm->points = gm->point_data;

	gm->npoints = first + npts;
    }

    /* Copy the point data. */
    ip = points;
    op = &gm->points[first];
    for (i=0;  i < npts;  i++) {
	op->x = (int) ip->x + 0.5;
	op->y = (int) ip->y + 0.5;
	ip++, op++;
    }

    /* If we're defining the vertices of a 'poly' marker update the
     * pgon[] array with the new set of points.  Polygons are initialized
     * with a unit rectangle and since vertices can't be set as an attribute
     * the must be set with a setVertices call so we need to update the
     * structure here.
     */
    if (gm->type == Gm_Polygon) {				/* MF018 */

        if (gm->pgon)						/* MF018 */
            XtFree ((char *)gm->pgon);
        gm->pgon = (DPoint *) XtCalloc (first+npts+1, sizeof(DPoint));

        /* Copy the point data to the polygon array. */
        op = &gm->points[0];
        pp = &gm->pgon[0];
        for (i=0; i< gm->npoints; i++, pp++, op++) {
            pp->x = (double)op->x - gm->x;
            pp->y = (double)op->y - gm->y;
        }
        gm->points[first+npts] = gm->points[0];   /* Close the polygon.       */

        gm->npoints = gm->pgon_npts = first + npts + 1;
        gm->rotangle = 0.0;             	  /* reset rotation angle     */
        gm->flags |= Gm_Modified; 		  /* marker has been modified */
    }

    /* Redraw the marker if autoredraw is enabled. */
    if (gm->flags & Gm_AutoRedraw) {
	GmMarkpos (gm);
	GmRedraw (gm, GXcopy, erase=True);
    }
}


/* GmGetVertices -- Get the vertices of a "poly" type object.  The actual
 * number of points output is returned as the function value.
 */
GmGetVertices (gm, points, first, maxpts)
    register Marker gm;
    register DPoint *points;	/* output array of points */
    int first;			/* first point to be returned */
    int maxpts;			/* max number of points to return */
{
    register XPoint *ip;
    register DPoint *op;
    register int i;
    int top, nout;

    if (first >= gm->npoints)
	return (0);
    top = min (first + maxpts, gm->npoints);
    nout = top - first;

    /* In the case of a poly object don't return the closing segment. */
    if (gm->type == Gm_Polygon) 				/* MF027 */
        --nout;

    if (points) {
	ip = &gm->points[first];
	op = points;
	for (i=0;  i < nout;  i++) {
	    op->x = ip->x;
	    op->y = ip->y;
	    ip++, op++;
	}
    }

    return (nout);
}


/* GmGetBoundingBox -- Returns a rect large enough to completely enclose a
 * marker, regardless of its type or orientation.
 */
GmGetBoundingBox (gm, x, y, width, height)
    register Marker gm;
    int *x, *y;
    int *width, *height;
{
    register XRectangle *r = &gm->cur_rect;

    *x = r->x;
    *y = r->y;
    *width = r->width;
    *height = r->height;
}


/* GmStrToType -- Convert a marker type string to a marker type code.
 */
GmStrToType (marker_type)
register char *marker_type;
{
    register int type;

    if (strcmp (marker_type, GmText) == 0)
	type = Gm_Text;
    else if (strcmp (marker_type, GmLine) == 0)
	type = Gm_Line;
    else if (strcmp (marker_type, GmPolyline) == 0)
	type = Gm_Polyline;
    else if (strcmp (marker_type, GmRectangle) == 0)
	type = Gm_Rectangle;
    else if (strcmp (marker_type, GmBox) == 0)
	type = Gm_Box;
    else if (strcmp (marker_type, GmCircle) == 0)
	type = Gm_Circle;
    else if (strcmp (marker_type, GmEllipse) == 0)
	type = Gm_Ellipse;
    else if (strcmp (marker_type, GmPolygon) == 0)
	type = Gm_Polygon;
    else
	type = 0;

    return (type);
}


/* GmStrToEvent -- Convert a marker event type string to a marker event code.
 */
GmStrToEvent (event_type)
register char *event_type;
{
    register int type;

    if (strcmp (event_type, "notify") == 0)
	type = GmEvNotify;
    else if (strcmp (event_type, "moveResize") == 0)
	type = GmEvMoveResize;
    else if (strcmp (event_type, "modify") == 0)
	type = GmEvModify;
    else if (strcmp (event_type, "redraw") == 0)
	type = GmEvRedraw;
    else if (strcmp (event_type, "destroy") == 0)
	type = GmEvDestroy ;
    else if (strcmp (event_type, "input") == 0)
	type = GmEvInput;
    else if (strcmp (event_type, "focusIn") == 0)
	type = GmEvFocusIn;
    else if (strcmp (event_type, "focusOut") == 0)
	type = GmEvFocusOut;
    else if (strcmp (event_type, "constraint") == 0)
	type = GmEvConstraint;
    else
	type = 0;

    return (type);
}


/* GmStrToFunction -- Convert a drawing function string to the corresponding
 * XLIB function code.
 */
GmStrToFunction (function)
register char *function;
{
    register int code;

    if (strcmp (function, "clear") == 0)
	code = GXclear;
    else if (strcmp (function, "and") == 0)
	code = GXand;
    else if (strcmp (function, "andReverse") == 0)
	code = GXandReverse;
    else if (strcmp (function, "copy") == 0)
	code = GXcopy;
    else if (strcmp (function, "andInverted") == 0)
	code = GXandInverted;
    else if (strcmp (function, "noop") == 0)
	code = GXnoop;
    else if (strcmp (function, "xor") == 0)
	code = GXxor;
    else if (strcmp (function, "or") == 0)
	code = GXor;
    else if (strcmp (function, "nor") == 0)
	code = GXnor;
    else if (strcmp (function, "equiv") == 0)
	code = GXequiv;
    else if (strcmp (function, "invert") == 0)
	code = GXinvert;
    else if (strcmp (function, "orReverse") == 0)
	code = GXorReverse;
    else if (strcmp (function, "copyInverted") == 0)
	code = GXcopyInverted;
    else if (strcmp (function, "orInverted") == 0)
	code = GXorInverted;
    else if (strcmp (function, "nand") == 0)
	code = GXnand;
    else if (strcmp (function, "set") == 0)
	code = GXset;
    else
	code = -1;

    return (code);
}


/* Internal procedures for above code.
 * ------------------------------------
 */

static int
gm_getint (value, type)
    XtArgVal value;
    char *type;
{
    register int ch;

    switch (gm_gettype (type)) {
    case Gt_Bool:
    case Gt_Int:
	return ((int)value);
    case Gt_DFloatP:
	return (*(double *)value);
    case Gt_String:
	ch = *((char *)value);
	if (ch == 'T' || ch == 't')
	    return (1);
	else if (ch == 'F' || ch == 'f')
	    return (0);
	else
	    return (atoi((char *)value));
    default:
	return (0);
    }
}


static Pixel
gm_getpixel (w, value, type)
    GtermWidget w;
    XtArgVal value;
    char *type;
{
    XrmValue from, to;
    Pixel pixel;
    char *str;

    switch (gm_gettype (type)) {
    case Gt_Int:
	/* Pixel value (colormap index). */
	return ((Pixel)value);

    case Gt_String:
	/* The pixel is expressed either as a pixel number input as a string,
	 * or as a color name.  The latter case requires a type conversion.
	 */
	str = (char *)value;
	if (isdigit(str[0]) && (int)strlen(str) <= 3) {
	    int index = atoi (str);
	    pixel = w->gterm.cmap[index];
	    return (pixel);
	}

	if (w->gterm.useDefaultCM || !w->gterm.haveColormap) {
	    /* Allocate color from default colormap.
	     */
	    from.size = strlen ((char *)value) + 1;
	    from.addr = (char *)value;
	    to.addr = (caddr_t) &pixel;
	    to.size = sizeof(pixel);

	    if (!XtConvertAndStore ((Widget)w, XtRString, &from, XtRPixel, &to))
		pixel = w->gterm.cmap[1];

	} else {
	    /* Allocate closest match from custom colormap.  This is crude,
	     * but for the standard colors this will return an exact match.
	     */
	    int index, min_dist, dist, i;
	    XColor exact, best, *cp;

	    pixel = w->gterm.cmap[1];
	    if (XLookupColor (w->gterm.display,
		    get_colormap(w), str, &exact, &best)) {
		min_dist = 9999;
		index = 1;

		for (i=0;  i < w->gterm.ncolors;  i++) {
		    cp = &w->gterm.color[i];
		    dist = abs((int)exact.red - (int)cp->red) +
			   abs((int)exact.green - (int)cp->green) +
			   abs((int)exact.blue - (int)cp->blue);
		    if (dist == 0) {
			index = i;
			break;
		    } else if (dist < min_dist) {
			index = i;
			min_dist = dist;
		    }
		}

		pixel = w->gterm.color[index].pixel;
	    }
	}
	return (pixel);

    default:
	return (w->gterm.cmap[1]);
    }
}


static int
gm_getfillstyle (w, value, type)
    GtermWidget w;
    XtArgVal value;
    char *type;
{
    switch (gm_gettype (type)) {
    case Gt_String:
	if (strcmp ((char *)value, "FillSolid") == 0)
	    return (FillSolid);
	else if (strcmp ((char *)value, "FillTiled") == 0)
	    return (FillTiled);
	else if (strcmp ((char *)value, "FillStippled") == 0)
	    return (FillStippled);
	else if (strcmp ((char *)value, "FillOpaqueStippled") == 0)
	    return (FillOpaqueStippled);
	break;
    default:
	break;
    }

    return (FillSolid);
}


static double
gm_getfloat (value, type)
    XtArgVal value;
    char *type;
{
    switch (gm_gettype (type)) {
    case Gt_Bool:
    case Gt_Int:
	return ((int)value);
    case Gt_DFloatP:
	return (*(double *)value);
    case Gt_String:
	return (atof((char *)value));
    default:
	return (0);
    }
}


static char *
gm_getstring (value, type)
    XtArgVal value;
    char *type;
{
    if (strcmp (type, XtRString) == 0)
	return ((char *)value);
    else
	return ("");
}


static int
gm_putint (ival, value, type)
    int ival;
    XtArgVal value;
    char *type;
{
    switch (gm_gettype (type)) {
    case Gt_Bool:
    case Gt_Int:
	*(int *)value = ival;
	break;
    case Gt_DFloatP:
	*(double *)value = (double) ival;
	break;
    case Gt_String:
	sprintf ((char *)value, "%d", ival);
	break;
    default:
	return (ERR);
    }
    return (OK);
}


static int
gm_putfloat (fval, value, type)
    double fval;
    XtArgVal value;
    char *type;
{
    switch (gm_gettype (type)) {
    case Gt_Bool:
    case Gt_Int:
	*(int *)value = (int) fval;
	break;
    case Gt_DFloatP:
	*(double *)value = fval;
	break;
    case Gt_String:
	sprintf ((char *)value, "%g", fval);
	break;
    default:
	return (ERR);
    }
    return (OK);
}


static int
gm_gettype (type)
    char *type;
{
    if (strcmp (type, XtRBool) == 0)
	return (Gt_Int);
    else if (strcmp (type, XtRInt) == 0)
	return (Gt_Int);
    else if (strcmp (type, XtRFloat) == 0)
	return (Gt_DFloatP);
    else if (strcmp (type, XtRPointer) == 0)
	return (Gt_Pointer);
    else if (strcmp (type, XtRString) == 0)
	return (Gt_String);
    else
	return (ERR);
}


static int
gm_getattribute (attribute)
    char *attribute;
{
    if (strcmp (attribute, GmType) == 0)
	return (Ga_Type);
    else if (strcmp (attribute, GmActivated) == 0)
	return (Ga_Activated);
    else if (strcmp (attribute, GmVisible) == 0)
	return (Ga_Visible);
    else if (strcmp (attribute, GmSensitive) == 0)
	return (Ga_Sensitive);
    else if (strcmp (attribute, GmAutoRedraw) == 0)
	return (Ga_AutoRedraw);
    else if (strcmp (attribute, GmTranslations) == 0)
	return (Ga_Translations);
    else if (strcmp (attribute, GmX) == 0)
	return (Ga_X);
    else if (strcmp (attribute, GmY) == 0)
	return (Ga_Y);
    else if (strcmp (attribute, GmWidth) == 0)
	return (Ga_Width);
    else if (strcmp (attribute, GmHeight) == 0)
	return (Ga_Height);
    else if (strcmp (attribute, GmRotangle) == 0)
	return (Ga_Rotangle);
    else if (strcmp (attribute, GmHighlightColor) == 0)
	return (Ga_HighlightColor);
    else if (strcmp (attribute, GmLineColor) == 0)
	return (Ga_LineColor);
    else if (strcmp (attribute, GmLineWidth) == 0)
	return (Ga_LineWidth);
    else if (strcmp (attribute, GmLineStyle) == 0)
	return (Ga_LineStyle);
    else if (strcmp (attribute, GmKnotColor) == 0)
	return (Ga_KnotColor);
    else if (strcmp (attribute, GmKnotSize) == 0)
	return (Ga_KnotSize);
    else if (strcmp (attribute, GmFill) == 0)
	return (Ga_Fill);
    else if (strcmp (attribute, GmFillColor) == 0)
	return (Ga_FillColor);
    else if (strcmp (attribute, GmFillBgColor) == 0)
	return (Ga_FillBgColor);
    else if (strcmp (attribute, GmFillPattern) == 0)
	return (Ga_FillPattern);
    else if (strcmp (attribute, GmFillStyle) == 0)
	return (Ga_FillStyle);
    else if (strcmp (attribute, GmTextColor) == 0)
	return (Ga_TextColor);
    else if (strcmp (attribute, GmTextBgColor) == 0)
	return (Ga_TextBgColor);
    else if (strcmp (attribute, GmTextBorder) == 0)
	return (Ga_TextBorder);
    else if (strcmp (attribute, GmImageText) == 0)
	return (Ga_ImageText);
    else if (strcmp (attribute, GmFont) == 0)
	return (Ga_Font);
    else if (strcmp (attribute, GmText) == 0)
	return (Ga_Text);
    else if (strcmp (attribute, GmRotIndicator) == 0)		/* MF020 */
	return (Ga_RotIndicator);
    else
	return (ERR);
}

static void
gm_linkafter (gm, prev)
    register Marker gm;
    register Marker prev;
{
    register GtermWidget w = gm->w;

    gm->prev = prev;
    gm->next = prev ? prev->next : NULL;
    if (prev)
	prev->next = gm;

    if (!w->gterm.gm_tail || prev == w->gterm.gm_tail)
	w->gterm.gm_tail = gm;
    if (!w->gterm.gm_head)
	w->gterm.gm_head = gm;

    w->gterm.preserve_screen++;
}


static void
gm_unlink (gm)
    register Marker gm;
{
    register GtermWidget w = gm->w;

    if (gm->prev)
	gm->prev->next = gm->next;
    if (gm->next)
	gm->next->prev = gm->prev;
    if (w->gterm.gm_head == gm)
	w->gterm.gm_head = gm->next;
    if (w->gterm.gm_tail == gm)
	w->gterm.gm_tail = gm->prev;

    gm->prev = gm->next = NULL;
    if (!w->gterm.gm_head)
	w->gterm.preserve_screen = 0;
}


/* gm_do_callbacks -- Call any client callbacks registered for the given
 * event type.
 */
static int
gm_do_callbacks (gm, events, event, params, nparams)
    Marker gm;
    register int events;
    XEvent *event;
    String *params;
    Cardinal nparams;
{
    register int n;
    register struct markerCallback *cb;
    struct markerCallback callback[GM_MAXCALLBACKS];
    int ncallbacks, status;

    /* Copy the callbacks list into local memory to ensure that it is not
     * changed by executing a callback.
     */
    ncallbacks = gm->ncallbacks;
    memmove ((char *)callback, (char *)gm->callback,
	sizeof (struct markerCallback) * GM_MAXCALLBACKS);

    for (n = ncallbacks, cb = callback;  --n >= 0;  cb++)
	if (cb->events & events) {
	    status = cb->func (cb->client_data,
		gm, events, event, params, nparams);
	    if (status)
		return (status);
	}

    return (0);
}


/* gm_constraint -- Handle the constraint callback.  This is a client
 * callback called when a marker position or size attribute is changed
 * interactively at runtime.  The purpose of the callback is to allow the
 * client to apply any constraints, e.g. to keep the marker within a
 * certain area or range of sizes, to forbid rotation, and so on.
 */
static int
gm_constraint (gm, new_gm, what)
    register Marker gm, new_gm;
    register int what;
{
    register char *ip, *op;
    char argbuf[2048];
    char *argv[30];
    int argc = 0;

    /* Return immediately if there are no constraint callbacks. */
    if (!gm->constraints)
	return;

    /* Prepare an argument list listing the marker attributes being changed
     * and their old and new values.  Each attribute is passed as three
     * arg strings: name old-value new-value.  Each argument string is
     * allocated a fixed amount of space of SZ_NUMBER characters.
     */
    op = argbuf;
    if (what & Gb_X) {
	strcpy (argv[argc++]=op, "x");			   op += SZ_NUMBER;
	sprintf (argv[argc++]=op, "%d", gm->x);		   op += SZ_NUMBER;
	sprintf (argv[argc++]=op, "%d", new_gm->x);	   op += SZ_NUMBER;
    }
    if (what & Gb_Y) {
	strcpy (argv[argc++]=op, "y");			   op += SZ_NUMBER;
	sprintf (argv[argc++]=op, "%d", gm->y);		   op += SZ_NUMBER;
	sprintf (argv[argc++]=op, "%d", new_gm->y);	   op += SZ_NUMBER;
    }
    if (what & Gb_Width) {
	strcpy (argv[argc++]=op, "width");		   op += SZ_NUMBER;
	sprintf (argv[argc++]=op, "%d", gm->width);	   op += SZ_NUMBER;
	sprintf (argv[argc++]=op, "%d", new_gm->width);	   op += SZ_NUMBER;
    }
    if (what & Gb_Height) {
	strcpy (argv[argc++]=op, "height");		   op += SZ_NUMBER;
	sprintf (argv[argc++]=op, "%d", gm->height);	   op += SZ_NUMBER;
	sprintf (argv[argc++]=op, "%d", new_gm->height);   op += SZ_NUMBER;
    }
    if (what & Gb_Rotangle) {					/* MF022 */
	double	rot = (gm->rotangle * ((double)180.0 / M_PI));
	double	new_rot = (new_gm->rotangle * ((double)180.0 / M_PI));
	strcpy (argv[argc++]=op, "rotangle");		   op += SZ_NUMBER;
	sprintf (argv[argc++]=op, "%g", rot);	   	   op += SZ_NUMBER;
	sprintf (argv[argc++]=op, "%g", new_rot); 	   op += SZ_NUMBER;
    }

    /* Call any constraint callbacks.  The argv value strings are modified
     * in place.
     */
    gm_do_callbacks (gm, GmEvConstraint, NULL, argv, argc);

    /* Copy the possibly edited values back into the new_gm struct.
     */
    ip = argbuf + SZ_NUMBER * 2;
    if (what & Gb_X) {
	new_gm->x = atoi (ip);			ip += SZ_NUMBER*3;
    }
    if (what & Gb_Y) {
	new_gm->y = atoi (ip);			ip += SZ_NUMBER*3;
    }
    if (what & Gb_Width) {
	new_gm->width = atoi (ip);		ip += SZ_NUMBER*3;
    }
    if (what & Gb_Height) {
	new_gm->height = atoi (ip);		ip += SZ_NUMBER*3;
    }
    if (what & Gb_Rotangle) {
	new_gm->rotangle = atof (ip);		ip += SZ_NUMBER*3;

	/* Convert back to radians.... */
	new_gm->rotangle *= (M_PI / (double)180.0);		/* MF022 */
    }
}


static void
gm_erase (gm)
    register Marker gm;
{
    register GtermWidget w = gm->w;
    register XRectangle *r = &gm->old_rect;

    if (!w || !XtIsRealized ((Widget)w))
	return;

    /* Any clipping to the marker border is set outside this routine. */
    if ((gm->flags & Gm_Visible) && !NullRect(r))
	XCopyArea (w->gterm.display, w->gterm.pixmap, w->gterm.window,
	    w->gterm.exposeGC, r->x, r->y, r->width, r->height, r->x, r->y);
}


/* Marker actions.
 * -------------------------
 */


/* M_create -- Create a marker.
 */
static void
M_create (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;
    int interactive, type;
    gmSelection what;

    savepos (w, event);

    /* If the marker has already been created in interactive mode the event
     * merely initializes the marker, otherwise we create and initialize a
     * new marker.
     */
    if (!(gm = w->gterm.gm_create)) {
	type = w->gterm.gm_defaultType;
	if (*nparams == 1) {
	    if (!(type = GmStrToType (params[0])))
		type = w->gterm.gm_defaultType;
	}
	gm = GmCreate (w, type, interactive=True);
    }

    gm->x = ev->x;
    gm->y = ev->y;
    gm->flags |= Gm_Activated;
    w->gterm.gm_create = NULL;

    what.type = (gm->type == Gm_Polygon) ? Ge_Marker : Ge_Point;
    what.vertex = 0;
    gm_focusin (w, gm, &what);
}


/* M_destroy -- Destroy a marker.
 */
static void
M_destroy (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    GmDestroy (gm);
}


/* M_destroyNull -- Destroy a marker if it is null sized.
 */
static void
M_destroyNull (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    if (gm && gm->width <= 2 && gm->height <= 2)
	GmDestroy (gm);
}


/* M_set -- Set a marker attribute.
 */
static void
M_set (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;
    int i;

    savepos (w, event);

    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    for (i=0;  i < *nparams;  i += 2)
	GmSetAttribute (gm,
	    params[i], (XtArgVal)params[i+1], XtRString); 	/* MF010 */
}


/* M_raise -- Raise a marker to the top of the display list.
 */
static void
M_raise (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;
    int i;

    savepos (w, event);

    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    GmRaise (gm, NULL);
}


/* M_lower -- Lower a marker to the bottom of the display list.
 */
static void
M_lower (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;
    int i;

    savepos (w, event);

    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    GmLower (gm, NULL);
}


/* M_notify -- Notify any clients that have registered callbacks for the
 * specified type of events.
 */
static void
M_notify (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;
    int events, i;

    savepos (w, event);

    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    for (i=0, events=0;  i < *nparams;  i++)
	if (strcmp (params[i], "notify") == 0)
	    events |= GmEvNotify;
	else if (strcmp (params[i], "moveResize") == 0)
	    events |= GmEvMoveResize;
	else if (strcmp (params[i], "modify") == 0)
	    events |= GmEvModify;
	else if (strcmp (params[i], "redraw") == 0)
	    events |= GmEvRedraw;
	else if (strcmp (params[i], "destroy") == 0)
	    events |= GmEvDestroy;
	else if (strcmp (params[i], "input") == 0)
	    events |= GmEvInput;
	else if (strcmp (params[i], "focusIn") == 0)
	    events |= GmEvFocusIn;
	else if (strcmp (params[i], "focusOut") == 0)
	    events |= GmEvFocusOut;

    GmNotify (gm, events, event, params + 1, *nparams - 1);
}


/* M_input -- Notify any clients that have registered a input callback
 * that a input event has occurred.
 */
static void
M_input (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XKeyEvent *ev = (XKeyEvent *) event;
    register Marker gm;

    savepos (w, event);

    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    GmNotify (gm, GmEvInput, event, params, *nparams);
}


/* M_markpos -- Mark the current position of the marker, e.g., so that it
 * can later be erased.
 */
static void
M_markpos (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    GmMarkpos (gm);
}


/* M_markposAdd -- Execute either the markpos or add action, depending upon
 * the pointer location.  If the pointer is over an active marker at a
 * location where the add action can be executed this is done, otherwise the
 * markpos action is executed.
 */
static void
M_markposAdd (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    GmSelection what = &w->gterm.gm_selection;
    register Marker gm;

    savepos (w, event);

    /* Get marker and type of active portion of marker. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, what)))
	    return;

    /* Always do a markpos whether we Add or not. */
    GmMarkpos (gm);

    /* Add a point if possible for the given marker and pointer location. */
    if (what->type == Ge_Edge &&
	    (gm->type == Gm_Polyline || gm->type == Gm_Polygon))
	GmAddPt (gm, ev->x, ev->y);
}


/* M_redraw -- Redraw a marker.
 */
static void
M_redraw (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;
    int erase;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    /* This redraw undoes the final Xor draw. */
    GmRedraw (gm, GXxor, erase=False);

    /* Redraw the full marker. */
    GmRedraw (gm, GXcopy, erase=True);
}


/* M_addPt -- Add a point.
 */
static void
M_addPt (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    GmSelection what = &w->gterm.gm_selection;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, what)))
	    return;

    /* Add a point if possible for the given marker and pointer location. */
    if (what->type == Ge_Edge &&
	    (gm->type == Gm_Polyline || gm->type == Gm_Polygon))
	GmAddPt (gm, ev->x, ev->y);
}


/* M_deletePt -- Delete a point.
 */
static void
M_deletePt (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    GmSelection what = &w->gterm.gm_selection;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, what)))
	    return;

    if (what->type == Ge_Point)
	GmDeletePt (gm, ev->x, ev->y);
}


/* M_movePt -- Move a point.
 */
static void
M_movePt (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    GmSelection what = &w->gterm.gm_selection;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, what)))
	    return;

    /* Move a point (vertex) if supported by marker type. */
    if (what->type == Ge_Point &&
	    (gm->type == Gm_Polyline || gm->type == Gm_Polygon))
	GmMovePt (gm, ev->x, ev->y);
}


/* M_deleteDestroy -- Delete a point or destroy a marker, depending upon the
 * pointer position.
 */
static void
M_deleteDestroy (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    GmSelection what = &w->gterm.gm_selection;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, what)))
	    return;

    switch (what->type) {
    case Ge_Point:
	GmDeletePt (gm, ev->x, ev->y);
	break;
    case Ge_Marker:
	GmDestroy (gm);
	break;
    }
}


/* M_move -- Move a marker.
 */
static void
M_move (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    if (ev->time - gm->time > GM_UPDATE) {
	GmMove (gm, ev->x, ev->y);
	XFlush (w->gterm.display);
	gm->time = ev->time;
    }
}


/* M_resize -- Resize a marker.
 */
static void
M_resize (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    if (ev->time - gm->time > GM_UPDATE) {
	GmResize (gm, ev->x, ev->y);
	XFlush (w->gterm.display);
	gm->time = ev->time;
    }
}


/* M_moveResize -- Move a point or marker, or resize a marker, depending
 * upon the pointer position.
 */
static void
M_moveResize (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    GmSelection what = &w->gterm.gm_selection;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, what)))
	    return;

    if (ev->time - gm->time > GM_UPDATE) {
	switch (what->type) {
	case Ge_Marker:
	    GmMove (gm, ev->x, ev->y);
	    break;
	case Ge_Point:
	    if (gm->type == Gm_Polygon || gm->type == Gm_Polyline)
		GmMovePt (gm, ev->x, ev->y);
	    else
		goto resize;
	    break;
	case Ge_Edge:
resize:	    GmResize (gm, ev->x, ev->y);
	    break;
	}
	XFlush (w->gterm.display);
	gm->time = ev->time;
    }
}


/* M_rotate -- Rotate a marker.
 */
static void
M_rotate (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    if (ev->time - gm->time > GM_UPDATE) {
	GmRotate (gm, ev->x, ev->y);
	XFlush (w->gterm.display);
	gm->time = ev->time;
    }
}


/* M_rotateResize -- Rotate or resize a marker.
 */
static void
M_rotateResize (widget, event, params, nparams)
    Widget widget;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    register GtermWidget w = (GtermWidget)widget;
    register XButtonEvent *ev = (XButtonEvent *) event;
    GmSelection what = &w->gterm.gm_selection;
    register Marker gm;

    savepos (w, event);

    /* Determine which marker gets this event. */
    if (!(gm = w->gterm.gm_active))
	if (!(gm = GmSelect (w, ev->x, ev->y, NULL)))
	    return;

    if (ev->time - gm->time > GM_UPDATE) {
	switch (what->type) {
	case Ge_Point:
	    GmRotate (gm, ev->x, ev->y);
	    break;
	case Ge_Edge:
	    if (gm->flags & Gm_Smooth)
		GmRotate (gm, ev->x, ev->y);
	    else
		GmResize (gm, ev->x, ev->y);
	    break;
	default:
	    GmResize (gm, ev->x, ev->y);
	    break;
	}

	XFlush (w->gterm.display);
	gm->time = ev->time;
    }
}


/*
 * Marker class code.
 * ---------------------
 * Each marker class implements a subset of the following procedures.  The
 * first set of procedures are required.  The second set are optional and
 * may be set to NULL in the marker descriptor if not implemented by the
 * marker class.
 *
 *	       gm_xxxx_init (gm, interactive)
 *    bool = gm_xxxx_select (gm, x, y, &what)
 *	    gm_xxxx_markpos (gm)
 *	     gm_xxxx_redraw (gm, func)
 *	     gm_xxxx_update (gm)
 *
 *	      gm_xxxx_addPt (gm, x, y)
 *	   gm_xxxx_deletePt (gm, x, y)
 *	     gm_xxxx_movePt (gm, x, y)
 *	       gm_xxxx_move (gm, x, y)
 *	     gm_xxxx_resize (gm, x, y)
 *	     gm_xxxx_rotate (gm, x, y)
 *
 * where xxxx is the 4 character marker class name.
 */

/* Marker class TEXT.
 */
static int gm_text_select();
static void gm_text_move(), gm_text_resize();
static void gm_text_markpos(), gm_text_redraw();
static void gm_text_update(), gm_text_updatePolygon();

static void
gm_text_init (gm, interactive)
    register Marker gm;
    int interactive;
{
    register GtermWidget w = gm->w;

    gm->type = Gm_Text;
    if (!(gm->flags & Gm_Activated)) {
	gm->highlightColor = w->gterm.gm_highlightColor;
	gm->lineColor = w->gterm.gm_TextLineColor;
	gm->lineWidth = w->gterm.gm_lineWidth;
	gm->lineStyle = w->gterm.gm_lineStyle;
	gm->textColor = w->gterm.gm_TextColor;
	gm->textBgColor = w->gterm.gm_TextBgColor;
	gm->textBorder = w->gterm.gm_TextBorder;
	gm->fill = w->gterm.gm_fill;
	gm->fillStyle = w->gterm.gm_fillStyle;
	gm->fillColor = w->gterm.gm_fillColor;
	gm->fillBgColor = w->gterm.gm_fillBgColor;
	gm->fillStyle = w->gterm.gm_fillStyle;
	gm->font = w->gterm.gm_TextFont;
	gm->imageText = False;
    }

    if (gm->points && gm->npoints > GM_MAXVERTICES)
	XtFree ((char *)gm->points);
    gm->npoints = 4 + 1;
    gm->points = gm->point_data;

    gm->select   = gm_text_select;
    gm->markpos  = gm_text_markpos;
    gm->redraw   = gm_text_redraw;
    gm->update   = gm_text_update;
    gm->addPt    = NULL;
    gm->deletePt = NULL;
    gm->movePt   = NULL;
    gm->move     = gm_text_move;
    gm->resize   = gm_text_resize;
    gm->rotate   = NULL;

    if (w->gterm.gm_TextString) {
	if (gm->text)
	    XtFree (gm->text);
	gm->text = (char *) XtMalloc (strlen(w->gterm.gm_TextString)+1);
	strcpy (gm->text, w->gterm.gm_TextString);
    } else
	gm->text = NULL;
}

static int
gm_text_select (gm, x, y, what)
    register Marker gm;
    int x, y;
    GmSelection what;
{
    if (gm_select (gm, x, y, what)) {
	if (what && what->type == Ge_Edge)
	    what->type = Ge_Marker;
	return (1);
    } else
	return (0);
}


static void
gm_text_markpos (gm)
    register Marker gm;
{
    gm_markpos (gm);
}


static void
gm_text_redraw (gm, function)
    register Marker gm;
    int function;
{
    register GtermWidget w = gm->w;
    int flags = (Gm_Activated|Gm_Visible);
    int char_width, char_height, xsize, ysize;
    int breakline, l_pix, r_pix, maxch, x, y;
    XFontStruct *fp = gm->font;
    char *ip, *op, *otop;
    char *l_ip, *l_op;
    char line[1024];

    if (!((gm->flags & flags) == flags))
	return;
    if (gm->width <= 0 || gm->height <= 0)
	return;

    /* In rubber-band mode just draw the outline of the text region. */
    if (function == GXxor) {
	int save_lineWidth = gm->lineWidth;

	if (gm->lineWidth <= 0)
	    gm->lineWidth = 1;
	gm_redraw (gm, function);
	gm->lineWidth = save_lineWidth;
	return;
    }

    /* General case.  First draw the text box. */
    gm_redraw (gm, function);

    /* Now draw the text. */
    if (!gm->text)
	return;

    char_width  = fp->max_bounds.width;
    char_height = fp->max_bounds.ascent + fp->max_bounds.descent;
    xsize = gm->width;
    ysize = gm->height;

    l_pix = (gm->lineWidth + 1) / 2 + gm->textBorder - 1;
    r_pix = (gm->lineWidth + 1) / 2 + gm->textBorder - 1 -
	(fp->max_bounds.width - fp->max_bounds.rbearing);
    if ((maxch = (xsize - l_pix - r_pix) / char_width) < 1)
	return;

    x = gm->x + (gm->lineWidth + 1) / 2 + gm->textBorder + 1;
    y = gm->y + (gm->lineWidth + 1) / 2 + gm->textBorder +
	fp->max_bounds.ascent;

    XSetForeground (w->gterm.display, w->gterm.gm_drawGC, gm->textColor);
    XSetBackground (w->gterm.display, w->gterm.gm_drawGC, gm->textBgColor);
    XSetFont (w->gterm.display, w->gterm.gm_drawGC, fp->fid);

    /* Fill lines in a multiline text box.
     */
    l_ip = l_op = NULL;
    otop = line + maxch;
    breakline = 0;

    for (ip = gm->text, op=line;  *ip || op > line;  ) {
	if (! *ip) {
	    breakline++;
	} else if (*ip == ' ' || *ip == '\t') {
	    l_ip = ip;
	    l_op = op;
	    *op++ = ' ';
	    ip++;
	} else if (*ip == '\n') {
	    ip++;
	    breakline++;
	} else
	    *op++ = *ip++;

	if (breakline || op > otop) {
	    if (op > otop) {
		if (l_ip && l_op) {
		    ip = l_ip + 1;
		    *l_op = '\0';
		} else {
		    while (op > otop) {
			if (ip > gm->text && isprint (*(ip-1)))
			    --ip;
			--op;
		    }
		    *op = '\0';
		}
	    } else
		*op = '\0';

	    if (gm->imageText) {
		while (op < otop)
		    *op++ = ' ';
		*op = '\0';
		XDrawImageString (w->gterm.display, w->gterm.window,
		    w->gterm.gm_drawGC, x, y, line, strlen(line));
	    } else {
		XDrawString (w->gterm.display, w->gterm.window,
		    w->gterm.gm_drawGC, x, y, line, strlen(line));
	    }

	    y += char_height;
	    if (breakline)
		y += gm->textBorder;
	    if (y + fp->max_bounds.descent > gm->y + ysize)
		break;

	    op = line;
	    l_ip = l_op = NULL;
	    breakline = 0;
	}
    }
}


static void
gm_text_update (gm)
    register Marker gm;
{
    register GtermWidget w = gm->w;
    int flags = (Gm_Activated|Gm_Visible);

    if (!((gm->flags & flags) == flags))
	return;
    if (gm->width <= 0 || gm->height <= 0)
	return;

    if (gm->flags & Gm_Modified) {
	gm_text_updatePolygon (gm);
	gm_setCurRect (gm);
	gm->flags &= ~Gm_Modified;
    }
}

static void
gm_text_move (gm, x, y)
    register Marker gm;
    int x, y;
{
    struct marker new_gm;
    new_gm.x = max (0, x - gm->width / 2);
    new_gm.y = max (0, y - gm->height / 2);
    gm_constraint (gm, &new_gm, Gb_X|Gb_Y);	/* corner */

    gm->x = new_gm.x;
    gm->y = new_gm.y;
    gm_text_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_text_resize (gm, x, y)
    register Marker gm;
    int x, y;
{
    struct marker new_gm;
    new_gm.width = abs (x - gm->x);
    new_gm.height = abs (y - gm->y);
    gm_constraint (gm, &new_gm, Gb_Width|Gb_Height);

    gm->width = new_gm.width;
    gm->height = new_gm.height;
    gm_text_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_text_updatePolygon (gm)
    register Marker gm;
{
    register XPoint *p = gm->points;
    int xsize = gm->width;
    int ysize = gm->height;

    p[0].x = gm->x;		p[0].y = gm->y;
    p[1].x = gm->x;		p[1].y = gm->y + ysize;
    p[2].x = gm->x + xsize;	p[2].y = gm->y + ysize;
    p[3].x = gm->x + xsize;	p[3].y = gm->y;
    p[4].x = gm->x;		p[4].y = gm->y;
}


/* Marker class LINE.
 */
static void
gm_line_init (gm, interactive)
    register Marker gm;
    int interactive;
{
    gm->type = Gm_Line;
    /* stub out for now */
}


/* Marker class POLYLINE.
 */
static void
gm_plin_init (gm, interactive)
    register Marker gm;
    int interactive;
{
    gm->type = Gm_Polyline;
    /* stub out for now */
}


/* Marker class RECTANGLE.
 */
static int gm_rect_select();
static void gm_rect_move(), gm_rect_resize(), gm_rect_rotate();
static void gm_rect_update(), gm_rect_updatePolygon();

static void
gm_rect_init (gm, interactive)
    register Marker gm;
    int interactive;
{
    register GtermWidget w = gm->w;

    gm->type = Gm_Rectangle;
    if (!(gm->flags & Gm_Activated)) {
	gm->lineWidth = w->gterm.gm_lineWidth;
	gm->lineStyle = w->gterm.gm_lineStyle;
	gm->highlightColor = w->gterm.gm_highlightColor;
	gm->lineColor = w->gterm.gm_RectLineColor;
	gm->knotColor = w->gterm.gm_RectKnotColor;
	gm->knotSize  = w->gterm.gm_RectKnotSize;
	gm->fill = w->gterm.gm_fill;
	gm->fillStyle = w->gterm.gm_fillStyle;
	gm->fillColor = w->gterm.gm_fillColor;
	gm->fillBgColor = w->gterm.gm_fillBgColor;
    }

    if (gm->points && gm->npoints > GM_MAXVERTICES)
	XtFree ((char *)gm->points);
    gm->points = gm->point_data;
    gm->npoints = 4 + 1;

    gm->select   = gm_rect_select;
    gm->markpos  = gm_markpos;
    gm->update   = gm_rect_update;
    gm->redraw   = gm_redraw;
    gm->addPt    = NULL;
    gm->deletePt = NULL;
    gm->movePt   = NULL;
    gm->move     = gm_rect_move;
    gm->resize   = gm_rect_resize;
    gm->rotate   = gm_rect_rotate;
}

static void
gm_rect_update (gm)
    register Marker gm;
{
    if (gm->flags & Gm_Modified) {
	gm_rect_updatePolygon (gm);
	gm_setCurRect (gm);
	gm->flags &= ~Gm_Modified;
    }
}

static int
gm_rect_select (gm, x, y, what)
    register Marker gm;
    int x, y;
    GmSelection what;
{
    if (gm_select (gm, x, y, what)) {
	if (what && what->type == Ge_Edge)
	    what->type = Ge_Marker;
	return (1);
    } else
	return (0);
}

static void
gm_rect_move (gm, x, y)
    register Marker gm;
    int x, y;
{
    struct marker new_gm;
    new_gm.x = x;  new_gm.y = y;
    gm_constraint (gm, &new_gm, Gb_X|Gb_Y);

    gm->x = new_gm.x;  gm->y = new_gm.y;
    gm_rect_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_rect_resize (gm, x, y)
    register Marker gm;
    int x, y;
{
/*  double cos_rotangle = cos (-(gm->rotangle));
    double sin_rotangle = sin (-(gm->rotangle)); */
    double cos_rotangle = cos ((gm->rotangle));			/* MF019 */
    double sin_rotangle = sin ((gm->rotangle));			/* MF019 */
    struct marker new_gm;
    int rx, ry;
    int ox = x, oy = y;

    /* Translate to the marker reference frame. */
    x = x - gm->x;
    y = y - gm->y;

    /* Rotate back to the unrotated reference frame. */
    rx = x * cos_rotangle - y * sin_rotangle;
    ry = x * sin_rotangle + y * cos_rotangle;

    /* Compute new width and height. */
    if (rx < 0)
	new_gm.x = gm->x - (-rx - gm->width) / 2;
    else
	new_gm.x = gm->x + (rx - gm->width) / 2;

    if (ry < 0)
	new_gm.y = gm->y - (-ry - gm->height) / 2;
    else
	new_gm.y = gm->y + (ry - gm->height) / 2;

    new_gm.width = gm->width + (abs(rx) - gm->width) / 2;
    new_gm.height = gm->height + (abs(ry) - gm->height) / 2;

    gm_constraint (gm, &new_gm, Gb_X|Gb_Y|Gb_Width|Gb_Height);
    gm->x = new_gm.x;
    gm->y = new_gm.y;
    gm->width = new_gm.width;
    gm->height = new_gm.height;

    gm_rect_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_rect_rotate (gm, x, y)
    register Marker gm;
    int x, y;
{
    double alpha, theta;
    struct marker new_gm;

    if (x == gm->x && y == gm->y)
	gm->rotangle = 0;
    else {

    /*  V1.1  These eqns have the effect of allowing a marker to be grabbed by
     *  any corner but doing so resets the rotation angle the first time the
     *  marker is rotated.

	theta = atan2 ((double)(y - gm->y), (double)(x - gm->x));
	alpha = atan2 ((double)gm->height, (double)gm->width);
 	new_gm.rotangle = gm_niceAngle (theta + alpha);
     */

	theta = atan2 ((double)(gm->y - y), (double)(x - gm->x)); /* MF019 */
	new_gm.rotangle = gm_niceAngle (theta);			  /* MF019 */
	gm_constraint (gm, &new_gm, Gb_Rotangle);
	gm_rotate_indicator (gm, GXxor);			  /* MF020 */
	gm->rotangle =  new_gm.rotangle;
	gm_rotate_indicator (gm, GXxor);			  /* MF020 */
	gm_rect_updatePolygon (gm);
	gm_setCurRect (gm);
    }
}

static void
gm_rect_updatePolygon (gm)
    Marker gm;
{
    register x, y;
    register XPoint *p = gm->points;
    double cos_rotangle, sin_rotangle;

/*  cos_rotangle = cos (gm->rotangle);
    sin_rotangle = sin (gm->rotangle);*/
    cos_rotangle = cos (-gm->rotangle);				/* MF019 */
    sin_rotangle = sin (-gm->rotangle);				/* MF019 */
    x = gm->width;
    y = gm->height;

    p[0].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
    p[0].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;

    x = -x;
    p[1].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
    p[1].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;

    y = -y;
    p[2].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
    p[2].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;

    x = -x;
    p[3].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
    p[3].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;

    p[4] = p[0];
}


/* Marker class BOX.  A box marker is like a rectangle except that it is
 * described and resized by the center and radius (width/height), like
 * the other "centered" marker types (circle, ellipse, etc.).
 */
static int gm_boxx_select();
static void gm_boxx_move(), gm_boxx_resize(), gm_boxx_rotate();
static void gm_boxx_update(), gm_boxx_updatePolygon();

static void
gm_boxx_init (gm, interactive)
    register Marker gm;
    int interactive;
{
    register GtermWidget w = gm->w;

    gm->type = Gm_Box;
    if (!(gm->flags & Gm_Activated)) {
	gm->lineWidth = w->gterm.gm_lineWidth;
	gm->lineStyle = w->gterm.gm_lineStyle;
	gm->highlightColor = w->gterm.gm_highlightColor;
	gm->lineColor = w->gterm.gm_RectLineColor;
	gm->knotColor = w->gterm.gm_RectKnotColor;
	gm->knotSize  = w->gterm.gm_RectKnotSize;
	gm->fill = w->gterm.gm_fill;
	gm->fillStyle = w->gterm.gm_fillStyle;
	gm->fillColor = w->gterm.gm_fillColor;
	gm->fillBgColor = w->gterm.gm_fillBgColor;
    }

    if (gm->points && gm->npoints > GM_MAXVERTICES)
	XtFree ((char *)gm->points);
    gm->points = gm->point_data;
    gm->npoints = 4 + 1;

    gm->select   = gm_boxx_select;
    gm->markpos  = gm_markpos;
    gm->update   = gm_boxx_update;
    gm->redraw   = gm_redraw;
    gm->addPt    = NULL;
    gm->deletePt = NULL;
    gm->movePt   = NULL;
    gm->move     = gm_boxx_move;
    gm->resize   = gm_boxx_resize;
    gm->rotate   = gm_boxx_rotate;
}

static void
gm_boxx_update (gm)
    register Marker gm;
{
    if (gm->flags & Gm_Modified) {
	gm_boxx_updatePolygon (gm);
	gm_setCurRect (gm);
	gm->flags &= ~Gm_Modified;
    }
}

static int
gm_boxx_select (gm, x, y, what)
    register Marker gm;
    int x, y;
    GmSelection what;
{
    if (gm_select (gm, x, y, what)) {
	if (what && what->type == Ge_Edge)
	    what->type = Ge_Marker;
	return (1);
    } else
	return (0);
}

static void
gm_boxx_move (gm, x, y)
    register Marker gm;
    int x, y;
{
    struct marker new_gm;
    new_gm.x = x;  new_gm.y = y;
    gm_constraint (gm, &new_gm, Gb_X|Gb_Y);

    gm->x = new_gm.x;  gm->y = new_gm.y;
    gm_boxx_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_boxx_resize (gm, x, y)
    register Marker gm;
    int x, y;
{
/*  double cos_rotangle = cos (-(gm->rotangle));
    double sin_rotangle = sin (-(gm->rotangle)); */
    double cos_rotangle = cos ((gm->rotangle));			/* MF019 */
    double sin_rotangle = sin ((gm->rotangle));			/* MF019 */
    struct marker new_gm;
    int rx, ry;

    /* Translate to the marker reference frame. */
    x = x - gm->x;
    y = y - gm->y;

    /* Rotate back to the unrotated reference frame. */
    rx = x * cos_rotangle - y * sin_rotangle;
    ry = x * sin_rotangle + y * cos_rotangle;

    /* Compute new width and height. */
    new_gm.width = abs(rx);
    new_gm.height = abs(ry);
    gm_constraint (gm, &new_gm, Gb_Width|Gb_Height);
    gm->width  = new_gm.width;
    gm->height = new_gm.height;

    gm_boxx_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_boxx_rotate (gm, x, y)
    register Marker gm;
    int x, y;
{
    double alpha, theta;
    struct marker new_gm;

    if (x == gm->x && y == gm->y)
	gm->rotangle = 0;
    else {
    /* V1.1
	theta = atan2 ((double)(y - gm->y), (double)(x - gm->x));
	alpha = atan2 ((double)gm->height, (double)gm->width);
	new_gm.rotangle = gm_niceAngle (theta + alpha);
     */
	theta = atan2 ((double)(gm->y - y), (double)(x - gm->x)); /* MF019 */
	new_gm.rotangle = gm_niceAngle (theta);			  /* MF019 */
	gm_constraint (gm, &new_gm, Gb_Rotangle);
	gm_rotate_indicator (gm, GXxor);			  /* MF020 */
	gm->rotangle = new_gm.rotangle;
	gm_rotate_indicator (gm, GXxor);			  /* MF020 */
	gm_boxx_updatePolygon (gm);
	gm_setCurRect (gm);
    }
}

static void
gm_boxx_updatePolygon (gm)
    Marker gm;
{
    register x, y;
    register XPoint *p = gm->points;
    double cos_rotangle, sin_rotangle;
	
    double alpha = atan2 ((double)gm->height, (double)gm->width);

/*  cos_rotangle = cos (gm->rotangle);
    sin_rotangle = sin (gm->rotangle); */
    cos_rotangle = cos (-gm->rotangle);				/* MF019 */
    sin_rotangle = sin (-gm->rotangle);				/* MF019 */
    x = gm->width;
    y = gm->height;

    p[0].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
    p[0].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;

    x = -x;
    p[1].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
    p[1].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;

    y = -y;
    p[2].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
    p[2].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;

    x = -x;
    p[3].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
    p[3].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;

    p[4] = p[0];
}


/* Marker class CIRCLE.
 */
static int gm_circ_select();
static void gm_circ_move(), gm_circ_resize(), gm_circ_rotate();
static void gm_circ_update(), gm_circ_updatePolygon();

static void
gm_circ_init (gm, interactive)
    register Marker gm;
    int interactive;
{
    register GtermWidget w = gm->w;

    gm->type = Gm_Circle;
    if (!(gm->flags & Gm_Activated)) {
	gm->lineWidth = w->gterm.gm_lineWidth;
	gm->lineStyle = w->gterm.gm_lineStyle;
	gm->highlightColor = w->gterm.gm_highlightColor;
	gm->lineColor = w->gterm.gm_CircleLineColor;
	gm->knotColor = w->gterm.gm_CircleKnotColor;
	gm->knotSize  = w->gterm.gm_CircleKnotSize;
	gm->fill = w->gterm.gm_fill;
	gm->fillStyle = w->gterm.gm_fillStyle;
	gm->fillColor = w->gterm.gm_fillColor;
	gm->fillBgColor = w->gterm.gm_fillBgColor;
	gm->flags |= Gm_Smooth;
    }

    gm->width = gm->height = (gm->width + gm->height) / 2.0;

    if (gm->points && gm->npoints > GM_MAXVERTICES)
	XtFree ((char *)gm->points);
    gm->points = gm->point_data;
    gm->npoints = GM_NPTSCIRCLE;				/* MF015 */

    gm->select   = gm_circ_select;
    gm->markpos  = gm_markpos;
    gm->update   = gm_circ_update;
    gm->redraw   = gm_redraw;
    gm->addPt    = NULL;
    gm->deletePt = NULL;
    gm->movePt   = NULL;
    gm->move     = gm_circ_move;
    gm->resize   = gm_circ_resize;
    gm->rotate   = NULL;
}

static void
gm_circ_update (gm)
    register Marker gm;
{
    if (gm->flags & Gm_Modified) {
	gm_circ_updatePolygon (gm);
	gm_setCurRect (gm);
	gm->flags &= ~Gm_Modified;
    }
}

static int
gm_circ_select (gm, x, y, what)
    register Marker gm;
    int x, y;
    GmSelection what;
{
    if (gm_select (gm, x, y, what)) {
	if (what && what->type == Ge_Point)
	    what->type = Ge_Edge;
	return (1);
    } else
	return (0);
}

static void
gm_circ_move (gm, x, y)
    register Marker gm;
    int x, y;
{
    struct marker new_gm;
    new_gm.x = x;  new_gm.y = y;
    gm_constraint (gm, &new_gm, Gb_X|Gb_Y);

    gm->x = new_gm.x;  gm->y = new_gm.y;
    gm_circ_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_circ_resize (gm, x, y)
    register Marker gm;
    int x, y;
{
    struct marker new_gm;
    new_gm.width = new_gm.height =
	sqrt ((double)(SQR(x - gm->x) + SQR(y - gm->y)));
    gm_constraint (gm, &new_gm, Gb_Width|Gb_Height);

    gm->width = gm->height = new_gm.width;
    gm_circ_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_circ_updatePolygon (gm)
    Marker gm;
{
    register XPoint *p = gm->points;
    register int npts, i, j;
    double theta, x, y;

    /*npts = (gm->npoints - 1) / 4;*/
    npts = gm->npoints / 4;					/* MF028 */

    for (i=0;  i < npts;  i++) {
	theta = PI_2 / npts * i;
	x = gm->width  * cos(theta);
	y = gm->height * sin(theta);
	
	j = i;
	p[npts*0+j].x = x + gm->x;
	p[npts*0+j].y = y + gm->y;
	
	x = -x;  j = npts-1 - i;
	p[npts*1+j].x = x + gm->x;
	p[npts*1+j].y = y + gm->y;
	
	y = -y;  j = i;
	p[npts*2+j].x = x + gm->x;
	p[npts*2+j].y = y + gm->y;
	
	x = -x;  j = npts-1 - i;
	p[npts*3+j].x = x + gm->x;
	p[npts*3+j].y = y + gm->y;
    }

    /*p[gm->npoints-1] = p[0];*/				/* MF015 */
}


/* Marker class ELLIPSE.
 */
static int gm_elip_select();
static void gm_elip_move(), gm_elip_resize(), gm_elip_rotate();
static void gm_elip_update(), gm_elip_updatePolygon();

static void
gm_elip_init (gm, interactive)
    register Marker gm;
    int interactive;
{
    register GtermWidget w = gm->w;

    gm->type = Gm_Ellipse;
    if (!(gm->flags & Gm_Activated)) {
	gm->lineWidth = w->gterm.gm_lineWidth;
	gm->lineStyle = w->gterm.gm_lineStyle;
	gm->highlightColor = w->gterm.gm_highlightColor;
	gm->lineColor = w->gterm.gm_EllipseLineColor;
	gm->knotColor = w->gterm.gm_EllipseKnotColor;
	gm->knotSize  = w->gterm.gm_EllipseKnotSize;
	gm->fill = w->gterm.gm_fill;
	gm->fillStyle = w->gterm.gm_fillStyle;
	gm->fillColor = w->gterm.gm_fillColor;
	gm->fillBgColor = w->gterm.gm_fillBgColor;
	gm->flags |= Gm_Smooth;
    }

    if (gm->points && gm->npoints > GM_MAXVERTICES)
	XtFree ((char *)gm->points);
    gm->points = gm->point_data;
/*    gm->npoints = GM_NPTSCIRCLE + 1;*/
    gm->npoints = GM_NPTSCIRCLE;				/* MF015 */

    gm->select   = gm_elip_select;
    gm->markpos  = gm_markpos;
    gm->update   = gm_elip_update;
    gm->redraw   = gm_redraw;
    gm->addPt    = NULL;
    gm->deletePt = NULL;
    gm->movePt   = NULL;
    gm->move     = gm_elip_move;
    gm->resize   = gm_elip_resize;
    gm->rotate   = gm_elip_rotate;
}

static void
gm_elip_update (gm)
    register Marker gm;
{
    if (gm->flags & Gm_Modified) {
	gm_elip_updatePolygon (gm);
	gm_setCurRect (gm);
	gm->flags &= ~Gm_Modified;
    }
}

static int
gm_elip_select (gm, x, y, what)
    register Marker gm;
    int x, y;
    GmSelection what;
{
    if (gm_select (gm, x, y, what)) {
	if (what && what->type == Ge_Point)
	    what->type = Ge_Edge;
	return (1);
    } else
	return (0);
}

static void
gm_elip_move (gm, x, y)
    register Marker gm;
    int x, y;
{
    struct marker new_gm;
    new_gm.x = x;  new_gm.y = y;
    gm_constraint (gm, &new_gm, Gb_X|Gb_Y);

    gm->x = new_gm.x;  gm->y = new_gm.y;
    gm_elip_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_elip_resize (gm, x, y)
    register Marker gm;
    int x, y;
{
    struct marker new_gm;
/*  double theta = -(gm->rotangle);*/
    double theta = (gm->rotangle);				/* MF019 */
    int rx, ry;

    /* Translate to the marker reference frame. */
    x = x - gm->x;
    y = y - gm->y;

    /* Rotate back to the unrotated reference frame. */
    rx = x * cos(theta) - y * sin(theta);
    ry = x * sin(theta) + y * cos(theta);

    /* Compute new width and height. */
    new_gm.width  = abs(rx);
    new_gm.height = abs(ry);

    gm_constraint (gm, &new_gm, Gb_Width|Gb_Height);
    gm->width  = new_gm.width;
    gm->height = new_gm.height;

    gm_elip_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_elip_rotate (gm, x, y)
    register Marker gm;
    int x, y;
{
    struct marker new_gm;
    double theta;

    if (x == gm->x && y == gm->y)
	gm->rotangle = 0;
    else {
/*	theta = atan2 ((double)(y - gm->y), (double)(x - gm->x));*/
	theta = atan2 ((double)(gm->y - y), (double)(x - gm->x)); /* MF019 */
	new_gm.rotangle = gm_niceAngle (theta);
	gm_constraint (gm, &new_gm, Gb_Rotangle);
	gm_rotate_indicator (gm, GXxor);			  /* MF020 */
	gm->rotangle = new_gm.rotangle;
	gm_rotate_indicator (gm, GXxor);			  /* MF020 */
	gm_elip_updatePolygon (gm);
	gm_setCurRect (gm);
    }
}

static void
gm_elip_updatePolygon (gm)
    Marker gm;
{
    register XPoint *p = gm->points;
    register int npts, i, j;
    double cos_rotangle, sin_rotangle;
    double theta, x, y;

    npts = (gm->npoints - 1) / 4 + 1;				/* MF017 */
/*  cos_rotangle = cos (gm->rotangle);
    sin_rotangle = sin (gm->rotangle); */
    cos_rotangle = cos (-gm->rotangle);				/* MF019 */
    sin_rotangle = sin (-gm->rotangle);				/* MF019 */

    for (i=0;  i < npts;  i++) {
	theta = PI_2 / npts * i;
	x = gm->width  * cos(theta);
	y = gm->height * sin(theta);
	
	j = i;
	p[npts*0+j].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
	p[npts*0+j].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;
	
	x = -x;  j = npts-1 - i;
	p[npts*1+j].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
	p[npts*1+j].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;
	
	y = -y;  j = i;
	p[npts*2+j].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
	p[npts*2+j].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;
	
	x = -x;  j = npts-1 - i;
	p[npts*3+j].x = x * cos_rotangle - y * sin_rotangle + gm->x + 0.5;
	p[npts*3+j].y = x * sin_rotangle + y * cos_rotangle + gm->y + 0.5;
    }

    /*p[gm->npoints-1] = p[0];*/				/* MF015 */
}


/* Marker class POLYGON.
 */
static int gm_pgon_select();
static void gm_pgon_addPt(), gm_pgon_deletePt(), gm_pgon_movePt();
static void gm_pgon_move(), gm_pgon_resize(), gm_pgon_rotate();
static void gm_pgon_redraw(), gm_pgon_update(), gm_pgon_updatePolygon();

static void
gm_pgon_init (gm, interactive)
    register Marker gm;
    int interactive;
{
    register GtermWidget w = gm->w;
    register DPoint *p;

    gm->type = Gm_Polygon;
    if (!(gm->flags & Gm_Activated)) {
	gm->lineWidth = w->gterm.gm_lineWidth;
	gm->lineStyle = w->gterm.gm_lineStyle;
	gm->highlightColor = w->gterm.gm_highlightColor;
	gm->lineColor = w->gterm.gm_PgonLineColor;
	gm->knotColor = w->gterm.gm_PgonKnotColor;
	gm->knotSize  = w->gterm.gm_PgonKnotSize;
	gm->fill = w->gterm.gm_fill;
	gm->fillStyle = w->gterm.gm_fillStyle;
	gm->fillColor = w->gterm.gm_fillColor;
	gm->fillBgColor = w->gterm.gm_fillBgColor;

	gm->npoints = gm->pgon_npts = 4 + 1;
	gm->points = gm->point_data;
	if (gm->pgon)
	    XtFree ((char *)gm->pgon);

	/* Start out with a small square polygon. */
	gm->pgon = p = (DPoint *) XtMalloc (5 * sizeof (DPoint));
	gm->x = w->gterm.last_x;
	gm->y = w->gterm.last_y;

	if (p) {
	    p[0].x = -1;  p[0].y = -1;
	    p[1].x = -1;  p[1].y =  1;
	    p[2].x =  1;  p[2].y =  1;
	    p[3].x =  1;  p[3].y = -1;
	    p[4].x = -1;  p[4].y = -1;

	    gm_pgon_updatePolygon (gm);
	    gm_setCurRect (gm);
	}

	if (interactive)
	    gm->flags |= Gm_PgonInit;
    }

    if (gm->points && gm->npoints > GM_MAXVERTICES)
	XtFree ((char *)gm->points);
    gm->points = gm->point_data;

    /* The following gets executed when an existing non-polygon marker is
     * turned into a polygon marker.
     */
    if (gm->pgon && gm->pgon_npts)
	gm->npoints = gm->pgon_npts;
    else {
	gm->npoints = gm->pgon_npts = 4 + 1;

	/* Start out with a small square polygon. */
	gm->pgon = p = (DPoint *) XtMalloc (5 * sizeof (DPoint));

	if (p) {
	    p[0].x = -gm->width;  p[0].y = -gm->height;
	    p[1].x = -gm->width;  p[1].y =  gm->height;
	    p[2].x =  gm->width;  p[2].y =  gm->height;
	    p[3].x =  gm->width;  p[3].y = -gm->height;
	    p[4].x = -gm->width;  p[4].y = -gm->height;

	    gm_pgon_updatePolygon (gm);
	    gm_setCurRect (gm);
	}
    }

    gm->select   = gm_select;
    gm->markpos  = gm_markpos;
    gm->update   = gm_pgon_update;
    gm->redraw   = gm_pgon_redraw;
    gm->addPt    = gm_pgon_addPt;
    gm->deletePt = gm_pgon_deletePt;
    gm->movePt   = gm_pgon_movePt;
    gm->move     = gm_pgon_move;
    gm->resize   = gm_pgon_resize;
    gm->rotate   = gm_pgon_rotate;
}

static void
gm_pgon_redraw (gm, function)
    register Marker gm;
    int function;
{
    /* The PgonInit flag is set when a polygon marker is interactively created
     * to cause any pointer motion event to resize the marker.  The first
     * pointer up causes a redraw which clears the flag.
     */
    if (function != GXxor && gm->width > 1 && gm->height > 1)
	gm->flags &= ~Gm_PgonInit;

    gm_redraw (gm, function);
}

static void
gm_pgon_update (gm)
    register Marker gm;
{
    if (gm->flags & Gm_Modified) {
	gm_pgon_updatePolygon (gm);
	gm_setCurRect (gm);
	gm->flags &= ~Gm_Modified;
    }
}

static void
gm_pgon_addPt (gm, x, y)
    register Marker gm;
    int x, y;
{
    register DPoint *pv;
    register GtermWidget w = gm->w;
/*  double cos_rotangle = cos (-(gm->rotangle));
    double sin_rotangle = sin (-(gm->rotangle)); */
    double cos_rotangle = cos ((gm->rotangle));			/* MF019 */
    double sin_rotangle = sin ((gm->rotangle));			/* MF019 */
    int vertex, nbytes;
    double rx, ry;

    if (gm->flags & Gm_PgonInit) {
	gm_pgon_resize (gm, x, y);
	return;
    }

    /* Translate to the marker reference frame. */
    x = x - gm->x;
    y = y - gm->y;

    /* Rotate back to the unrotated reference frame. */
    rx = x * cos_rotangle - y * sin_rotangle;
    ry = x * sin_rotangle + y * cos_rotangle;

    /* Add the point. */
    vertex = w->gterm.gm_selection.vertex;
    if (vertex < 0 || vertex >= gm->npoints)
	return;
    nbytes = (gm->npoints + 1) * sizeof (DPoint);
    if ((pv = (DPoint *)
	    XtRealloc ((char *)gm->pgon, nbytes)) == (DPoint *)NULL)
	return;

    gm->pgon = pv;
    memmove (&pv[vertex+2], &pv[vertex+1],
	(gm->npoints - vertex - 1) * sizeof(DPoint));
    pv[vertex+1].x = rx;
    pv[vertex+1].y = ry;
    gm->npoints++;

    nbytes = gm->npoints * sizeof (XPoint);
    if (gm->npoints > GM_MAXVERTICES) {
	if (gm->points != gm->point_data)
	    gm->points = (XPoint *) XtRealloc ((char *)gm->points, nbytes);
	else
	    gm->points = (XPoint *) XtMalloc (nbytes);
    } else
	gm->points = gm->point_data;

    gm_pgon_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_pgon_deletePt (gm, x, y)
    register Marker gm;
    int x, y;
{
    register DPoint *pv;
    register GtermWidget w = gm->w;
    int vertex, nbytes;

    if (gm->npoints <= 2)
	return;

    if (gm->flags & Gm_PgonInit) {
	gm_pgon_resize (gm, x, y);
	return;
    }

    /* Delete the point. */
    vertex = w->gterm.gm_selection.vertex;
    if (vertex < 0 || vertex >= gm->npoints)
	return;
    pv = gm->pgon;

    memmove (&pv[vertex], &pv[vertex+1],
	(gm->npoints - vertex - 1) * sizeof(DPoint));
    gm->npoints--;

    nbytes = gm->npoints * sizeof (DPoint);
    if ((pv = (DPoint *)
	    XtRealloc ((char *)gm->pgon, nbytes)) == (DPoint *)NULL)
	return;
    gm->pgon = pv;

    if (gm->npoints <= GM_MAXVERTICES && gm->points != gm->point_data)
	XtFree ((char *)gm->points);
    gm->points = gm->point_data;

    gm_pgon_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_pgon_movePt (gm, x, y)
    register Marker gm;
    int x, y;
{
    register DPoint *p;
    register GtermWidget w = gm->w;
/*  double cos_rotangle = cos (-(gm->rotangle));
    double sin_rotangle = sin (-(gm->rotangle)); */
    double cos_rotangle = cos ((gm->rotangle));			/* MF019 */
    double sin_rotangle = sin ((gm->rotangle));			/* MF019 */
    double rx, ry;
    int vertex;

    if (gm->flags & Gm_PgonInit) {
	gm_pgon_resize (gm, x, y);
	return;
    }

    /* Translate to the marker reference frame. */
    x = x - gm->x;
    y = y - gm->y;

    /* Rotate back to the unrotated reference frame. */
    rx = x * cos_rotangle - y * sin_rotangle;
    ry = x * sin_rotangle + y * cos_rotangle;

    /* Get vertex. */
    vertex = w->gterm.gm_selection.vertex;
    if (vertex < 0 || vertex >= gm->npoints)
	return;
    p = &gm->pgon[vertex];

    /* Edit point. */
    p->x = rx;
    p->y = ry;

    gm_pgon_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_pgon_move (gm, x, y)
    register Marker gm;
    int x, y;
{
    struct marker new_gm;

    if (gm->flags & Gm_PgonInit) {
	gm_pgon_resize (gm, x, y);
	return;
    }

    new_gm.x = x;  new_gm.y = y;
    gm_constraint (gm, &new_gm, Gb_X|Gb_Y);
    gm->x = new_gm.x;  gm->y = new_gm.y;
    gm_pgon_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_pgon_resize (gm, x, y)
    Marker gm;
    int x, y;
{
    register DPoint *p, *q;
    GtermWidget w = gm->w;
    double cos_rotangle = cos (-(gm->rotangle));
    double sin_rotangle = sin (-(gm->rotangle));
    double theta, scale, slope, rx, ry, x1, y1, x2, y2, xi;
    int vertex, i;

    /* Translate to the marker reference frame. */
    x = x - gm->x;
    y = y - gm->y;

    /* Rotate back to the unrotated reference frame. */
    rx = x * cos_rotangle - y * sin_rotangle;
    ry = x * sin_rotangle + y * cos_rotangle;

    /* Get first vertex of nearest edge. */
    vertex = w->gterm.gm_selection.vertex;
    if (vertex < 0 || vertex >= gm->npoints)
	return;
    p = &gm->pgon[vertex];
    q = p + 1;

    /* Rotate reference frame so that intercept is at y=0. */
    if (abs(rx) + abs(ry) < 1.0)
	scale = 1.0;
    else {
	theta = atan2 (ry, rx);
	cos_rotangle = cos (-theta);
	sin_rotangle = sin (-theta);

	x1 = p->x * cos_rotangle - p->y * sin_rotangle;
	y1 = p->x * sin_rotangle + p->y * cos_rotangle;
	x2 = q->x * cos_rotangle - q->y * sin_rotangle;
	y2 = q->x * sin_rotangle + q->y * cos_rotangle;

	/* Compute scale factor. */
	if (y1 == y2 || x1 == x2)
	    scale = 1.0;
	else {
	    slope = (y2 - y1) / (x2 - x1);
	    xi = x1 - y1 / slope;
	    scale = sqrt (SQR(rx) + SQR(ry)) / xi;
	}
    }

    /* Rescale the polygon. */
    for (i=0, p=gm->pgon;  i < gm->npoints;  i++, p++) {
	p->x *= scale;
	p->y *= scale;
    }

    gm_pgon_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_pgon_rotate (gm, x, y)
    register Marker gm;
    int x, y;
{
    register DPoint *p;
    register GtermWidget w = gm->w;
    double cos_rotangle = cos (-(gm->rotangle));
    double sin_rotangle = sin (-(gm->rotangle));
    double alpha, beta, rx, ry;
    double theta = atan2 ((double)(gm->y - y), (double)(x - gm->x));/* MF019 */
    struct marker new_gm;
    int vertex;

    if (gm->flags & Gm_PgonInit) {
	gm_pgon_resize (gm, x, y);
	return;
    }

    if (x == gm->x && y == gm->y)
	return;

    /* Translate to the marker reference frame. */
    x = x - gm->x;
    y = y - gm->y;

    /* Rotate back to the unrotated reference frame. */
    rx = x * cos_rotangle - y * sin_rotangle;
    ry = x * sin_rotangle + y * cos_rotangle;
    if (abs(rx) + abs(ry) < 1.0)
	return;

    vertex = w->gterm.gm_selection.vertex;
    if (vertex < 0 || vertex >= gm->npoints)
	return;

    p = &gm->pgon[vertex];
    alpha = atan2 (p->y, p->x);	/* angle btw origin & selected vertex */
    beta  = atan2 (ry, rx);	/* angle btw origin & cursor position */

    new_gm.rotangle = gm_niceAngle (gm->rotangle + (beta - alpha));

    new_gm.rotangle = gm_niceAngle (theta);			/* MF019 */

    gm_constraint (gm, &new_gm, Gb_Rotangle);
    gm_rotate_indicator (gm, GXxor);			  	/* MF020 */
    gm->rotangle = new_gm.rotangle;
    gm_rotate_indicator (gm, GXxor);			  	/* MF020 */

    gm_pgon_updatePolygon (gm);
    gm_setCurRect (gm);
}

static void
gm_pgon_updatePolygon (gm)
    Marker gm;
{
    register npts, i;
    register DPoint *ip = gm->pgon;
    register XPoint *op = gm->points;
    double cos_rotangle, sin_rotangle;
    int width, height, xp, xn, yp, yn;

    npts = gm->npoints;
/*  cos_rotangle = cos (gm->rotangle);
    sin_rotangle = sin (gm->rotangle); */
    cos_rotangle = cos (-gm->rotangle);				/* MF019 */
    sin_rotangle = sin (-gm->rotangle);				/* MF019 */
    xp = xn = yp = yn = 0;

    for (i=0;  i < npts;  i++, ip++, op++) {
	/* Compute the rotated point. */
	op->x = ip->x * cos_rotangle - ip->y * sin_rotangle + gm->x + 0.5;
	op->y = ip->x * sin_rotangle + ip->y * cos_rotangle + gm->y + 0.5;

	/* Compute a width/height estimate for the polygon.
	 */
	if (ip->x > xp)
	    xp = ip->x;
	else if (ip->x < xn)
	    xn = ip->x;

	if (ip->y > yp)
	    yp = ip->y;
	else if (ip->y < yn)
	    yn = ip->y;

	gm->width  = (xp + -xn) / 2;
	gm->height = (yp + -yn) / 2;
    }

    gm->points[npts-1] = gm->points[0];
    gm->pgon_npts = gm->npoints;
}


/* Internal procedures for above code.
 * -----------------------------------
 */

/* gm_select -- Determine if a point is within or near a marker, and if so,
 * determine whether the point selects a vertex, edge, or the entire marker.
 */
static int
gm_select (gm, x, y, what)
    Marker gm;
    register int x, y;
    GmSelection what;
{
    register XPoint *p, *ptop;
    GtermWidget w = gm->w;
    int v_dist = w->gterm.gm_nearVertex;
    int e_dist = w->gterm.gm_nearEdge;
    double seglen, d1, d2, s, K, frac;
    int ncrossings, x0, y0;
    XPoint *q;
    int n;
    int use_old_method = 0;

    /* Determine if the point is near a vertex.  */
    for (p = gm->points, n = gm->npoints - 1;  --n >= 0;  p++)
	if (abs (x - p->x) < v_dist && abs (y - p->y) < v_dist) {
	    if (what) {
		what->type = Ge_Point;
		what->vertex = p - gm->points;
	    }
	    return (1);
	}

    /* Determine if the point is near an edge.  The test is based on the
     * observation that when a point is near a line segment, the sum of the
     * distances from the point to either end-point of the line segment is
     * nearly the same as the length of the line segment.
     */
    p = gm->points;

    ptop = p + (gm->npoints - 1);  				/* MF014 */
    x0 = p->x;  y0 = p->y;
    d1 = sqrt ((double)(SQR(x - x0) + SQR(y - y0)));

    for (p++;  p < ptop;  p++) {
	seglen = sqrt ((double)(SQR(p->x - x0) + SQR(p->y - y0)));
	d2 = sqrt ((double)(SQR(x - p->x) + SQR(y - p->y)));

	if (abs(d1 + d2 - seglen) < e_dist) {			/* MF028 */
	    if (what) {
		what->type = Ge_Edge;
		what->vertex = (p - 1) - gm->points;
	    }
	    return (1);
	}

	d1 = d2;
	x0 = p->x;  y0 = p->y;
    }

    /* If the marker is one of the closed polygon types, determine if the
     * point is inside the marker.
     */
    switch (gm->type) {
    case Gm_Line:
    case Gm_Polyline:
	return (0);
	break;
    case Gm_Circle:
        d1 = sqrt ((double)(SQR(x - gm->x) + SQR(y - gm->y)));
	if (d1 < gm->width) {
	    if (what) what->type = Ge_Marker;
	    return (1);
	} else
	    return (0);
	break;
    }

    if (use_old_method) {
        for (p = gm->points, ncrossings=0;  p < ptop;  p++) {
	    /* Scan forward until we find a line segment that crosses Y.
	     */
	    if (p->y > y) {
	        for (p++;  p < ptop && p->y >= y;  p++)
		    ;
	        --p;
	    } else if (p->y < y) {
	        for (p++;  p < ptop && p->y <= y;  p++)
		    ;
	        --p;
	    }

	    /* The line segment p[0]:p[1] crosses the Y plane.  If this lies
	     * entirely to the left of the X plane we can ignore it.  If any
	     * portion of the line segment lies to the right of X we compute
	     * the point where the line intersects the Y plane.  If this point
	     * is to the right of the X plane we have a crossing.
	     */
	    q = p + 1;
	    if (q < ptop && p->x > x || q->x > x) {
                if (q->y == p->y)
                     frac = (double) 0.0;
                else
                     frac = (double)(y - p->y) / (double)(q->y - p->y);
	        if ((frac * (q->x - p->x) + p->x) >= x)
		    ncrossings++;
	    }
        }

    } else {
        float xp[64], yp[64];
        int i;

        for (i=0, p=gm->points, ncrossings=0;  p <= ptop;  p++, i++) {
	    xp[i] = (float) p->x;
	    yp[i] = (float) p->y;
        }
        ncrossings = point_in_poly (gm->npoints, xp, yp, (float)x, (float)y);
    }

    if (ncrossings & 1) {
	if (what)
	    what->type = Ge_Marker;
	return (1);
    }

    return (0);
}

point_in_poly (npol, xp, yp, x, y)
int 	npol;
float 	*xp, *yp, x, y;
{
      int i, j, c = 0;

      for (i = 0, j = npol-1; i < npol; j = i++) {
        if ((((yp[i] <= y) && (y < yp[j])) ||
             ((yp[j] <= y) && (y < yp[i]))) &&
            (x < (xp[j] - xp[i]) * (y - yp[i]) / (yp[j] - yp[i]) + xp[i]))

          c = !c;
      }
      return c;
}




/* gm_markpos -- Mark the current position of a marker.
 */
static void
gm_markpos (gm)
    register Marker gm;
{
    gm->old_rect = gm->cur_rect;
    XUnionRegion (gm->cur_region, null_region, gm->old_region);
}


/* gm_redraw -- Redraw a marker expressed as a list of vertices.
 */
static void
gm_redraw (gm, function)
    register Marker gm;
    int function;
{
    GtermWidget w = gm->w;
    Display *display = w->gterm.display;
    Window window = w->gterm.window;
    int flags = (Gm_Activated|Gm_Visible);
    GC gc = (function == GXxor) ? w->gterm.gm_rubberGC : w->gterm.gm_drawGC;

    if (!w || !XtIsRealized ((Widget)w))
	return;
    if (!((gm->flags & flags) == flags))
	return;

    /* Fill the polygon area if indicated. */
    if (gm->fill && function != GXxor) {
	if (gm->fillPattern) {
	    XSetStipple (display, gc, gm->fillPattern);
	    XSetForeground (display, gc, gm->fillColor);
	    XSetBackground (display, gc, gm->fillBgColor);
	    XSetFillStyle (display, gc, gm->fillStyle);
	} else {
	    XSetForeground (display, gc, gm->fillColor);
	    XSetFillStyle (display, gc, FillSolid);
	}

	XFillPolygon (display, window, gc,
	    gm->points, gm->npoints, Nonconvex, CoordModeOrigin);
    }

    /* Set up the drawing GC. */
    if (function != GXxor) {
	XSetFunction (display, gc, function);
	XSetFillStyle (display, gc, FillSolid);
	XSetForeground (display, gc, (gm == w->gterm.gm_active) ?
	    gm->highlightColor : gm->lineColor);

	XSetLineAttributes (display, gc,
	    gm->lineWidth +
		((gm == w->gterm.gm_active) ? w->gterm.gm_highlightWidth : 0),
	    gm->lineStyle,
	    CapButt,
	    (gm->type == Gm_Polygon || gm->type == Gm_Polyline) ?
		JoinBevel : JoinMiter);
    }

    /* Draw the marker outline. */
    if (gm->lineWidth > 0) {
	if (gm->type == Gm_Circle ||
		(gm->type == Gm_Ellipse && abs(gm->rotangle) < 0.01)) {

	    /* Special case - use X arc drawing primitive.  We could use the
	     * gm->points polygon instead, as this outline polygon is
	     * maintained for all classes of marker.
	     */
	    if (w->gterm.gm_xorFill && function == GXxor) {
		XFillArc (display, window, gc,
		    gm->x - gm->width, gm->y - gm->height,
		    gm->width * 2, gm->height * 2, 0, 360 * 64);
	    }
	    XDrawArc (display, window, gc,
		gm->x - gm->width, gm->y - gm->height,
		gm->width * 2, gm->height * 2, 0, 360 * 64);

	} else {
	    /* Draw marker expressed as a polygon. */
	    if (w->gterm.gm_xorFill && function == GXxor) {
		XFillPolygon (display, window, gc,
		    gm->points, gm->npoints, Convex, CoordModeOrigin);
	    }
	    XDrawLines (display, window, gc,
		gm->points, gm->npoints, CoordModeOrigin);
	}
    }

    /* Draw the knots if enabled. */
    if (function != GXxor && gm->knotSize > 0) {
	int knotsize = gm->knotSize;
	int halfsize = gm->knotSize / 2;
	int i;

	XSetForeground (display, gc, gm->knotColor);
	for (i=0;  i < gm->npoints;  i++) {
	    XFillRectangle (display, window, gc,
		gm->points[i].x - halfsize, gm->points[i].y - halfsize,
		gm->knotSize, gm->knotSize);
	}
    }
}


/* gm_rotate_indicator -- Draw a line indicating the rotation angle.
 */
static void
gm_rotate_indicator (gm, function)				/* MF020 */
Marker gm;
int    function;
{
    GtermWidget w = gm->w;
    Display *display = w->gterm.display;
    Window window = w->gterm.window;
    GC gc = (function == GXxor) ? w->gterm.gm_rubberGC : w->gterm.gm_drawGC;

    if (!gm->rotIndicator)
	return ;

    if (function == GXxor) {
        if (gm->type == Gm_Polygon ||
            gm->type == Gm_Ellipse ||
            gm->type == Gm_Box ||
            gm->type == Gm_Rectangle) {
    		int	x, y, x2, y2;
    		double  ar, cos_rotangle, sin_rotangle;
    		double  alpha = atan2 ((double)gm->height,(double)gm->width);

	        cos_rotangle = cos ((double)(-gm->rotangle - alpha));
    	        sin_rotangle = sin ((double)(-gm->rotangle - alpha));
		ar = (double) gm->height / (double) gm->width;
    	        x = (int) (ar * (gm->width / 2));
	        y = (int) (ar * (gm->height / 2));
    	        x2 = x * cos_rotangle - y * sin_rotangle + gm->x;
    	        y2 = x * sin_rotangle + y * cos_rotangle + gm->y;

  	        XDrawLine (display, window, gc, gm->x, gm->y, x2, y2); 
        }
    } else {
	; 	/* no-op at present */
    }
}


/* gm_setCurRect -- Compute a bounding rectangle which completely encloses
 * a marker (assumes that the marker is expressed as list of points).
 */
static void
gm_setCurRect (gm)
Marker gm;
{
    int border;

    XDestroyRegion (gm->cur_region);
    gm->cur_rect = null_rect;

    if (gm->npoints <= 0)
	gm->cur_region = XCreateRegion();
    else {
	gm->cur_region = XPolygonRegion (gm->points, gm->npoints, EvenOddRule);
	border = (max (gm->lineWidth, gm->knotSize) + 1) / 2;
	border = max (border, BORDER);
	XShrinkRegion (gm->cur_region, -border, -border);
	XClipBox (gm->cur_region, &gm->cur_rect);
    }
}


/* gm_niceAngle -- Round a rotation angle to a "nice" value.
 */
static double
gm_niceAngle (alpha)
    double alpha;
{
    double tol = 0.003;
    double beta;

    if (     abs (alpha - PI_2*0) < tol)
	beta = PI_2*0;
    else if (abs (alpha - PI_2*1) < tol)
	beta = PI_2*1;
    else if (abs (alpha - PI_2*2) < tol)
	beta = PI_2*2;
    else if (abs (alpha - PI_2*3) < tol)
	beta = PI_2*3;
    else if (abs (alpha - PI_2*4) < tol)
	beta = PI_2*0;
    else
	beta = alpha;

    return (beta);
}


