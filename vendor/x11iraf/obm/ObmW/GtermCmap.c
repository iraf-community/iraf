


/*
 * Internal procedures for the above code.
 * ----------------------------------------
 */

/* get_colormap -- Get a private colormap.  On all calls after the first
 * this just returns the existing gterm widget colormap.  On the first call
 * we query the server for the named custom colormap, and if the colormap
 * exists we modify the gterm widget to use it.  If the custom colormap has
 * not yet been created by some other client, we create it.
 *
 * This code creates a custom colormap using the "standard colormap"
 * facilities provided by XLIB.  Although we do not use any of the predefined
 * standard colormaps, use of the standard colormap facilities allows any
 * number of clients to share the same custom colormap.  Use of a custom
 * colormap helps avoid running out of space in the default colormap, ensures
 * that the gterm widget will get the color cells it needs, and  makes it
 * easier for several imaging clients which share the same colormap to
 * simultaneously display their windows.
 *
 * To minimize colormap flashing we try to avoid using the full colormap,
 * setting the unused cells to the colors set in the default colormap.  In
 * most cases this will prevent the rest of the screen from changing color
 * when the custom colormap is installed.
 */
static Colormap
get_colormap (w)
    GtermWidget w;
{
    register int i, j;
    Display *display = w->gterm.display;
    Screen *screen = w->gterm.screen;
    XColor def_colors[SZ_STATIC_CMAP], *cp, *c1, *c2;
    XStandardColormap cm, *cm_p;
    XColor colors[MAX_SZCMAP];
    int base_pixel, p1, p2;
    Colormap colormap;
    char property[128];
    int ncmap, nitems;
    Pixel pixel;
    Atom atom;


    if (DBG_TRACE && DBG_CM_VERB)
 	fprintf (stderr, "get_colormap: have=%d ncols=%d maxcols=%d base=%d\n",
	    w->gterm.haveColormap, w->gterm.ncolors, w->gterm.maxColors,
	    w->gterm.base_pixel);

    if (w->gterm.haveColormap || w->gterm.useGlobalCmap)
	return (w->core.colormap);

    /* Map custom colormap name to atom. */
    sprintf (property, "GT_%s", w->gterm.cmapName);
    atom = XInternAtom (display, property, False);
    w->gterm.cmapAtom = atom;


    /* Get custom colormap.
     */
    if (DBG_TRACE && DBG_CM_VERB)
 	fprintf (stderr, "get_colormap: cmapInitialize=%d GetRGB=%d of %d\n",
	    w->gterm.cmapInitialize,
	    XGetRGBColormaps (display, w->gterm.root, &cm_p, &ncmap, atom),
	    ncmap);


    if (!w->gterm.cmapInitialize &&
	    XGetRGBColormaps (display, w->gterm.root, &cm_p, &ncmap, atom)) {

	/* Colormap aleady exists, just use it.
	 */
	cm = *cm_p;
	colormap = cm.colormap;
	w->gterm.base_pixel = cm.base_pixel;

	if (DBG_TRACE)
 	    fprintf (stderr, "get_colormap: use existing cmap=0x%x; base=%d\n",
	        colormap, w->gterm.base_pixel);


    } else if (w->gterm.w_depth > 8) {

	/*  Setup for TrueColor visual.
	 */
	if (DBG_TRACE && DBG_CM_VERB)
 	    fprintf (stderr, "get_colormap: TrueColor gt.ncolors=%d base=%d\n",
	        w->gterm.ncolors, w->gterm.base_pixel);

        nitems = MAX_SZCMAP;
        w->gterm.ncolors = SZ_STATIC_CMAP + w->gterm.maxColors;

        /* Get a private colormap.
	*/
        for (i=0;  i < MAX_SZCMAP;  i++)
	    colors[i].pixel = i;

    	for (i = SZ_STATIC_CMAP;  i < w->gterm.ncolors;  i++) {
	    w->gterm.color[i].pixel = w->gterm.cmap[i] = pixel =
	        min (nitems - 1, w->gterm.base_pixel + i - SZ_STATIC_CMAP);

	    w->gterm.color[i] = colors[pixel];
    	}

	if (DBG_TRACE)
	    fprintf (stderr,
		"\n\nget_colormap:  gt.ncolors = %d  maxcols = %d\n\n\n", 
		w->gterm.ncolors, w->gterm.maxColors);

	goto use_default;

    } else {
	/* Create or reinitialize a global colormap.
	 */
	XVisualInfo template, *vi;
	Display *d;
	Screen *s;
	Window root;
	long mask;


	if (DBG_TRACE)
 	    fprintf (stderr, "get_colormap: ...creating colormap, ncols=%d\n",
		w->gterm.ncolors);


	if (!(d = XOpenDisplay (DisplayString(display))))
	    goto use_default;
	s = DefaultScreenOfDisplay (d);
	root = DefaultRootWindow (d);

	/* Try to get a pseudocolor visual. */
	mask = 0;
	template.screen = DefaultScreen(d);	mask |= VisualScreenMask;
	template.depth  = RasterDepth;		mask |= VisualDepthMask;
	template.class  = PseudoColor;		mask |= VisualClassMask;

	if (!(vi = XGetVisualInfo (d, mask, &template, &nitems))) {
	    XCloseDisplay (d);
	    goto use_default;
	}

	/* Create custom colormap with all cells allocated read/write */
	colormap = XCreateColormap (d, root, vi->visual, AllocAll);

	/* Initialize colormap to be same as default colormap. */
	nitems = min (MAX_SZCMAP, CellsOfScreen(s));
	for (i=0;  i < nitems;  i++)
	    colors[i].pixel = i;
	XQueryColors (d, DefaultColormapOfScreen(s), colors, nitems);
	XStoreColors (d, colormap, colors, nitems);

	/* Globally define permanent server custom colormap. */
	memset ((char *)&cm, 0, sizeof(cm));
	cm.colormap = colormap;
	cm.base_pixel = w->gterm.base_pixel;
	cm.red_max = 0;
	cm.visualid = vi->visualid;
	cm.killid = 1;
	XSetRGBColormaps (d, root, &cm, 1, atom);

	XSetCloseDownMode (d, RetainPermanent);
	XCloseDisplay (d);
	w->gterm.cmapInitialize = False;

	/* Free the XVisualInfo struct. */
	if (vi)
	    XFree ((void *)vi);					/* MF040 */


	if (DBG_TRACE && DBG_CM_VERB)
 	    fprintf (stderr, "get_colormap: .............creating done\n");
    }

    /* Save default color assignments for static colors. */
    for (i=0;  i < SZ_STATIC_CMAP;  i++)
	def_colors[i] = w->gterm.color[i];

    nitems = min (MAX_SZCMAP, CellsOfScreen(screen));
    w->gterm.ncolors = SZ_STATIC_CMAP + w->gterm.maxColors;
    base_pixel = w->gterm.base_pixel;

    if (DBG_TRACE && DBG_CM_VERB)
	fprintf (stderr,
	    "\n\nget_colormap:  gt.ncolors = %d  maxcols = %d\n\n\n", 
	    w->gterm.ncolors, w->gterm.maxColors);

    /* Get the private colormap. */
    for (i=0;  i < nitems;  i++)
	colors[i].pixel = i;
    XQueryColors (display, colormap, colors, nitems);

    /* Initialize the raster pixel to display pixel mapping and set the
     * color assigned to each pixel value in the private colormap.
     */
    for (i = SZ_STATIC_CMAP;  i < w->gterm.ncolors;  i++) {
	w->gterm.color[i].pixel = w->gterm.cmap[i] = pixel =
	    min (nitems - 1, base_pixel + i - SZ_STATIC_CMAP);
	w->gterm.color[i] = colors[pixel];
    }

    /* Check the static part of the cmap to make sure that the pixel numbers
     * aren't aliased to pixels in the dynamic part of the custom colormap.
     * If this happens, reassign these color numbers to the pixels just
     * preceeding the dynamic part of the custom colormap.  The red_max
     * field of the colormap descriptor is used to keep track of the number
     * of static colors allocated by different clients.  These static colors
     * are shared, hence the same color will not be stored twice.
     */
    p1 = p2 = base_pixel - cm.red_max;
    for (i=0;  i < SZ_STATIC_CMAP;  i++) {
	pixel = w->gterm.cmap[i];
	if (pixel >= base_pixel && pixel < base_pixel+DEF_MAXCOLORS && p1 > 2) {
	    /* First check to see if we already have a static entry reserved
	     * for this color.
	     */
	    c1 = &def_colors[i];
	    for (j=p1, cp=NULL;  j < base_pixel;  j++) {
		c2 = &colors[j];
		if (c1->red == c2->red && c1->green == c2->green &&
			c1->blue == c2->blue) {
		    cp = c2;
		    break;
		}
	    }

	    /* Assign a new screen pixel value. */
	    if (cp)
		w->gterm.cmap[i] = cp->pixel;
	    else {
		cp = &colors[--p1];
		*cp = def_colors[i];
		cp->flags = (DoRed | DoGreen | DoBlue);
		cp->pixel = w->gterm.cmap[i] = p1;
		cm.red_max++;
	    }
	    w->gterm.color[i].pixel = w->gterm.cmap[i];
	}
    }
    if (p1 < p2) {
	XStoreColors (display, colormap, &colors[p1], p2 - p1);
	XSetRGBColormaps (display, w->gterm.root, &cm, 1, atom);
    }

    /* Assign the new colormap to the gterm widget's window. */
    XtVaSetValues ((Widget)w, XtNcolormap, (XtArgVal)colormap, NULL);
    w->gterm.haveColormap++;

    /* If the pointer is in the window, advise window manager to load the
     * colortable for the window.
     */
    if (w->gterm.in_window)
	request_colormap_focus (w);

    return (colormap);

use_default:
    /* Unable to create custom colormap. */
    w->gterm.useDefaultCM++;
    w->gterm.haveColormap++;
    return (w->core.colormap);
}


/* request_colormap_focus -- Modify the WM_COLORMAP_WINDOWS property on a
 * widget's top level shell window to advise the window manager that the
 * widget's window should have its colormap loaded.  This should only be
 * used for windows that have a colormap different than that of the top
 * level window.
 */
static
request_colormap_focus (w)
    GtermWidget w;
{
    Widget p;


    if (!w || !XtIsRealized ((Widget)w))
	return;

    /* Find the top level window. */
    for (p = XtParent(w);  !XtIsShell(p);  p = XtParent(p))
	;

    /* Modify WM_COLORMAP_WINDOWS to give the current window priority.
     */
    if (p) {
	Window window = XtWindow (p);
	Window *wl = NULL, n_wl[MAX_WMWIN+1];
	register int n_nw, i;
	int nw;

	/* If WM_COLORMAP_WINDOWS is already set save its value, otherwise
	 * start a list initially containing only the top level window.
	 */
	w->gterm.wmTop = window;
	if (XGetWMColormapWindows (w->gterm.display, window, &wl, &nw)) {
	    memmove (w->gterm.wmWindows, (char *)wl, nw * sizeof(int));
	    w->gterm.n_wmWindows = nw = min (nw, MAX_WMWIN);
	    free ((char *)wl);
	} else {
	    w->gterm.wmWindows[0] = window;
	    w->gterm.n_wmWindows = nw = 1;
	}

	n_nw = 0;
	wl = w->gterm.wmWindows;
	n_wl[n_nw++] = XtWindow(w);

	for (i=0;  i < nw;  i++)
	    if (wl[i] != XtWindow(w))
		n_wl[n_nw++] = wl[i];

	XSetWMColormapWindows (w->gterm.display, window, n_wl, n_nw);
    }
}


/* restore_colormap_focus -- Reset WM_COLORMAP_WINDOWS.  Retain the window
 * that had the focus in the list, but drop its priority one notch.  This
 * should follow a prior call to request_colormap_focus.
 */
static
restore_colormap_focus (w)
    GtermWidget w;
{
    register int nw, n_nw, i;
    Window *wl, n_wl[MAX_WMWIN+1], old;

    if (!w || !XtIsRealized ((Widget)w))
	return;

    old = XtWindow(w);
    wl = w->gterm.wmWindows;
    if ((nw = w->gterm.n_wmWindows) == 0 || (nw == 1 && wl[0] == old))
	return;

    n_nw = 0;
    if (wl[0] != old)
	n_wl[n_nw++] = wl[0];
    n_wl[n_nw++] = old;

    for (i=1;  i < nw;  i++)
	if (wl[i] != old)
	    n_wl[n_nw++] = wl[i];

    XSetWMColormapWindows (w->gterm.display, w->gterm.wmTop, n_wl, n_nw);
}


/* inherit_default_colormap -- Set any unused cells of the custom colormap
 * to the colors defined for the corresponding cells of the default colormap.
 * This minimizes colormap flashing when using a custom colormap, but only
 * works if a few unused cells can be reserved, e.g., at the beginning of
 * the colormap (which is usually where X allocates its colors).
 */
static
inherit_default_colormap (w)
    GtermWidget w;
{
    register XColor *cp, *ap;
    register int ncolors, i;
    Display *display = w->gterm.display;
    Screen *screen = w->gterm.screen;
    Window root = w->gterm.root;
    Atom atom = w->gterm.cmapAtom;
    XColor colors[MAX_SZCMAP];
    XStandardColormap *cm;
    int first, nitems, ncmap;



    if (DBG_TRACE)
 	fprintf (stderr, "inherit_default_cmap: ncols=%d maxcols=%d base=%d\n",
	    w->gterm.ncolors, w->gterm.maxColors, w->gterm.base_pixel);

    if (!w || !XtIsRealized ((Widget)w))
	return;
    if (w->gterm.useDefaultCM || !w->gterm.haveColormap)
	return;
    if (w->gterm.base_pixel <= 0)
	return;				/* fully allocated colormap */

    /* We have to read the colormap property again as another client could
     * have reserved more static colors (i.e.,changed red_max), and we don't
     * want to clobber these colors.
     */
    if (XGetRGBColormaps (display, root, &cm, &ncmap, atom)) {
	/* Make sure we have the right colormap. */
	if (w->core.colormap != cm->colormap)
	    XtVaSetValues ((Widget)w,XtNcolormap,(XtArgVal)cm->colormap,NULL);

	/* Get lower part of default colormap. */
	ncolors = cm->base_pixel - cm->red_max;
	for (cp=colors, i=0;  i < ncolors;  i++, cp++) {
	    cp->flags = (DoRed | DoGreen | DoBlue);
	    cp->pixel = i;
	}

	/* Get upper part of default colormap. */
	first = cm->base_pixel + w->gterm.ncolors - SZ_STATIC_CMAP;
	ncolors = min (MAX_SZCMAP, CellsOfScreen(screen)) - first;
	for (i=0;  i < ncolors;  i++, cp++) {
	    cp->flags = (DoRed | DoGreen | DoBlue);
	    cp->pixel = first + i;
	}

	/* Inherit values from default colormap. */
	ncolors = cp - colors;
	XQueryColors (display, DefaultColormapOfScreen(screen),
	    colors, ncolors);
	XStoreColors (display, w->core.colormap, colors, ncolors);

	/* The global gterm colormap may have changed.  Compare widget's
	 * version of color table with the global colortable and update
	 * the widget's state if the global colortable has changed.
	 */
	ncolors = w->gterm.ncolors;
	memmove (colors, w->gterm.color, ncolors * sizeof(*cp));
	XQueryColors (display, w->core.colormap, colors, ncolors);
	for (i=ncolors, cp=colors, ap=w->gterm.color;  --i >= 0;  cp++, ap++)
	    if (cp->red != ap->red || cp->green != ap->green ||
		    cp->blue != ap->blue) {
		memmove (w->gterm.color, colors, ncolors * sizeof(*cp));
		invalidate_cmap (w);
	    }

    } else {
        if (DBG_TRACE)
 	    fprintf (stderr, "inherit_default_cmap: else XGetRGBColormaps\n");
    }
}


/* update_default_colormap -- Update the default colormap so that any
 * unallocated cells mirror the widget's custom colormap.  This increases
 * the chance that the widget's contents will be visible when the window
 * does not have the colormap focus, and minimizes flashing when the
 * colormap focus changes.
 */
static
update_default_colormap (w)
    GtermWidget w;
{
    register XColor *ip, *op;
    register int j, n;
    register Pixel v;

    XColor colors[MAX_SZCMAP];
    Pixel pixels[MAX_SZCMAP];
    char allocated[MAX_SZCMAP];
    int overflow, req, need, first, nelem, i;
    unsigned long plane_masks[1];
    Colormap defcmap;


    if (!w || !XtIsRealized ((Widget)w))
	return;
    if (w->gterm.useDefaultCM || !w->gterm.haveColormap)
	return;
    if (w->gterm.useGlobalCmap)				/* 24-bit 	*/
	return;

    if (DBG_TRACE)
	fprintf (stderr, "update_def_colormap:  ENTER\n");

    first = SZ_STATIC_CMAP;
    nelem = w->gterm.ncolors;

    defcmap = DefaultColormapOfScreen (w->gterm.screen);
    /* need = min (MAX_SZCMAP, first + nelem - SZ_STATIC_CMAP); */
    need = MAX_SZCMAP;

    /* Get the colormap cells. */
    for (req=need, n=0;  req > 0 && n < need;  )
	if (XAllocColorCells (w->gterm.display, defcmap, False,
		plane_masks, 0, &pixels[n], req)) {
	    n += req;
	} else
	    req /= 2;

    /* Perform the color matching.  This is awkward as the pixel value
     * assignments may be different in the two colormaps.  We have to look
     * up each pixel before attempting to assign a color, or XStoreColors
     * below will result in a server error.
     */
    memset (allocated, 0, sizeof(allocated));
    overflow = 0;

    for (i=0;  i < n;  i++) {
	v = pixels[i];
	if (v < MAX_SZCMAP)
	    allocated[v] = 1;
	else {
	    overflow++;
	    break;
	}
    }

    ip = &w->gterm.color[first];
    op = colors;
    if (overflow) {
	for (i=0;  i < nelem;  i++, ip++) {
	    for (j=0, v = ip->pixel;  j < n;  j++) {
		if (pixels[j] == v) {
		    *op++ = *ip;
		    break;
		}
	    }
	}
    } else {
	for (j=0;  j < nelem;  j++, ip++) {
	    if (allocated[ip->pixel]) {
		allocated[ip->pixel] = 0;
		*op++ = *ip;
	    }
	}
    }

    if (op > colors)
	XStoreColors (w->gterm.display, defcmap, colors, op - colors);

    if (!w->gterm.useGlobalCmap)
        XFreeColors (w->gterm.display, defcmap, pixels, n, 0);

done:
    if (DBG_TRACE)
	fprintf (stderr, "update_def_colormap:  LEAVING\n");
}




/* Global Colormap routines.
 */
static int SetGlobalCmap(w)
     GtermWidget w;
{
  static int init=0;

  if( !init ){
      strcpy (global_cmapname, XtNcmapName);
      global_ncolors = 0;
      global_mincolors = 0;
      init = 1;
  }
  if( !w->gterm.useGlobalCmap ){
      w->gterm.cmap = (Pixel *)XtCalloc(MAX_SZCMAP, sizeof(Pixel));
      w->gterm.color = (XColor *)XtCalloc(MAX_SZCMAP, sizeof(XColor));

      return(0);

  } else{
      w->gterm.cmap  = global_cmap;
      w->gterm.color = global_color;

      return(global_ncolors);
  }
}


static int ParseGlobalCmap(w)
     GtermWidget w;
{
  char *s;
  char *t;
  char *cmapname;
  int colors;
  int usedefault=0;

  /* process a directive such as "default[n:m,name]", where
   *  n = min number of colors that must be allocated or else
   *  use a private map called "name". m is the max colors to
   *  allocate (same as maxColors).  Either or both can be omitted
   */
  cmapname = w->gterm.cmapName;
  if( !strncmp (cmapname, "default", 7) ){
    usedefault = 1;
    if( (s=strchr(cmapname,'[')) != NULL ){
      /* skip open bracket */
      s++;
      /* get min number of colors */
      global_mincolors = strtol(s, &t, 10);
      /* for n:m syntax, get max colors */
      if( *t == ':' ){
        s = ++t;
        colors = strtol(s, &t, 10);
        if( colors > 0 )
          w->gterm.maxColors = colors;
      }
      /* look for default name */
      if( *t == ',' ){
        t++;
      }
      s = t;
      /* this is the new name of the cmap -- but it can't be "default"! */
      if( (strncmp(s, "default", 7)) && (*s != ']') ){
        strcpy(global_cmapname, s);
        /* null out closing bracket */
        if( (s = strchr(global_cmapname, ']')) != NULL )
          *s = '\0';
      }
      /* now make sure we can grab the min number of colors,
         or else set up to use a private color map */
      colors = GetMaxCmapColors(w);
      if( colors < global_mincolors ){
        usedefault = 0;
        w->gterm.haveColormap = 0;
        strcpy(w->gterm.cmapName, global_cmapname);
      }
      else{
        w->gterm.maxColors = min(w->gterm.maxColors, colors);
      }
    }
  }
  return(usedefault);
}

/*
 *
 * GetMaxCmapColors -- try to determine how many colors we can alloc in the
 * default colormap. We do this now in order set maxColors to this number,
 * to avoid the situation where a larger colormap is used that repeats
 * the actually-allocated colormap -- very ugly ...
 *
 */
static int GetMaxCmapColors(w)
     GtermWidget w;
{
  register int n;
  unsigned long plane_masks[1];
  int req;
  int first, nelem, maxelem;
  Pixel cmap[MAX_SZCMAP];
  Display *dpy;
  Colormap colormap;
  Visual *visual;
  int screen;


  dpy      = XtDisplay(w);
  screen   = XDefaultScreen(dpy);
  visual   = XDefaultVisual(dpy, screen);
  colormap = XDefaultColormap(dpy, screen);

  /* Make sure we have the right sort of visual.
  */
  if (visual->class == TrueColor)
    return (global_ncolors);

  if ((visual->class != PseudoColor) && (visual->class != GrayScale))
    return (0);

  /* Get current colormap specs.
  */
  GtQueryColormap (w, 0, &first, &nelem, &maxelem);

  /* Try to alloc the max size colormap.
  */
  if (maxelem > 0) {
    req = min(MAX_SZCMAP, maxelem);
    for (n=0;  req > 0 && n < maxelem;  ){
      if (XAllocColorCells (dpy, colormap,
            False, plane_masks, 0,  &cmap[n], req)) {
        n += req;
      } else
        req /= 2;
    }

    /* just wondering ... don't really need this.
    */
    if (! w->gterm.useGlobalCmap)
	XFreeColors (dpy, colormap, cmap, n, 0);

  } else
    n = 0;

  return (n);
}


static int
GetGlobalColors()
{
  return (global_ncolors);
}


static void
SetGlobalColors(n)
     int n;
{
  global_ncolors = n;
}


/*
 * This routine will search the STATIC colors of w->core.colormap to find an
 * exact match for the color name. If no match is found, the foreground is
 * returned.
 */

static Pixel
ColorNameToPixel (w, str)
GtermWidget     w;
String          str;
{
    int i;
    XColor color;
    XColor cmap[SZ_STATIC_CMAP];
    char   *ip, *op, name[32];
    unsigned long r, g, b;
    unsigned long val = 0;



    bzero (cmap, (SZ_STATIC_CMAP * sizeof(XColor)));
    if (!str)
	return (0);

    if (w->gterm.w_depth > ColormapDepth && w->gterm.useGlobalCmap) {
	bzero (name, 32);
	for (ip=str, op=name; ip && *ip; ) 
	    *op++ = (char) tolower ((int)*ip++);


        for (i=0; i < (SZ_STATIC_CMAP + SZ_OVERLAY_CMAP); i++) {
	    if (strcmp (name, static_colors[i].name) == 0) {
		r = ((uchar)static_colors[i].r & 0xff) << 16;
		g = ((uchar)static_colors[i].g & 0xff) << 8;
		b = ((uchar)static_colors[i].b & 0xff);
		val = static_colors[i].value;
		break;
	    }
	}
	return (val);

    } else {
        /* Find what colors are available... */
        for (i=0; i < SZ_STATIC_CMAP; i++) {
            memset (&cmap[i], 0, sizeof(XColor));
            cmap[i].pixel = i;
            cmap[i].flags = DoRed | DoGreen | DoBlue;
        }

        XQueryColors(w->gterm.display, w->core.colormap, cmap, SZ_STATIC_CMAP);

        /* ...and find a match */
        if (XParseColor(w->gterm.display, w->core.colormap, str, &color)) {
            for (i=0; i<SZ_STATIC_CMAP; i++) {
                if ((color.red   == cmap[i].red) &&
                    (color.green == cmap[i].green) &&
                    (color.blue  == cmap[i].blue)) {
                        return i;
                }
            }
        }

    }

    return 1;
}

