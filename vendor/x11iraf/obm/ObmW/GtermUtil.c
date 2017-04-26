
static XImage *cached_ximage =	NULL;			/* MF004 BEGIN */

/* GetCachedXImage -- 
 */
static XImage *
GetCachedXImage (w, pixmap, width, height)
     GtermWidget w;
     Pixmap pixmap;
     int width;
     int height;
{
    if ((cached_ximage != NULL)) {
        if ((pixmap == w->gterm.pixmap) &&
            (width  == w->core.width)   &&
            (height == w->core.height)) {
                return (cached_ximage);
        }
    }
    return(NULL);
}


/* DestroyCachedXImage --
 */
static void 
DestroyCachedXImage ()
{
    if (cached_ximage != NULL) {
        XDestroyImage (cached_ximage);
      cached_ximage = NULL;
    }
}


/* NewCachedXImage --
 */
static void 
NewCachedXImage (w, xin, pixmap, width, height)
     GtermWidget w;
     XImage *xin;
     Pixmap pixmap;
     int width;
     int height;
{
    if ((pixmap ==  w->gterm.pixmap) &&
        (width  ==  w->core.width)   &&
        (height ==  w->core.height)) {
            DestroyCachedXImage();
            cached_ximage = xin;
    }
}							/* MF004 END   */
