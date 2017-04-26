

static char *dbg_wSize (GtermWidget w)
{
    static char b[32];

    bzero (b, 32);
    sprintf (b, "%dx%dx%d", w->core.width, w->core.height, w->core.depth);
    return (b);
}


static char *dbg_visStr (int class)
{
    switch (class) {
       case StaticGray:         return ( "StaticGray" );
       case StaticColor:        return ( "StaticColor" );
       case GrayScale:          return ( "GrayScale" );
       case PseudoColor:        return ( "PseudoColor" );
       case TrueColor:          return ( "TrueColor" );
       default:                 return ( "unknown" );
    }
}
