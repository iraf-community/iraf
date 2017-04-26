/*
 *  CDL_F77.H  -- header file for the CDL Fortran interface.  Fortran compilers
 *  on various platforms may append one or more trailing underscores to 
 *  symbol names, we'll use macros for the interface names and use defines
 *  to see what the symbol name is.
 */


#ifdef _NO_US_

#define	CDF_OPEN		cfopen
#define	CDF_DISPLAYPIX		cfdisplaypix
#define	CDF_READCURSOR		cfreadcursor
#define	CDF_SETCURSOR		cfsetcursor
#define	CDF_CLEARFRAME		cfclearframe
#define	CDF_SELECTFB		cfselectfb
#define	CDF_CLOSE		cfclose
#define	CDF_DISPLAYIRAF		cfdisplayiraf
#define	CDF_ISIRAF		cfisiraf
#define	CDF_READIRAF		cfreadiraf
#define	CDF_DISPLAYFITS		cfdisplayfits
#define	CDF_ISFITS		cfisfits
#define	CDF_READFITS		cfreadfits
#define	CDF_COMPZSCALE		cfcompzscale
#define	CDF_ZSCALEIMAGE		cfzscaleimage
#define	CDF_PRINTPIX		cfprintpix
#define	CDF_PRINTPIXTOFILE	cfprintpixtofile
#define	CDF_READIMAGE		cfreadimage
#define	CDF_READFRAMEBUFFER	cfreadframebuffer
#define	CDF_READSUBRAS		cfreadsubras
#define	CDF_WRITESUBRAS		cfwritesubras
#define	CDF_SETWCS		cfsetwcs
#define	CDF_SETFBCONFIG		cfsetfbconfig
#define	CDF_GETFBCONFIG		cfgetfbconfig
#define	CDF_LOOKUPFBSIZE	cflookupfbsize
#define	CDF_SETFRAME		cfsetframe
#define	CDF_SETZTRANS		cfsetztrans
#define	CDF_SETZSCALE		cfsetzscale
#define	CDF_SETSAMPLE		cfsetsample
#define	CDF_SETSAMPLELINES	cfsetsamplelines
#define	CDF_SETCONTRAST		cfsetcontrast
#define	CDF_SETNAME		cfsetname
#define	CDF_SETTITLE		cfsettitle
#define	CDF_GETWCS		cfgetwcs
#define	CDF_GETFRAME		cfgetframe
#define	CDF_GETZTRANS		cfgetztrans
#define	CDF_GETZSCALE		cfgetzscale
#define	CDF_GETSAMPLE		cfgetsample
#define	CDF_GETSAMPLELINES	cfgetsamplelines
#define	CDF_GETCONTRAST		cfgetcontrast
#define	CDF_GETNAME		cfgetname
#define	CDF_GETTITLE		cfgettitle
#define	CDF_MAPFRAME		cfmapframe
#define	CDF_MARKCOORDSFILE	cfmarkcoordsfile
#define	CDF_MARKPOINT		cfmarkpoint
#define	CDF_MARKPOINTLABEL	cfmarkpointlabel
#define	CDF_MARKLINE		cfmarkline
#define	CDF_MARKBOX		cfmarkbox
#define	CDF_MARKPOLYLINE	cfmarkpolyline
#define	CDF_MARKPOLYGON		cfmarkpolygon
#define	CDF_MARKCIRCLE		cfmarkcircle
#define	CDF_MARKCIRCANNULI	cfmarkcircannuli
#define	CDF_MARKELLIPSE		cfmarkellipse
#define	CDF_MARKELLIPANNULI	cfmarkellipannuli
#define	CDF_SETFONT		cfsetfont
#define	CDF_SETLINEWIDTH	cfsetlwidth
#define	CDF_SETLINESTYLE	cfsetlstyle
#define	CDF_SETTEXTWIDTH	cfsettwidth
#define	CDF_MARKTEXT		cfmarktext
#define	CDF_DELETEMARK		cfdeletemark
#define	CDF_CLEAROVERLAY	cfclearoverlay
#define	CDF_REDRAWOVERLAY	cfredrawoverlay
#define	CDF_SETDEBUG		cfsetdebug
#define CDF_SETMAPPING          cfsetmapping
#define CDF_GETMAPPING          cfgetmapping
#define CDF_QUERYMAP            cfquerymap


#else

#define	CDF_OPEN		cfopen_
#define	CDF_DISPLAYPIX		cfdisplaypix_
#define	CDF_READCURSOR		cfreadcursor_
#define	CDF_SETCURSOR		cfsetcursor_
#define	CDF_CLEARFRAME		cfclearframe_
#define	CDF_SELECTFB		cfselectfb_
#define	CDF_CLOSE		cfclose_
#define	CDF_DISPLAYIRAF		cfdisplayiraf_
#define	CDF_ISIRAF		cfisiraf_
#define	CDF_READIRAF		cfreadiraf_
#define	CDF_DISPLAYFITS		cfdisplayfits_
#define	CDF_ISFITS		cfisfits_
#define	CDF_READFITS		cfreadfits_
#define	CDF_COMPZSCALE		cfcompzscale_
#define	CDF_ZSCALEIMAGE		cfzscaleimage_
#define	CDF_PRINTPIX		cfprintpix_
#define	CDF_PRINTPIXTOFILE	cfprintpixtofile_
#define	CDF_READIMAGE		cfreadimage_
#define	CDF_READFRAMEBUFFER	cfreadframebuffer_
#define	CDF_READSUBRAS		cfreadsubras_
#define	CDF_WRITESUBRAS		cfwritesubras_
#define	CDF_SETWCS		cfsetwcs_
#define	CDF_SETFBCONFIG		cfsetfbconfig_
#define	CDF_GETFBCONFIG		cfgetfbconfig_
#define	CDF_LOOKUPFBSIZE	cflookupfbsize_
#define	CDF_SETFRAME		cfsetframe_
#define	CDF_SETZTRANS		cfsetztrans_
#define	CDF_SETZSCALE		cfsetzscale_
#define	CDF_SETSAMPLE		cfsetsample_
#define	CDF_SETSAMPLELINES	cfsetsamplelines_
#define	CDF_SETCONTRAST		cfsetcontrast_
#define	CDF_SETNAME		cfsetname_
#define	CDF_SETTITLE		cfsettitle_
#define	CDF_GETWCS		cfgetwcs_
#define	CDF_GETFRAME		cfgetframe_
#define	CDF_GETZTRANS		cfgetztrans_
#define	CDF_GETZSCALE		cfgetzscale_
#define	CDF_GETSAMPLE		cfgetsample_
#define	CDF_GETSAMPLELINES	cfgetsamplelines_
#define	CDF_GETCONTRAST		cfgetcontrast_
#define	CDF_GETNAME		cfgetname_
#define	CDF_GETTITLE		cfgettitle_
#define	CDF_MAPFRAME		cfmapframe_
#define	CDF_MARKCOORDSFILE	cfmarkcoordsfile_
#define	CDF_MARKPOINT		cfmarkpoint_
#define	CDF_MARKPOINTLABEL	cfmarkpointlabel_
#define	CDF_MARKLINE		cfmarkline_
#define	CDF_MARKBOX		cfmarkbox_
#define	CDF_MARKPOLYLINE	cfmarkpolyline_
#define	CDF_MARKPOLYGON		cfmarkpolygon_
#define	CDF_MARKCIRCLE		cfmarkcircle_
#define	CDF_MARKCIRCANNULI	cfmarkcircannuli_
#define	CDF_MARKELLIPSE		cfmarkellipse_
#define	CDF_MARKELLIPANNULI	cfmarkellipannuli_
#define	CDF_MARKTEXT		cfmarktext_
#define	CDF_SETFONT		cfsetfont_
#define	CDF_SETLINEWIDTH	cfsetlwidth_
#define	CDF_SETLINESTYLE	cfsetlstyle_
#define	CDF_SETTEXTWIDTH	cfsettwidth_
#define	CDF_DELETEMARK		cfdeletemark_
#define	CDF_CLEAROVERLAY	cfclearoverlay_
#define	CDF_REDRAWOVERLAY	cfredrawoverlay_
#define	CDF_SETDEBUG		cfsetdebug_
#define CDF_SETMAPPING          cfsetmapping_
#define CDF_GETMAPPING          cfgetmapping_
#define CDF_QUERYMAP            cfquerymap_

#endif


#ifdef ANSI_FUNC

void CDF_OPEN(char *imtdev, int *ier, int len);
void CDF_DISPLAYPIX(uchar *pix, int *nx, int *ny, int *bitpix, int *frame, int *fbconfig, int *zscale, int *ier);
void CDF_READCURSOR(int *sample, float *x, float *y, int *wcs, char *key, int *ier);
void CDF_DISPLAYIRAF(char *fname, int *band, int *frame, int *fbconfig, int *zscale, int *ier, int len);
void CDF_ISIRAF(char *fname, int *isiraf, int len);
void CDF_READIRAF(char *fname, int *band, uchar *pix, int *nx, int *ny, int *bitpix, char *title, int *ier, int len);
void CDF_DISPLAYFITS(char *fname, int *frame, int *fbconfig, int *zscale, int *ier, int len);
void CDF_ISFITS(char *fname, int *isfits, int len);
void CDF_READFITS(char *fname, uchar *pix, int *nx, int *ny, int *bitpix, char *title, int *ier, int len);
void CDF_SETCURSOR(int *x, int *y, int *wcs, int *ier);
void CDF_SETWCS(char *name, char *title, float *a, float *b, float *c, float *d, float *tx, float *ty, float *z1, float *z2, int *zt, int *ier, int nlen, int tlen);
void CDF_GETWCS(char *name, char *title, float *a, float *b, float *c, float *d, float *tx, float *ty, float *z1, float *z2, int *zt, int *ier, int nlen, int tlen);
void CDF_CLEARFRAME(int *ier);
void CDF_SELECTFB(int *nx, int *ny, int *fb, int *w, int *h, int *nf, int *reset);
void CDF_CLOSE(void);
void CDF_READIMAGE(uchar *pix, int *nx, int *ny, int *ier);
void CDF_READFRAMEBUFFER(uchar *pix, int *nx, int *ny, int *ier);
void CDF_COMPZSCALE(uchar *pix, int *nx, int *ny, int *bitpix, float *z1, float *z2);
void CDF_ZSCALEIMAGE(uchar *pix, int *nx, int *ny, int *bitpix, float *z1, float *z2);
void CDF_PRINTPIX(char *cmd, uchar *pix, int *nx, int *ny, int *annotate, int *ier, int len);
void CDF_PRINTPIXTOFILE(char *fname, uchar *pix, int *nx, int *ny, int *annotate, int *ier, int len);
void CDF_READSUBRAS(int *lx, int *ly, int *nx, int *ny, uchar *pix, int *ier);
void CDF_WRITESUBRAS(int *lx, int *ly, int *nx, int *ny, uchar *pix, int *ier);
void CDF_SETFBCONFIG(int *configno);
void CDF_GETFBCONFIG(int *configno, int *w, int *h, int *nframes);
void CDF_LOOKUPFBSIZE(int *configno, int *w, int *h, int *nf);
void CDF_SETFRAME(int *frame);
void CDF_SETZTRANS(int *ztrans);
void CDF_SETZSCALE(float *z1, float *z2);
void CDF_SETSAMPLE(int *nsample);
void CDF_SETSAMPLELINES(int *nlines);
void CDF_SETCONTRAST(float *contrast);
void CDF_SETNAME(char *imname, int len);
void CDF_SETTITLE(char *imtitle, int len);
void CDF_GETFRAME(int *frame);
void CDF_GETZTRANS(int *ztrans);
void CDF_GETZSCALE(float *z1, float *z2);
void CDF_GETSAMPLE(int *nsample);
void CDF_GETSAMPLELINES(int *nlines);
void CDF_GETCONTRAST(float *contrast);
void CDF_GETNAME(char *imname, int len);
void CDF_GETTITLE(char *imtitle, int len);
void CDF_MAPFRAME(int *frame, int *ier);
void CDF_MARKCOORDSFILE(char *fname, int *type, int *size, int *color, int *label, int *ier, int len);
void CDF_MARKPOINT(int *x, int *y, int *number, int *size, int *type, int *color, int *ier);
void CDF_MARKPOINTLABEL(int *x, int *y, char *label, int *size, int *type, int *color, int *ier, int len);
void CDF_MARKLINE(int *xs, int *ys, int *xe, int *ye, int *color, int *ier);
void CDF_MARKBOX(int *lx, int *ly, int *ux, int *uy, int *fill, int *color, int *ier);
void CDF_MARKPOLYGON(int *xarray, int *yarray, int *npts, int *fill, int *color, int *ier);
void CDF_MARKPOLYLINE(int *xarray, int *yarray, int *npts, int *color, int *ier);
void CDF_MARKCIRCLE(int *x, int *y, int *radius, int *fill, int *color, int *ier);
void CDF_MARKCIRCANNULI(int *x, int *y, int *radius, int *nannuli, int *sep, int *color, int *ier);
void CDF_MARKELLIPSE(int *x, int *y, int *xrad, int *yrad, float *ang, int *fill, int *color, int *ier);
void CDF_MARKELLIPANNULI(int *x, int *y, int *xrad, int *yrad, float *ang, int *nannuli, int *sep, int *color, int *ier);
void CDF_MARKTEXT(int *x, int *y, char *str, float *size, float *angle, int *color, int *ier, int len);
void CDF_SETFONT(int *font);
void CDF_SETLINEWIDTH(int *width);
void CDF_SETLINESTYLE(int *style);
void CDF_SETTEXTWIDTH(int *width);
void CDF_DELETEMARK(int *x, int *y, int *ier);
void CDF_CLEAROVERLAY(int *ier);
void CDF_REDRAWOVERLAY(int *ier);
void CDF_SETDEBUG(int *state);
void CDF_SETMAPPING(char *region, float *sx, float *sy, int *snx, int *sny, int *dx, int *dy, int *dnx, int *dny, char *ref, int *ier, int reglen, int reflen);
void CDF_GETMAPPING(char *region, float *sx, float *sy, int *snx, int *sny, int *dx, int *dy, int *dnx, int *dny, char *ref, int *ier, int reglen, int reflen);
void CDF_QUERYMAP(int *wcs, char *region, float *sx, float *sy, int *snx, int *sny, int *dx, int *dy, int *dnx, int *dny, char *ref, int *ier, int reglen, int reflen);


#else

/* Function Definitions. */
void    CDF_OPEN();
void    CDF_DISPLAYPIX(), CDF_DISPLAYIRAF(), CDF_DISPLAYFITS();
void    CDF_READIRAF(), CDF_READFITS(), CDF_CLEARFRAME();
void    CDF_READIMAGE(), CDF_READFRAMEBUFFER(), CDF_READSUBRAS();
void    CDF_WRITESUBRAS(), CDF_SETCURSOR(), CDF_READCURSOR();
void    CDF_SELECTFB(), CDF_CLOSE(), CDF_COMPZSCALE(), CDF_ZSCALEIMAGE();
void    CDF_SETFRAME(), CDF_SETFBCONFIG(), CDF_SETZTRANS(), CDF_SETZOOM();
void    CDF_SETSAMPLE(), CDF_SETCONTRAST(), CDF_SETNAME(), CDF_SETTITLE();
void    CDF_GETFRAME(), CDF_GETFBCONFIG(), CDF_GETZTRANS(), CDF_GETZOOM();
void    CDF_GETSAMPLE(), CDF_GETCONTRAST(), CDF_GETNAME(), CDF_GETTITLE();
void    CDF_SETZSCALE(), CDF_GETZSCALE(), CDF_LOOKUPFBSIZE();
void    CDF_ISIRAF(), CDF_ISFITS(), CDF_SETDEBUG();
void    CDF_PRINTPIX (), CDF_PRINTPIXTOFILE ();
void    CDF_SETSAMPLELINES (), CDF_GETSAMPLELINES ();
void    CDF_SETWCS(), CDF_GETWCS();
void 	CDF_SETMAPPING(), CDF_GETMAPPING(), CDF_QUERYMAP();

/* Marker Function Definitions. */
void    CDF_MARKPOINT(), CDF_MARKLINE(), CDF_MARKBOX(), CDF_MARKPOLYLINE();
void    CDF_MARKPOLYGON(), CDF_MARKCIRCLE(), CDF_MARKCIRCANNULI();
void    CDF_MARKELLIPSE(), CDF_MARKELLIPANNULI(), CDF_MARKTEXT();
void    CDF_MARKPOINTLABEL(), CDF_SETFONT();
void    CDF_DELETEMARK(), CDF_CLEAROVERLAY(), CDF_REDRAWOVERLAY();
void    CDF_SETLINEWIDTH(), CDF_SETTEXTWIDTH(), CDF_SETLINESTYLE();
void 	CDF_MARKCOORDSFILE();

#endif
