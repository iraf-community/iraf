/*
 *  CDL_SPP.H  -- Header file for the CDL SPP interface.  Fortran compilers
 *  on various platforms may append one or more trailing underscores to 
 *  symbol names, we'll use macros for the interface names and use defines
 *  to see what the symbol name is.
 */

#include "cdl_f77.h"

#ifdef _NO_US_

#define	CDS_OPEN		cdopen
#define	CDS_DISPLAYPIX		cdsppx
#define	CDS_READCURSOR		crdcur
#define	CDS_SETCURSOR		cscurs
#define	CDS_CLEARFRAME		cclfrm
#define	CDS_SELECTFB		cselfb
#define	CDS_CLOSE		cclose
#define	CDS_DISPLAYIRAF		cdspir
#define	CDS_ISIRAF		cisirf
#define	CDS_READIRAF		crdirf
#define	CDS_DISPLAYFITS		cdspft
#define	CDS_ISFITS		cisfts
#define	CDS_READFITS		crdfts
#define	CDS_COMPZSCALE		ccmpzs
#define	CDS_ZSCALEIMAGE		czscim
#define	CDS_PRINTPIX		cprpix
#define	CDS_PRINTPIXTOFILE	cprpfl
#define	CDS_READIMAGE		crdimg
#define	CDS_READFRAMEBUFFER	crdfrb
#define	CDS_READSUBRAS		crsubr
#define	CDS_WRITESUBRAS		cwsubr
#define	CDS_SETWCS		cstwcs
#define	CDS_SETFBCONFIG		csfbcf
#define	CDS_GETFBCONFIG		cgfbcf
#define	CDS_LOOKUPFBSIZE	clkfbs
#define	CDS_SETFRAME		csfram
#define	CDS_SETZTRANS		csztrn
#define	CDS_SETZSCALE		cszscl
#define	CDS_SETSAMPLE		cssamp
#define	CDS_SETSAMPLELINES	cssaml
#define	CDS_SETCONTRAST		cscntr
#define	CDS_SETNAME		csname
#define	CDS_SETTITLE		cstitl
#define	CDS_GETWCS		cgtwcs
#define	CDS_GETFRAME		cgfram
#define	CDS_GETZTRANS		cgztrn
#define	CDS_GETZSCALE		cgzscl
#define	CDS_GETSAMPLE		cgsamp
#define	CDS_GETSAMPLELINES	cgsmpl
#define	CDS_GETCONTRAST		cgcntr
#define	CDS_GETNAME		cgname
#define	CDS_GETTITLE		cgtitl
#define	CDS_MAPFRAME		cmapfr
#define	CDS_MARKCOORDSFILE	cmkcfl
#define	CDS_MARKPOINT		cmkpnt
#define	CDS_MARKPOINTLABEL	cmkpnl
#define	CDS_MARKLINE		cmklin
#define	CDS_MARKBOX		cmkbox
#define	CDS_MARKPOLYLINE	cmkpln
#define	CDS_MARKPOLYGON		cmkpgn
#define	CDS_MARKCIRCLE		cmkcrc
#define	CDS_MARKCIRCANNULI	cmkcan
#define	CDS_MARKELLIPSE		cmkell
#define	CDS_MARKELLIPANNULI	cmkela
#define	CDS_MARKTEXT		cmktxt
#define	CDS_SETFONT		csfont
#define	CDS_SETLINEWIDTH	cslwid
#define	CDS_SETLINESTYLE	cslsty
#define	CDS_SETTEXTWIDTH	cstwid
#define	CDS_DELETEMARK		cdelmk
#define	CDS_CLEAROVERLAY	cclrov
#define	CDS_REDRAWOVERLAY	crdrov
#define	CDS_SETDEBUG		cstdbg
#define	CDS_SETMAPPING		cstmap
#define	CDS_GETMAPPING		cgtmap
#define	CDS_QUERYMAP		cqrmap

#else

#define	CDS_OPEN		cdopen_
#define	CDS_DISPLAYPIX		cdsppx_
#define	CDS_READCURSOR		crdcur_
#define	CDS_SETCURSOR		cscurs_
#define	CDS_CLEARFRAME		cclfrm_
#define	CDS_SELECTFB		cselfb_
#define	CDS_CLOSE		cclose_
#define	CDS_DISPLAYIRAF		cdspir_
#define	CDS_ISIRAF		cisirf_
#define	CDS_READIRAF		crdirf_
#define	CDS_DISPLAYFITS		cdspft_
#define	CDS_ISFITS		cisfts_
#define	CDS_READFITS		crdfts_
#define	CDS_COMPZSCALE		ccmpzs_
#define	CDS_ZSCALEIMAGE		czscim_
#define	CDS_PRINTPIX		cprpix_
#define	CDS_PRINTPIXTOFILE	cprpfl_
#define	CDS_READIMAGE		crdimg_
#define	CDS_READFRAMEBUFFER	crdfrb_
#define	CDS_READSUBRAS		crsubr_
#define	CDS_WRITESUBRAS		cwsubr_
#define	CDS_SETWCS		cstwcs_
#define	CDS_SETFBCONFIG		csfbcf_
#define	CDS_GETFBCONFIG		cgfbcf_
#define	CDS_LOOKUPFBSIZE	clkfbs_
#define	CDS_SETFRAME		csfram_
#define	CDS_SETZTRANS		csztrn_
#define	CDS_SETZSCALE		cszscl_
#define	CDS_SETSAMPLE		cssamp_
#define	CDS_SETSAMPLELINES	cssaml_
#define	CDS_SETCONTRAST		cscntr_
#define	CDS_SETNAME		csname_
#define	CDS_SETTITLE		cstitl_
#define	CDS_GETWCS		cgtwcs_
#define	CDS_GETFRAME		cgfram_
#define	CDS_GETZTRANS		cgztrn_
#define	CDS_GETZSCALE		cgzscl_
#define	CDS_GETSAMPLE		cgsamp_
#define	CDS_GETSAMPLELINES	cgsmpl_
#define	CDS_GETCONTRAST		cgcntr_
#define	CDS_GETNAME		cgname_
#define	CDS_GETTITLE		cgtitl_
#define	CDS_MAPFRAME		cmapfr_
#define	CDS_MARKCOORDSFILE	cmkcfl_
#define	CDS_MARKPOINT		cmkpnt_
#define	CDS_MARKPOINTLABEL	cmkpnl_
#define	CDS_MARKLINE		cmklin_
#define	CDS_MARKBOX		cmkbox_
#define	CDS_MARKPOLYLINE	cmkpln_
#define	CDS_MARKPOLYGON		cmkpgn_
#define	CDS_MARKCIRCLE		cmkcrc_
#define	CDS_MARKCIRCANNULI	cmkcan_
#define	CDS_MARKELLIPSE		cmkell_
#define	CDS_MARKELLIPANNULI	cmkela_
#define	CDS_MARKTEXT		cmktxt_
#define	CDS_SETFONT		csfont_
#define	CDS_SETLINEWIDTH	cslwid_
#define	CDS_SETLINESTYLE	cslsty_
#define	CDS_SETTEXTWIDTH	cstwid_
#define	CDS_DELETEMARK		cdelmk_
#define	CDS_CLEAROVERLAY	cclrov_
#define	CDS_REDRAWOVERLAY	crdrov_
#define	CDS_SETDEBUG		cstdbg_
#define	CDS_SETMAPPING		cstmap_
#define	CDS_GETMAPPING		cgtmap_
#define	CDS_QUERYMAP		cqrmap_

#endif


#ifdef ANSI_FUNC

void CDS_OPEN(char *imtdev, int *ier);
void CDS_DISPLAYPIX(uchar *pix, int *nx, int *ny, int *bitpix, int *frame, int *fbconfig, int *zscale, int *ier);
void CDS_READCURSOR(int *sample, float *x, float *y, int *wcs, char *key, int *ier);
void CDS_DISPLAYIRAF(char *fname, int *band, int *frame, int *fbconfig, int *zscale, int *ier);
void CDS_ISIRAF(char *fname, int *isiraf);
void CDS_READIRAF(char *fname, int *band, uchar *pix, int *nx, int *ny, int *bitpix, char *title, int *ier);
void CDS_DISPLAYFITS(char *fname, int *frame, int *fbconfig, int *zscale, int *ier);
void CDS_ISFITS(char *fname, int *isfits);
void CDS_READFITS(char *fname, uchar *pix, int *nx, int *ny, int *bitpix, char *title, int *ier);
void CDS_SETCURSOR(int *x, int *y, int *wcs, int *ier);
void CDS_SETWCS(char *name, char *title, float *a, float *b, float *c, float *d, float *tx, float *ty, float *z1, float *z2, int *zt, int *ier);
void CDS_GETWCS(char *name, char *title, float *a, float *b, float *c, float *d, float *tx, float *ty, float *z1, float *z2, int *zt, int *ier);
void CDS_CLEARFRAME(int *ier);
void CDS_SELECTFB(int *nx, int *ny, int *fb, int *w, int *h, int *nf, int *reset);
void CDS_CLOSE(void);
void CDS_READIMAGE(uchar *pix, int *nx, int *ny, int *ier);
void CDS_READFRAMEBUFFER(uchar *pix, int *nx, int *ny, int *ier);
void CDS_COMPZSCALE(uchar *pix, int *nx, int *ny, int *bitpix, float *z1, float *z2);
void CDS_ZSCALEIMAGE(uchar *pix, int *nx, int *ny, int *bitpix, float *z1, float *z2);
void CDS_PRINTPIX(char *cmd, uchar *pix, int *nx, int *ny, int *annotate, int *ier);
void CDS_PRINTPIXTOFILE(char *fname, uchar *pix, int *nx, int *ny, int *annotate, int *ier);
void CDS_READSUBRAS(int *lx, int *ly, int *nx, int *ny, uchar *pix, int *ier);
void CDS_WRITESUBRAS(int *lx, int *ly, int *nx, int *ny, uchar *pix, int *ier);
void CDS_SETFBCONFIG(int *configno);
void CDS_GETFBCONFIG(int *configno, int *w, int *h, int *nframes);
void CDS_LOOKUPFBSIZE(int *configno, int *w, int *h, int *nf);
void CDS_SETFRAME(int *frame);
void CDS_SETZTRANS(int *ztrans);
void CDS_SETZSCALE(float *z1, float *z2);
void CDS_SETSAMPLE(int *nsample);
void CDS_SETSAMPLELINES(int *nlines);
void CDS_SETCONTRAST(float *contrast);
void CDS_SETNAME(char *imname);
void CDS_SETTITLE(char *imtitle);
void CDS_GETFRAME(int *frame);
void CDS_GETZTRANS(int *ztrans);
void CDS_GETZSCALE(float *z1, float *z2);
void CDS_GETSAMPLE(int *nsample);
void CDS_GETSAMPLELINES(int *nlines);
void CDS_GETCONTRAST(float *contrast);
void CDS_GETNAME(char *imname);
void CDS_GETTITLE(char *imtitle);
void CDS_MAPFRAME(int *frame, int *ier);
void CDS_MARKCOORDSFILE(char *fname, int *type, int *size, int *color, int *label, int *ier);
void CDS_MARKPOINT(int *x, int *y, int *number, int *size, int *type, int *color, int *ier);
void CDS_MARKPOINTLABEL(int *x, int *y, char *label, int *size, int *type, int *color, int *ier);
void CDS_MARKLINE(int *xs, int *ys, int *xe, int *ye, int *color, int *ier);
void CDS_MARKBOX(int *lx, int *ly, int *ux, int *uy, int *fill, int *color, int *ier);
void CDS_MARKPOLYGON(int *xarray, int *yarray, int *npts, int *fill, int *color, int *ier);
void CDS_MARKPOLYLINE(int *xarray, int *yarray, int *npts, int *color, int *ier);
void CDS_MARKCIRCLE(int *x, int *y, int *radius, int *fill, int *color, int *ier);
void CDS_MARKCIRCANNULI(int *x, int *y, int *radius, int *nannuli, int *sep, int *color, int *ier);
void CDS_MARKELLIPSE(int *x, int *y, int *xrad, int *yrad, float *ang, int *fill, int *color, int *ier);
void CDS_MARKELLIPANNULI(int *x, int *y, int *xrad, int *yrad, float *ang, int *nannuli, int *sep, int *color, int *ier);
void CDS_MARKTEXT(int *x, int *y, char *str, float *size, float *angle, int *color, int *ier);
void CDS_SETFONT(int *font);
void CDS_SETLINEWIDTH(int *width);
void CDS_SETLINESTYLE(int *style);
void CDS_SETTEXTWIDTH(int *width);
void CDS_DELETEMARK(int *x, int *y, int *ier);
void CDS_CLEAROVERLAY(int *ier);
void CDS_REDRAWOVERLAY(int *ier);
void CDS_SETDEBUG(int *state);
void CDS_SETMAPPING(char *region, float *sx, float *sy, int *snx, int *sny, int *dx, int *dy, int *dnx, int *dny, char *ref, int *ier, int reglen, int reflen);
void CDS_GETMAPPING(char *region, float *sx, float *sy, int *snx, int *sny, int *dx, int *dy, int *dnx, int *dny, char *ref, int *ier, int reglen, int reflen);
void CDS_QUERYMAP(int *wcs, char *region, float *sx, float *sy, int *snx, int *sny, int *dx, int *dy, int *dnx, int *dny, char *ref, int *ier, int reglen, int reflen);

static void strupk(char *str, char *outstr, int maxch);
static void strpak(char *sppstr, char *cstr, int maxch);

#else

/* Function Definitions. */
void    CDS_OPEN();
void    CDS_DISPLAYPIX(), CDS_DISPLAYIRAF(), CDS_DISPLAYFITS();
void    CDS_READIRAF(), CDS_READFITS(), CDS_CLEARFRAME();
void    CDS_READIMAGE(), CDS_READFRAMEBUFFER(), CDS_READSUBRAS();
void    CDS_WRITESUBRAS(), CDS_SETCURSOR(), CDS_READCURSOR();
void    CDS_SELECTFB(), CDS_CLOSE(), CDS_COMPZSCALE(), CDS_ZSCALEIMAGE();
void    CDS_SETFRAME(), CDS_SETFBCONFIG(), CDS_SETZTRANS(), CDS_SETZOOM();
void    CDS_SETSAMPLE(), CDS_SETCONTRAST(), CDS_SETNAME(), CDS_SETTITLE();
void    CDS_GETFRAME(), CDS_GETFBCONFIG(), CDS_GETZTRANS(), CDS_GETZOOM();
void    CDS_GETSAMPLE(), CDS_GETCONTRAST(), CDS_GETNAME(), CDS_GETTITLE();
void    CDS_SETZSCALE(), CDS_GETZSCALE(), CDS_LOOKUPFBSIZE();
void    CDS_ISIRAF(), CDS_ISFITS(), CDS_SETDEBUG();
void    CDS_PRINTPIX (), CDS_PRINTPIXTOFILE ();
void    CDS_SETSAMPLELINES (), CDS_GETSAMPLELINES ();
void    CDS_SETWCS(), CDS_GETWCS();
void	CDS_SETMAPPING(), CDS_GETMAPPING(), CDS_QUERYMAP(); 

/* Marker Function Definitions. */
void    CDS_MARKPOINT(), CDS_MARKLINE(), CDS_MARKBOX(), CDS_MARKPOLYLINE();
void    CDS_MARKPOLYGON(), CDS_MARKCIRCLE(), CDS_MARKCIRCANNULI();
void    CDS_MARKELLIPSE(), CDS_MARKELLIPANNULI(), CDS_MARKTEXT();
void    CDS_DELETEMARK(), CDS_CLEAROVERLAY(), CDS_REDRAWOVERLAY();
void    CDS_SETFONT(), CDS_SETLINEWIDTH(), CDS_SETTEXTWIDTH();
void    CDS_SETLINESTYLE(), CDS_MARKCOORDSFILE(), CDS_MARKPOINTLABEL();

static void	strupk(), strpak();

#endif
