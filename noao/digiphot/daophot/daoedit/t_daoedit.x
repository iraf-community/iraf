include <imhdr.h>
include <gset.h>
include "../lib/daophotdef.h"
include "daoedit.h"

define	HELPFILE	"daophot$daoedit/daoedit.key"
define	IHELPFILE	"daophot$daoedit/daoedit.key"

# T_DAOEDIT -- Edit the DAOPHOT parameters interactively using the image
# display and a radial profile plot.

procedure t_daoedit ()

pointer	image			# the name of the input image
int	cache			# cache the image pixels
pointer	graphics		# the graphics device
pointer	display			# the image display

real	wx, wy, xlast, ylast
pointer	sp, cmd, im, gd, id
int	wcs, key, redraw, gcurtype, curtype, xwcs, ywcs, lastkey
int	req_size, memstat, old_size, buf_size

pointer	immap(), gopen()
int	dp_gcur(), btoi(), sizeof(), dp_memstat()
bool	clgetb(), streq()
errchk	gopen()

data	gcurtype /'g'/

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (graphics, SZ_FNAME, TY_CHAR)
	call salloc (display, SZ_FNAME, TY_CHAR)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	call clgstr ("image", Memc[image], SZ_FNAME)
	im = immap (Memc[image], READ_ONLY, 0)

	call clgstr ("graphics", Memc[graphics], SZ_FNAME)
	iferr (gd = gopen (Memc[graphics], AW_DEFER+NEW_FILE, STDGRAPH))
	    gd = NULL
	call clgstr ("display", Memc[display], SZ_FNAME)
        if (Memc[display] == EOS)
            id = NULL
        else if (streq (Memc[graphics], Memc[display]))
            id = gd
        else {
            iferr {
                id = gopen (Memc[display], APPEND, STDIMAGE)
            } then {
                call eprintf (
                "Warning: Graphics overlay not available for display device.\n")
                id = NULL
            }
        }
	cache = btoi (clgetb ("cache"))

	# Set up the display coordinate system.
        if ((id != NULL) && (id != gd))
            call dp_gswv (id, Memc[image], im, 4)

        # Cache the input image pixels.
        req_size = MEMFUDGE * IM_LEN(im,1) * IM_LEN(im,2) *
	    sizeof (IM_PIXTYPE(im))
        memstat = dp_memstat (cache, req_size, old_size)
        if (memstat == YES)
            call dp_pcache (im, INDEFI, buf_size)

	xlast = INDEFR
	ylast = INDEFR
	lastkey = INDEFI
	xwcs = WCS_XPIX
	ywcs = WCS_YCOUNT
	curtype = 'i'
	redraw = NO

	while (dp_gcur (curtype, wx, wy, wcs, key, Memc[cmd], SZ_LINE) != EOF) {

	    # Convert the cursor coordinates if necessary.
	    if (curtype == 'i')
		call dp_vtol (im, wx, wy, wx, wy, 1)

	    switch (key) {

	    # Print help page.
	    case '?':
		if (curtype == 'i') {
		    call pagefile (HELPFILE, "[space=cmhelp,q=quit,?=help]")
		} else if (curtype == 'g') {
		    #call greactivate (gd, 0)
		    call gpagefile (gd, HELPFILE, "")
		}


	    # Quit the program.
	    case 'q':
		break

	    # Toggle between the image display and graphics cursor.
	    case 'g':
		if (curtype == 'i') {
		    call greactivate (gd, 0)
		    curtype = 'g'
		} else { 
		    call gdeactivate (gd, 0)
		    curtype = 'i'
		}

	    # Toggle between the pixel and scale units plot scale in x.
	    case 'x':
		if (xwcs == WCS_XPIX)
		    xwcs = WCS_XSCALE
		else if (xwcs == WCS_XSCALE)
		    xwcs = WCS_XPIX
		redraw = YES

	    # Toggle between the counts and norm plot scale in y.
	    case 'y':
		if (ywcs == WCS_YCOUNT)
		    ywcs = WCS_YNORM
		else if (ywcs == WCS_YNORM)
		    ywcs = WCS_YCOUNT
		redraw = YES

	    case 'a':
		if (curtype == 'i') {
		    if (lastkey == 'a')
		        call dp_erprofile (NULL, id, NO, xwcs, ywcs, im, wx, wy)
		    else
		        call dp_erprofile (NULL, id, YES, xwcs, ywcs, im,
			    wx, wy)
		} else {
		    call dp_erprofile (NULL, id, NO, xwcs, ywcs, im,
		        xlast, ylast)
		}

	    case 'r':
		if (curtype == 'i') {
		    xlast = wx
		    ylast = wy
		}
		redraw = YES

	    case 'i':
		if (curtype == 'i') {
		    xlast = wx
		    ylast = wy
		}
		xwcs = WCS_XPIX
		ywcs = WCS_YCOUNT
		redraw = YES

	    case ':':
		call dp_ecolon (Memc[cmd], gd, redraw)

	    default:
		call printf ("Unknown or ambiguous keystroke command\7\n")
	    }

	    # Draw the plot.
	    if (redraw == YES) {
		call dp_erprofile (gd, id, NO, xwcs, ywcs, im, xlast, ylast)
		if (key == 'i')
		    call dp_isetup (gcurtype, gd)
		redraw = NO
		if (curtype == 'i')
		    call gdeactivate (gd, 0)
	    }

	    lastkey = key
	}

        if (id == gd && gd != NULL) {
	    call gclose (gd)
	} else {
	    if (gd != NULL)
		call gclose (gd)
	    if (id != NULL)
		call gclose (id)
	}

	call imunmap (im)

        # Uncache memory.
        call fixmem (old_size)

	call sfree (sp)
end


# DP_GCUR --Get a SETPARS cursor value.

int procedure dp_gcur (curtype, x, y, wcs, key, strval, maxch)

int     curtype                 # cursor type
real    x, y                    # cursor position
int	wcs			# cursor wcs
int     key                     # keystroke value of cursor event
char    strval[ARB]             # string value, if any
int     maxch                   # max chars out

int	nitems
int	clgcur()

begin
	# Initialize.
	strval[1] = EOS
	 
	# Get a cursor values from the desired cursor parameter.
	 switch (curtype) {
	 case 'i':
	     nitems = clgcur ("icommands", x, y, wcs, key, strval, maxch)
	 case 'g':
	     nitems = clgcur ("gcommands", x, y, wcs, key, strval, maxch)
	}

	return (nitems)
end


# DP_ISETUP -- Setup the daophot parameters interactively using a
# radial profile plot.

procedure dp_isetup (curtype, gd)

int	curtype			# the cursor type graphics or display
pointer	gd			# pointer to the graphics stream

int	wcs, key
pointer	sp, cmd
real	wx, wy
int	dp_gcur()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	call printf (
	    "Interactive setup menu (?=help, spbar=default, q=quit):\n")
	while (dp_gcur (curtype, wx, wy, wcs, key, Memc[cmd],
	    SZ_LINE) != EOF) {
	    switch (key) {
	    case '?':
		call gpagefile (gd, IHELPFILE, "")
	    case 'q':
		break
	    case 'f':
		call dp_mfwhmpsf(gd)
		call dp_cfwhmpsf()
	    case 's':
		call dp_msigma(gd)
		call dp_csigma()
	    case 'u':
		call dp_mdmax(gd)
		call dp_cdmax()
	    case 'l':
		call dp_mdmin(gd)
		call dp_cdmin()
	    case 'c':
		call dp_mcbox(gd)
		call dp_ccbox()
	    case 'n':
		call dp_mrclean(gd)
		call dp_crclean()
	    case 'p':
		call dp_mrclip(gd)
		call dp_crclip()
	    case 'a':
		call dp_mannulus(gd)
		call dp_cannulus()
	    case 'd':
		call dp_mdannulus(gd)
		call dp_cdannulus()
	    case 'g':
		call dp_mrgrow(gd)
		call dp_crgrow()
	    case 'r':
		call dp_maper(gd)
		call dp_caper()
	    case 'w':
		call dp_mpsfrad(gd)
		call dp_cpsfrad()
	    case 'b':
		call dp_mfitrad(gd)
		call dp_cfitrad()
	    case ' ':
		call dp_mfwhmpsf(gd)
		call dp_mcbox(gd)
		call dp_mannulus(gd)
		call dp_mdannulus(gd)
		call dp_maper(gd)
		call dp_mpsfrad(gd)
		call dp_mfitrad(gd)
		call gdeactivate(gd, 0)
		call dp_cbanner()
		call dp_cfwhmpsf()
		call dp_ccbox()
		call dp_cannulus()
		call dp_cdannulus()
		call dp_caper()
		call dp_cpsfrad()
		call dp_cfitrad()
		call greactivate(gd, 0)
	    default:
	        call printf (
	        "Interactive setup menu (?=help, spbar=default, q=quit):\n")
	    }
	    call printf (
	        "Interactive setup menu (?=help, spbar=default, q=quit):\n")
	}

	if (curtype == 'i')
	    call gdeactivate (gd, 0)
	call sfree (sp)
end
