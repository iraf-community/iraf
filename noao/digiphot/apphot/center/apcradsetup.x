include "../lib/display.h"

define	HELPFILE	"apphot$center/icenter.key"

# AP_CRADSETUP -- Procedure to set up apphot interactively

procedure ap_cradsetup (ap, im, wx, wy, gd, out, stid)

pointer	ap			# pointer to apphot structure
pointer	im			# pointero to the IRAF image
real	wx, wy			# cursor coordinates
pointer	gd			# pointer to graphics stream
int	out			# output file descriptor
int	stid			# output file sequence number

int	cier, wcs, key
pointer	sp, cmd
real	xcenter, ycenter, xc, yc, rmin, rmax, imin, imax, rval
real	u1, u2, v1, v2, x1, x2, y1, y2

int	ap_showplot(), apfitcenter(), apstati(), clgcur()
real	ap_cfwhmpsf(), ap_ccapert(), ap_csigma()
real	ap_cdatamin(), ap_cdatamax(), ap_crclean(), ap_crclip()

begin
	# Check for open graphics stream.
	if (gd == NULL)
	    return
	call greactivate (gd, 0)

	# Store old viewport and window coordinates.
	call ggview (gd, u1, u2, v1, v2)
	call ggwind (gd, x1, x2, y1, y2)

	# Make the plot.
	if (ap_showplot (ap, im, wx, wy, gd, xcenter, ycenter, rmin, rmax,
	    imin, imax) == ERR) {
	    call gdeactivate (gd, 0)
	    return
	}

	# Allocate temporary space.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	call printf (
	"Waiting for setup menu command (?=help, v=default setup, q=quit):\n")

	while (clgcur ("gcommands", xc, yc, wcs, key, Memc[cmd],
	    SZ_LINE) != EOF) {

	switch (key) {
	    case 'q':
	        break
	    case '?':
		call gpagefile (gd, HELPFILE, "")
	    case 'f':
	        rval = ap_cfwhmpsf (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 's':
	        rval = ap_csigma (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 'l':
	        rval = ap_cdatamin (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 'u':
	        rval = ap_cdatamax (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 'c':
		rval = ap_ccapert (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 'n':
		rval = ap_crclean (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 'p':
		rval = ap_crclip (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 'v':
	        rval = ap_cfwhmpsf (ap, gd, out, stid, rmin, rmax, imin, imax)
		rval = ap_ccapert (ap, gd, out, stid, rmin, rmax, imin, imax)
	        rval = ap_csigma (ap, gd, out, stid, rmin, rmax, imin, imax)
	        #rval = ap_ccthresh (ap, gd, out, stid, rmin, rmax, imin, imax)
	    default:
		call printf ("Unknown or ambiguous keystroke command\007\n")
	    }
	    call printf (
	 "Waiting for setup menu command (?=help, v=default setup, q=quit):\n")
	}

	# Set up interactively.
	call printf (
	    "Interactive setup is complete [Type w to save parameters].\n")

	# Restore old window and viewport coordinates.
	call gsview (gd, u1, u2, v1, v2)
	call gswind (gd, x1, x2, y1, y2)

	# Free plots and working space.
	call sfree (sp)
	call gdeactivate (gd, 0)

	# Fit the center and plot the results.
	cier = apfitcenter (ap, im, xcenter, ycenter)
	call ap_cplot (ap, 0, gd, apstati (ap, RADPLOTS))
	call ap_qcenter (ap, cier)
end
