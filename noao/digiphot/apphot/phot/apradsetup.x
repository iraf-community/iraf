include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/center.h"
include "../lib/fitsky.h"

define	HELPFILE	"apphot$phot/iphot.key"

# AP_RADSETUP -- Procedure to set up phot interactively using a radial profile
# plot of a bright star.

procedure ap_radsetup (ap, im, wx, wy, gd, out, stid)

pointer	ap			# pointer to apphot structure
pointer	im			# pointer to the IRAF image
real	wx, wy			# cursor coordinates
pointer	gd			# pointer to graphics stream
int	out			# output file descriptor
int	stid			# output file sequence number

int	cier, sier, pier, key, wcs
pointer	sp, cmd, str
real	xcenter, ycenter, xc, yc, rmin, rmax, imin, imax
real	u1, u2, v1, v2, x1, x2, y1, y2, rval

int	apfitcenter(), apfitsky(), ap_wmag(), apstati(), clgcur(), ap_showplot()
real	apstatr(), ap_cfwhmpsf(), ap_ccapert(), ap_cannulus(), ap_cdannulus()
real	ap_csigma(), ap_crgrow(), ap_crclean(), ap_crclip()
real	ap_cdatamin(), ap_cdatamax()

begin
	# Check for open graphics stream
	if (gd == NULL)
	    return
	call greactivate (gd, 0)
	call gclear (gd)

	# Store the viewport and window coordinates.
	call ggview (gd, u1, u2, v1, v2)
	call ggwind (gd, x1, x2, y1, y2)

	# Make the plot.
	if (ap_showplot (ap, im, wx, wy, gd, xcenter, ycenter, rmin, rmax,
	    imin, imax) == ERR) {
	    call gdeactivate (gd, 0)
	    return
	}

	# Initialize.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

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
	    case 'a':
	        rval = ap_cannulus (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 'd':
	        rval = ap_cdannulus (ap, gd, out, stid, apstatr (ap, ANNULUS),
		    rmin, rmax, imin, imax)
	    case 'g':
	        rval = ap_crgrow (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 'r':
		call ap_caper (ap, gd, out, stid, Memc[str], rmin, rmax,
		    imin, imax)
	    case 'v':
	        rval = ap_cfwhmpsf (ap, gd, out, stid, rmin, rmax, imin, imax)
	        rval = ap_ccapert (ap, gd, out, stid, rmin, rmax, imin, imax)
	        rval = ap_csigma (ap, gd, out, stid, rmin, rmax, imin, imax)
	        #rval = ap_ccthresh (ap, gd, out, stid, rmin, rmax, imin, imax)
	        rval = ap_cannulus (ap, gd, out, stid, rmin, rmax, imin, imax)
	        rval = ap_cdannulus (ap, gd, out, stid, apstatr (ap, ANNULUS),
		    rmin, rmax, imin, imax)
		call ap_caper (ap, gd, out, stid, Memc[str], rmin, rmax,
		    imin, imax)
	    default:
		call printf ("Unknown or ambiguous keystroke command\007\n")
	    }
	    call printf (
	  "Waiting for setup menu command (?=help, v=default setup, q=quit):\n")
	}
	call printf (
	    "Interactive setup is complete. Type w to save parameters.\n")

	# Restore the viewport and window coordinates.
	call gsview (gd, u1, u2, v1, v2)
	call gswind (gd, x1, x2, y1, y2)

	call gdeactivate (gd, 0)
	call sfree (sp)

	# Compute and print the answer.
	cier = apfitcenter (ap, im, xcenter, ycenter)
	if (! IS_INDEFR (apstatr (ap, XCENTER)) &&
	    ! IS_INDEFR (apstatr (ap, YCENTER)))
	    sier = apfitsky (ap, im, apstatr (ap, XCENTER), apstatr (ap,
	        YCENTER), NULL, gd)
	if (! IS_INDEFR (apstatr (ap, SKY_MODE)))
	    pier = ap_wmag (ap, im, apstatr (ap, XCENTER), apstatr (ap,
		YCENTER), apstati (ap, POSITIVE), apstatr (ap, SKY_MODE),
		apstatr (ap, SKY_SIGMA), apstati (ap, NSKY))
	call ap_pplot (ap, im, 0, gd, apstati (ap, RADPLOTS))
	call ap_qpmag (ap, cier, sier, pier)
end
