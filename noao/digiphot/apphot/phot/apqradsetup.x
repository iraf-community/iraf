include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/fitsky.h"
include "../lib/center.h"

define	HELPFILE	"apphot$phot/iqphot.key"

# AP_QRADSETUP -- Procedure to set up phot interactively using a radial profile
# plot of a bright star.

procedure ap_qradsetup (ap, im, wx, wy, gd, out, stid)

pointer	ap			# pointer to apphot structure
pointer	im			# pointer to the IRAF image
real	wx, wy			# cursor coordinates
pointer	gd			# pointer to graphics stream
int	out			# output file descriptor
int	stid			# output file sequence number

int	cier, sier, pier, wcs, key
pointer	sp, str, cmd
real	xcenter, ycenter, rmin, rmax, imin, imax, xc, yc, rval
real	u1, u2, v1, v2, x1, x2, y1, y2

int	apfitcenter(), apfitsky(), ap_wmag(), apstati(), clgcur(), ap_showplot()
real	apstatr(), ap_ccapert(), ap_cannulus(), ap_cdannulus()

begin
	# Check for open graphics stream
	if (gd == NULL)
	    return
	call greactivate (gd, 0)

	# Store the viewport and window coordinates.
	call ggview (gd, u1, u2, v1, v2)
	call ggwind (gd, x1, x2, y1, y2)

	# Plot the radial profile.
	if (ap_showplot (ap, im, wx, wy, gd, xcenter, ycenter, rmin, rmax,
	    imin, imax) == ERR) {
	    call gdeactivate (gd, 0)
	    return
	}

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
	    case 'c':
	        rval = ap_ccapert (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 'a':
	        rval = ap_cannulus (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 'd':
	        rval = ap_cdannulus (ap, gd, out, stid, apstatr (ap, ANNULUS),
		    rmin, rmax, imin, imax)
	    case 'r':
		call ap_caper (ap, gd, out, stid, Memc[str], rmin, rmax,
		    imin, imax)
	    case 'v':
	        rval = ap_ccapert (ap, gd, out, stid, rmin, rmax, imin, imax)
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

	# Print the answer.
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
