include "../lib/display.h"
include "../lib/fitsky.h"

define	HELPFILE	"apphot$fitsky/ifitsky.key"

# AP_SRADSETUP -- Procedure to set up the sky fitting interactively using
# radial profile plot around the given coordinates.

procedure ap_sradsetup (ap, im, wx, wy, gd, out, stid)

pointer	ap			# pointer to apphot structure
pointer	im			# pointero to the IRAF image
real	wx, wy			# cursor coordinates
pointer	gd			# pointer to graphics stream
int	out			# output file descriptor
int	stid			# output file sequence number

int	ier, key, wcs
pointer	sp, cmd
real	xcenter, ycenter, xc, yc, rmin, rmax, imin, imax, rval
real	u1, u2, v1, v2, x1, x2, y1, y2

int	apstati(), apfitsky(), clgcur(), ap_showplot()
real	apstatr(), ap_cannulus(), ap_cdannulus(), ap_csigma()
real	ap_cdatamin(), ap_cdatamax(), ap_crgrow()

begin
	if (gd == NULL)
	    return
	call greactivate (gd, 0)

	# Store the old window and viewport coordinates.
	call ggview (gd, u1, u2, v1, v2)
	call ggwind (gd, x1, x2, y1, y2)

	# Plot the profile.
	if (ap_showplot (ap, im, wx, wy, gd, xcenter, ycenter, rmin, rmax,
    	    imin, imax) == ERR) {
	    call gdeactivate (gd, 0)
	    return
	}

	# Allocate memory.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	call printf (
	    "Waiting for setup menu command (?=help, v=default setup, q=quit):")
	while (clgcur ("gcommands", xc, yc, wcs, key, Memc[cmd],
	    SZ_LINE) != EOF) {

	switch (key) {

	    case 'q':
	        break
	    case '?':
		call gpagefile (gd, HELPFILE, "")
	    case 's':
	        rval = ap_csigma (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 'l':
	        rval = ap_cdatamin (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 'u':
	        rval = ap_cdatamax (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 'a':
		rval = ap_cannulus (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 'd':
		rval = ap_cdannulus (ap, gd, out, stid, apstatr (ap, ANNULUS),
		    rmin, rmax, imin, imax)
	    case 'g':
		rval = ap_crgrow (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 'v':
		rval = ap_cannulus (ap, gd, out, stid, rmin, rmax, imin, imax)
		rval = ap_cdannulus (ap, gd, out, stid, apstatr (ap, ANNULUS),
		    rmin, rmax, imin, imax)
	        rval = ap_csigma (ap, gd, out, stid, rmin, rmax, imin, imax)
	    default:
		call printf ("Unknown or ambiguous keystroke command\007\n")
	    }
	    call printf (
	  "Waiting for setup menu command (?=help, v=default setup, q=quit):")
	}
	call printf (
	    "Interactive setup is complete. Type w to store parameters.\n")

	# Store the old window and viewport coordinates.
	call gsview (gd, u1, u2, v1, v2)
	call gswind (gd, x1, x2, y1, y2)

	# Free the plotting space.
	call sfree (sp)
	call gdeactivate (gd, 0)

	# Fit the new sky value and print it on the standard output.
	ier = apfitsky (ap, im, xcenter, ycenter, NULL, gd)
	call ap_splot (ap, 0, gd, apstati (ap, RADPLOTS))
	call ap_qspsky (ap, ier)
end
