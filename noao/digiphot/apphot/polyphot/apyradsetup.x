include "../lib/fitsky.h"
include "../lib/polyphot.h"

define	HELPFILE	"apphot$polyphot/ipolyphot.key"

# AP_YRADSETUP - Set up polyphot interactively using a radial profile
# plot of a bright star.

int procedure ap_yradsetup (ap, im, id, gd, out, stid, x, y, max_nvertices)

pointer	ap			# pointer to apphot structure
pointer	im			# pointer to the IRAF image
pointer	id			# pointer to the image display
pointer	gd			# pointer to graphics stream
int	out			# output file descriptor
int	stid			# output file sequence number
real	x[ARB]			# array of x vertices
real	y[ARB]			# array of y vertices
int	max_nvertices		# maximum number of vertices

int	nvertices, cier, sier, pier, key, wcs
pointer	sp, str, cmd
real	rmin, rmax, imin, imax, u1, u2, v1, v2, x1, x2, y1, y2
real	xcenter, ycenter, xc, yc, rval

int	ap_ycenter(), clgcur(), ap_showplot()
int	apfitsky(),  ap_yfit(), apstati(), ap_ymkpoly()
real	apstatr(), ap_cfwhmpsf(), ap_ccapert(), ap_cannulus(), ap_csigma()
real	ap_cdannulus(), ap_cdatamin(), ap_cdatamax()
real	ap_crgrow(), ap_crclean(), ap_crclip()

begin
	# Mark the polygon interactively.
	if (id == gd)
	    nvertices = ap_ymkpoly (ap, im, id, x, y, max_nvertices, NO)
	else
	    nvertices = ap_ymkpoly (ap, im, id, x, y, max_nvertices, YES)
	if (id != NULL) {
	    if (gd == id)
		call gflush (id)
	    else
		call gframe (id)
	}
	if (nvertices <= 0)
	    return (nvertices)

	# Store the viewport and window coordinates.
	call ggview (gd, u1, u2, v1, v2)
	call ggwind (gd, x1, x2, y1, y2)

	# Check for open display device and graphics stream.
	if (gd == NULL)
	    return (0)
	call greactivate (gd, 0)

	# Show the plot.
	if (ap_showplot (ap, im, apstatr (ap, PYCX), apstatr (ap, PYCY), gd,
	    xcenter, ycenter, rmin, rmax, imin, imax) == ERR) {
	    call gdeactivate (gd, 0)
	    return (nvertices)
	}

	# Allocate memory.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
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
	    case 'h':
		#rval = ap_ccthresh (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 'c':
		rval = ap_ccapert (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 's':
	        rval = ap_csigma (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 'n':
		rval = ap_crclean (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 'p':
		rval = ap_crclip (ap, gd, out, stid, rmin, rmax, imin, imax)
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
		rval = ap_cfwhmpsf (ap, gd, out, stid, rmin, rmax, imin, imax)
		rval = ap_ccapert (ap, gd, out, stid, rmin, rmax, imin, imax)
	        rval = ap_csigma (ap, gd, out, stid, rmin, rmax, imin, imax)
		rval = ap_cannulus (ap, gd, out, stid, rmin, rmax, imin, imax)
		rval = ap_cdannulus (ap, gd, out, stid, apstatr (ap, ANNULUS),
		    rmin, rmax, imin, imax)
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

	# Clean up memory space.
	call gdeactivate (gd, 0)
	call sfree (sp)

	# Print the answer.
	cier = ap_ycenter (ap, im, xcenter, ycenter, x, y, nvertices + 1)
	sier = apfitsky (ap, im, apstatr (ap, PYCX), apstatr (ap, PYCY),
	    NULL, gd)
	pier = ap_yfit (ap, im, x, y, nvertices + 1, apstatr (ap, SKY_MODE),
	    apstatr (ap, SKY_SIGMA), apstati (ap, NSKY))
	call ap_qyprint (ap, cier, sier, pier)

	return (nvertices)
end
