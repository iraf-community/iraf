define	HELPFILE	 "apphot$daofind/idaofind.key"

# AP_FDRADSETUP -- Procedure to set up daofind interactively using the radial
# profile plot of a bright star.

procedure ap_fdradsetup (ap, im, wx, wy, gd, out, stid)

pointer	ap			# pointer to apphot structure
pointer	im			# pointer to the IRAF image
real	wx, wy			# cursor coordinates
pointer	gd			# pointer to graphics stream
int	out			# output file descriptor
int	stid			# output file sequence number)

int	key, wcs
pointer	sp, cmd
real	rmin, rmax, imin, imax, xcenter, ycenter, rval
real	u1, u2, v1, v2, x1, x2, y1, y2
int	ap_showplot(), clgcur()
real	ap_cfwhmpsf(), ap_csigma(), ap_cdatamin(), ap_cdatamax()

begin
	# Check for open graphics stream.
	if (gd == NULL)
	    return
	call greactivate (gd, 0)

	# Store the old viewport and window coordinates.
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

	call printf (
	"Waiting for setup menu command (?=help, v=default setup, q=quit):\n")
	while (clgcur ("gcommands", xcenter, ycenter, wcs, key, Memc[cmd],
	    SZ_LINE) != EOF) {

	# Enter the cursor setup loop.
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
	    case 'v':
	        rval = ap_cfwhmpsf (ap, gd, out, stid, rmin, rmax, imin, imax)
	        rval = ap_csigma (ap, gd, out, stid, rmin, rmax, imin, imax)
	    default:
		call printf ("Unknown or ambiguous keystroke command\007\n")
	    }
	    call printf (
	 "Waiting for setup menu command (?=help, v=default setup, q=quit):\n")
	}

	# Interactive setup is complete.
	call printf (
	    "Interactive setup is complete. Type w to store parameters.\n")

	# Restore the viewport and window coordinates.
	call gsview (gd, u1, u2, v1, v2)
	call gswind (gd, x1, x2, y1, y2)

	call gdeactivate (gd, 0)
	call sfree (sp)
end
