define	HELPFILE	"apphot$fitpsf/ifitpsf.key"

# AP_PFRADSETUP -- Procedure to set up fitpsf interactively

procedure ap_pfradsetup (ap, im, wx, wy, gd, out, stid)

pointer	ap			# pointer to apphot structure
pointer	im			# pointer to to the IRAF image
real	wx, wy			# cursor coordinates
pointer	gd			# pointer to graphics stream
int	out			# output file descriptor
int	stid			# output file sequence number

int	ier, wcs, key
pointer	sp, cmd
real	xcenter, ycenter, rmin, rmax, imin, imax, xc, yc, rval
real	u1, u2, v1, v2, x1, x2, y1, y2

int	apsffit(), clgcur(), ap_showplot()
real	ap_cfwhmpsf(), ap_cpapert(), ap_cdatamin(), ap_cdatamax()

begin
	if (gd == NULL)
	    return
	call greactivate (gd, 0)
	call gclear (gd)

	# Save old viewport and window coordinates
	call ggview (gd, u1, u2, v1, v2)
	call ggwind (gd, x1, x2, y1, y2)

	# plot the radial profile.
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
	while (clgcur ("gcommands", xc, yc, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {

	switch (key) {

	    case 'q':
	        break
	    case '?':
		call gpagefile (gd, HELPFILE, "")
	    case 'f':
	        rval = ap_cfwhmpsf (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 'l':
	        rval = ap_cdatamin (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 'u':
	        rval = ap_cdatamax (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 'b':
		rval = ap_cpapert (ap, gd, out, stid, rmin, rmax, imin, imax)
	    case 'v':
		rval = ap_cpapert (ap, gd, out, stid, rmin, rmax, imin, imax)
	        rval = ap_cfwhmpsf (ap, gd, out, stid, rmin, rmax, imin, imax)
	    default:
		call printf ("Unknown or ambiguous keystroke command\007\n")
	    }
	    call printf (
	 "Waiting for setup menu command (?=help, v=default setup, q=quit):\n")
	}

	# Interactive setup is complete.
	call printf (
	    "Interactive setup is complete. Type w to save parameters.\n")

	# Restore old viewport and window coords.
	call gsview (gd, u1, u2, v1, v2)
	call gswind (gd, x1, x2, y1, y2)

	call gdeactivate (gd, 0)
	call sfree (sp)

	# Fit the object.
	ier = apsffit (ap, im, xcenter, ycenter)
	call ap_qppsf (ap, ier)
end
