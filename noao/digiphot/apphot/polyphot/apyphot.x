include <ctype.h>
include <gset.h>
include <imhdr.h>
include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/fitsky.h"
include "../lib/polyphot.h"

define	HELPFILE	"apphot$polyphot/polyphot.key"

# AP_YPHOT -- Compute the flux inside polygonal apertures.

int procedure ap_yphot (py, im, cl, pl, gd, id, out, stid, interactive, cache)

pointer py		# pointer to apphot structure
pointer	im		# pointer to IRAF image
int	cl		# coordinate file descriptor
int	pl		# starlist file descriptor
pointer	gd		# pointer to graphcis stream
pointer	id		# pointer to image display stream
int	out		# output file descriptor
int	stid		# output file sequence number
int	interactive	# interactive mode
int	cache		# cache the input image pixels

real	wx, wy
pointer	sp, x, y, xout, yout, cmd
int	nvertices, cier, sier, pier, wcs, key, ip, colonkey
int	prev_num, req_num, ptid, ltid, delim, newlist, newimage
int	newcenterbuf, newcenter, newskybuf, newsky, newmagbuf, newmag
int	req_size, old_size, buf_size, memstat

real	apstatr()
int	ap_ymkpoly(), ap_yfit(), clgcur(), apfitsky(), aprefitsky()
int	apstati(), ctoi(), ap_ynextobj(), ap_ycenter(), ap_yrecenter()
int	apgqverify(), apgtverify(), ap_yradsetup(), apnew(), ap_avsky()
int	ap_memstat(), sizeof()
bool	fp_equalr()
data	delim /';'/

define	endswitch_  99

begin
	# Allocate temporary space.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (x, MAX_NVERTICES + 1, TY_REAL)
	call salloc (y, MAX_NVERTICES + 1, TY_REAL)
	call salloc (xout, MAX_NVERTICES + 1, TY_REAL)
	call salloc (yout, MAX_NVERTICES + 1, TY_REAL)

	# Initialize the cursor read.
	key = ' '
	Memc[cmd] = EOS

	# Initialize the fit.
	newimage = NO
	newcenterbuf = YES; newcenter = YES
	newskybuf = YES; newsky = YES
	newmagbuf = YES; newmag = YES
	cier = AP_OK; sier = AP_OK; pier = AP_OK

	# Initialize the sequencing.
	newlist = NO
	ptid = 0
	ltid = 0

	# Loop over the polygon file.
	nvertices = 0
	while (clgcur ("icommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {

	    # Store the current coordinates.
	    call ap_vtol (im, wx, wy, wx, wy, 1)
	    call apsetr (py, CWX, wx)
	    call apsetr (py, CWY, wy)

	    # Has the polygon moved ?
	    if (apnew (py, wx, wy, apstatr (py, PYCX), apstatr (py, PYCY),
	        newlist) == YES) {
		newcenterbuf = YES; newcenter = YES
		newskybuf = YES; newsky = YES
		newmagbuf = YES; newmag = YES
	    }

	    # Loop over the colon commands.
	    switch (key) {

	    # Quit.
	    case 'q':
		if (interactive == YES) {
		    if (apgqverify ("polyphot", py, key) == YES) {
			call sfree (sp)
		        return (apgtverify (key))
		    }
		} else {
		    call sfree (sp)
		    return (NO)
		}

	    # Print the help page.
	    case '?':
		if ((id != NULL) && (id == gd))
		    call gpagefile (id, HELPFILE, "")
		else if (interactive == YES)
		    call pagefile (HELPFILE, "[space=morehelp,q=quit,?=help]")

	    # Plot a centered stellar radial profile.
	    case 'd':
		if (interactive == YES)
		    call ap_qrad (py, im, wx, wy, gd)

	    # Setup the parameters using a radial profile plot
	    case 'i':
		if (interactive == YES) {
		    nvertices = ap_yradsetup (py, im, id, gd, out, stid,
		        Memr[x], Memr[y], MAX_NVERTICES)
		    newlist = NO
		    newcenterbuf = YES; newcenter = YES
		    newskybuf = YES; newsky = YES
		    newmagbuf = YES; newmag = YES
		}

	    # Verify the critical parameters.
	    case 'v':
		call ap_yconfirm (py, out, stid)
		newcenterbuf = YES; newcenter = YES
		newskybuf = YES; newsky = YES
		newmagbuf = YES; newmag = YES

	    # Define a polygon interactively.
	    case 'g':
		if (interactive == YES) {
		    if (id == gd)
		        nvertices = ap_ymkpoly (py, im, id, Memr[x], Memr[y],
		            MAX_NVERTICES, NO)
		    else
		        nvertices = ap_ymkpoly (py, im, id, Memr[x], Memr[y],
		            MAX_NVERTICES, YES)
		    if (id != NULL) {
			if (id == gd)
			    call gflush (id)
			else
			    call gframe (id)
		    }
		    newlist = NO
		    newcenterbuf = YES; newcenter = YES
		    newskybuf = YES; newsky = YES
		    newmagbuf = YES; newmag = YES
		}

	    # Print error messages.
	    case 'e':
		if (interactive == YES)
		    call ap_pyerrors (py, cier, sier, pier)

	    # Rewind the polygon and coordinate lists.
	    case 'r':
		if (pl != NULL) {
		    call seek (pl, BOF)
		    if (cl != NULL)
		        call seek (cl, BOF)
		    ptid = 0
		    ltid = 0
		} else if (interactive == YES)
		    call printf ("No polygon list\n")

	    # Show/set polyphot parameters.
	    case ':':
		for (ip = 1; IS_WHITE(Memc[cmd+ip-1]); ip = ip + 1)
		    ;

		colonkey = Memc[cmd+ip-1]
		switch (colonkey) {
		case 'm', 'n':

		    # Show/set polyphot parameters
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
		        call ap_ycolon (py, im, pl, cl, out, stid, ptid, ltid,
			    Memc[cmd], newimage, newcenterbuf, newcenter,
			    newskybuf, newsky, newmagbuf, newmag)
			goto endswitch_
		    }

	    	    # Measure the next object in the list.
		    if (pl != NULL) {
                        if (interactive == YES)
                            call printf ("No polygon list\n")
                        goto endswitch_
		    }

		    # Get the next polygon.
		    ip = ip + 1
		    prev_num = ltid
		    if (ctoi (Memc[cmd], ip, req_num) <= 0)
		        req_num = ltid + 1
		    nvertices = ap_ynextobj (py, im, id, pl, cl, delim, Memr[x],
			Memr[y], MAX_NVERTICES, prev_num, req_num, ltid, ptid)
		    if (nvertices == EOF) {
			if (interactive == YES)
			    call printf (
				"End of polygon list, use r key to rewind\n")
			goto endswitch_
		    }

		    # Move to the next object.
		    newlist = YES
		    if (colonkey == 'm') {
			newcenterbuf = YES; newcenter = YES
		        newskybuf = YES; newsky = YES
			newmagbuf = YES; newmag = YES
			goto endswitch_
		    }

		    # Fit the center, sky and measure polygon.
		    cier = ap_ycenter (py, im, apstatr (py, PYCX), apstatr (py,
		        PYCY), Memr[x], Memr[y], nvertices + 1)
		    sier = apfitsky (py, im, apstatr (py, PYCX), apstatr (py,
		        PYCY), NULL, gd)
		    pier = ap_yfit (py, im, Memr[x], Memr[y], nvertices + 1,
		        apstatr (py, SKY_MODE), apstatr (py, SKY_SIGMA),
			apstati (py, NSKY))
		    if (interactive == YES)
		        call ap_qyprint (py, cier, sier, pier)

		    # Draw the polygon.
		    if (id != NULL) {
		        call appymark (py, id, Memr[x], Memr[y], nvertices + 1,
		            apstati (py, MKCENTER), apstati (py, MKSKY),
			    apstati (py, MKPOLYGON))
			if (id == gd)
			    call gflush (id)
			else
			    call gframe (id)
		    }

		    # Write the results to output.
		    if (stid == 1)
		        call ap_param (py, out, "polyphot")

            	    switch (apstati(py,WCSOUT)) {
            	    case WCS_WORLD, WCS_PHYSICAL:
                	call ap_ltoo (py, Memr[x], Memr[y], Memr[xout],
			    Memr[yout], nvertices + 1)
            	    case WCS_TV:
                	call ap_ltov (im, Memr[x], Memr[y], Memr[xout],
			    Memr[yout], nvertices + 1)
            	    default:
			call amovr (Memr[x], Memr[xout], nvertices + 1)
			call amovr (Memr[y], Memr[yout], nvertices + 1)
            	    }
		    if (newlist == YES)
		        call ap_yprint (py, out, Memr[xout], Memr[yout],
			    nvertices, stid, ltid, ptid, cier, sier, pier)
		    else
			call ap_yprint (py, out, Memr[xout], Memr[yout],
			    nvertices, stid, 0, ptid, cier, sier, pier)

		     # Set up for the next object.
		     stid = stid + 1
		     newcenterbuf = NO; newcenter = NO
		     newskybuf = NO; newsky = NO
		     newmagbuf = NO; newmag = NO

		# Show/set polyphot parameters.
		default:
		    call ap_ycolon (py, im, pl, cl, out, stid, ptid, ltid,
		        Memc[cmd], newimage, newcenterbuf, newcenter,
			newskybuf, newsky, newmagbuf, newmag)
		}

		# Reestablish the image display viewport and window.
		if (newimage == YES) {
		    if ((id != NULL) && (id != gd))
		        call ap_gswv (id, Memc[cmd], im, 4)
                    req_size = MEMFUDGE * IM_LEN(im,1) * IM_LEN(im,2) *
                        sizeof (IM_PIXTYPE(im))
                    memstat = ap_memstat (cache, req_size, old_size)
                    if (memstat == YES)
                        call ap_pcache (im, INDEFI, buf_size)
		}
		newimage = NO

	    # Get measure next object in the list.
	    case 'm', 'n':
		if (pl == NULL) {
		    if (interactive == YES)
		        call printf ("No polygon list\n")
		    goto endswitch_
		}

		# Get the next polygon.
		prev_num = ltid
		req_num = ltid + 1
		nvertices = ap_ynextobj (py, im, id, pl, cl, delim, Memr[x],
		    Memr[y], MAX_NVERTICES, prev_num, req_num, ltid, ptid)
		if (nvertices == EOF) {
		    if (interactive == YES)
			call printf (
			    "End of polygon list, use r key to rewind\n")
		    goto endswitch_
		}

		# Move to next object.
		newlist = YES
		if (key == 'm') {
		    newcenterbuf = YES; newcenter = YES
		    newskybuf = YES; newsky = YES
		    newmagbuf = YES; newmag = YES
		    goto endswitch_
		}

		# Measure next object.
		cier = ap_ycenter (py, im, apstatr (py, PYCX), apstatr (py,
		    PYCY), Memr[x], Memr[y], nvertices + 1)
		sier = apfitsky (py, im, apstatr (py, PYCX), apstatr (py,
		    PYCY), NULL, gd)
		pier = ap_yfit (py, im, Memr[x], Memr[y], nvertices + 1,
		    apstatr (py, SKY_MODE), apstatr (py, SKY_SIGMA),
		    apstati (py, NSKY))
	        if (interactive == YES)
	            call ap_qyprint (py, cier, sier, pier)

		# Draw the polygon.
		if (id != NULL) {
		    call appymark (py, id, Memr[x], Memr[y], nvertices + 1,
		        apstati (py, MKCENTER), apstati (py, MKSKY),
			apstati (py, MKPOLYGON))
		    if (id == gd)
			call gflush (id)
		    else
			call gframe (id)
		}

		# Write the results to output.
		if (stid == 1)
		    call ap_param (py, out, "polyphot")
                switch (apstati(py,WCSOUT)) {
                case WCS_WORLD, WCS_PHYSICAL:
                    call ap_ltoo (py, Memr[x], Memr[y], Memr[xout], Memr[yout],
			nvertices + 1)
                case WCS_TV:
                    call ap_ltov (im, Memr[x], Memr[y], Memr[xout], Memr[yout],
			nvertices + 1)
                default:
		    call amovr (Memr[x], Memr[xout], nvertices + 1)
		    call amovr (Memr[y], Memr[yout], nvertices + 1)
                }
		if (newlist == YES)
		    call ap_yprint (py, out, Memr[xout], Memr[yout], nvertices,
		        stid, ltid, ptid, cier, sier, pier)
		else
		    call ap_yprint (py, out, Memr[xout], Memr[yout], nvertices,
		        stid, 0, ptid, cier, sier, pier)

		# Set up for the next object.
		stid = stid + 1
		newcenterbuf = NO; newcenter = NO
		newskybuf = NO; newsky = NO
		newmagbuf = NO; newmag = NO

	    # Process the remainder of the list.
	    case 'l':
		if (pl != NULL) {
		    call ap_ybphot (py, im, cl, pl, out, stid, ltid, ptid, id,
		        YES)
		    if (id != NULL) {
			if (id == gd)
			    call gflush (id)
			else
			    call gframe (id)
		    }
		} else if (interactive == YES)
		    call printf ("No polygon list\n")

	    # Save current parameters in the pset file.
	    case 'w':
		call ap_pypars (py)

	    # Center the current polygon around the cursor poition.
	    case 'c':
		if (newcenterbuf == YES)
		    cier = ap_ycenter (py, im, wx, wy, Memr[x], Memr[y],
		        nvertices + 1)
		else if (newcenter == YES)
		    cier = ap_yrecenter (py, im, Memr[x], Memr[y],
		        nvertices + 1, cier)
		if (interactive == YES)
		    call ap_qcenter (py, cier)
		if (id != NULL) {
		    call apmark (py, id, apstati (py, MKCENTER), NO, NO)
		    if (id == gd)
		        call gflush (id)
		    else
			call gframe (id)
		}
		newcenterbuf = NO; newcenter = NO

	    # Fit the sky around the cursor position.
	    case 't':
		if (newskybuf == YES || ! fp_equalr (wx,
		    apstatr (py, SXCUR)) || ! fp_equalr (wy, apstatr (py,
		    SYCUR)))
		    sier = apfitsky (py, im, wx, wy, NULL, gd)
	        else if (newsky == YES)
		    sier = aprefitsky (py, im, gd)
		if (interactive == YES)
		    call ap_qspsky (py, sier)
		if (id != NULL) {
		    call apmark (py, id, NO, apstati (py, MKSKY), NO)
		    if (id == gd)
		        call gflush (id)
		    else
			call gframe (id)
		}
		newskybuf = NO; newsky = NO

 	    # Compute the average of several sky measurements around
            # different cursor postions.
            case 'a':
                sier = ap_avsky (py, im, stid, NULL, id, gd, interactive)
                if (interactive == YES)
                    call ap_qaspsky (py, sier)
                newskybuf = NO; newsky = NO

	     # Fit sky in an annulus around the current polygon.
	     case 's':
	        if (newskybuf == YES || ! fp_equalr (apstatr (py, PYCX),
		    apstatr (py, SXCUR)) || ! fp_equalr (apstatr (py, PYCY),
		    apstatr (py, SYCUR)))
		    sier = apfitsky (py, im, apstatr (py, PYCX), apstatr (py,
		        PYCY), NULL, gd)
		else if (newsky == YES)
		    sier = aprefitsky (py, im, gd)
		if (interactive == YES)
		    call ap_qspsky (py, sier)
		if (id != NULL) {
		    call apmark (py, id, NO, apstati (py, MKSKY), NO)
		    if (id == gd)
		        call gflush (id)
		    else
			call gframe (id)
		}
		newskybuf = NO; newsky = NO

	    # Compute the magnitudes of current polygon using the current sky.
	    case 'p', 'o':
		if (newcenterbuf == YES)
		    cier = ap_ycenter (py, im, wx, wy, Memr[x], Memr[y],
		        nvertices + 1)
		else if (newcenter == YES)
		    cier = ap_yrecenter (py, im, Memr[x], Memr[y],
		        nvertices + 1, cier)
		pier = ap_yfit (py, im, Memr[x], Memr[y], nvertices + 1,
		    apstatr (py, SKY_MODE), apstatr (py, SKY_SIGMA),
		    apstati (py, NSKY))
		if (interactive == YES)
		    call ap_qyprint (py, cier, sier, pier)
		if (id != NULL) {
		    call appymark (py, id, Memr[x], Memr[y], nvertices + 1,
		        NO, NO, apstati (py, MKPOLYGON))
		    if (id == gd)
		        call gflush (id)
		    else
			call gframe (id)
		}
		newcenterbuf = NO; newcenter = NO
		newmagbuf = NO; newmag = NO

		# Write the results.
		if (key == 'o') {
		    if (stid == 1)
		        call ap_param (py, out, "polyphot")
            	    switch (apstati(py,WCSOUT)) {
            	    case WCS_WORLD, WCS_PHYSICAL:
                	call ap_ltoo (py, Memr[x], Memr[y], Memr[xout],
			    Memr[yout], nvertices + 1)
            	    case WCS_TV:
                	call ap_ltov (im, Memr[x], Memr[y], Memr[xout],
			    Memr[yout], nvertices + 1)
            	    default:
		        call amovr (Memr[x], Memr[xout], nvertices + 1)
		        call amovr (Memr[y], Memr[yout], nvertices + 1)
            	    }
		    if (newlist == YES)
		        call ap_yprint (py, out, Memr[xout], Memr[yout],
			    nvertices, stid, ltid, ptid, cier, sier, pier)
		    else
		        call ap_yprint (py, out, Memr[xout], Memr[yout],
			    nvertices, stid, 0, ptid, cier, sier, pier)
		    stid = stid + 1
		}

	    # Center, compute the sky and the magnitudes and save the results.
	    case 'h', 'j', 'f', ' ':

		# Compute the centers.
		if (key == 'f' || key == ' ') {
		    if (newcenterbuf == YES)
		        cier = ap_ycenter (py, im, wx, wy, Memr[x], Memr[y],
		            nvertices + 1)
		    else if (newcenter == YES)
		        cier = ap_yrecenter (py, im, Memr[x], Memr[y],
			    nvertices + 1, cier)
		} else {
		    if (newcenterbuf == YES)
		        cier = ap_ycenter (py, im, apstatr (py, PYCX),
			    apstatr (py, PYCY), Memr[x], Memr[y], nvertices + 1)
		    else if (newcenter == YES)
		        cier = ap_yrecenter (py, im, Memr[x], Memr[y],
			    nvertices + 1, cier)
		}

		# Compute the sky values and the magnitudes.
	        if (newskybuf == YES || ! fp_equalr (apstatr (py,
		    PYCX), apstatr (py, SXCUR)) || ! fp_equalr (apstatr (py,
		    PYCY), apstatr (py, SYCUR))) sier = apfitsky (py, im,
		    apstatr (py, PYCX), apstatr (py, PYCY), NULL, gd)
		else if (newsky == YES)
		    sier = aprefitsky (py, im, gd)
		pier = ap_yfit (py, im, Memr[x], Memr[y], nvertices + 1,
		    apstatr (py, SKY_MODE), apstatr (py, SKY_SIGMA),
		    apstati (py, NSKY))

		if (interactive == YES)
		    call ap_qyprint (py, cier, sier, pier)
		if (id != NULL) {
		    call appymark (py, id, Memr[x], Memr[y], nvertices + 1,
		        apstati (py, MKCENTER), apstati (py, MKSKY),
			apstati (py, MKPOLYGON))
		    if (id == gd)
		        call gflush (id)
		    else
			call gframe (id)
		}

		newcenterbuf = NO; newcenter = NO
		newskybuf = NO; newsky = NO
		newmagbuf = NO; newmag = NO

		# Write the results to an output file.
		if (key == ' ' || key == 'j') {
		    if (stid == 1)
		        call ap_param (py, out, "polyphot")
            	    switch (apstati(py,WCSOUT)) {
            	    case WCS_WORLD, WCS_PHYSICAL:
                	call ap_ltoo (py, Memr[x], Memr[y], Memr[xout],
			    Memr[yout], nvertices + 1)
            	    case WCS_TV:
                	call ap_ltov (im, Memr[x], Memr[y], Memr[xout],
			    Memr[yout], nvertices + 1)
            	    default:
		        call amovr (Memr[x], Memr[xout], nvertices + 1)
		        call amovr (Memr[y], Memr[yout], nvertices + 1)
            	    }
		    if (newlist == YES)
		        call ap_yprint (py, out, Memr[xout], Memr[yout],
			    nvertices, stid, ltid, ptid, cier, sier, pier)
		    else
		        call ap_yprint (py, out, Memr[xout], Memr[yout],
			    nvertices, stid, 0, ptid, cier, sier, pier)
		    stid = stid + 1
		}

	    default:
		call printf ("Unknown or ambigous keystroke command\n")
	    }

endswitch_
	    # Reset the keystroke and command defaults.
	    key = ' '
	    Memc[cmd] = EOS
	    call apsetr (py, WX, apstatr (py, CWX))
	    call apsetr (py, WY, apstatr (py, CWY))
	}

end
