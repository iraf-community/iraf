include <ctype.h>
include <gset.h>
include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/fitsky.h"
include "../lib/polyphot.h"

define	HELPFILE	"apphot$polyphot/polyphot.key"

# AP_YPHOT -- Procedure to compute flux inside polygonal apertures.

int procedure ap_yphot (py, im, cl, pl, gd, id, out, stid, interactive)

pointer py		# pointer to apphot structure
pointer	im		# pointer to IRAF image
int	cl		# coordinate file list
int	pl		# starlist file descriptor
pointer	gd		# pointer to graphcis stream
pointer	id		# pointer to image display stream
int	out		# output file descriptor
int	stid		# output file sequence number
int	interactive	# interactive mode

int	nvertices, cier, sier, pier, wcs, key, ip, colonkey
int	prev_num, req_num, ptid, ltid, delim, newlist
int	newcenterbuf, newcenter, newskybuf, newsky, newmagbuf, newmag
pointer	sp, x, y, cmd
real	wx, wy

bool	fp_equalr()
int	ap_ymkpoly(), ap_yfit(), clgcur(), apfitsky(), aprefitsky()
int	apstati(), ctoi(), ap_ynextobj(), ap_ycenter(), ap_yrecenter()
int	apgqverify(), apgtverify(), ap_yradsetup(), apnew()
real	apstatr()
data	delim /';'/

begin
	# Allocate temporary space.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (x, MAX_NVERTICES + 1, TY_REAL)
	call salloc (y, MAX_NVERTICES + 1, TY_REAL)

	# Initialize the cursor read.
	key = ' '
	Memc[cmd] = EOS

	# Initialize the fit.
	newcenterbuf = YES
	newcenter = YES
	newskybuf = YES
	newsky = YES
	newmagbuf = YES
	newmag = YES

	cier = AP_OK
	sier = AP_OK
	pier = AP_OK

	# Initialize the sequencing.
	newlist = NO
	ptid = 0
	ltid = 0

	# Loop over the polygon file.
	nvertices = 0
	while (clgcur ("commands", wx, wy, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {

	    # Store the current coordinates
	    call apsetr (py, CWX, wx)
	    call apsetr (py, CWY, wy)

	    # Has the polygon moved?
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
		if (id != NULL)
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

	    # Define a polygon interactively.
	    case 'g':
		if (interactive == YES) {
		    nvertices = ap_ymkpoly (py, id, Memr[x], Memr[y],
		        MAX_NVERTICES)
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
		    call printf ("No polygon list\7\n")

	    # Show/set polyphot parameters.
	    case ':':
		for (ip = 1; IS_WHITE(Memc[cmd+ip-1]); ip = ip + 1)
		    ;
		colonkey = Memc[cmd+ip-1]
		switch (colonkey) {
		case 'm':

		    # Show/set polyphot parameters
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
		        call ap_ycolon (py, im, pl, cl, out, stid, ptid, ltid,
			    Memc[cmd], newcenterbuf, newcenter, newskybuf,
			    newsky, newmagbuf, newmag)

	    	    # Move to the next object in the list.
		    } else if (pl != NULL) {
			ip = ip + 1
			prev_num = ltid
			if (ctoi (Memc[cmd], ip, req_num) <= 0)
			    req_num = ltid + 1
		        nvertices = ap_ynextobj (py, id, pl, cl, delim, Memr[x],
			    Memr[y], MAX_NVERTICES, prev_num, req_num,
			    ltid, ptid)
		        if (nvertices == EOF) {
			    call printf (
			    "End of polygon list, use r key to rewind\7\n")
			} else
		            newlist = YES

		    } else if (interactive == YES)
		        call printf ("No polygon list\7\n")

		case 'n':

		    # Show/set polyphot parameters
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
		        call ap_ycolon (py, im, pl, cl, out, stid, ptid, ltid,
			    Memc[cmd], newcenterbuf, newcenter, newskybuf,
			    newsky, newmagbuf, newmag)

	    	    # Measure the next object in the list.
		    } else if (pl != NULL) {

			# Get the next polygon.
			ip = ip + 1
			prev_num = ltid
			if (ctoi (Memc[cmd], ip, req_num) <= 0)
			    req_num = ltid + 1
		        nvertices = ap_ynextobj (py, id, pl, cl, delim, Memr[x],
			    Memr[y], MAX_NVERTICES, prev_num, req_num,
			    ltid, ptid)
		        if (nvertices == EOF) {
			    call printf (
			    "End of polygon list, use r key to rewind\7\n")
			} else {

		            newlist = YES

		            # Fit the center, sky and measure polygon.
		    	    cier = ap_ycenter (py, im, apstatr (py, PYCX),
			    	apstatr (py, PYCY), Memr[x], Memr[y],
				nvertices + 1)
		            sier = apfitsky (py, im, apstatr (py, PYCX),
			        apstatr (py, PYCY), NULL, gd)
		            pier = ap_yfit (py, im, Memr[x], Memr[y],
			        nvertices + 1, apstatr (py, SKY_MODE),
				apstatr (py, SKY_SIGMA), apstati (py, NSKY))

		            # Draw the polygon.
			    if (interactive == YES)
		                call ap_qyprint (py, cier, sier, pier)
			    call appymark (py, id, Memr[x], Memr[y],
			        nvertices + 1, apstati (py, MKCENTER),
				apstati (py, MKSKY), apstati (py, MKPOLYGON))

		            # Write the results to output.
		            if (stid == 1)
			        call ap_param (py, out, "polyphot")
		            if (newlist == YES)
		                call ap_yprint (py, out, Memr[x], Memr[y],
				    nvertices, stid, ltid, ptid, cier, sier,
				    pier)
		            else
			        call ap_yprint (py, out, Memr[x], Memr[y],
				    nvertices, stid, 0, ptid, cier, sier, pier)

		            # Set up for the next object.
		            stid = stid + 1
			    newcenterbuf = NO
			    newcenter = NO
		            newskybuf = NO
			    newsky = NO
			    newmagbuf = NO
			    newmag = NO
			}

		    } else if (interactive == YES)
		        call printf ("No coordinate list\7\n")

		# Show/set polyphot parameters.
		default:
		    call ap_ycolon (py, im, pl, cl, out, stid, ptid, ltid,
		        Memc[cmd], newcenterbuf, newcenter, newskybuf, newsky,
			newmagbuf, newmag)
		}

	    # Process the remainder of the list
	    case 'l':
		if (pl != NULL) {
		    call ap_ybphot (py, im, cl, pl, out, stid, ltid, ptid, id,
		        YES)
		} else if (interactive == YES)
		    call printf ("No polygon list\7\n")

	    # Save current parameters in the pset file.
	    case 'w':
		call ap_pypars (py)

	    # Center the current polygon around the cursor poition.
	    case 'c':
		if (newcenterbuf == YES)
		    cier = ap_ycenter (py, im, wx, wy, Memr[x], Memr[y],
		        nvertices + 1)
		else if (newcenter == YES)
		    cier = ap_yrecenter (py, Memr[x], Memr[y], nvertices + 1,
			cier)
		if (interactive == YES)
		    call ap_qcenter (py, cier)
		call apmark (py, id, apstati (py, MKCENTER), NO, NO)
		newcenterbuf = NO
		newcenter = NO

	    # Fit the sky around the cursor position.
	    case 't':
		if (newskybuf == YES || ! fp_equalr (wx,
		    apstatr (py, SXCUR)) || ! fp_equalr (wy, apstatr (py,
		    SYCUR)))
		    sier = apfitsky (py, im, wx, wy, NULL, gd)
	        else if (newsky == YES)
		    sier = aprefitsky (py, gd)
		if (interactive == YES)
		    call ap_qspsky (py, sier)
		call appymark (py, id, Memr[x], Memr[y], nvertices + 1, NO,
		    apstati (py, MKSKY), NO)
		newskybuf = NO
		newsky = NO

	     # Fit sky in an annulus around the current polygon.
	     case 's':
	        if (newskybuf == YES || ! fp_equalr (apstatr (py, PYCX),
		    apstatr (py, SXCUR)) || ! fp_equalr (apstatr (py, PYCY),
		    apstatr (py, SYCUR)))
		    sier = apfitsky (py, im, apstatr (py, PYCX), apstatr (py,
		        PYCY), NULL, gd)
		else if (newsky == YES)
		    sier = aprefitsky (py, gd)
		if (interactive == YES)
		    call ap_qspsky (py, sier)
		call appymark (py, id, Memr[x], Memr[y], nvertices + 1, NO,
		    apstati (py, MKSKY), NO)
		newskybuf = NO
		newsky = NO

	    # Compute the magnitudes of current polygon using the current sky.
	    case 'p':
		pier = ap_yfit (py, im, Memr[x], Memr[y], nvertices + 1,
		    apstatr (py, SKY_MODE), apstatr (py, SKY_SIGMA),
		    apstati (py, NSKY))
		if (interactive == YES)
		    call ap_qyprint (py, cier, sier, pier)
		call appymark (py, id, Memr[x], Memr[y], nvertices + 1, NO, NO,
		    apstati (py, MKPOLYGON))
		newmagbuf = NO
		newmag = NO

	    # Center, compute the sky and the magnitudes and save the results.
	    case 'h', 'm', 'f', ' ':

		# Compute the centers
		if (key == 'f' || key == ' ') {
		    if (newcenterbuf == YES)
		        cier = ap_ycenter (py, im, wx, wy, Memr[x], Memr[y],
		            nvertices + 1)
		    else if (newcenter == YES)
		        cier = ap_yrecenter (py, Memr[x], Memr[y],
			    nvertices + 1, cier)
		} else {
		    if (newcenterbuf == YES)
		        cier = ap_ycenter (py, im, apstatr (py, PYCX),
			    apstatr (py, PYCY), Memr[x], Memr[y], nvertices + 1)
		    else if (newcenter == YES)
		        cier = ap_yrecenter (py, Memr[x], Memr[y],
			    nvertices + 1, cier)
		}

		# Compute the sky values and the magnitudes.
	        if (newskybuf == YES || ! fp_equalr (apstatr (py,
		    PYCX), apstatr (py, SXCUR)) || ! fp_equalr (apstatr (py,
		    PYCY), apstatr (py, SYCUR))) sier = apfitsky (py, im,
		    apstatr (py, PYCX), apstatr (py, PYCY), NULL, gd)
		else if (newsky == YES)
		    sier = aprefitsky (py, gd)
		pier = ap_yfit (py, im, Memr[x], Memr[y], nvertices + 1,
		    apstatr (py, SKY_MODE), apstatr (py, SKY_SIGMA),
		    apstati (py, NSKY))

		if (interactive == YES)
		    call ap_qyprint (py, cier, sier, pier)
		call appymark (py, id, Memr[x], Memr[y], nvertices + 1,
		    apstati (py, MKCENTER), apstati (py, MKSKY), apstati (py,
		    MKPOLYGON))

		newcenterbuf = NO; newcenter = NO
		newskybuf = NO; newsky = NO
		newmagbuf = NO; newmag = NO

		# Write the results to an output file.
		if (key == ' ') {
		    if (stid == 1)
		        call ap_param (py, out, "polyphot")
		    if (newlist == YES)
		        call ap_yprint (py, out, Memr[x], Memr[y], nvertices,
			    stid, ltid, ptid, cier, sier, pier)
		    else
		        call ap_yprint (py, out, Memr[x], Memr[y], nvertices,
			    stid, 0, ptid, cier, sier, pier)
		    stid = stid + 1
		}

	    default:
		call printf ("Unknown or ambigous keystroke command\7\n")
	    }

	    # Reset the keystroke and command defaults.
	    key = ' '
	    Memc[cmd] = EOS
	    call apsetr (py, WX, apstatr (py, CWX))
	    call apsetr (py, WY, apstatr (py, CWY))
	}

end
