include <ctype.h>
include <gset.h>
include "../lib/apphot.h"
include "../lib/polyphot.h"

define	HELPFILE	"apphot$polyphot/polymark.key"

# AP_MKPYLIST -- Procedure to make polygon and accompanying coordinate list.

int procedure ap_mkpylist (im, py, pl, cl, id, gd, pid, cid)

pointer	im		# pointer to IRAF image
pointer	py		# pointer to the POLYPHOT structure
int	pl		# starlist file descriptor
int	cl		# coordinate file list
pointer	id		# pointer to image display stream
pointer	gd		# pointer to graphics display stream
int	pid		# polygon id sequence number
int	cid		# coordinate list sequence number

int	key, nvertices, wcs, ptid, ltid, prev_num, req_num
int	ip, colonkey, firstpoly, newpoly, delim
pointer	sp, cmd, x, y, xshift, yshift
real	wx, wy, xmean, ymean

int	clgcur(), apgqverify(), apgtverify(), ap_ymkpoly(), ctoi()
int	ap_ynextobj()
real	apstatr()
data	delim /';'/

define	endswitch_  99

begin
	# Allocate temporary space.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (x, MAX_NVERTICES + 1, TY_REAL)
	call salloc (y, MAX_NVERTICES + 1, TY_REAL)
	call salloc (xshift, MAX_NVERTICES + 1, TY_REAL)
	call salloc (yshift, MAX_NVERTICES + 1, TY_REAL)

	# Initialize the cursor read.
	key = ' '
	Memc[cmd] = EOS

	# Initialize the sequencing.
	firstpoly = YES
	newpoly = NO
	ptid = 0
	ltid = 0
	xmean = INDEFR
	ymean = INDEFR

	# Loop over the polygon file.
	nvertices = 0
	while (clgcur ("icommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {

	    # Set the current cursor coordinates.
	    call ap_vtol (im, wx, wy, wx, wy, 1)
	    call apsetr (py, CWX, wx)
	    call apsetr (py, CWY, wy)

	    # Loop over the colon commands.
	    switch (key) {

	    # Quit.
	    case 'q':
		 if (apgqverify ("polymark", NULL, key) == YES) {
		    call sfree (sp)
		    return (apgtverify (key))
		 }

	    # Plot a centered stellar radial profile.
	    case 'd':
		call ap_qrad (py, im, wx, wy, gd)

	    # Print the help page.
	    case '?':
		if ((id != NULL) && (id == gd))
		    call gpagefile (id, HELPFILE, "")
		else 
		    call pagefile (HELPFILE, "[space=morehelp,q=quit,?=help]")

	    # Colon escape commands.
	    case ':':
		for (ip = 1; IS_WHITE(Memc[cmd+ip-1]); ip = ip + 1)
		    ;
		colonkey = Memc[cmd+ip-1]

		switch (colonkey) {
		case 'm':

		    # Decode a polymark colon command.
		    if (Memc[cmd+ip] != EOS && Memc[cmd+ip] != ' ') {
		        call printf (
			    "Unknown or ambigous keystroke command\n")
			goto endswitch_
		    }

		    # The polygon file is undefined.
		    if (pl == NULL) {
			call printf ("The polygon list is undefined\n")
			goto endswitch_
		    }

		    # Read the next polygon.
		    ip = ip + 1
		    prev_num = ltid
		    if (ctoi (Memc[cmd], ip, req_num) <= 0)
			req_num = ltid + 1
		    nvertices = ap_ynextobj (py, im, id, pl, cl, delim,
			Memr[xshift], Memr[yshift], MAX_NVERTICES, prev_num,
			req_num, ltid, ptid)
		    if (nvertices == EOF) {
			call printf (
			    "End of polygon list, use r key to rewind\n")
			goto endswitch_
		    }

		    # The polygon is undefined.
		    if (nvertices < 3) {
			call printf ("The polygon has fewer than 3 vertices\n")
			goto endswitch_
		    }

		   # Mark the polygon.
		   if (id != NULL) {
		        call appymark (py, id, Memr[xshift], Memr[yshift],
		           nvertices + 1, NO, NO, YES)
			if (gd == id)
			    call gflush (id)
			else
		    	    call gframe (id)
		    }

		default:
		    call printf ("Unknown or ambigous keystroke command\n")
		}

	    # Draw the next polygon in the list.
	    case 'm':

		# No polygon file.
		if (pl == NULL) {
		    call printf ("The polygon list is undefined\n")
		    goto endswitch_
		}

		# Read the next polygon.
		prev_num = ltid
		req_num = ltid + 1
		nvertices = ap_ynextobj (py, im, id, pl, cl, delim,
		    Memr[xshift], Memr[yshift], MAX_NVERTICES, prev_num,
		    req_num, ltid, ptid)

		# The polygon is undefined.
		if (nvertices == EOF) {
		    call printf ("End of polygon list, use r key to rewind\n")
		    goto endswitch_
	        }

		# The polygon is ill-defined.
		if (nvertices < 3) {
		    call printf ("The polygon has fewer than 3 vertices\n")
		    goto endswitch_
		}

		# Mark the polygon.
		if (id != NULL) {
		    call appymark (py, id, Memr[xshift], Memr[yshift],
		        nvertices + 1, NO, NO, YES)
		    if (gd == id)
		    	call gflush (id)
		    else
		    	call gframe (id)
		}

	    # Rewind the polygon and coordinate lists.
	    case 'r':
		if (pl != NULL) {
		    call seek (pl, BOF)
		    if (cl != NULL)
		        call seek (cl, BOF)
		    ptid = 0
		    ltid = 0
		} else 
		    call printf ("The polygon list is undefined\n")

	    # Draw the remaining polygons on the display.
	    case 'l':
		if (pl == NULL) {
		    call printf ("The polygon list is undefined\n")
		} else if (id != NULL) {
		    call ap_ydraw (py, im, cl, pl, ltid, ptid, id)
		    if (gd == id)
		        call gflush (id)
		    else
		        call gframe (id)
		}

	    # Define the polygon interactively.
	    case 'g':
		if (gd == id)
		    nvertices = ap_ymkpoly (py, im, id, Memr[x], Memr[y],
		        MAX_NVERTICES, NO)
		else
		    nvertices = ap_ymkpoly (py, im, id, Memr[x], Memr[y],
		        MAX_NVERTICES, YES)
		xmean = apstatr (py, PYXMEAN)
		ymean = apstatr (py, PYYMEAN)
		if (nvertices == EOF) {
		    newpoly = NO
		    call printf ("The polygon is undefined\n")
		} else if (nvertices <= 2) {
		    newpoly = NO
		    call printf (
		        "The polygon has fewer then 3 vertices\n")
		} else { 
		    newpoly = YES
		    if (id != NULL) {
			if (gd == id)
			    call gflush (id)
			else
			    call gframe (id)
		    }
		}

	    # Mark the current polygon on the display.
	    case 'f':
		if (id != NULL) {
		    if (! IS_INDEFR(xmean) && ! IS_INDEFR(ymean)) {
		        call aaddkr (Memr[x], wx - xmean, Memr[xshift],
			    nvertices + 1)
		        call aaddkr (Memr[y], wy - ymean, Memr[yshift],
			    nvertices + 1)
		        call appymark (py, id, Memr[xshift], Memr[yshift],
		            nvertices + 1, NO, NO, YES)
			if (gd == id)
			    call gflush (id)
			else
			    call gframe (id)
		    } else
			call printf ("The polygon is undefined\n")
		}


	    # Mark the current polygon on the display and write to file.
	    case ' ':
		if (! IS_INDEFR(xmean) && ! IS_INDEFR(ymean)) {
		    call ap_ywrite (py, im, cl, pl, Memr[x], Memr[y], nvertices,
		        cid, pid, firstpoly, newpoly)
		    if (id != NULL) {
		        call aaddkr (Memr[x], wx - xmean, Memr[xshift],
			    nvertices + 1)
		        call aaddkr (Memr[y], wy - ymean, Memr[yshift],
			    nvertices + 1)
		        call appymark (py, id, Memr[xshift], Memr[yshift],
		            nvertices + 1, NO, NO, YES)
			if (gd == id)
			    call gflush (id)
			else
			    call gframe (id)
		    }
		} else
		    call printf ("The polygon is undefined\n")

	    default:
		call printf ("Unknown or ambigous keystroke command\n")
	    }

endswitch_
	    # Reset the keystroke and command defaults.
	    call apsetr (py, WX, apstatr (py, CWX))
	    call apsetr (py, WY, apstatr (py, CWY))
	    key = ' '
	    Memc[cmd] = EOS
	}
end
