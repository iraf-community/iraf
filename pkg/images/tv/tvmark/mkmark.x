include <fset.h>
include <imhdr.h>
include "tvmark.h"

define	HELPFILE	"iraf$lib/scr/tvmark.key"

# MK_MARK -- Procedure to mark symbols in the frame buffer interactively.

int procedure mk_mark (mk, im, iw, cl, dl, log, fnt, autolog, interactive)

pointer	mk		# pointer to the mark structure
pointer	im		# frame image descriptor
pointer	iw		# pointer to the wcs structure
int	cl		# coordinate file descriptor
int	dl		# pointer to the deletions file
int	log		# output log file descriptor
int	fnt		# font file descriptor
int	autolog		# automatic logging enabled
int	interactive	# interactive mode

int	ncmd, ncols, nlines, nc, nr
int	wcs, bkey, skey, vkey, ekey, fkey, okey, key
int	id, ltid, ndelete, req_num, lreq_num, prev_num, newlist
pointer	sim, sp, scratchim, cmd, str, keepcmd, label
real	cwx, cwy, wx, wy, owx, owy, fx, fy, ofx, ofy
real	xlist, ylist, oxlist, oylist, rmax

int	imd_gcur(), mk_stati(), strdic(), mk_gscur(), nscan(), mk_new()
int	mk_find(), fstati()
real	mk_statr()

begin
	# Allocate working memory.
	call smark (sp)
	call salloc (scratchim, SZ_FNAME, TY_CHAR)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (keepcmd, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (label, SZ_LINE, TY_CHAR)

	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)

	sim = NULL

	# Reinitialize.
	ekey = ' '
	fkey = ' '
	okey = ' '
	skey = ' '
	vkey = ' '
	bkey = ' '
	ltid = 0
	ndelete = 0
	newlist = NO
	owx = INDEFR
	owy = INDEFR
	Memc[cmd] = EOS
	Memc[keepcmd] = EOS

	while (imd_gcur ("commands", wx,wy,wcs,key,Memc[cmd],SZ_LINE) != EOF) {

	    # Save current cursor coordinates.
	    cwx = wx
	    cwy = wy

	    # Check for new object.
	    if (mk_new (wx, wy, owx, owy, xlist, ylist, newlist) == YES)
		;

	    # Transform to frame buffer coordinates.
	    call iw_im2fb (iw, wx, wy, fx, fy)

	    switch (key) {

	    # Print the help page.
	    case '?':
		if (interactive == YES)
		    call pagefile (HELPFILE, "Type ? for help, q to quit")

	    # Quit the task.
	    case 'q':
		break

	    # Keep the previous cursor command.
	    case 'k':
		if (log != NULL)
		    if (autolog == YES)
			call printf ("Automatic logging is already enabled.\n")
		    else
		        call mk_logcmd (log, Memc[keepcmd])
		else {
		    if (interactive == YES)
		        call printf ("The log file is undefined.\n")
		}

	    # Rewind the coordinate list.
	    case 'o':
	        if (cl != NULL) {
		    call seek (cl, BOF)
		    oxlist = INDEFR
		    oylist = INDEFR
		    ltid = 0
	        } else if (interactive == YES)
		    call printf ("Coordinate list is undefined.\n")

	    # Move to the previous object.
	    case '-':
		if (cl != NULL) {
		    prev_num = ltid
		    req_num = ltid - 1
		    if (req_num < 1) {
			if (interactive == YES)
			    call printf ("Requested object is less than 1.\n")
		    } else if (mk_gscur (cl, NULL, xlist, ylist, Memc[label],
		        prev_num, req_num, ltid) != EOF) {
			if (interactive == YES)
			    call printf ("Moved to object: %d  %g  %g\n")
				call pargi (ltid)
				call pargr (xlist)
				call pargr (ylist)
			newlist = YES
		    } else if (interactive == YES)
			call printf (
			    "End of coordinate list, type o to rewind.\n")
		} else if (interactive == YES) 
		    call printf ("Coordinate file is undefined.\n")

	    # Mark the previous object.
	    case 'p':
		if (cl != NULL) {
		    prev_num = ltid
		    req_num = ltid - 1
		    if (req_num < 1) {
			if (interactive == YES)
			    call printf ("Requested object is less than 1.\n")
		    } else if (mk_gscur (cl, NULL, xlist, ylist, Memc[label],
		        prev_num, req_num, ltid) != EOF) {
			call mk_onemark (mk, im, iw, xlist, ylist, oxlist,
			    oylist, Memc[label], ltid)
			newlist = YES
		    } else if (interactive == YES) {
			call printf (
			    "End of coordinate list, type o to rewind.\n")
		    }
		} else if (interactive == YES) 
		    call printf ("Coordinate file is undefined.\n")

	    # Move to the next object.
	    case 'm':
		if (cl != NULL) {
		    prev_num = ltid
		    req_num = ltid + 1
		    if (mk_gscur (cl, NULL, xlist, ylist, Memc[label],
		        prev_num, req_num, ltid) != EOF) {
			if (interactive == YES)
			    call printf ("Moved to object: %d  %g  %g\n")
				call pargi (ltid)
				call pargr (xlist)
				call pargr (ylist)
			newlist = YES
		    } else if (interactive == YES)
			call printf (
			    "End of coordinate list, type o to rewind.\n")
		} else if (interactive == YES) 
		    call printf ("Coordinate file is undefined.\n")

	    # Mark the next object.
	    case 'n':
		if (cl != NULL) {
		    prev_num = ltid
		    req_num = ltid + 1
		    if (mk_gscur (cl, NULL, xlist, ylist, Memc[label],
		        prev_num, req_num, ltid) != EOF) {
			call mk_onemark (mk, im, iw, xlist, ylist, oxlist,
			    oylist, Memc[label], ltid)
			newlist = YES
		    } else if (interactive == YES)
			call printf (
			    "End of coordinate list, type o to rewind.\n")
		} else if (interactive == YES) 
		    call printf ("Coordinate file is undefined.\n")

	    # Mark the entire list.
	    case 'l':
	        if (cl != NULL) {
		    call seek (cl, BOF)
		    ltid = 0
		    call mk_bmark (mk, im, iw, cl, ltid, fnt)
	        } else if (interactive == YES)
		    call printf ("Coordinate list is undefined.\n")

	    # Append to the coordinate list.
	    case 'a':
		if (cl == NULL) {
		    if (interactive == YES)
		        call printf ("Coordinate file is undefined.\n")
		} else if (fstati (cl, F_MODE) != READ_WRITE) {
		    if (interactive == YES)
			call printf (
			"No write permission on coordinate file.\n")
		} else  {

		    # Move to the end of the list.
		    prev_num = ltid
		    req_num = ltid + 1
		    while (mk_gscur (cl, NULL, xlist, ylist, Memc[label],
		        prev_num, req_num, ltid) != EOF) {
			prev_num = ltid
			req_num = ltid + 1
		    }

		    # Add the object.
		    call fprintf (cl, "%g %g\n")
			call pargr (wx)
			call pargr (wy)
		    call flush (cl)
		    ltid = ltid + 1
		    #call seek (cl, EOF)

		    # Mark the object.
		    call mk_onemark (mk, im, iw, wx, wy, oxlist, oylist, "",
		        ltid)

		} 

	    # Delete an object.
	    case 'd':
		if (cl == NULL) {
		    if (interactive == YES)
		        call printf ("Coordinate file is undefined.\n")
		} else if (fstati (cl, F_MODE) != READ_WRITE) {
		    if (interactive == YES)
			call printf (
			"No write permission on coordinate file.\n")
		} else {

		    # Find the nearest object to the cursor and delete.
		    if (mk_find (cl, wx, wy, xlist, ylist, Memc[label], id,
		        ltid, mk_statr (mk, TOLERANCE)) > 0) {
		        call fprintf (dl, "%d\n")
			    call pargi (id)
		        ndelete = ndelete + 1
		        call mk_onemark (mk, im, iw, xlist, ylist, oxlist,
			    oylist, Memc[label], ltid)
		    } else if (interactive == YES)
			call printf ("Object not in coordinate list.\n")

		}

	    # Draw a dot.
	    case '.':
		call mk_dmark (mk, im, fx, fy) 

	    # Draw a plus sign.
	    case '+':
		call mk_tmark (mk, im, "+", fx, fy, YES)

	    # Draw a cross.
	    case 'x':
		call mk_tmark (mk, im, "x", fx, fy, YES)

	    # Mark and erase a region.
	    case 'e':
		if (sim != NULL) {
		    if ((key == ekey) && (okey == 'e' || okey == 'k')) {
			call mk_imsection (mk, sim, im, nint (ofx), nint (fx),
			    nint (ofy), nint (fy))
			ekey = ' '
		    } else {
		        if (interactive == YES)
		            call printf ("Type e again to define region.\n")
		        ekey = key
		        ofx = fx
		        ofy = fy
		    }
		} else if (interactive == YES)
		    call printf ("Define a scratch image with :save.\n")

	    # Fill region
	    case 'f':
		if ((key == fkey) && (okey == 'f' || okey == 'k')) {
		    call mk_imsection (mk, NULL, im, nint (ofx), nint (fx),
			nint (ofy), nint (fy))
		    fkey = ' '
		} else {
		    if (interactive == YES)
		        call printf ("Type f again to define region.\n")
		    fkey = key
		    ofx = fx
		    ofy = fy
		}

	    # Mark a single circle.
	    case 'v':
		if ((key == vkey) && (okey == 'v' || okey == 'k')) {
		    rmax = sqrt ((fx - ofx) ** 2 + (fy - ofy) ** 2)
		    call mk_ocmark (mk, im, iw, ofx, ofy, rmax)
		    vkey = ' '
		} else {
		    if (interactive == YES)
		        call printf ("Type v again to draw circle.\n")
		    vkey = key
		    ofx = fx
		    ofy = fy
		}

	    # Draw concentric circles.
	    case 'c':
		nc = mk_stati (mk, NCIRCLES)
		if (nc > 0) {
		    call mk_cmark (mk, im, iw, fx, fy)
		} else if (interactive == YES)
		    call printf ("Use :radii to specifiy radii.\n")

	    # Draw concentric rectangles.
	    case 'r':
		nr = mk_stati (mk, NRECTANGLES)
		if (nr > 0) {
		    call mk_rmark (mk, im, iw, fx, fy)
		} else if (interactive == YES)
		    call printf ("Use :lengths to specify box lengths.\n")

	    # Draw a vector segment.
	    case 's':
		if ((skey == key) && (okey == 's' || okey == 'k'))
		    call mk_lmark (mk, im, ofx, ofy, fx, fy)
		if (interactive == YES)
		    call printf ("Type s again to draw line segment.\n")
		ofx = fx
		ofy = fy
		skey = key

	    # Draw a box
	    case 'b':
		if ((key == bkey) && (okey == 'b' || okey == 'k')) {
		    call mk_xmark (mk, im, ofx, ofy, fx, fy)
		    bkey = ' '
		} else {
		    if (interactive == YES)
		        call printf ("Type b again to draw box.\n")
		    bkey = key
		    ofx = fx
		    ofy = fy
		}

	    # Execute the colon command.
	    case ':':
		call sscan (Memc[cmd])
		    call gargwrd (Memc[str], SZ_LINE)
		ncmd = strdic (Memc[str], Memc[str], SZ_LINE, MKCMDS2)

		if (ncmd <= 0)
		    call mk_colon (mk, Memc[cmd], im, iw, sim, log, cl, ltid,
		        dl)

		else if (ncmd == MKCMD2_WTEXT) {
		    call gargstr (Memc[str], SZ_LINE)
		    if (Memc[str] != EOS)
			call mk_tmark (mk, im, Memc[str], fx, fy, NO)

		} else if (ncmd == MKCMD2_MOVE) {
		    if (cl != NULL) {
		        call gargi (req_num)
			prev_num = ltid
			if (nscan () < 2)
			    req_num = ltid + 1
			if (req_num < 1) {
			    if (interactive == YES)
				call printf (
				"Requested object is less than 1.\n")
		        } else if (mk_gscur (cl, NULL, xlist, ylist,
			    Memc[label], prev_num, req_num, ltid) != EOF) {
			    if (interactive == YES)
			        call printf ("Moved to object: %d  %g  %g\n")
				    call pargi (ltid)
				    call pargr (xlist)
				    call pargr (ylist)
			    newlist = YES
		        } else if (interactive == YES) {
			    call printf (
			    	"End of coordinate list, type o to rewind.\n")
			}
		    } else if (interactive == YES)
			call printf ("Coordinate file is undefined.\n")

		} else if (ncmd == MKCMD2_NEXT) {
		    if (cl != NULL) {
			call gargi (req_num)
			call gargi (lreq_num)
			prev_num = ltid
			if (nscan () < 2) {
			    req_num = ltid + 1
			    lreq_num = req_num
			} else if (nscan () < 3)
			    lreq_num = req_num
		        while (mk_gscur (cl, NULL, xlist, ylist, Memc[label],
			    prev_num, req_num, ltid) != EOF) {
			    if (ltid > lreq_num)
				break
			    call mk_onemark (mk, im, iw, xlist, ylist, oxlist,
			        oylist, Memc[label], ltid)
			    newlist = YES
			    prev_num = ltid
			    req_num = ltid + 1
			}
		    } else if (interactive == YES)
			call printf ("Coordinate field is undefined.\n")
		}

	    default:
		call printf ("Unrecognized keystroke command.\7\n")
	    }

	    # Encode and log the last cursor command. Do not encode any
	    # keep commands if autologging is turned off.

	    if (autolog == YES) {
	        call mk_encodecmd (wx, wy, wcs, key, Memc[cmd], Memc[keepcmd])
		if (log == NULL) {
		    if (interactive == YES)
		        call printf ("The logfile is undefined.\n")
		} else
		    call mk_logcmd (log, Memc[keepcmd])
	    } else if (key != 'k')
	        call mk_encodecmd (wx, wy, wcs, key, Memc[cmd], Memc[keepcmd])

	    # Get set up for next cursor command.
	    owx = cwx
	    owy = cwy
	    okey = key
	    Memc[cmd] = EOS
	    if (newlist == YES) {
	        oxlist = xlist
	        oylist = ylist
	    }
	}

	# Delete scratch image.
	if (sim != NULL) {
	    call strcpy (IM_HDRFILE(sim), Memc[scratchim], SZ_FNAME)
	    call imunmap (sim)
	    call imdelete (Memc[scratchim])
	}

	call sfree (sp)

	return (ndelete)
end


# MK_ENCODECMD -- Encode the cursor command.

procedure mk_encodecmd (wx, wy, wcs, key, cmd, keepcmd)

real	wx, wy		# cursor position
int	wcs		# world coordinate system
int	key		# cursor keystroke command
char	cmd[ARB]	# command
char	keepcmd[ARB]	# encode cursor command

begin
	call sprintf (keepcmd, SZ_LINE,  "%g  %g  %d  %c  %s")
	    call pargr (wx)
	    call pargr (wy)
	    call pargi (wcs)
	    call pargi (key)
	    call pargstr (cmd)
end


# MK_LOGCMD -- Log the command.

procedure mk_logcmd (log, cmd)

int	log		# logfile descriptor
char	cmd[ARB]	# command

begin
	call fprintf (log, "%s\n")
	    call pargstr (cmd)
end
