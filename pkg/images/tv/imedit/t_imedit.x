include	<error.h>
include	<imhdr.h>
include	"epix.h"
 
define	HELP		"imedit_help$"
define	PROMPT		"imedit options"
 
# T_IMEDIT -- Edit image pixels.
# This task provides selection of pixels to be edit via cursor or file
# input.  The regions to be edited may be defined as a rectangle or a
# center and radius for a circular or square aperture.  The replacement
# options include constant substitution, background substitution, column
# or line interpolation, and moving one region to another.  In addition
# this task can be used to select and display regions in surface perspective
# and to print statistics.  The image display interface temporarily
# used simple calls to a user specified display task (such as TV.DISPLAY).
# The editing is done in a temporary image buffer.  The commands which
# alter the input image may be logged if a log file is given.
 
procedure t_imedit ()
 
int	inlist		# List of input images
int	outlist		# List of output images
 
int	i, key, ap, xa, ya, xb, yb, x1, x2, y1, y2
int	change, changes, newdisplay, newimage
bool	erase
pointer	sp, ep, cmd, temp
pointer	im
 
bool	streq()
pointer	immap(), imgl2r(), impl2r()
int	imtopenp(), imtlen(), imtgetim(), imaccess(), ep_gcur()
errchk	immap, imdelete, ep_imcopy, ep_setpars, imgl2r, impl2r
 
define	newim_	99
 
begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Allocate and initialize imedit descriptor.
	call salloc (ep, EP_LEN, TY_STRUCT)
	call aclri (Memi[ep], EP_LEN)
 
	# Check the input and output image lists have proper format.
	inlist = imtopenp ("input")
	outlist = imtopenp ("output")
	if (imtlen (outlist) > 0 && imtlen (outlist) != imtlen (inlist))
	    call error (1, "Input and output lists are not the same length")
 
	# Set the rest of the task parameters.
	call ep_setpars (ep)
 
	# Repeat on each input image.
	while (imtgetim (inlist, EP_INPUT(ep), EP_SZFNAME) != EOF) {
	    if (imtgetim (outlist, EP_OUTPUT(ep), EP_SZFNAME) == EOF)
		call strcpy (EP_INPUT(ep), EP_OUTPUT(ep), EP_SZFNAME)
	    else if (imaccess (EP_OUTPUT(ep), READ_ONLY) == YES) {
		call eprintf ("%s: Output image %s exists\n")
		    call pargstr (EP_INPUT(ep))
		    call pargstr (EP_OUTPUT(ep))
		next
	    }
 
	    # The editing takes place in a temporary editing image buffer.
newim_	    call strcpy (EP_OUTPUT(ep), EP_WORK(ep), EP_SZFNAME)
	    call xt_mkimtemp (EP_OUTPUT(ep), EP_WORK(ep), EP_OUTPUT(ep),
		EP_SZFNAME)
	    iferr (call ep_imcopy (EP_INPUT(ep), EP_WORK(ep))) {
		call erract (EA_WARN)
		next
	    }

	    EP_IM(ep) = immap (EP_WORK(ep), READ_WRITE, 0)
	    EP_INDATA(ep) = NULL
	    EP_OUTDATA(ep) = NULL
 
	    if (EP_LOGFD(ep) != NULL) {
		call fprintf (EP_LOGFD(ep), "# Input image %s\n")
		    call pargstr (EP_INPUT(ep))
	    }
 
	    if (EP_DISPLAY(ep) == YES) {
		key = '0'
		call ep_zoom (ep, xa, ya, xb, yb, key, erase)
	        call ep_command (ep, EP_WORK(ep), erase)
	    }
 
 
	    # Enter the cursor loop.  The apertures and commands are
	    # returned by the EP_GCUR procedure.
 
	    newimage = NO
	    changes = 0
	    while (ep_gcur (ep,ap,xa,ya,xb,yb,key,Memc[cmd],SZ_LINE) != EOF) {
	        newdisplay = NO
	        change = NO
 
		iferr {
		    switch (key) {
		    case '?':	# Print help
			call pagefile (HELP, PROMPT)
		    case ':':	# Process colon commands
			call ep_colon (ep, Memc[cmd], newimage)
			if (newimage == YES)
			    break
		    case 'a', 'b':	# Background replacement
			call ep_background (ep, ap, xa, ya, xb, yb)
			if (EP_OUTDATA(ep) != NULL) {
			    change = YES
			    changes = changes + 1
			}
		    case 'c':	# Column interpolation
			call ep_col (ep, ap, xa, ya, xb, yb)
			if (EP_OUTDATA(ep) != NULL) {
			    change = YES
			    changes = changes + 1
			}
		    case 'd', 'e', 'v':	# Constant value
			call ep_constant (ep, ap, xa, ya, xb, yb)
			if (EP_OUTDATA(ep) != NULL) {
			    change = YES
			    changes = changes + 1
			}
		    case 'f':	# Diagonal aperture
			if (ap == APCDIAG)
			    call ep_col (ep, ap, xa, ya, xb, yb)
			else
			    call ep_line (ep, ap, xa, ya, xb, yb)
			if (EP_OUTDATA(ep) != NULL) {
			    change = YES
			    changes = changes + 1
			}
		    case '=', '<', '>':	# Replace
			if (IM_PIXTYPE(EP_IM(ep)) == TY_INT)
			    call ep_replacei (ep, xa, ya, key)
			else
			    call ep_replacer (ep, xa, ya, key)
			if (EP_OUTDATA(ep) != NULL) {
			    change = YES
			    changes = changes + 1
			}
		    case 'i':	# Initialize
			call imunmap (EP_IM(ep))
			goto newim_
		    case 'j', 'k':	# Replace with input
			call ep_input (ep, ap, xa, ya, xb, yb)
			if (EP_OUTDATA(ep) != NULL) {
			    change = YES
			    changes = changes + 1
			}
		    case 'l':	# Line interpolation
			call ep_line (ep, ap, xa, ya, xb, yb)
			if (EP_OUTDATA(ep) != NULL) {
			    change = YES
			    changes = changes + 1
			}
		    case 'm', 'n':	# Move
			    i = ep_gcur (ep, ap, x1, y1, x2, y2, key,
				Memc[cmd],SZ_LINE)
			call ep_move (ep, ap, xa, ya, xb, yb, x1, y1, x2, y2,
			    key)
			if (EP_OUTDATA(ep) != NULL) {
			    change = YES
			    changes = changes + 1
			}
		    case 'g':	# Surface graph
			call ep_dosurface (ep)
		    case ' ':	# Statistics
			call ep_statistics (ep, ap, xa, ya, xb, yb, NO)
		    case 'p':
			call ep_statistics (ep, ap, xa, ya, xb, yb, YES)
		    case 't':
			EP_SEARCH(ep) = -EP_SEARCH(ep)
			call ep_colon (ep, "search", newimage)
		    case '+':
			EP_RADIUS(ep) = EP_RADIUS(ep) + 1.
			call ep_colon (ep, "radius", newimage)
		    case '-':
			EP_RADIUS(ep) = max (0., EP_RADIUS(ep) - 1.)
			call ep_colon (ep, "radius", newimage)
		    case 's':	# Surface plot
			i = max (5.,
			    abs (EP_SEARCH(ep))+EP_BUFFER(ep)+EP_WIDTH(ep)+1)
			x1 = min (xa, xb) - i
			x2 = max (xa, xb) + i
			y1 = min (ya, yb) - i
			y2 = max (ya, yb) + i
			call ep_gindata (ep, x1, x2, y1, y2)
			EP_OUTDATA(ep) = NULL
			call ep_dosurface (ep)
		    case 'q':	# Quit and save
		    case 'u':	# Undo
			if (EP_OUTDATA(ep) != NULL && EP_INDATA(ep) != NULL) {
			    call malloc (temp, EP_NPTS(ep), TY_REAL)
			    call amovr (Memr[EP_OUTDATA(ep)], Memr[temp],
				EP_NPTS(ep))
			    call amovr (Memr[EP_INDATA(ep)],
				Memr[EP_OUTDATA(ep)], EP_NPTS(ep))
			    call amovr (Memr[temp], Memr[EP_INDATA(ep)],
				EP_NPTS(ep))
			    call mfree (temp, TY_REAL)
			    change = YES
			} else
			    call eprintf ("Can't undo last change\007\n")
		    case 'r', 'E', 'P', 'R', 'Z', '0', '1', '2', '3', '4', '5',
			'6', '7', '8', '9':
			if (EP_DISPLAY(ep) == YES) {
			    call ep_zoom (ep, xa, ya, xb, yb, key, erase)
			    newdisplay = YES
			}
		    case 'Q':	# Quit and no save
			changes = 0
		    case 'I':	# Immediate interrupt
			    call imdelete (EP_WORK(ep))
			call fatal (1, "Interrupt")
		    default:
			call printf ("\007")
		    }
		} then
		    call erract (EA_WARN)

		if (key == 'q' || key == 'Q')
		    break
 
		if (change == YES && EP_AUTOSURFACE(ep) == YES)
		    call ep_dosurface (ep)
 
		if (change == YES && EP_AUTODISPLAY(ep) == YES)
		    newdisplay = YES
		if (newdisplay == YES && EP_DISPLAY(ep) == YES)
		    call ep_display (ep, EP_WORK(ep), erase)
 
		# Log certain commands.  Note that this is done after
		# centering.
		if (EP_LOGFD(ep) != NULL) {
		    switch (key) {
		    case 'a', 'c', 'd', 'f', 'j', 'l', 'v':
			call fprintf (EP_LOGFD(ep), "%d %d 1 %c\n")
			    call pargi (xa)
			    call pargi (ya)
			    call pargi (key)
			call fprintf (EP_LOGFD(ep), "%d %d 1 %c\n")
			    call pargi (xb)
			    call pargi (yb)
			    call pargi (key)
		    case 'b', 'e', 'k':
			call fprintf (EP_LOGFD(ep), "%d %d 1 %c\n")
			    call pargi ((xa+xb)/2)
			    call pargi ((ya+yb)/2)
			    call pargi (key)
		    case 'u':
			if (EP_OUTDATA(ep) != NULL) {
			    call fprintf (EP_LOGFD(ep), "%c\n")
				call pargi (key)
			}
		    case 'm', 'n':
			call fprintf (EP_LOGFD(ep), "%d %d 1 %c\n")
			    call pargi ((xa+xb)/2)
			    call pargi ((ya+yb)/2)
			    call pargi (key)
			call fprintf (EP_LOGFD(ep), "%d %d 1 %c\n")
			    call pargi ((x1+x2)/2)
			    call pargi ((y1+y2)/2)
			    call pargi (key)
		    }
		}
	    }
 
	    call imunmap (EP_IM(ep))
	    # Only create the output if the input has been changed.
	    if (changes > 0) {
		if (streq (EP_INPUT(ep), EP_OUTPUT(ep))) {
		    EP_IM(ep) = immap (EP_OUTPUT(ep), READ_WRITE, 0)
		    im = immap (EP_WORK(ep), READ_ONLY, 0)
		    do i = 1, IM_LEN(EP_IM(ep),2)
		        call amovr (Memr[imgl2r(im,i)],
			    Memr[impl2r(EP_IM(ep),i)], IM_LEN(im,1))
		    call imunmap (im)
		    call imunmap (EP_IM(ep))
		    call imdelete (EP_WORK(ep))
		} else {
		    if (imaccess (EP_OUTPUT(ep), READ_ONLY) == YES)
			call imdelete (EP_OUTPUT(ep))
		    call imrename (EP_WORK(ep), EP_OUTPUT(ep))
		}
	    } else
	        call imdelete (EP_WORK(ep))
 
	    # Check for a new image based on a colon command.  This case
	    # always uses the input image name as output.
	    if (newimage == YES) {
		call strcpy (EP_INPUT(ep), EP_OUTPUT(ep), EP_SZFNAME)
		goto newim_
	    }
	}
 
	# Finish up.
	if (EP_LOGFD(ep) != NULL)
	    call close (EP_LOGFD(ep))
	call imtclose (inlist)
	call imtclose (outlist)
	call sfree (sp)
end
