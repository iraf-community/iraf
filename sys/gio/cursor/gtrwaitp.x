# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ttyset.h>
include	<error.h>
include	<fset.h>
include	<gio.h>
include	"gtr.h"
include	"grc.h"

# GTR_WAITPAGE -- Print the "hit return to continue" message on the terminal
# screen and wait for the user to respond before returning to graphics mode.
# Redrawing of the graphics frame is optional.

procedure gtr_waitpage (fd, stream)

int	fd			# output file
int	stream			# graphics stream

int	key, i
pointer	tty, tr
int	getci(), ttystati()
pointer	ttyodes(), gtr_init()
errchk	gtr_init, ttyodes

begin
	tr = gtr_init (stream)
	tty = ttyodes ("terminal")

	repeat {
	    # Print prompt in standout mode.
	    call ttyclearln (fd, tty)
	    call ttyso (fd, tty, YES)
	    call fprintf (fd,
		"[space=cmhelp,return=quit+redraw,q=quit+noredraw]")
	    call ttyso (fd, tty, NO)
	    call flush (fd)

	    # Wait for user to hit a key.  This is done in text mode via
	    # a raw getc rather than via a cursor read to avoid switching to
	    # graphics mode.  On some terminals with separate text and
	    # graphics planes a switch to graphics mode turns off the text
	    # plane.

	    call fseti (STDIN, F_RAW, YES)
	    if (getci (STDIN, key) == EOF)
		key = '\r'
	    call fseti (STDIN, F_RAW, NO)

	    # Take the action commanded by the user.  At present the morehelp
	    # option merely prints cursor mode help; this is appropriate
	    # because the first waitpage call occurs after printing user help
	    # in response to ? (or after a :.show).

	    switch (key) {
	    case 'q':
		# Quit, do not clear graphics and redraw.
		if (TR_PAGE(tr) == NO) {
		    # If screen paging is disabled (text drawn underneath
		    # transparent graphics overlay), clear the text frame
		    # only, using the clear line function.

		    do i = 1, ttystati (tty, TTY_NLINES) {
			call ttygoto (fd, tty, 1, i)
			call ttyclearln (fd, tty)
		    }
		} else
		    call ttyclearln (fd, tty)

		call flush (fd)
		call gki_reactivatews (stream, 0)
		break

	    case '\r', '\n':
		# Quit, clear graphics and redraw.
		call ttyclearln (fd, tty)
		call flush (fd)
		call gki_reactivatews (stream, 0)
		call gtr_redraw (stream)
		break

	    case ' ':
		# Print cursor mode help.
		iferr (call pagefile (KEYSFILE, "cursor mode help"))
		    call erract (EA_WARN)

	    default:
		# Illegal keystroke.
		call printf ("\007")
		call flush (STDOUT)
	    }
	}

	call ttycdes (tty)
end
