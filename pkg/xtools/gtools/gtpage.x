# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ttyset.h>

# GT_PAGE -- Page the terminal screen by writing the page sequence to the
# output file.  This procedure should not be used anymore.

procedure gt_page (fd)

int	fd			# output file

pointer	tty
pointer	ttyodes()

begin
	tty = ttyodes ("terminal")
	call ttyclear (fd, tty)
	call flush (fd)
	call ttycdes (tty)
end


# GT_WAITPAGE -- Print the "hit return to continue" message.

procedure gt_waitpage (fd)

int	fd			# output file

int	i
pointer	tty

int	ttystati(), fscan()
pointer	ttyodes()

begin
	tty = ttyodes ("terminal")

	# Move to status line and clear the line.
	# Print prompt in standout mode.

	call ttygoto (fd, tty, 1, ttystati (tty, TTY_NLINES))
	call ttyclearln (fd, tty)
	call ttyso (fd, tty, YES)
	call fprintf (fd, "[hit return to continue]")
	call ttyso (fd, tty, NO)
	call flush (fd)
	i = fscan (STDIN)

	call ttycdes (tty)
end
