# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<chars.h>
include	"lroff.h"

define	LEFT	1
define	RIGHT	2

# BREAKLINE -- Break the current output line and output with or without
# justification.  The "current output line" is the set of NWORDS words
# pointed to by the WORDS array of word pointers.  Copy the NWORDS words
# into the output buffer; do nothing if the word buffer is empty.  If the
# OUTPUT buffer is not empty, append a newline, causing the buffer to be
# flushed.  Breakline is called by textout() when enough words have been
# collected to fill an output line, or whenever it is desired to flush
# the output buffer.  If both the word buffer and the output buffer are
# empty, breakline is essentially a nop.

procedure breakline (out, justify_flag)

extern	out()
int	justify_flag

int	w, i, end_to_fill_from, next_output_column
int	nholes, nfill, n_per_hole, nextra, hole1, hole2
errchk	outstr, outc
include	"lroff.com"
include	"words.com"

begin
	if (wbuf == NULL || words == NULL)
	    call error (1, "No Lroff word buffer allocated")

	# First we flush the word buffer.
	if (nwords > 0) {
	    # Strip any trailing whitespace from the line.
	    for (wp=wp-2;  Memc[wp] == BLANK && wp > wbuf;  wp=wp-1)
		wcols = wcols - 1
	    wp = wp + 1
	    Memc[wp] = EOS
	    wp = wp + 1

	    # If justification is disabled or if there is only one word on
	    # the line, do not add spaces to right justify.

	    if (justify_flag == NJ || justify == NO || nwords <= 1) {
		for (w=1;  w <= nwords;  w=w+1)
		    call outstr (out, Memc[Memi[words+w-1]])

	    } else {
		# To justify the line, determine the number of extra spaces
		# needed to right justify the last character on the line.
		# Determine the number of holes between words, and how many
		# spaces to add to each.

		nholes = nwords - 1
		nfill = max (0, right_margin - left_margin + 1 - wcols)
		n_per_hole = nfill / nholes
		nextra = nfill - (n_per_hole * nholes)

		# Determine where the extra spaces need to be added.  Add
		# extra spaces from the left and then the right on succesive
		# lines.
		if (end_to_fill_from == LEFT) {
		    hole1 = 1
		    hole2 = nextra
		    end_to_fill_from = RIGHT
		} else {
		    hole1 = nwords - nextra
		    hole2 = nholes
		    end_to_fill_from = LEFT
		}

		# Fill the output line.  Move the word and then add the
		# requisite number of blanks per hole, plus an extra if in
		# the range hole1 to hole2 (at left or right).

		do w = 1, nwords {
		    call outstr (out, Memc[Memi[words+w-1]])
		    do i = 1, n_per_hole
			call outc (out, BLANK)
		    if (w >= hole1 && w <= hole2)
			call outc (out, BLANK)
		}
	    }
	}


	# If there is anything in the output buffer, append a newline, flushing
	# the buffer.

	call getoutcol (next_output_column)
	if (next_output_column > left_margin)
	    call outc (out, '\n')

	wp = wbuf				# clear the word buffer
	nwords = 0
	wcols = 0
end
