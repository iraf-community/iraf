# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<chars.h>
include	"tty.h"

.help ttyputline
.nf ___________________________________________________________________________
TTYPUTLINE -- Put a line to a tty device, processing any special control codes
therein.  We are concerned with formfeeds, tabs, and standout mode.  Formfeeds
are dealt with by sending the ff capability, if defined for the device (FF is
sent if there is no ff entry).  Tabs are expanded if the device does not have
hardware tabs; if tabs are not expanded, the ta capability is sent to the device
for each tab (HT is sent if there is no ta string).

Standout mode is more complex.  We distinguish between four types of devices
for standout mode control (presented in order of desirability):

    (1)	[SOSE] Device has so/se.  Map SO, SE chars in input stream into so/se
	sequences.

    (2)	[BSOS] Device can backspace and overstrike.  Backspace each printable
	char in standout mode and overstrike with the underscore character.

    (3) [CROS] Device cannot backspace, but can overstrike.  Follow the output
	line with a CR and then a line consisting only of spaces and
	underscores.

    (4) [TOUP] No conventional way to generate standout mode for device.
	Map alpha chars to upper case to make them standout.

Long lines are automatically broken at the right margin.
.endhelp ______________________________________________________________________


procedure ttyputline (fd, tty, text, map_cc)

int	fd			# output file
pointer	tty			# TTY descriptor
char	text[ARB]		# line of text to be output
int	map_cc			# enable mapping of SO, SE control chars

char	obuf[SZ_LINE]
int	ip, op, pos, col, maxcols, tabstop, tabchar, ch
errchk	write
define	hardcase_ 91

begin
	maxcols = min (SZ_LINE, T_NCOLS(tty))
	tabchar = T_TABCHAR(tty)
	pos	= 1
	col	= 1
	op	= 1

	# Optimize the special case of a line less than the maximum length
	# which contains no special control characters.  As long as this is
	# handled efficiently, the rest doesn't matter.

	for (pos=1;  text[pos] != EOS;  pos=pos+1) {
	    do ip = pos, ARB {
		ch = text[ip]

		if (ch >= ' ') {
		    # Ordinary printable character; the most common case.
		    obuf[op] = ch
		    op  = op + 1
		    col = col + 1

		    # If col > maxcols then we have completely filled the
		    # output line.  If there is more text to come we must
		    # break the line.  If the next character is newline we
		    # may also break the line and discard the newline.

		    if (col > maxcols && text[ip+1] != EOS) {
			if (T_AM(tty) == NO) {
			    obuf[op] = '\n'
			    op = op + 1
			}
			pos = ip
			if (text[ip+1] == '\n')
			    pos = pos + 1
			break
		    }

		} else if (ch == '\n') {
		    # End of line.
		    obuf[op] = ch
		    op  = op + 1
		    pos = ip
		    break

		} else if (ch == '\t') {
		    # Tab.
		    tabstop = min (maxcols, ((col-1) / 8 + 1) * 8 + 1)
		    if (tabchar != 0) {
			obuf[op] = tabchar
			op  = op + 1
			col = tabstop
		    } else {
			while (col < tabstop) {
			    obuf[op] = ' '
			    op  = op + 1
			    col = col + 1
			}
		    }

		} else if (ch == EOS) {
		    pos = ip - 1
		    break

		} else
		    goto hardcase_
	    }

	    if (op > 1)
		call write (fd, obuf, op - 1)

	    op  = 1
	    col = 1
	}

	return

hardcase_
	# Special processsing is needed.
	call ttygputline (fd, tty, text, map_cc)
end


# TTYGPUTLINE -- This is the original ttypuline.  The code is not very
# efficient, but it handles formfeeds, standout mode, etc. in a generalized
# fashion.

procedure ttygputline (fd, tty, text, map_cc)

int	fd			# output file
pointer	tty			# TTY descriptor
char	text[ARB]		# line of text to be output
int	map_cc			# enable mapping of SO, SE control chars

pointer	sp, ostrike, op
bool	so_seen, so_mode_in_effect
int	ip, so_type, ocol, junk, ch, tabchar
int	ttyctrl()
errchk	tty_break_line, putci, ttyctrl, ttyso

begin
	call smark (sp)
	call salloc (ostrike, SZ_LINE, TY_CHAR)

	so_mode_in_effect = false
	so_type = T_SOTYPE(tty)
	tabchar = T_TABCHAR(tty)
	so_seen = false
	ocol	= 1
	op	= ostrike

	# Process the input line, mapping all known sequences.  Other control
	# chars are passed on without modification.  The input line should be
	# an entire line, or CROS mode will not work correctly.  Lines longer
	# than T_NCOLS are broken at the right margin.

	for (ip=1;  text[ip] != EOS;  ip=ip+1) {
	    ch = text[ip]

	    # Break line if newline seen or at right margin.
	    if (ch == '\n' || ocol > T_NCOLS(tty)) {
		call tty_break_line (fd, tty, ostrike, op, so_type, so_seen)
		so_mode_in_effect = false
		ocol = 1

		# Output a newline if short line or the terminal does not
		# have automargins.

		if (ocol < T_NCOLS(tty) || T_AM(tty) == NO)
		    call putci (fd, '\n')

		# Fall through and output ch if ch was not newline.
		if (ch == '\n')
		    next
	    }

	    # Deal with common printable characters.  If standout mode is
	    # in effect, we must take special actions to make the char
	    # stand out if the terminal does not have the so/se capability.
	    # Note that blanks may be made to standout; if this is not
	    # desired, the high level code must turn standout mode on and off.

	    if (ch >= BLANK) {
		if (so_type != SOSE)
		    switch (so_type) {
		    case BSOS:
			if (so_mode_in_effect) {
			    call putci (fd, '_')
			    if (T_BSOK(tty) == YES)
				call putci (fd, BS)
			    else
				junk = ttyctrl (fd, tty, "bc", 1)
			}
		    case CROS:
			if (so_mode_in_effect)
			    Memc[op] = '_'
			else
			    Memc[op] = BLANK
			op = op + 1
		    case TOUP:
			if (so_mode_in_effect && IS_LOWER(ch))
			    ch = TO_UPPER (ch)
		    }

		call putci (fd, ch)
		ocol = ocol + 1
		next
	    }

	    # We get here only if the character is a control character.

	    if (ch == '\t') {
		# If hardware tab expansion is enabled, use that, otherwise
		# wait and put out blanks in next block of code.

		if (T_HTOK(tty) == YES) {
		    call putci (fd, tabchar)
		    if (so_type == CROS) {
			Memc[op] = '\t'
			op = op + 1
		    }
		}

		# Keep track of virtual output column, also output blanks to
		# expand tab if hardware expansion is disabled.

		repeat {
		    if (T_HTOK(tty) != YES) {
			call putci (fd, BLANK)
			if (so_type == CROS) {
			    Memc[op] = BLANK
			    op = op + 1
			}
		    }
		    ocol = ocol + 1
		} until (mod (ocol+TABSIZE-1, TABSIZE) == 0)

	    } else if (ch == FF) {
		# Formfeed breaks the output line if not at beginning of a
		# line.

		if (ocol > 1) {
		    call tty_break_line (fd, tty, ostrike, op, so_type, so_seen)
		    if (ocol < T_NCOLS(tty) || T_AM(tty) == NO)
			call putci (fd, '\n')
		}
		if (ttyctrl (fd, tty, "ff", T_NLINES(tty)) == ERR)
		    call putci (fd, FF)
		ocol = 1
		so_mode_in_effect = false

	    } else if (ch == SO_ON) {
		# Begin standout mode.
		if (so_type == SOSE)
		    call ttyso (fd, tty, YES)
		so_mode_in_effect = true
		so_seen = true

	    } else if (ch == SO_OFF) {
		# End standout mode.
		if (so_type == SOSE)
		    call ttyso (fd, tty, NO)
		so_mode_in_effect = false

	    } else {
		# Unknown control character.  Do not increment ocol.
		if (map_cc == YES) {
		    call putci (fd, '^')
		    call putci (fd, ch + 'A' - 1)
		} else
		    call putci (fd, ch)
	    }
	}

	# If EOS is seen, but not newline, do not issue a newline, but do
	# ignore contents of overstrike buffer.  Thus, we can be used to output
	# portions of a line on non-CROS terminals.

	call sfree (sp)
end


# TTY_BREAK_LINE -- Break the output line.  If overstrike is selected,
# overstrike output line with the ostrike line.  We assume that OP is
# valid only if so_type is CROS.  Note that OP and SO_SEEN are reset.

procedure tty_break_line (fd, tty, ostrike, op, so_type, so_seen)

int	fd
pointer	tty, ostrike, op
int	so_type
bool	so_seen

int	ch
pointer	ip

begin
	# If carriage return, overstrike is enabled and the line had a standout
	# mode directive, output the overstrike line.

	if (so_type == CROS && so_seen) {
	    call putci (fd, '\r')
	    Memc[op] = EOS

	    # Output overstrike line.
	    for (ip=ostrike;  Memc[ip] != EOS;  ip=ip+1) {
		ch = Memc[ip]
		if (ch == '\t')
		    ch = T_TABCHAR(tty)
		call putci (fd, ch)
	    }
	}

	op = ostrike
	so_seen = false
end
