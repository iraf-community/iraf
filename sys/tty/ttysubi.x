# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<chars.h>
include	<ctype.h>
include	"tty.h"

.help ttysubi
.nf ___________________________________________________________________________
TTYSUBI -- Argument substitution on a control string.

    Process a capability string containing arguments.  Examples of such
capability strings are cursor motion to [x,y], and set scrolling region to
[line1,line2].  Note that arguments in the termcap database are zero-indexed
by default, while the TTYSUBI arguments are one-indexed.  The control string
given as input has already been processed to reduce all escape sequences to
single characters.

Various output formats are supported (some of these are completely off the
wall, very special case, but that's how termcap does it):

	%d	print decimal integer, zero origin.
	%2	like %2d.
	%3	like %3d.
	%.	put binary value of x,y arg as a character
	%+x	like %., but add value of char X first
	%%	print a single %.

The following format codes affect the arguments, but do not directly cause
any output:

	%>xy	if next arg value > char x add char y.
	%r	reverses order of arguments.
	%i	increments arg values by one, for 1 origin.
	%n	exlusive-or args with 0140B. (DM2500)
	%B	BCD next arg, (16*(x/10))+(mod(x,10).
	%D	Reverse coding (x-2*(mod(x,16))). (Delta Data)

We have generalized the termcap formats somewhat to permit a greater range
of %n formats (%1-%4), as well as %o and %x formats, in case a terminal
comes along which likes octal or hex numbers.

The %. format causes special problems.  If the terminal requires coordinates
in binary in the range zero or one to 40B, we can expect problems trying to
push such chars through the OS driver and any other software (networks, etc.),
since system software likes to map control characters on output.  To get around
this we have defined a set of reserved codes which are not to be generated.
This set is defined in tty.h, and includes newline, tab, etc.  When asked to
output one of these chars, we output a char with a somewhat larger value
and return the delta to our caller, which does whatever is appropriate to
complete the function.
.endhelp ______________________________________________________________________

int procedure ttysubi (ctrlstr, outstr, maxch, coords, ncoords)

char	ctrlstr[ARB]		# control string containing % formats
char	outstr[ARB]		# receives processed string
int	maxch
int	coords[ncoords]		# on input, coords; on output, deltas
int	ncoords

bool	reverse			# reverse deltas on output
int	revstart		# first arg/coord reversed
int	args[MAX_COORDS]	# processed values of arguments
int	argnum			# arg being processed
int	nargs			# number of args (min(MAX_COORDS,ncoords))
char	driver_chars[NDCHARS]
char	ch, format_char
int	i, ip, op, field_width, left, right, temp
int	stridx(), strlen(), xor()
data	driver_chars /DRIVER_CHARS/
errchk	sprintf, pargi

begin
	# Make a local copy of the arguments to make reversal etc. easy.
	# Also switch to zero-indexing internally, since the termcap entry
	# is zero-indexed.

	nargs = min (MAX_COORDS, ncoords)
	do i = 1, nargs {
	    args[i] = coords[i] - 1		# make zero-indexed
	    coords[i] = 0			# init delta
	}
	argnum = 1				# output x first by default
	reverse = false

	op = 1
	for (ip=1;  ctrlstr[ip] != EOS && op <= maxch;  ip=ip+1) {
	    ch = ctrlstr[ip]

	    # If normal char, we do not get past this if statement.
	    if (ch != '%') {
		outstr[op] = ch
		op = op + 1
		next
	    } else {
		ip = ip + 1			# fetch format-type char
		ch = ctrlstr[ip]
	    }

	    # Get here only if processing a %x format specification.
	    switch (ch) {
	    case '%':				# %% --> %
		outstr[op] = ch
		op = op + 1

	    case 'd', 'o', 'x', '1', '2', '3', '4':
		# Output next argument according to the format given.
		if (IS_DIGIT(ch)) {
		    field_width = TO_INTEG(ch)
		    format_char = 'd'
		} else {
		    field_width = 0
		    format_char = ch
		}

		call sprintf (outstr[op], maxch-op+1, "%0*.0*")
		    call pargi (field_width)
		    call pargc (format_char)
		    call pargi (args[argnum])

		argnum = min (nargs, argnum + 1)
		op = op + strlen (outstr[op])

	    case '.', '+':
		# Binary output format.  Coordinate output in binary is a
		# problem because the OS driver may see a tab, newline, or
		# whatever and map it into something else.  If the value of
		# args[argnum] corresponds to a special control character,
		# we increment it until we have an acceptable value, leaving
		# it up to our caller to do the rest.

		if (ch == '+') {
		    ip = ip + 1
		    args[argnum] = args[argnum] + ctrlstr[ip]
		}

		repeat {
		    ch = char (args[argnum])
		    if (stridx (ch, driver_chars) > 0) {
			args[argnum] = args[argnum] + 1
			coords[argnum] = coords[argnum] + 1
		    } else
			break
		}

		outstr[op] = args[argnum]
		op = op + 1
		argnum = min (nargs, argnum + 1)

	    # The remaining cases are used to change the values of the
	    # remaining arguments, and do not cause any output.

	    case '>':					# %>xy
		if (args[argnum] > ctrlstr[ip+1])
		    args[argnum] = args[argnum] + ctrlstr[ip+2]
		ip = ip + 2
	    case 'r':					# swap remaining args
		do left = argnum, (nargs - argnum + 1) / 2 {
		    right = nargs - (left - argnum)
		    temp = args[left]
		    args[left] = args[right]
		    args[right] = temp
		}
		reverse = !reverse
		revstart = argnum
	    case 'i':					# increment by one
		do i = argnum, nargs
		    args[i] = args[i] + 1
	    case 'n':					# exclusive or with 140B
		do i = argnum, nargs
		    args[i] = xor (args[i], 140B)
	    case 'B':					# BCD encode next arg
		temp = args[argnum]
		args[argnum] = 16 * (temp / 10) + mod (temp, 10)
	    case 'D':					# Reverse code next arg
		temp = args[argnum]
		args[argnum] = temp - 2 * mod (temp, 16)
	    }
	}

	# If the input coordinates were reversed, we must reverse the
	# correction deltas, too.

	if (reverse)
	    do left = revstart, (nargs - revstart + 1) / 2 {
		right = nargs - (left - revstart)
		temp = coords[left]
		coords[left] = coords[right]
		coords[right] = temp
	    }

	outstr[op] = EOS
	return (op - 1)
end
