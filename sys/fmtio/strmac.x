# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

define	MAX_ARGS	9

.help strmac
.nf ___________________________________________________________________________
STRMAC -- Expand a macro (nonrecursively) by string substitution.
The macro string may contain zero or more occurrences of the sequences
"$1" through "$9".  The substitution strings are passed in the string
buffer "argstr", wherein successive strings are delimited by the EOS marker.
A double EOS marks the end of the list.

Macros $1-$9 are replaced by the substitution string.  The sequence $$ is
replaced by a single $.  If any other character follows the $, both the $
and the following character are passed to the output unchanged.  An error
action is taken if there are insufficient arguments or if the output buffer
overflows.  Bugs: null substitution strings don't work.
.endhelp ______________________________________________________________________

int procedure strmac (macro, argstr, outstr, maxch)

char	macro[ARB]		# substitution string
char	argstr[ARB]		# argument strings, if any
char	outstr[maxch]		# output string
int	maxch	

short	offset[MAX_ARGS]
char	ch
int	i, ip, op, arg, nargs, nchars
int	strlen()

begin
	# Determine the offsets of the argument strings.
	ip = 1
	for (nargs=1;  nargs <= MAX_ARGS;  nargs=nargs+1) {
	    nchars = strlen (argstr[ip])
	    if (nchars > 0) {
		offset[nargs] = ip
		ip = ip + nchars + 1
	    } else
		break
	}
	nargs = nargs - 1

	# Expand the macro. 
	op = 1
	for (ip=1;  macro[ip] != EOS;  ip=ip+1) {
	    ch = macro[ip]

	    if (ch == '$') {			# Process $ arg sequence.
		ip = ip + 1
		ch = macro[ip]
		if (ch >= '1' && ch <= '9') {
		    arg = TO_INTEG(ch)
		    if (arg > nargs)
			call error (1, "Strmac: too few substitution arguments")
		    for (i = offset[arg];  argstr[i] != EOS;  i=i+1) {
			outstr[op] = argstr[i]
			op = op + 1
		    }

		} else if (ch == '$') {		# "$$" --> "$"
		    outstr[op] = '$'
		    op = op + 1

		} else {			# "$?" --> "$?"
		    outstr[op] = '$'
		    op = op + 1
		    outstr[op] = ch
		    op = op + 1
		}

	    } else {				# ordinary character
		outstr[op] = ch
		op = op + 1
	    }

	    if (op > maxch)
		call error (2, "Strmac: output buffer overflow")
	}

	outstr[op] = EOS
	return (op - 1)
end
