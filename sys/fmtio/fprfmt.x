# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<printf.h>

.help
.nf _________________________________________________________________________
Process a format descriptor, setting the variables "decpl", "fill_char",
"format_char", and "width" in the fmtio common.  Called from PARG_
to determine the format specification for printing a variable.

Format: "%[w[.d]]C[n]", where W is the field width, D the number of decimal
places or precision, C the format type character, and N the radix numeral,
for format type FMT_RADIX only.  A negative field width signifies left
justification.  A leading zero in the W field sets the fill character to the
numeral zero (when right justifying).  Default values will be supplied if
any of the fields are omitted.  The minimum format is "%C".

If any of the fields (wdCn) have the value GET_FIELD (= "*") the value of
the field will be taken from the next PARG_ call, rather than from the
format string.  This makes it easy to vary the format specification at run
time.  For example, "%10.*g" would print a number in G-floating format,
with a constant field width of 10, and with the number of digits of precision
being given by a PARGI call at execution time (followed by a PARG_ call to
pass the value to be printed).
.endhelp ____________________________________________________________________

# The following macro marks the position in the FPRFMT procedure (saves the
# code for the needed field), and returns the not done status to PARG_.
# A subsequent call to a PARG_ (with the value of the field we are waiting for
# as argument) causes FPRFMT to be reentered at the point where we left off.

define	(waitfor,	if (ival_already_used) { fmt_state = $1; return (NOT_DONE_YET) } ; $1 ival_already_used = true)

#define	(waitfor,	if (ival_already_used) {
#			    fmt_state = $1
#			    return (NOT_DONE_YET)
#			}
#			$1 ival_already_used = true)

# FPRFMT -- Process a %W.Dn format specification.  ALL_DONE is returned when
# the format specification has been fully processed, else NOT_DONE_YET is
# returned, indicating that an additional PARG call is required to complete
# the format (which therefore contained one or more "*" specifiers).

int procedure fprfmt (ival)

int	ival					# argument value (from parg_)
bool	ival_already_used			# wait for next parg
int	ctoi(), stridx()
char	ch, chrlwr()
include "fmt.com"

begin
	# This routine functions as a coroutine.  If one of the fields in
	# the format spec is to be given in a pargi call, an early return
	# is taken.  The routine is later reentered with the value of the
	# needed field, and execution continues at the point it left off.
	# (Sorry, I could not think of a simpler way to do it...)

	switch (fmt_state) {				# return from "waitfor"
	case FMT_START:					# initial state
	    ival_already_used = false
	case GET_WIDTH_1:				# "%*.dC"
	    goto GET_WIDTH_1
	case GET_WIDTH_2:				# "%-0*.dC"
	    goto GET_WIDTH_2
	case GET_DECPL:					# "%w.*C"
	    goto GET_DECPL
	case GET_FMTCHAR:				# "%w.d*"
	    goto GET_FMTCHAR
	case GET_RADIX:					# "%w.dr*"
	    goto GET_RADIX
	case GET_OPERAND:				# used ival for format
	    goto GET_OPERAND
	}
	
	# It is not an error if there is no format string.
	if (format[ip] == EOS || format[ip] != START_OF_FORMAT) {
	    width = USE_DEFAULT
	    decpl = USE_DEFAULT
	    format_char = USE_DEFAULT
	    fill_char = ' '
	    left_justify = NO
	    fmt_state = FMT_START
	    return (ALL_DONE)
	} else
	    ip = ip + 1					# eat the "%"
	    
	if (format[ip] == GET_FIELD) {			# "%*.dC"
	    ip = ip + 1
	    waitfor (GET_WIDTH_1)			# go get field width...
	    if (ival < 0)				# ...and come back here
		left_justify = YES
	    else
		left_justify = NO

	    fill_char = ' '
	    width = abs (ival)

	} else {					# "%-0*.dC"
	    if (format[ip] == '-') {			# left or right justify
		left_justify = YES
		ip = ip + 1
	    } else
		left_justify = NO

	    fill_char = ' '				# zero or blank fill
	    if (format[ip] == '0') {
		if (IS_DIGIT (format[ip+1]) || format[ip+1] == GET_FIELD) {
		    fill_char = '0'
		    ip = ip + 1
		} else
		    fill_char = ' '
	    }

	    if (format[ip] == GET_FIELD) {
		ip = ip + 1
		waitfor (GET_WIDTH_2)			# go get field width...
		if (ival < 0)				# ... and come back here
		    left_justify = YES
		else
		    left_justify = NO
		width = abs (ival)

	    } else if (ctoi (format, ip, width) <= 0)	# "%N.dC"
		width = USE_DEFAULT
	}

	if (width == 0)					# make as big as needed
	    width = USE_DEFAULT

	if (format[ip] == '.') {			# get decpl field
	    ip = ip + 1
	    if (format[ip] == GET_FIELD) {		# "%w.*C"
		ip = ip + 1
		waitfor (GET_DECPL)
		decpl = ival
	    } else if (ctoi (format, ip, decpl) <= 0)	# "%w.NC"
		decpl = USE_DEFAULT
	} else
	    decpl = USE_DEFAULT

	if (format[ip] == GET_FIELD) {			# "%w.d*"
	    ip = ip + 1
	    waitfor (GET_FMTCHAR)
	    format_char = ival
	} else {
	    format_char = format[ip]			# "%w.dC"
	    ip = ip + 1
	}

	ch = format_char
	if (stridx (ch, "bcdefghHmMorstuwxz") <= 0) {
	    call putline (STDERR, "Warning: Unknown format type char\n")
	    call fmt_err ("", format, ip-1)
	    format_char = USE_DEFAULT

	} else if (format_char == FMT_RADIX) {		# get radix
	    ch = chrlwr (format[ip])
	    ip = ip + 1
	    if (ch == GET_FIELD) {			# "%w.dr*"
		waitfor (GET_RADIX)
		radix = ival
	    } else if (IS_DIGIT (ch)) {
		radix = TO_INTEG (ch)
	    } else if (IS_LOWER (ch)) {
		radix = ch - 'a' + 10
	    } else {
		radix = DECIMAL
		ip = ip - 1
	    }

	} else if (format_char == FMT_WHITESPACE || format_char == FMT_TOCOLUMN)
	    ival_already_used = false			# no operand

	waitfor (GET_OPERAND)				# used ival for format,
	fmt_state = FMT_START				# need to get another
	return (ALL_DONE)
end
