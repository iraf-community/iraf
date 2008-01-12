include <ctype.h>

include "fi.h"


# FI_PRECORD -- extract and output a record of fields.

procedure fi_precord (in_fname, linebuf, fields, nfields, quit, name)

char	in_fname[SZ_FNAME]		# Name of input file
int	linebuf[SZ_LINE]		# Line containing fields
int	fields[MAX_FIELDS]		# List of fields to extract
int	nfields				# Number of fields to extract
bool	quit				# Quit if missing field (y/n)?
bool	name				# Print name in output line (y/n)?

char	word[SZ_LINE], white_space[LEN_WS]
int	ip, in_field, out_field, i

int	ctowrd()
errchk	ctowrd

begin
	# Fill white space array to be used a field delimeter
	do i = 1, LEN_WS
	    call strcpy (" ", white_space[i], 1)

	# Print file name as first field of output list?
	if (name) {
	    call printf ("%s%s")
		call pargstr (in_fname)
	        call pargstr (white_space)
	}

	# Position to specific field
	for (i=1; i <= nfields; i=i+1) {
	    out_field = fields[i]
	    in_field = 0
	    ip = 1

	    repeat {
		if (ctowrd (linebuf, ip, word, SZ_LINE) == 0) {
		    if (quit) {
			call eprintf ("Missing field in input. FILE: %s\n")
			call pargstr (in_fname)
			call error (0, "Missing field")
		    } else {
			call printf ("\n")
			return
		    }
		} else
		    in_field = in_field + 1
	    } until (in_field == out_field)

	    call printf ("%s%s")
		call pargstr (word)
	        call pargstr (white_space)
	}

	call printf ("\n")
end
