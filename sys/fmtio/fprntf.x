# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<printf.h>

# FPRNTF -- Initiate a formatted print.  Called by FPRINTF, SPRINTF, etc.
# Check that the previous print has completed, initialize the current
# print, and advance to the first format specification (if any).

procedure fprntf (new_fd, format_string, file_type)

int	new_fd, file_type
char	format_string[ARB]
include "fmt.com"

begin
	# Printf is not reentrant.  An expression in a PARG_ call must not
	# directly or indirectly call any of the printf entry points.  There
	# must be a PARG_ for each "%w.dC" format specification in the format
	# string.  Errors result in lost output, but are otherwise harmless,
	# and are diagnosed below.

 	if (format[ip] != EOS) {
	    call putline (STDERR, "Warning: Incomplete or reentrant printf\n")
	    call fmt_err ("Old ", format, ip)
	    call fmt_err ("New ", format_string, ARB)

	    while (format[ip] != EOS)		# discard rest of format string
		ip = ip + 1
	    call fpradv()			# possibly close mem file
	}

	fd = new_fd				# normal initialization
	ip = 1	
	col = 1
	fmt_state = FMT_START			# initialize FPRFMT state
	ofile_type = file_type

	call strcpy (format_string, format, SZ_OBUF)
	call fpradv()
end
