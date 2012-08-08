# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<printf.h>

# PRINTF -- Format output to the standard output.

procedure printf (format_string)

char	format_string[ARB]

begin
	call fprntf (STDOUT, format_string, REGULAR_FILE)
end
