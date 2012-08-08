# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<printf.h>

# EPRINTF -- Format output to the standard error output.

procedure eprintf (format_string)

char	format_string[ARB]

begin
	call flush (STDOUT)
	call fprntf (STDERR, format_string, REGULAR_FILE)
end
