# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<printf.h>

# FPRINTF  -- Format output to a file.

procedure fprintf (fd, format_string)

int	fd
char	format_string[ARB]

begin
	call fprntf (fd, format_string, REGULAR_FILE)
end
