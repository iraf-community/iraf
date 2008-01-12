# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<printf.h>

# CLPRINTF -- Format and output a string to the CL to set the value of the
# named (string or struct type) parameter.  For example, to set a cursor
# struct parameter,  "clprintf (param, "%8.4f %8.4f %c")" ...

procedure clprintf (param, format_string)

char	param[ARB], format_string[ARB]

begin
	call putline (CLOUT, param)
	call putline (CLOUT, " = \"")
	call fprntf (CLOUT, format_string, CL_PARAM)
end
