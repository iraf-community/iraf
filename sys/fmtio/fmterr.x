# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# FMT_ERR -- Print the format string on the standard error output, marking
# the position within the string to which the error refers.

procedure fmt_err (preamble, format, index)

char	preamble[ARB], format[ARB]
int	index, ip

begin
	call putline (STDERR, "(")
	call putline (STDERR, preamble)
	call putline (STDERR, "format = \"")

	for (ip=1;  ip < index && format[ip] != EOS;  ip=ip+1)
	    call putcc (STDERR, format[ip])

	if (format[ip] != EOS) {			# mark position of error
	    call putline (STDERR, "<>")
	    for (;  format[ip] != EOS;  ip=ip+1)
		call putcc (STDERR, format[ip])
	}
	call putline (STDERR, "\")\n")
end
