# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<printf.h>

# FMT_READ -- Read and interpret a format specification.  Called in
# circumstances where a NOT_DONE_YET return from FPRFMT is certain to
# indicate a missing PARGI type argument.  If this happens, print warning
# message, and exhaust format string so that default formats will be used.

procedure fmt_read()

int	fprfmt()
include	"fmt.com"

begin
	while (fprfmt(0) != ALL_DONE) {			# read format
	    call putline (STDERR, "Warning: Missing argument to printf\n")
	    call fmt_err ("", format, ip)
	    while (format[ip] != EOS)			# discard rest of format
		ip = ip + 1
	    fmt_state = FMT_START			# set defaults
	}
end
