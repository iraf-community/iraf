# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<printf.h>

# FMT_INIT -- The following is called by SPRINTF and CLPRINTF to set a flag to
# close the mem file or finish the CL command, respectively, when
# the end of the format string is reached.  This entry is also called
# by the iraf main at startup time to initialize the printf common.

procedure fmt_init (ftype)

int	ftype
include	"fmt.com"

begin
	if (ftype == FMT_INITIALIZE) {
	    ip = 1
	    format[ip] = EOS
	    ofile_type = REGULAR_FILE			# fpradv
	    fmt_state = FMT_START			# fprfmt
	} else
	    ofile_type = ftype
end
