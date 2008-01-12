# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"fmio.h"

# FMIO_POSTERR -- Post an error.  This is called to flag an error condition
# by low level code that cannot call the error handling code directly.

procedure fmio_posterr (fm, errcode, opstr)

pointer	fm			#I FMIO descriptor
int	errcode			#I error code
char	opstr[ARB]		#I operand id string

begin
	# In case of multiple errors, post only the first one.
	if (FM_ERRCODE(fm) == OK) {
	    FM_ERRCODE(fm) = errcode
	    call strcpy (opstr, FM_ERROPSTR(fm), SZ_ERROPSTR)
	}
end
