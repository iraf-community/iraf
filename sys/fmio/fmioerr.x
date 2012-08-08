# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"fmio.h"

# FMIO_ERRCHK -- Check for a posted error, and process the error if one is
# posted.

procedure fmio_errchk (fm)

pointer	fm			#I FMIO descriptor

int	errcode

begin
	errcode = FM_ERRCODE(fm)
	if (errcode != OK) {
	    FM_ERRCODE(fm) = OK
	    call syserrs (errcode, FM_ERROPSTR(fm))
	}
end
