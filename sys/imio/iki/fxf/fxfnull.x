# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 
include	<syserr.h>
include "fxf.h"

# FXF_NULL -- Null driver entry point.

procedure fxf_null()

errchk	syserr, syserrs

begin
	call syserr (SYS_FXFFKNULL)
end
