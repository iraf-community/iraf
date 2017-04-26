# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<mwset.h>
include	"mwcs.h"

# MW_SETI -- Set the value of a MWCS interface parameter.

procedure mw_seti (mw, param, value)

pointer	mw			#I pointer to MWCS descriptor
int	param			#I parameter code as defined in <mwset.h>
int	value			#I new value for parameter

begin
	switch (param) {
	case MW_NWCS:
	    MI_NWCS(mw) = max (2, value)
	case MW_REFIM:
	    MI_REFIM(mw) = value
	case MW_USEAXMAP:
	    MI_USEAXMAP(mw) = value
	default:
	    call syserr (SYS_MWSET)
	}
end
