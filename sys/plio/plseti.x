# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<plset.h>
include	<plio.h>

# PL_SETI -- Set a PLIO parameter.

procedure pl_seti (pl, param, value)

pointer	pl			#I mask descriptor
int	param			#I parameter code
int	value			#I parameter value

begin
	switch (param) {
	case P_PRIVATE1:
	    PL_PRIVATE1(pl) = value
	case P_PRIVATE2:
	    PL_PRIVATE2(pl) = value
	case P_MAXLINE:
	    PL_MAXLINE(pl) = value
	case P_DEPTH:
	    PL_MAXVAL(pl) = MV(value)
	default:
	    call syserr (SYS_PLINVPAR)
	}
end
