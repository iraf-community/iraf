# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<pmset.h>
include	<plio.h>
include	<imio.h>

# PM_SETI -- Set a PMIO or PLIO parameter.

procedure pm_seti (pl, param, value)

pointer	pl			#I mask descriptor
int	param			#I parameter code
int	value			#I parameter value

begin
	switch (param) {
	case P_REFIM:
	    PM_REFIM(pl) = value
	    PM_MAPXY(pl) = IM_SECTUSED(value)
	case P_MAPXY:
	    PM_MAPXY(pl) = value
	case P_MAXLINE:
	    PL_MAXLINE(pl) = value
	case P_DEPTH:
	    PL_MAXVAL(pl) = MV(value)
	default:
	    call syserr (SYS_PLINVPAR)
	}
end
