# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<pmset.h>
include	<plio.h>
include	<imio.h>

# PM_SET[ILP] -- Set a PMIO or PLIO parameter.

procedure pm_setl (pl, param, lvalue)

pointer	pl			#I mask descriptor
int	param			#I parameter code
long	lvalue			#I parameter value

begin
	switch (param) {
	case P_REFIM:
	    PM_REFIM(pl) = lvalue
	    PM_MAPXY(pl) = IM_SECTUSED(lvalue)
	case P_MAPXY:
	    PM_MAPXY(pl) = lvalue
	case P_MAXLINE:
	    PL_MAXLINE(pl) = lvalue
	case P_DEPTH:
	    PL_MAXVAL(pl) = MV(lvalue)
	default:
	    call syserr (SYS_PLINVPAR)
	}
end


procedure pm_seti (pl, param, value)

pointer	pl			#I mask descriptor
int	param			#I parameter code
int	value			#I parameter value

long	lvalue

begin
	lvalue = value
	call pm_setl (pl, param, lvalue)
end


procedure pm_setp (pl, param, pvalue)

pointer	pl			#I mask descriptor
int	param			#I parameter code
pointer	pvalue			#I parameter value

begin
	switch (param) {
	case P_REFIM:
	    PM_REFIM(pl) = pvalue
	    PM_MAPXY(pl) = IM_SECTUSED(pvalue)
	default:
	    call syserr (SYS_PLINVPAR)
	}
end
