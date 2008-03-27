# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<pmset.h>
include	<plio.h>
include	<imio.h>

# PM_STAT[ILP] -- Stat a PMIO or PLIO parameter.

long procedure pm_statl (pl, param)

pointer	pl			#I mask descriptor
int	param			#I parameter code

int	i

begin
	switch (param) {
	case P_REFIM:
	    return (PM_REFIM(pl))
	case P_MAPXY:
	    return (PM_MAPXY(pl))
	case P_MAXLINE:
	    return (PL_MAXLINE(pl))
	case P_DEPTH:
	    do i = 0, ARB
		if (2**i > min (I_PVMAX, PL_MAXVAL(pl)))
		    return (i)
	default:
	    call syserr (SYS_PLINVPAR)
	}
end


int procedure pm_stati (pl, param)

pointer	pl			#I mask descriptor
int	param			#I parameter code

long	pm_statl()

begin
	return (pm_statl(pl, param))
end


pointer procedure pm_statp (pl, param)

pointer	pl			#I mask descriptor
int	param			#I parameter code

begin
	switch (param) {
	case P_REFIM:
	    return (PM_REFIM(pl))
	default:
	    call syserr (SYS_PLINVPAR)
	}
end
