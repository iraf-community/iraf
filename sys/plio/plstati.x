# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<plset.h>
include	<plio.h>

# PL_STATI -- Stat a PLIO parameter.

int procedure pl_stati (pl, param)

pointer	pl			#I mask descriptor
int	param			#I parameter code

int	i

begin
	switch (param) {
	case P_PRIVATE1:
	    return (PL_PRIVATE1(pl))
	case P_PRIVATE2:
	    return (PL_PRIVATE2(pl))
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
