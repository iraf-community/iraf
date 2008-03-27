# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<plset.h>
include	<plio.h>

# PL_SET[IL] -- Set a PLIO parameter.

procedure pl_setl (pl, param, lvalue)

pointer	pl			#I mask descriptor
int	param			#I parameter code
long	lvalue			#I parameter value

int	value

begin
	value = lvalue
	switch (param) {
	case P_PRIVATE1:
	    PL_PRIVATE1(pl) = lvalue
	case P_PRIVATE2:
	    PL_PRIVATE2(pl) = lvalue
	case P_MAXLINE:
	    PL_MAXLINE(pl) = lvalue
	case P_DEPTH:
	    PL_MAXVAL(pl) = MV(value)
	default:
	    call syserr (SYS_PLINVPAR)
	}
end


procedure pl_seti (pl, param, value)

pointer	pl			#I mask descriptor
int	param			#I parameter code
int	value			#I parameter value

long	lvalue

begin
	lvalue = value
	call pl_setl(pl, param, lvalue)
end
