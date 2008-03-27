# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<pmset.h>
include	"mio.h"

# MIO_SET[IP] -- Set an MIO parameter.

procedure mio_seti (mp, param, value)

pointer	mp			#I MIO descriptor
int	param			#I parameter to be set
int	value			#I new value

begin
	switch (param) {
	case P_REGCOORDS:
	    M_REGCOORDS(mp) = value
	case P_PMCLOSE:
	    M_PMCLOSE(mp) = value
	default:
	    call syserr (SYS_PLINVPAR)
	}
end


procedure mio_setp (mp, param, pvalue)

pointer	mp			#I MIO descriptor
int	param			#I parameter to be set
pointer	pvalue			#I new value

begin
	switch (param) {
	case P_PMDES:
	    M_PM(mp) = pvalue
	    M_ACTIVE(mp) = NO
	case P_IMDES:
	    M_IM(mp) = pvalue
	    M_ACTIVE(mp) = NO
	default:
	    call syserr (SYS_PLINVPAR)
	}
end
