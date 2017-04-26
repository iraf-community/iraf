# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"clpset.h"

# CLPPSETD -- Set the double value of a pset parameter.

procedure clppsetd (pp, parname, dval)

pointer	pp			# pset descriptor
char	parname[ARB]		# parameter name
double	dval			# new value of parameter

pointer	clpset_parname()

begin
	call clputd (PARNAME(pp,parname), dval)
end
