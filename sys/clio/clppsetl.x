# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"clpset.h"

# CLPPSETL -- Set the long integer value of a pset parameter.

procedure clppsetl (pp, parname, lval)

pointer	pp			# pset descriptor
char	parname[ARB]		# parameter name
long	lval			# new value of parameter

pointer	clpset_parname()

begin
	call clputl (PARNAME(pp,parname), lval)
end
