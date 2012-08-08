# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"clpset.h"

# CLPPSETR -- Set the real value of a pset parameter.

procedure clppsetr (pp, parname, rval)

pointer	pp			# pset descriptor
char	parname[ARB]		# parameter name
real	rval			# new value of parameter

pointer	clpset_parname()

begin
	call clputr (PARNAME(pp,parname), rval)
end
