# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"clpset.h"

# CLPPSETS -- Set the short integer value of a pset parameter.

procedure clppsets (pp, parname, sval)

pointer	pp			# pset descriptor
char	parname[ARB]		# parameter name
short	sval			# new value of parameter

pointer	clpset_parname()

begin
	call clputs (PARNAME(pp,parname), sval)
end
