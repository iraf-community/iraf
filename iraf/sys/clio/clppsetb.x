# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"clpset.h"

# CLPPSETB -- Set the boolean value of a pset parameter.

procedure clppsetb (pp, parname, bval)

pointer	pp			# pset descriptor
char	parname[ARB]		# parameter name
bool	bval			# new value of parameter

pointer	clpset_parname()

begin
	call clputb (PARNAME(pp,parname), bval)
end
