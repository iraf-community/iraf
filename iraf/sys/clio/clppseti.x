# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"clpset.h"

# CLPPSETI -- Set the integer value of a pset parameter.

procedure clppseti (pp, parname, ival)

pointer	pp			# pset descriptor
char	parname[ARB]		# parameter name
int	ival			# new value of parameter

pointer	clpset_parname()

begin
	call clputi (PARNAME(pp,parname), ival)
end
