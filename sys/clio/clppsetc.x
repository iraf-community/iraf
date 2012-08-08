# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"clpset.h"

# CLPPSETC -- Set the char value of a pset parameter.

procedure clppsetc (pp, parname, cval)

pointer	pp			# pset descriptor
char	parname[ARB]		# parameter name
char	cval			# new value of parameter

pointer	clpset_parname()

begin
	call clputc (PARNAME(pp,parname), cval)
end
