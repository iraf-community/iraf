# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"clpset.h"

# CLPPSETX -- Set the complex value of a pset parameter.

procedure clppsetx (pp, parname, xval)

pointer	pp			# pset descriptor
char	parname[ARB]		# parameter name
complex	xval			# new value of parameter

pointer	clpset_parname()

begin
	call clputx (PARNAME(pp,parname), xval)
end
