# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"clpset.h"

# CLPPSET -- Set the string value of the named pset parameter.
# [OBSOLETE ROUTINE - see clppseta.x]

procedure clppset (pp, pname, sval)

pointer	pp			# pset descriptor
char	pname[ARB]		# parameter name
char	sval[ARB]		# string value of parameter

pointer	clpset_parname()

begin
	call clpstr (PARNAME(pp,pname), sval)
end
