# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"clpset.h"

# CLGPSET -- Get the string value of the named pset parameter.
# [OBSOLETE ROUTINE - see clgpseta.x]

procedure clgpset (pp, pname, outstr, maxch)

pointer	pp			# pset descriptor
char	pname[ARB]		# parameter name
char	outstr[maxch]		# output string
int	maxch			# max chars out

pointer	clpset_parname()

begin
	call clgstr (PARNAME(pp,pname), outstr, maxch)
end
