# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"clpset.h"

# CLPSET_PARNAME -- Return a pointer to the full name (pset.parname) of a
# parameter in the referenced pset.

pointer procedure clpset_parname (pp, parname)

pointer	pp			# pset descriptor
char	parname[ARB]		# name of parameter in pset

pointer	op
int	gstrcpy()

begin
	op = PS_PARNAMEP(pp)
	if (Memc[PS_PSETNAMEP(pp)] != EOS) {
	    op = op + gstrcpy (PS_PSETNAME(pp), Memc[op], SZ_PSPARNAME)
	    Memc[op] = '.';  op = op + 1
	}
	call strcpy (parname, Memc[op], SZ_PSPARNAME-(op-PS_PARNAMEP(pp)))

	return (PS_PARNAMEP(pp))
end
