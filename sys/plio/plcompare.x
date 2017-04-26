# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plset.h>
include	<plio.h>

# PL_COMPARE -- Compare two masks for equality, optionally noting any
# differences on the output file.  PL_EQUAL is returned if the two masks
# are equivalent.

int procedure pl_compare (pl_1, pl_2, outfd)

pointer	pl_1, pl_2		#I masks to be compared
int	outfd			#I file for diagnostic output, or NULL

int	i
bool	pll_equal()

begin
	if (PL_NAXES(pl_1) != PL_NAXES(pl_2) || PL_NLP(pl_1) != PL_NLP(pl_2)) {
	    if (outfd != NULL)
		call fprintf (outfd, "the masks are not the same size\n")
	    return (PL_NOTEQUAL)
	}

	do i = 1, PL_NLP(pl_1)
	    if (!pll_equal (LL(pl_1,PL_LP(pl_1,i)), LL(pl_2,PL_LP(pl_2,i)))) {
		if (outfd != NULL) {
		    call fprintf (outfd, "masks differ at line %d\n")
			call pargi (i)
		}
		return (PL_NOTEQUAL)
	    }

	return (PL_EQUAL)
end
