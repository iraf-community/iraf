# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"qpex.h"

# QPEX_GETFILTER -- Return the currently active filter as a text string,
# i.e., as a series of "attribute = expr" terms.  The number of chars
# output is returned as the function value.

int procedure qpex_getfilter (ex, outstr, maxch)

pointer	ex			#I QPEX descriptor
char	outstr[maxch]		#O receives the filter string
int	maxch			#I max chars out

pointer	et
int	op, otop
int	gstrcpy()

begin
	op = 1
	otop = maxch + 1
	for (et=EX_ETHEAD(ex);  et != NULL;  et=ET_NEXT(et)) {
	    if (ET_DELETED(et) == YES)
		next

	    # Add term delimiter if not first term.
	    if (op > 1) {
		outstr[op] = ',';  op = min(otop, op + 1)
		outstr[op] = ' ';  op = min(otop, op + 1)
	    }

	    # Attribute name.
	    op = min (otop,
		op + gstrcpy (Memc[ET_ATNAME(et)], outstr[op], otop-op))
	    outstr[op] = ' ';  op = min(otop, op + 1)

	    # Assignment operator ("=" or "+=").
	    op = min (otop,
		op + gstrcpy (Memc[ET_ASSIGNOP(et)], outstr[op], otop-op))
	    outstr[op] = ' ';  op = min(otop, op + 1)

	    # The expression text (may be very large).
	    op = min (otop,
		op + gstrcpy (Memc[ET_EXPRTEXT(et)], outstr[op], otop-op))
	}
	outstr[op] = EOS

	return (op - 1)
end
