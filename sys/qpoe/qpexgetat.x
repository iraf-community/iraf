# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"qpex.h"

# QPEX_GETATTRIBUTE -- Get the filter expression for the named attribute
# as a text string.  The length of the string is returned as the function
# value.  If the referenced QPEX descriptor does not contain any filter
# terms for the named attribute, zero will be returned.  If the expression
# contains multiple eterms the successive terms will be delimited by
# semicolons, e.g., "(a:b,c:d); (e:f,g)".  The lists of ranges within an
# eterm are OR-ed to produce a filter term; successive eterms are AND-ed
# to produce the final filter (hence the example above is equivalent to
# "(a to b OR c to d) AND (e to f OR g)").

int procedure qpex_getattribute (ex, attribute, outstr, maxch)

pointer	ex			#I QPEX descriptor
char	attribute[ARB]		#I attribute name
char	outstr[maxch]		#O receives the filter string
int	maxch			#I max chars out

pointer	sp, atname, et
int	nchars, op, otop
int	gstrcpy(), qp_expandtext()
bool	strne()

begin
	call smark (sp)
	call salloc (atname, SZ_FNAME, TY_CHAR)

	# Translate attribute name, in case it is aliased.
	nchars = qp_expandtext (EX_QP(ex), attribute, Memc[atname], SZ_FNAME)

	# Construct filter expression for named attribute.
	op = 1
	otop = maxch + 1
	for (et=EX_ETHEAD(ex);  et != NULL;  et=ET_NEXT(et)) {
	    if (ET_DELETED(et) == YES)
		next

	    # Skip entry if not for the named attribute.
	    if (strne (Memc[ET_ATNAME(et)], Memc[atname]))
		next

	    # Add term delimiter if not first term.
	    if (op > 1) {
		outstr[op] = ';';  op = min(otop, op + 1)
		outstr[op] = ' ';  op = min(otop, op + 1)
	    }

	    # The expression text (may be very large).
	    op = min (otop,
		op + gstrcpy (Memc[ET_EXPRTEXT(et)], outstr[op], otop-op))
	}
	outstr[op] = EOS

	# Return the string length, or zero if no filter for named attribute.
	call sfree (sp)
	return (op - 1)
end
