# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<ctype.h>
include	"../qpex.h"

# QPEX_ATTRL -- Get the good-value range list for the named attribute, as a
# binary range list of the indicated type.  This range list is a simplified
# version of the original filter expression, which may have contained
# multiple fields, some negated or overlapping, in any order, subsequently
# modified or deleted with qpex_modfilter, etc.  The final resultant range
# list is ordered and consists of discreet nonoverlapping ranges.
#
# Upon input the variables XS and XE should either point to a pair of
# preallocated buffers of length XLEN, or they should be set to NULL.
# The routine will reallocate the buffers as necessary to allow for long
# range lists, updating XLEN so that it always contains the actual length
# of the arrays (which may not be completely full).  Each list element
# consists of a pair of values (xs[i],xe[i]) defining the start and end
# points of the range.  If xs[1] is INDEF the range is open to the left,
# if xe[nranges] is INDEF the range is open to the right.  The number of
# ranges output is returned as the function value.

int procedure qpex_attrlr (ex, attribute, xs, xe, xlen)

pointer	ex			#I QPEX descriptor
char	attribute[ARB]		#I attribute name
pointer	xs			#U pointer to array of start values
pointer	xe			#U pointer to array of end values
int	xlen			#U length of xs/xe arrays

pointer	ps, pe, qs, qe
pointer	sp, expr, ip, ep
int	plen, qlen, np, nq, nx
int	neterms, nchars, maxch
int	qpex_getattribute(), qpex_parser(), qp_rlmerger()

begin
	call smark (sp)

	# Get attribute filter expression.  In the unlikely event that the
	# expression is too large to fit in our buffer, repeat with a buffer
	# twice as large until it fits.

	maxch = DEF_SZEXPRBUF
	nchars = 0

	repeat {
	    maxch = maxch * 2
	    call salloc (expr, maxch, TY_CHAR)
	    nchars = qpex_getattribute (ex, attribute, Memc[expr], maxch)
	    if (nchars <= 0)
		break
	} until (nchars < maxch)

	# Parse expression to produce a range list.  If the expression
	# contains multiple eterms each is parsed separately and merged
	# into the final output range list.

	nx = 0
	neterms = 0

	if (nchars > 0) {
	    # Get range list storage.
	    plen = DEF_XLEN
	    call malloc (ps, plen, TY_REAL)
	    call malloc (pe, plen, TY_REAL)
	    qlen = DEF_XLEN
	    call malloc (qs, qlen, TY_REAL)
	    call malloc (qe, qlen, TY_REAL)

	    # Parse each subexpression and merge into output range list.
	    for (ip=expr;  Memc[ip] != EOS;  ) {
		# Get next subexpression.
		while (IS_WHITE (Memc[ip]))
		    ip = ip + 1
		for (ep=ip;  Memc[ip] != EOS;  ip=ip+1)
		    if (Memc[ip] == ';') {
			Memc[ip] = EOS
			ip = ip + 1
			break
		    }
		if (Memc[ep] == EOS)
		    break

		# Copy output range list to X list temporary.
		if (max(nx,1) > plen) {
		    plen = max(xlen,1)
		    call realloc (ps, plen, TY_REAL)
		    call realloc (pe, plen, TY_REAL)
		}
		if (neterms <= 0) {
		    Memr[ps] = LEFTR
		    Memr[pe] = RIGHTR
		    np = 1
		} else {
		    call amovr (Memr[xs], Memr[ps], nx)
		    call amovr (Memr[xe], Memr[pe], nx)
		    np = nx
		}

		# Parse next eterm into Y list temporary.
		nq = qpex_parser (Memc[ep], qs, qe, qlen)

		# Merge the X and Y lists, leaving result in output list.
		nx = qp_rlmerger (xs,xe,xlen,
		    Memr[ps], Memr[pe], np, Memr[qs], Memr[qe], nq)

		neterms = neterms + 1
	    }

	    # Free temporary range list storage.
	    call mfree (ps, TY_REAL);  call mfree (pe, TY_REAL)
	    call mfree (qs, TY_REAL);  call mfree (qe, TY_REAL)

	    # Convert LEFT/RIGHT magic values to INDEF.
	    if (nx > 0) {
		if (IS_LEFTR (Memr[xs]))
		    Memr[xs] = INDEFR
		if (IS_RIGHTR (Memr[xe+nx-1]))
		    Memr[xe+nx-1] = INDEFR
	    }
	}

	call sfree (sp)
	return (nx)
end
