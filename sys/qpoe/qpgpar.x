# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include <ctype.h>
include "qpoe.h"

# QP_GETPARAM -- Lookup the named parameter in the symbol table and return
# a pointer to the scalar parameter value.  A NULL pointer is returned if
# the parameter exists but does not currently have a value.  The parameter
# datatype code is returned as the function value.  The pointed to parameter
# value will be clobbered in the next call, hence should be used promptly.
# The data element pointed to may be a structure as well as a primitive type.

int procedure qp_getparam (qp, param, o_pp)

pointer qp			#I QPOE descriptor
char	param[ARB]		#I parameter name
pointer o_pp			#O pointer to parameter value

int	loc_pval, loc_Mem, ip, ch, elem, sz_elem, fd
pointer sp, key, fm, pp, op, sym
double	pval[LEN_PVAL+1]
data	pp /NULL/

pointer qp_gpsym()
int	qp_sizeof(), fm_getfd(), qp_ctoi(), read()
errchk	qp_bind, qp_gpsym, syserrs, fm_getfd, seek, read

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)

	if (QP_ACTIVE(qp) == NO)
	    call qp_bind (qp)

	fm = QP_FM(qp)

	# Compute pointer (Memc index) to the static pval buffer.
	# Make sure that the computed pointer is double aligned.

	if (pp == NULL) {
	    call zlocva (pval, loc_pval)
	    call zlocva (Memc, loc_Mem)
	    pp = (loc_pval+SZ_DOUBLE - loc_Mem) / SZ_DOUBLE * SZ_DOUBLE + 1
	}

	# Extract the primary parameter name, minus any whitespace and
	# subscript (e.g., "param[elem]").

	op = key
	do ip = 1, SZ_FNAME {
	    ch = param[ip]
	    if (IS_WHITE(ch))
		next
	    else if (ch == '[' || ch == EOS)
		break
	    Memc[op] = ch
	    op = op + 1
	}
	Memc[op] = EOS

	# Determine the array element (default [1]).
	elem = 1
	if (param[ip] == '[') {
	    ip = ip + 1
	    if (qp_ctoi (param, ip, elem) <= 0)
		elem = 1
	}

	# Lookup the symbol in the symbol table.
	sym = qp_gpsym (qp, Memc[key])
	if (sym == NULL)
	    call syserrs (SYS_QPUKNPAR, param)

	# Check to make sure the parameter value exists, and fetch the 
	# value from the lfile where the parameter data is stored, setting
	# the parameter value pointer to point to the stored value.

	if (elem < 1 || elem > S_NELEM(sym))
	    o_pp = NULL
	else {
	    sz_elem = qp_sizeof (qp, S_DTYPE(sym), sym, INSTANCEOF)
	    if (sz_elem > LEN_PVAL * SZ_DOUBLE)
		call syserrs (SYS_QPPVALOVF, QP_DFNAME(qp))

	    fd = fm_getfd (fm, S_LFILE(sym), READ_ONLY, 0)

	    call seek (fd, S_OFFSET(sym) + (elem - 1) * sz_elem)
	    if (read (fd, Memc[pp], sz_elem) < sz_elem)
		o_pp = NULL
	    else if (S_DTYPE(sym) == TY_USER)
		o_pp = (pp - 1) / SZ_STRUCT + 1
	    else
		o_pp = (pp - 1) / sz_elem + 1

	    call fm_retfd (fm, S_LFILE(sym))
	}

	call sfree (sp)
	return (S_DTYPE(sym))
end
