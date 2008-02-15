# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<ctype.h>
include	"qpoe.h"

# QP_PUTPARAM -- Lookup the named parameter in the symbol table and return
# a pointer to a buffer into which the scalar parameter value is to be
# placed.  A subsequent call to QPOE_FLUSHPAR updates the parameter value
# in the datafile.  A NULL pointer is returned if the parameter exists but
# does not currently have a value.  The parameter datatype code is returned
# as the function value.

int procedure qp_putparam (qp, param, o_pp)

pointer	qp			#I QPOE descriptor
char	param[ARB]		#I parameter name
pointer	o_pp			#O pointer to parameter value

bool	first_time
pointer	sp, key, fm, op
int	loc_pval, loc_Mem, ip, ch, sz_elem
data	first_time /true/

int	elem
pointer	pp, sym
bool	put_value
double	pval[LEN_PVAL+1]
common	/qppval/ pval, sym, elem, pp, put_value

pointer	qp_gpsym()
int	ctoi(), qp_sizeof()
errchk	qp_bind, syserrs

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)

	if (QP_ACTIVE(qp) == NO)
	    call qp_bind (qp)

	fm = QP_FM(qp)

        # Compute pointer (Memc index) to the static pval buffer.
	# Make sure that the computed pointer is double aligned.

	if (first_time) {
	    call zlocva (pval, loc_pval)
	    call zlocva (Memc, loc_Mem)
	    pp = (loc_pval+SZ_DOUBLE - loc_Mem) / SZ_DOUBLE * SZ_DOUBLE + 1
	    put_value = false
	    first_time = false
	} else if (put_value)
	    call qp_flushpar (qp)

	# Extract the primary parameter name, minus any whitespace and
	# subscript (e.g., "param[elem]").

	op = key
	do ip = 1, SZ_FNAME {
	    ch = param[ip]
	    if (IS_WHITE(ch))
		next
	    else if (ch == '[')
		break
	    Memc[op] = ch
	    op = op + 1
	}
	Memc[op] = EOS

	# Determine the array element (default [1]).
	elem = 1
	if (param[ip] == '[') {
	    ip = ip + 1
	    if (ctoi (param, ip, elem) <= 0)
		elem = 1
	}

	# Lookup the symbol in the symbol table.
	sym = qp_gpsym (qp, Memc[key])
	if (sym == NULL)
	    call syserrs (SYS_QPUKNPAR, param)

	# Check to make sure storage for the parameter value exists, and
	# set the parameter buffer pointer for the indicated datatype.

	sz_elem = qp_sizeof (qp, S_DTYPE(sym), sym, INSTANCEOF)
	if (sz_elem > LEN_PVAL * SZ_DOUBLE)
	    call syserrs (SYS_QPPVALOVF, QP_DFNAME(qp))

	if (elem < 1 || elem > S_MAXELEM(sym))
	    o_pp = NULL
	else if (S_DTYPE(sym) == TY_USER)
	    o_pp = (pp - 1) / SZ_STRUCT + 1
	else
	    o_pp = (pp - 1) / sz_elem + 1

	# Set a flag to flush the value after the user has entered it.
	put_value = true

	call sfree (sp)
	return (S_DTYPE(sym))
end


# QP_FLUSHPAR -- Update the saved parameter value in the indicated lfile.
# Repeated calls are harmless.

procedure qp_flushpar (qp)

pointer	qp			#I QPOE descriptor

int	sz_elem, fd
int	qp_sizeof(), fm_getfd()
errchk	fm_getfd, seek, write

int	elem
pointer	pp, sym
bool	put_value
double	pval[LEN_PVAL+1]
common	/qppval/ pval, sym, elem, pp, put_value

begin
	if (put_value) {
	    sz_elem = qp_sizeof (qp, S_DTYPE(sym), S_DSYM(sym), INSTANCEOF)
	    fd = fm_getfd (QP_FM(qp), S_LFILE(sym), READ_WRITE, 0)

	    call seek (fd, S_OFFSET(sym) + (elem - 1) * sz_elem)
	    call write (fd, Memc[pp], sz_elem)
	    S_NELEM(sym) = max (S_NELEM(sym), elem)
	    QP_MODIFIED(qp) = YES

	    call fm_retfd (QP_FM(qp), S_LFILE(sym))
	    put_value = false
	}
end
