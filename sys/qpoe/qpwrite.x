# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include "qpoe.h"

# QP_WRITE -- Write to a range of elements in a parameter.  Works for any
# parameter, including scalar parameters and both static and variable
# length array valued parameters.  Automatic datatype conversion is
# performed for the primitive types.

procedure qp_write (qp, param, buf, nelem, first, datatype)

pointer qp			#I QPOE descriptor
char	param[ARB]		#I parameter name
char	buf[ARB]		#I user data buffer containing data
int	nelem			#I number of data elements to write
int	first			#I first data element to write to
char	datatype[ARB]		#I datatype of input data

pointer	sp, fm, sym, tbuf, isym, osym
int	fd, sz_itype, sz_otype, last, otype, itype
errchk	qp_bind, qp_gpsym, fm_getfd, seek, syserrs
int	fm_getfd(), qp_sizeof(), qp_dtype()
pointer	qp_gpsym()

begin
	if (nelem <= 0)
	    return

	if (QP_ACTIVE(qp) == NO)
	    call qp_bind (qp)

	itype = qp_dtype (qp, datatype, isym)
	fm = QP_FM(qp)

	# Lookup the symbol in the symbol table.
	sym = qp_gpsym (qp, param)
	if (sym == NULL)
	    call syserrs (SYS_QPUKNPAR, param)
	else {
	    otype = S_DTYPE(sym)
	    osym  = sym
	}

	sz_itype = qp_sizeof (qp, itype, isym, INSTANCEOF)
	sz_otype = qp_sizeof (qp, otype, osym, IMMEDIATE)
	last = first + nelem - 1

	# Check that the write is inbounds.
	if (first <= 0 || (S_MAXELEM(sym) > 0 && last > S_MAXELEM(sym)))
	    call syserrs (SYS_QPINDXOOR, param)

	# Verify that any type conversion is legal.
	if (otype != itype)
	    if (min(otype,itype) < TY_CHAR || max(otype,itype) > TY_DOUBLE)
		call syserrs (SYS_QPBADCONV, param)

	# Open the lfile and update the data segment.
	fd = fm_getfd (fm, S_LFILE(sym), READ_WRITE, 0)
	call seek (fd, S_OFFSET(sym) + (first - 1) * sz_otype)

	# Output the data.
	if (otype != itype) {
	    call smark (sp)
	    call salloc (tbuf, nelem * sz_otype, TY_CHAR)
	    call acht (buf, Memc[tbuf], nelem, itype, otype)
	    call write (fd, Memc[tbuf], nelem * sz_otype)
	    call sfree (sp)
	} else
	    call write (fd, buf, nelem * sz_otype)

	# Update the array size if it got bigger.
	if (last > S_NELEM(sym)) {
	    S_NELEM(sym) = last
	    QP_MODIFIED(qp) = YES
	}

	call fm_retfd (fm, S_LFILE(sym))
end
