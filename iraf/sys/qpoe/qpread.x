# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include "qpoe.h"

# QP_READ -- Read a range of elements from a parameter.  Works for any
# parameter, including scalar parameters and both static and variable
# length array valued parameters.  Automatic datatype conversion is
# performed for the primitive types.

int procedure qp_read (qp, param, buf, maxelem, first, datatype)

pointer qp			#I QPOE descriptor
char	param[ARB]		#I parameter name
char	buf[ARB]		#O user data buffer to receive data
int	maxelem			#I max number of data elements to read
int	first			#I first data element to read
char	datatype[ARB]		#I datatype to be returned

pointer	sp, fm, sym, tbuf, isym, osym
int	fd, sz_itype, sz_otype, nelem, itype, otype

pointer	qp_gpsym()
int	fm_getfd(), qp_sizeof(), read(), qp_dtype()
errchk	qp_bind, qp_gpsym, fm_getfd, seek, read, syserrs

begin
	if (QP_ACTIVE(qp) == NO)
	    call qp_bind (qp)

	fm = QP_FM(qp)
	otype = qp_dtype (qp, datatype, osym)

	# Lookup the symbol in the symbol table.
	sym = qp_gpsym (qp, param)
	if (sym == NULL)
	    call syserrs (SYS_QPUKNPAR, param)
	else {
	    itype = S_DTYPE(sym)
	    isym  = sym
	}

	# Determine the number of inbounds elements.
	nelem = max(0, min(maxelem, S_NELEM(sym) - first + 1))
	if (first <= 0)
	    call syserrs (SYS_QPINDXOOR, param)

	# Verify that any type conversion is legal.
	if (otype != itype)
	    if (min(otype,itype) < TY_CHAR || max(otype,itype) > TY_DOUBLE)
		call syserrs (SYS_QPBADCONV, param)

	# Open the lfile and read the data segment.
	fd = fm_getfd (fm, S_LFILE(sym), READ_ONLY, 0)

	if (nelem > 0) {
	    sz_itype = qp_sizeof (qp, itype, isym, IMMEDIATE)
	    sz_otype = qp_sizeof (qp, otype, osym, INSTANCEOF)

	    # Read and output the data.
	    call seek (fd, S_OFFSET(sym) + (first - 1) * sz_itype)
	    if (sz_itype <= sz_otype) {
		# Read the data directly into the user's buffer.
		nelem = read (fd, buf, nelem * sz_itype) / sz_itype
		if (nelem > 0 && otype != itype)
		    call acht (buf, buf, nelem, itype, otype)
	    } else {
		# Read the data into a temporary buffer.
		call smark (sp)
		call salloc (tbuf, nelem * sz_itype, TY_CHAR)
		nelem = read (fd, Memc[tbuf], nelem * sz_itype) / sz_itype
		if (nelem > 0)
		    call acht (Memc[tbuf], buf, nelem, itype, otype)
		call sfree (sp)
	    }
	}

	call fm_retfd (fm, S_LFILE(sym))
	return (nelem)
end
