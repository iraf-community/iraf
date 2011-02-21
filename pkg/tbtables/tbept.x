include <mach.h>		# for MAX_INT, MAX_SHORT, and MAX_REAL
include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbept[tbirds] -- put a single element
# These routines write a single element into a table.  The input value
# is assigned to a buffer of the same data type as the column in the
# table, and then the value is put into the table with a "primitive
# put element" (tbepp[]) routine.
#
# Phil Hodge, 17-Sep-1987  Subroutine created.
# Phil Hodge,  7-Mar-1988  Check nscan() in tbeptt
# Phil Hodge, 14-Jan-1992  Add option for text table type.
# Phil Hodge, 31-Mar-1993  Include short datatype.
# Phil Hodge,  4-Nov-1993  Call sscan as a subroutine, not a function.
# Phil Hodge, 29-Jul-1994  Change calling sequence of tbeoff.
# Phil Hodge,  3-Apr-1995  Set TB_MODIFIED to true.
# Phil Hodge, 14-Jun-1995  Modify for FITS tables.
# Phil Hodge,  2-Jun-1997  Replace INDEFD with TBL_INDEFD.
# Phil Hodge,  3-Mar-1998  Modify to allow for row selector.
# Phil Hodge,  5-Feb-1999  Set TB_MODIFIED to true.

procedure tbeptd (tp, cptr, selrow, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
int	selrow			# i: row number (or selected row number)
double	buffer			# i: value to be put
#--
int	rownum			# actual row number
long	offset			# offset in char to location for writing
int	dtype			# data type of column
# buffers for copying elements of various data types
char	cbuf[SZ_FNAME]
double	dbuf
real	rbuf
int	ibuf
short	sbuf
bool	bbuf
long	tbeoff()
errchk	tbswer, tbeppb, tbeppd, tbeppi, tbepps, tbeppr, tbeppt, tbfapd, tbzptd

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")

	# If we're writing beyond EOF, write extra rows and update TB_NROWS.
	call tbswer (tp, selrow, rownum)

	TB_MODIFIED(tp) = true

	if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzptd (tp, cptr, rownum, buffer)
	    return
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfapd (tp, cptr, rownum, buffer, 1, 1)
	    return
	}

	offset = tbeoff (tp, cptr, rownum, 1)

	dtype = COL_DTYPE(cptr)
	switch (dtype) {
	case TBL_TY_REAL:
	    if (IS_INDEFD (buffer) || abs (buffer) > MAX_REAL)
		rbuf = INDEFR
	    else
		rbuf = buffer
	    call tbeppr (tp, cptr, offset, rownum, rbuf)
	case TBL_TY_DOUBLE:
	    if (IS_INDEFD (buffer))
		dbuf = TBL_INDEFD
	    else
		dbuf = buffer
	    call tbeppd (tp, cptr, offset, rownum, dbuf)
	case TBL_TY_INT:
	    if (IS_INDEFD (buffer) || abs (buffer) > MAX_INT)
		ibuf = INDEFI
	    else
		ibuf = nint (buffer)
	    call tbeppi (tp, cptr, offset, rownum, ibuf)
	case TBL_TY_SHORT:
	    if (IS_INDEFD (buffer) || abs (buffer) > MAX_SHORT)
		sbuf = INDEFS
	    else
		sbuf = nint (buffer)
	    call tbepps (tp, cptr, offset, rownum, sbuf)
	case TBL_TY_BOOL:
	    if (IS_INDEFD (buffer) || abs (buffer) > MAX_INT)
		bbuf = false
	    else if (nint (buffer) == NO)
		bbuf = false
	    else
		bbuf = true
	    call tbeppb (tp, cptr, offset, rownum, bbuf)
	default:
	    if (dtype < 0 || dtype == TY_CHAR) {
		call sprintf (cbuf, SZ_FNAME, "%-25.17g")
		    call pargd (buffer)
		call tbeppt (tp, cptr, offset, rownum, cbuf)
	    } else {
		call error (ER_TBCOLBADTYP,
			"tbeptd:  bad data type; table or memory corrupted?")
	    }
	}
end

procedure tbeptr (tp, cptr, selrow, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
int	selrow			# i: row number (or selected row number)
real	buffer			# i: value to be put
#--
int	rownum			# actual row number
long	offset			# offset in char to location for writing
int	dtype			# data type of column
# buffers for copying elements of various data types
char	cbuf[SZ_FNAME]
double	dbuf
int	ibuf
short	sbuf
bool	bbuf
long	tbeoff()
errchk	tbswer, tbeppb, tbeppd, tbeppi, tbepps, tbeppr, tbeppt, tbfapr, tbzptr

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")

	# If we're writing beyond EOF, write extra rows and update TB_NROWS.
	call tbswer (tp, selrow, rownum)

	TB_MODIFIED(tp) = true

	if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzptr (tp, cptr, rownum, buffer)
	    return
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfapr (tp, cptr, rownum, buffer, 1, 1)
	    return
	}

	offset = tbeoff (tp, cptr, rownum, 1)

	dtype = COL_DTYPE(cptr)
	switch (dtype) {
	case TBL_TY_REAL:
	    call tbeppr (tp, cptr, offset, rownum, buffer)
	case TBL_TY_DOUBLE:
	    if (IS_INDEF (buffer))
		dbuf = TBL_INDEFD
	    else
		dbuf = buffer
	    call tbeppd (tp, cptr, offset, rownum, dbuf)
	case TBL_TY_INT:
	    if (IS_INDEF (buffer) || abs (buffer) > MAX_INT)
		ibuf = INDEFI
	    else
		ibuf = nint (buffer)
	    call tbeppi (tp, cptr, offset, rownum, ibuf)
	case TBL_TY_SHORT:
	    if (IS_INDEF (buffer) || abs (buffer) > MAX_SHORT)
		sbuf = INDEFS
	    else
		sbuf = nint (buffer)
	    call tbepps (tp, cptr, offset, rownum, sbuf)
	case TBL_TY_BOOL:
	    if (IS_INDEF (buffer) || abs (buffer) > MAX_INT)
		bbuf = false
	    else if (nint (buffer) == NO)
		bbuf = false
	    else
		bbuf = true
	    call tbeppb (tp, cptr, offset, rownum, bbuf)
	default:
	    if (dtype < 0 || dtype == TY_CHAR) {
		call sprintf (cbuf, SZ_FNAME, "%-15.7g")
		    call pargr (buffer)
		call tbeppt (tp, cptr, offset, rownum, cbuf)
	    } else {
		call error (ER_TBCOLBADTYP,
			"tbeptr:  bad data type; table or memory corrupted?")
	    }
	}
end

procedure tbepti (tp, cptr, selrow, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
int	selrow			# i: row number (or selected row number)
int	buffer			# i: value to be put
#--
int	rownum			# actual row number
long	offset			# offset in char to location for writing
int	dtype			# data type of column
# buffers for copying elements of various data types
char	cbuf[SZ_FNAME]
double	dbuf
real	rbuf
short	sbuf
bool	bbuf
long	tbeoff()
errchk	tbswer, tbeppb, tbeppd, tbeppi, tbepps, tbeppr, tbeppt, tbfapi, tbzpti

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")

	# If we're writing beyond EOF, write extra rows and update TB_NROWS.
	call tbswer (tp, selrow, rownum)

	TB_MODIFIED(tp) = true

	if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzpti (tp, cptr, rownum, buffer)
	    return
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfapi (tp, cptr, rownum, buffer, 1, 1)
	    return
	}

	offset = tbeoff (tp, cptr, rownum, 1)

	dtype = COL_DTYPE(cptr)
	switch (dtype) {
	case TBL_TY_REAL:
	    if (IS_INDEFI (buffer))
		rbuf = INDEFR
	    else
		rbuf = buffer
	    call tbeppr (tp, cptr, offset, rownum, rbuf)
	case TBL_TY_DOUBLE:
	    if (IS_INDEFI (buffer))
		dbuf = TBL_INDEFD
	    else
		dbuf = buffer
	    call tbeppd (tp, cptr, offset, rownum, dbuf)
	case TBL_TY_INT:
	    call tbeppi (tp, cptr, offset, rownum, buffer)
	case TBL_TY_SHORT:
	    if (IS_INDEFI (buffer) || abs (buffer) > MAX_SHORT)
		sbuf = INDEFS
	    else
		sbuf = buffer
	    call tbepps (tp, cptr, offset, rownum, sbuf)
	case TBL_TY_BOOL:
	    if (IS_INDEFI (buffer) || (buffer == NO))
		bbuf = false
	    else
		bbuf = true
	    call tbeppb (tp, cptr, offset, rownum, bbuf)
	default:
	    if (dtype < 0 || dtype == TY_CHAR) {
		call sprintf (cbuf, SZ_FNAME, "%-11d")
		    call pargi (buffer)
		call tbeppt (tp, cptr, offset, rownum, cbuf)
	    } else {
		call error (ER_TBCOLBADTYP,
			"tbepti:  bad data type; table or memory corrupted?")
	    }
	}
end

procedure tbepts (tp, cptr, selrow, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
int	selrow			# i: row number (or selected row number)
short	buffer			# i: value to be put
#--
int	rownum			# actual row number
long	offset			# offset in char to location for writing
int	dtype			# data type of column
# buffers for copying elements of various data types
char	cbuf[SZ_FNAME]
double	dbuf
real	rbuf
int	ibuf
bool	bbuf
long	tbeoff()
errchk	tbswer, tbeppb, tbeppd, tbeppi, tbepps, tbeppr, tbeppt, tbfaps, tbzpts

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")

	# If we're writing beyond EOF, write extra rows and update TB_NROWS.
	call tbswer (tp, selrow, rownum)

	TB_MODIFIED(tp) = true

	if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzpts (tp, cptr, rownum, buffer)
	    return
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfaps (tp, cptr, rownum, buffer, 1, 1)
	    return
	}

	offset = tbeoff (tp, cptr, rownum, 1)

	dtype = COL_DTYPE(cptr)
	switch (dtype) {
	case TBL_TY_REAL:
	    if (IS_INDEFS (buffer))
		rbuf = INDEFR
	    else
		rbuf = buffer
	    call tbeppr (tp, cptr, offset, rownum, rbuf)
	case TBL_TY_DOUBLE:
	    if (IS_INDEFS (buffer))
		dbuf = TBL_INDEFD
	    else
		dbuf = buffer
	    call tbeppd (tp, cptr, offset, rownum, dbuf)
	case TBL_TY_INT:
	    if (IS_INDEFS (buffer))
		ibuf = INDEFI
	    else
		ibuf = buffer
	    call tbeppi (tp, cptr, offset, rownum, ibuf)
	case TBL_TY_SHORT:
	    call tbepps (tp, cptr, offset, rownum, buffer)
	case TBL_TY_BOOL:
	    if (IS_INDEFS (buffer) || (buffer == NO))
		bbuf = false
	    else
		bbuf = true
	    call tbeppb (tp, cptr, offset, rownum, bbuf)
	default:
	    if (dtype < 0 || dtype == TY_CHAR) {
		call sprintf (cbuf, SZ_FNAME, "%-11d")
		    call pargs (buffer)
		call tbeppt (tp, cptr, offset, rownum, cbuf)
	    } else {
		call error (ER_TBCOLBADTYP,
			"tbepts:  bad data type; table or memory corrupted?")
	    }
	}
end

procedure tbeptb (tp, cptr, selrow, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
int	selrow			# i: row number (or selected row number)
bool	buffer			# i: value to be put
#--
int	rownum			# actual row number
long	offset			# offset in char to location for writing
int	dtype			# data type of column
# buffers for copying elements of various data types
char	cbuf[SZ_FNAME]
double	dbuf
real	rbuf
int	ibuf
short	sbuf
long	tbeoff()
errchk	tbswer, tbeppb, tbeppd, tbeppi, tbepps, tbeppr, tbeppt, tbfapb, tbzptb

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")

	# If we're writing beyond EOF, write extra rows and update TB_NROWS.
	call tbswer (tp, selrow, rownum)

	TB_MODIFIED(tp) = true

	if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzptb (tp, cptr, rownum, buffer)
	    return
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfapb (tp, cptr, rownum, buffer, 1, 1)
	    return
	}

	offset = tbeoff (tp, cptr, rownum, 1)

	dtype = COL_DTYPE(cptr)
	switch (dtype) {
	case TBL_TY_REAL:
	    if (buffer)
		rbuf = real (YES)
	    else
		rbuf = real (NO)
	    call tbeppr (tp, cptr, offset, rownum, rbuf)
	case TBL_TY_DOUBLE:
	    if (buffer)
		dbuf = double (YES)
	    else
		dbuf = double (NO)
	    call tbeppd (tp, cptr, offset, rownum, dbuf)
	case TBL_TY_INT:
	    if (buffer)
		ibuf = YES
	    else
		ibuf = NO
	    call tbeppi (tp, cptr, offset, rownum, ibuf)
	case TBL_TY_SHORT:
	    if (buffer)
		sbuf = YES
	    else
		sbuf = NO
	    call tbepps (tp, cptr, offset, rownum, sbuf)
	case TBL_TY_BOOL:
	    call tbeppb (tp, cptr, offset, rownum, buffer)
	default:
	    if (dtype < 0 || dtype == TY_CHAR) {
		call sprintf (cbuf, SZ_FNAME, "%-3b")
		    call pargb (buffer)
		call tbeppt (tp, cptr, offset, rownum, cbuf)
	    } else {
		call error (ER_TBCOLBADTYP,
			"tbeptb:  bad data type; table or memory corrupted?")
	    }
	}
end

procedure tbeptt (tp, cptr, selrow, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
int	selrow			# i: row number (or selected row number)
char	buffer[ARB]		# i: value to be put
#--
int	rownum			# actual row number
long	offset			# offset in char to location for writing
int	dtype			# data type of column
# buffers for copying elements of various data types
double	dbuf
real	rbuf
int	ibuf
short	sbuf
bool	bbuf
long	tbeoff()
int	nscan(), strlen()
errchk	tbswer, tbeppb, tbeppd, tbeppi, tbepps, tbeppr, tbeppt, tbfapt, tbzptt

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")

	# If we're writing beyond EOF, write extra rows and update TB_NROWS.
	call tbswer (tp, selrow, rownum)

	TB_MODIFIED(tp) = true

	if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzptt (tp, cptr, rownum, buffer)
	    return
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfapt (tp, cptr, rownum, buffer, strlen(buffer), 1, 1)
	    return
	}

	offset = tbeoff (tp, cptr, rownum, 1)

	dtype = COL_DTYPE(cptr)
	switch (dtype) {
	case TBL_TY_REAL:
	    call sscan (buffer)
		call gargr (rbuf)
	    if (nscan() < 1)
		rbuf = INDEFR
	    call tbeppr (tp, cptr, offset, rownum, rbuf)
	case TBL_TY_DOUBLE:
	    call sscan (buffer)
		call gargd (dbuf)
	    if (nscan() < 1)
		dbuf = TBL_INDEFD
	    else if (IS_INDEFD (dbuf))
		dbuf = TBL_INDEFD
	    call tbeppd (tp, cptr, offset, rownum, dbuf)
	case TBL_TY_INT:
	    call sscan (buffer)
		call gargi (ibuf)
	    if (nscan() < 1)
		ibuf = INDEFI
	    call tbeppi (tp, cptr, offset, rownum, ibuf)
	case TBL_TY_SHORT:
	    call sscan (buffer)
		call gargs (sbuf)
	    if (nscan() < 1)
		sbuf = INDEFS
	    call tbepps (tp, cptr, offset, rownum, sbuf)
	case TBL_TY_BOOL:
	    call sscan (buffer)
		call gargb (bbuf)
	    if (nscan() < 1)
		bbuf = false
	    call tbeppb (tp, cptr, offset, rownum, bbuf)
	default:
	    if (dtype < 0 || dtype == TY_CHAR) {
		call tbeppt (tp, cptr, offset, rownum, buffer)
	    } else {
		call error (ER_TBCOLBADTYP,
			"tbeptt:  bad data type; table or memory corrupted?")
	    }
	}
end
