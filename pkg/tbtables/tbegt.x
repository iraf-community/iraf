include <mach.h>		# for MAX_INT, MAX_SHORT, and MAX_REAL
include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbegt[tbirds] -- get a single element
# These routines read a single element from a table.  The value is
# read by a "primitive get element" (tbegp[]) routine into a buffer
# of the same data type as the column in the table, and that value is
# assigned to the output buffer.
#
# Phil Hodge, 17-Sep-1987  Subroutine created.
# Phil Hodge, 14-Jan-1992  Add option for text table type.
# Phil Hodge, 31-Mar-1993  Include short datatype; in tbegtb, for types other
#	than boolean, change test from "if (buf == YES)" to "if (buf != NO)".
# Phil Hodge,  4-Nov-1993  Include check on row number less than one;
#			call sscan as a subroutine, not a function.
# Phil Hodge, 29-Jul-1994  Change calling sequence of tbeoff.
# Phil Hodge,  9-Jun-1995  Modify for FITS tables.
# Phil Hodge,  2-Jun-1997  Replace IS_INDEFD with TBL_IS_INDEFD.
# Phil Hodge,  2-Mar-1998  Map selected row number to actual row number.
# Phil Hodge, 14-Apr-1998  Use COL_FMT directly, instead of calling tbcftg.

procedure tbegtd (tp, cptr, selrow, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
int	selrow			# i: row number (or selected row number)
double	buffer			# o: buffer for value to be gotten
#--
int	rownum			# actual row number
long	offset			# offset in char to location for reading
int	dtype			# data type of column
int	nret
# buffers for copying elements of various data types
char	cbuf[SZ_FNAME]
real	rbuf
int	ibuf
short	sbuf
bool	bbuf
long	tbeoff()
int	tbfagd()
int	nscan()
errchk	tbsirow, tbegpb, tbegpd, tbegpi, tbegps, tbegpr, tbegpt, tbfagd, tbzgtd

begin
	call tbsirow (tp, selrow, rownum)

	if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzgtd (tp, cptr, rownum, buffer)
	    return
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    nret = tbfagd (tp, cptr, rownum, buffer, 1, 1)
	    return
	}

	offset = tbeoff (tp, cptr, rownum, 1)

	dtype = COL_DTYPE(cptr)
	switch (dtype) {
	case TBL_TY_REAL:
	    call tbegpr (tp, cptr, offset, rownum, rbuf)
	    if (IS_INDEFR(rbuf))
		buffer = INDEFD
	    else
		buffer = rbuf
	case TBL_TY_DOUBLE:
	    call tbegpd (tp, cptr, offset, rownum, buffer)
	    if (TBL_IS_INDEFD (buffer))
		buffer = INDEFD
	case TBL_TY_INT:
	    call tbegpi (tp, cptr, offset, rownum, ibuf)
	    if (IS_INDEFI(ibuf))
		buffer = INDEFD
	    else
		buffer = ibuf
	case TBL_TY_SHORT:
	    call tbegps (tp, cptr, offset, rownum, sbuf)
	    if (IS_INDEFS(sbuf))
		buffer = INDEFD
	    else
		buffer = sbuf
	case TBL_TY_BOOL:
	    call tbegpb (tp, cptr, offset, rownum, bbuf)
	    if (bbuf)
		buffer = double (YES)
	    else
		buffer = double (NO)
	default:
	    if (dtype < 0 || dtype == TY_CHAR) {
		call tbegpt (tp, cptr, offset, rownum, cbuf, SZ_FNAME)
		call sscan (cbuf)
		    call gargd (buffer)
		if (nscan() < 1)
		    buffer = INDEFD
	    } else {
		call error (ER_TBCOLBADTYP,
			"tbegtd:  bad data type; table or memory corrupted?")
	    }
	}
end

procedure tbegtr (tp, cptr, selrow, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
int	selrow			# i: row number (or selected row number)
real	buffer			# o: buffer for value to be gotten
#--
int	rownum			# actual row number
long	offset			# offset in char to location for reading
int	dtype			# data type of column
int	nret
# buffers for copying elements of various data types
char	cbuf[SZ_FNAME]
double	dbuf
int	ibuf
short	sbuf
bool	bbuf
long	tbeoff()
int	tbfagr()
int	nscan()
errchk	tbsirow, tbegpb, tbegpd, tbegpi, tbegps, tbegpr, tbegpt, tbfagr, tbzgtr

begin
	call tbsirow (tp, selrow, rownum)

	if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzgtr (tp, cptr, rownum, buffer)
	    return
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    nret = tbfagr (tp, cptr, rownum, buffer, 1, 1)
	    return
	}

	offset = tbeoff (tp, cptr, rownum, 1)

	dtype = COL_DTYPE(cptr)
	switch (dtype) {
	case TBL_TY_REAL:
	    call tbegpr (tp, cptr, offset, rownum, buffer)
	case TBL_TY_DOUBLE:
	    call tbegpd (tp, cptr, offset, rownum, dbuf)
	    if (TBL_IS_INDEFD (dbuf) || abs (dbuf) > MAX_REAL)
		buffer = INDEFR
	    else
		buffer = dbuf
	case TBL_TY_INT:
	    call tbegpi (tp, cptr, offset, rownum, ibuf)
	    if (IS_INDEFI(ibuf))
		buffer = INDEFR
	    else
		buffer = ibuf
	case TBL_TY_SHORT:
	    call tbegps (tp, cptr, offset, rownum, sbuf)
	    if (IS_INDEFS(sbuf))
		buffer = INDEFR
	    else
		buffer = sbuf
	case TBL_TY_BOOL:
	    call tbegpb (tp, cptr, offset, rownum, bbuf)
	    if (bbuf)
		buffer = real (YES)
	    else
		buffer = real (NO)
	default:
	    if (dtype < 0 || dtype == TY_CHAR) {
		buffer = INDEFR
		call tbegpt (tp, cptr, offset, rownum, cbuf, SZ_FNAME)
		call sscan (cbuf)
		    call gargr (buffer)
		if (nscan() < 1)
		    buffer = INDEFR
	    } else {
		call error (ER_TBCOLBADTYP,
			"tbegtr:  bad data type; table or memory corrupted?")
	    }
	}
end

procedure tbegti (tp, cptr, selrow, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
int	selrow			# i: row number (or selected row number)
int	buffer			# o: buffer for value to be gotten
#--
int	rownum			# actual row number
long	offset			# offset in char to location for reading
int	dtype			# data type of column
int	nret
# buffers for copying elements of various data types
char	cbuf[SZ_FNAME]
double	dbuf
real	rbuf
short	sbuf
bool	bbuf
long	tbeoff()
int	tbfagi()
int	nscan()
errchk	tbsirow, tbegpb, tbegpd, tbegpi, tbegps, tbegpr, tbegpt, tbfagi, tbzgti

begin
	call tbsirow (tp, selrow, rownum)

	if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzgti (tp, cptr, rownum, buffer)
	    return
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    nret = tbfagi (tp, cptr, rownum, buffer, 1, 1)
	    return
	}

	offset = tbeoff (tp, cptr, rownum, 1)

	dtype = COL_DTYPE(cptr)
	switch (dtype) {
	case TBL_TY_REAL:
	    call tbegpr (tp, cptr, offset, rownum, rbuf)
	    if (IS_INDEFR(rbuf) || abs (rbuf) > MAX_INT)
		buffer = INDEFI
	    else
		buffer = nint (rbuf)
	case TBL_TY_DOUBLE:
	    call tbegpd (tp, cptr, offset, rownum, dbuf)
	    if (TBL_IS_INDEFD (dbuf) || abs (dbuf) > MAX_INT)
		buffer = INDEFI
	    else
		buffer = nint (dbuf)
	case TBL_TY_INT:
	    call tbegpi (tp, cptr, offset, rownum, buffer)
	case TBL_TY_SHORT:
	    call tbegps (tp, cptr, offset, rownum, sbuf)
	    if (IS_INDEFS(sbuf))
		buffer = INDEFI
	    else
		buffer = sbuf
	case TBL_TY_BOOL:
	    call tbegpb (tp, cptr, offset, rownum, bbuf)
	    if (bbuf)
		buffer = YES
	    else
		buffer = NO
	default:
	    if (dtype < 0 || dtype == TY_CHAR) {
		call tbegpt (tp, cptr, offset, rownum, cbuf, SZ_FNAME)
		call sscan (cbuf)
		    call gargd (dbuf)
		if (nscan() < 1)
		    buffer = INDEFI
		else if (IS_INDEFD(dbuf) || abs (dbuf) > MAX_INT)
		    buffer = INDEFI
		else
		    buffer = nint (dbuf)
	    } else {
		call error (ER_TBCOLBADTYP,
			"tbegti:  bad data type; table or memory corrupted?")
	    }
	}
end

procedure tbegts (tp, cptr, selrow, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
int	selrow			# i: row number (or selected row number)
short	buffer			# o: buffer for value to be gotten
#--
int	rownum			# actual row number
long	offset			# offset in char to location for reading
int	dtype			# data type of column
int	nret
# buffers for copying elements of various data types
char	cbuf[SZ_FNAME]
double	dbuf
real	rbuf
int	ibuf
bool	bbuf
long	tbeoff()
int	tbfags()
int	nscan()
errchk	tbsirow, tbegpb, tbegpd, tbegpi, tbegps, tbegpr, tbegpt, tbfags, tbzgts

begin
	call tbsirow (tp, selrow, rownum)

	if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzgts (tp, cptr, rownum, buffer)
	    return
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    nret = tbfags (tp, cptr, rownum, buffer, 1, 1)
	    return
	}

	offset = tbeoff (tp, cptr, rownum, 1)

	dtype = COL_DTYPE(cptr)
	switch (dtype) {
	case TBL_TY_REAL:
	    call tbegpr (tp, cptr, offset, rownum, rbuf)
	    if (IS_INDEFR(rbuf) || (abs (rbuf) > MAX_SHORT))
		buffer = INDEFS
	    else
		buffer = nint (rbuf)
	case TBL_TY_DOUBLE:
	    call tbegpd (tp, cptr, offset, rownum, dbuf)
	    if (TBL_IS_INDEFD (dbuf) || abs (dbuf) > MAX_SHORT)
		buffer = INDEFS
	    else
		buffer = nint (dbuf)
	case TBL_TY_INT:
	    call tbegpi (tp, cptr, offset, rownum, ibuf)
	    if (IS_INDEFI(ibuf) || (abs (ibuf) > MAX_SHORT))
		buffer = INDEFS
	    else
		buffer = ibuf
	case TBL_TY_SHORT:
	    call tbegps (tp, cptr, offset, rownum, buffer)
	case TBL_TY_BOOL:
	    call tbegpb (tp, cptr, offset, rownum, bbuf)
	    if (bbuf)
		buffer = YES
	    else
		buffer = NO
	default:
	    if (dtype < 0 || dtype == TY_CHAR) {
		call tbegpt (tp, cptr, offset, rownum, cbuf, SZ_FNAME)
		call sscan (cbuf)
		    call gargd (dbuf)
		if (nscan() < 1)
		    buffer = INDEFS
		else if (IS_INDEFD(dbuf) || abs (dbuf) > MAX_SHORT)
		    buffer = INDEFS
		else
		    buffer = nint (dbuf)
	    } else {
		call error (ER_TBCOLBADTYP,
			"tbegts:  bad data type; table or memory corrupted?")
	    }
	}
end

procedure tbegtb (tp, cptr, selrow, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
int	selrow			# i: row number (or selected row number)
bool	buffer			# o: buffer for value to be gotten
#--
int	rownum			# actual row number
long	offset			# offset in char to location for reading
int	dtype			# data type of column
int	nret
# buffers for copying elements of various data types
char	cbuf[SZ_FNAME]
double	dbuf
real	rbuf
int	ibuf
short	sbuf
long	tbeoff()
int	tbfagb()
int	nscan()
errchk	tbsirow, tbegpb, tbegpd, tbegpi, tbegps, tbegpr, tbegpt, tbfagb, tbzgtb

begin
	call tbsirow (tp, selrow, rownum)

	if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzgtb (tp, cptr, rownum, buffer)
	    return
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    nret = tbfagb (tp, cptr, rownum, buffer, 1, 1)
	    return
	}

	offset = tbeoff (tp, cptr, rownum, 1)

	dtype = COL_DTYPE(cptr)
	switch (dtype) {
	case TBL_TY_REAL:
	    call tbegpr (tp, cptr, offset, rownum, rbuf)
	    if (IS_INDEFR(rbuf) || (nint (rbuf) == NO))
		buffer = false
	    else
		buffer = true
	case TBL_TY_DOUBLE:
	    call tbegpd (tp, cptr, offset, rownum, dbuf)
	    if (TBL_IS_INDEFD (dbuf) || (nint (dbuf) == NO))
		buffer = false
	    else
		buffer = true
	case TBL_TY_INT:
	    call tbegpi (tp, cptr, offset, rownum, ibuf)
	    if (IS_INDEFI(ibuf) || (ibuf == NO))
		buffer = false
	    else
		buffer = true
	case TBL_TY_SHORT:
	    call tbegps (tp, cptr, offset, rownum, sbuf)
	    if (IS_INDEFS(sbuf) || (sbuf == NO))
		buffer = false
	    else
		buffer = true
	case TBL_TY_BOOL:
	    call tbegpb (tp, cptr, offset, rownum, buffer)
	default:
	    if (dtype < 0 || dtype == TY_CHAR) {
		call tbegpt (tp, cptr, offset, rownum, cbuf, SZ_FNAME)
		call sscan (cbuf)
		    call gargb (buffer)
		if (nscan() < 1)
		    buffer = false
	    } else {
		call error (ER_TBCOLBADTYP,
			"tbegtb:  bad data type; table or memory corrupted?")
	    }
	}
end

procedure tbegtt (tp, cptr, selrow, buffer, maxch)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
int	selrow			# i: row number (or selected row number)
char	buffer[ARB]		# o: buffer for value to be gotten
int	maxch			# i: max number of char in output string
#--
int	rownum			# actual row number
long	offset			# offset in char to location for reading
int	dtype			# data type of column
int	nret
# buffers for copying elements of various data types
double	dbuf
real	rbuf
int	ibuf
short	sbuf
bool	bbuf
long	tbeoff()
int	tbfagt()
errchk	tbsirow, tbegpb, tbegpd, tbegpi, tbegps, tbegpr, tbegpt, tbfagt, tbzgtt

begin
	call tbsirow (tp, selrow, rownum)

	if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    call tbzgtt (tp, cptr, rownum, buffer, maxch)
	    return
	} else if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    nret = tbfagt (tp, cptr, rownum, buffer, maxch, 1, 1)
	    return
	}

	offset = tbeoff (tp, cptr, rownum, 1)

	dtype = COL_DTYPE(cptr)
	switch (dtype) {
	case TBL_TY_REAL:
	    call tbegpr (tp, cptr, offset, rownum, rbuf)
	    call sprintf (buffer, maxch, COL_FMT(cptr))
		call pargr (rbuf)
	case TBL_TY_DOUBLE:
	    call tbegpd (tp, cptr, offset, rownum, dbuf)
	    if (TBL_IS_INDEFD (dbuf)) {
		call strcpy ("INDEF", buffer, maxch)
	    } else {
		call sprintf (buffer, maxch, COL_FMT(cptr))
		    call pargd (dbuf)
	    }
	case TBL_TY_INT:
	    call tbegpi (tp, cptr, offset, rownum, ibuf)
	    call sprintf (buffer, maxch, COL_FMT(cptr))
		call pargi (ibuf)
	case TBL_TY_SHORT:
	    call tbegps (tp, cptr, offset, rownum, sbuf)
	    call sprintf (buffer, maxch, COL_FMT(cptr))
		call pargs (sbuf)
	case TBL_TY_BOOL:
	    call tbegpb (tp, cptr, offset, rownum, bbuf)
	    call sprintf (buffer, maxch, COL_FMT(cptr))
		call pargb (bbuf)
	default:
	    if (dtype < 0 || dtype == TY_CHAR) {
		call tbegpt (tp, cptr, offset, rownum, buffer, maxch)
	    } else {
		call error (ER_TBCOLBADTYP,
			"tbegtt:  bad data type; table or memory corrupted?")
	    }
	}
end
