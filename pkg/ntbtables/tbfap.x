include "tbtables.h"

# tbfap[tbirds] -- put an array of elements to a FITS table
#
# Phil Hodge,  6-Jul-1995  Subroutine created
# Phil Hodge, 29-Jul-1997  Call tbfwer to create new rows and set to indef.
# Phil Hodge,  3-Mar-1998  Remove calls to tbfwer, since tbswer is called
#			in higher-level routines.
# Phil Hodge, 19-Mar-1999  In tbfapt, there was a missing "j = j + 1" in
#			the section for column data type short.
# Phil Hodge, 27-Aug-2002  In tbfapt, include an explicit test for INDEF,
#			rather than relying on 'nscan() < 1'.

# tbfapd -- put double-precision elements

procedure tbfapd (tp, cp, rownum, buffer, first, nelem)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
int	rownum		# i: row number
double	buffer[ARB]	# i: buffer for values to be written
int	first		# i: number of first array element to write
int	nelem		# i: maximum number of elements to write
#--
pointer sp
pointer cbuf		# for writing to a string
bool	bbuf
bool	anyf		# true if any input value is INDEF
int	status		# zero is OK
int	i, j
errchk	tbferr

begin
	status = 0

	if (COL_DTYPE(cp) < 0) {		# text string

	    call smark (sp)
	    call salloc (cbuf, SZ_FNAME, TY_CHAR)
	    j = first
	    do i = 1, nelem {
		if (IS_INDEFD(buffer[i])) {
		    call fspclu (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, status)
		} else {
		    call sprintf (Memc[cbuf], SZ_FNAME, "%-25.16g")
			call pargd (buffer[i])
		    call fspcls (TB_FILE(tp), COL_NUMBER(cp), rownum, j, 1,
				Memc[cbuf], SZ_FNAME, status)
		}
		j = j + 1
	    }
	    call sfree (sp)

	} else if (COL_DTYPE(cp) == TBL_TY_BOOL) {

	    j = first
	    do i = 1, nelem {
		if (IS_INDEFD(buffer[i])) {
		    call fspclu (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, status)
		} else {
		    bbuf = (buffer[i] != 0.d0)
		    call fspcll (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, bbuf, status)
		}
		j = j + 1
	    }

	} else {

	    # FITSIO should be able to do any other type conversion.

	    # Check for INDEF values.
	    anyf = false
	    do i = 1, nelem {
		if (IS_INDEFD(buffer[i])) {
		    anyf = true
		    break
		}
	    }

	    if (anyf) {
		# Check each element as we go.
		j = first
		do i = 1, nelem {
		    if (IS_INDEFD(buffer[i])) {
			call fspclu (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, status)
		    } else {
			call fspcld (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, buffer[i], status)
		    }
		    j = j + 1
		}
	    } else {
		# No INDEFs; write the entire array.
		call fspcld (TB_FILE(tp), COL_NUMBER(cp), rownum, first, nelem,
			buffer, status)
	    }
	}

	if (status != 0)
	    call tbferr (status)

	TB_NROWS(tp) = max (TB_NROWS(tp), rownum)
end

# tbfapr -- put single-precision elements

procedure tbfapr (tp, cp, rownum, buffer, first, nelem)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
int	rownum		# i: row number
real	buffer[ARB]	# i: buffer for values to be written
int	first		# i: number of first array element to write
int	nelem		# i: maximum number of elements to write
#--
pointer sp
pointer cbuf		# for writing to a string
bool	bbuf
bool	anyf		# true if any input value is INDEF
int	status		# zero is OK
int	i, j
errchk	tbferr

begin
	status = 0

	if (COL_DTYPE(cp) < 0) {		# text string

	    call smark (sp)
	    call salloc (cbuf, SZ_FNAME, TY_CHAR)
	    j = first
	    do i = 1, nelem {
		if (IS_INDEFR(buffer[i])) {
		    call fspclu (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, status)
		} else {
		    call sprintf (Memc[cbuf], SZ_FNAME, "%-15.7g")
			call pargr (buffer[i])
		    call fspcls (TB_FILE(tp), COL_NUMBER(cp), rownum, j, 1,
				Memc[cbuf], SZ_FNAME, status)
		}
		j = j + 1
	    }
	    call sfree (sp)

	} else if (COL_DTYPE(cp) == TBL_TY_BOOL) {

	    j = first
	    do i = 1, nelem {
		if (IS_INDEFR(buffer[i])) {
		    call fspclu (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, status)
		} else {
		    bbuf = (buffer[i] != 0.)
		    call fspcll (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, bbuf, status)
		}
		j = j + 1
	    }

	} else {

	    # Check for INDEF values.
	    anyf = false
	    do i = 1, nelem {
		if (IS_INDEFR(buffer[i])) {
		    anyf = true
		    break
		}
	    }

	    if (anyf) {
		j = first
		do i = 1, nelem {
		    if (IS_INDEFR(buffer[i])) {
			call fspclu (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, status)
		    } else {
			call fspcle (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, buffer[i], status)
		    }
		    j = j + 1
		}
	    } else {
		call fspcle (TB_FILE(tp), COL_NUMBER(cp), rownum, first, nelem,
			buffer, status)
	    }
	}

	if (status != 0)
	    call tbferr (status)

	TB_NROWS(tp) = max (TB_NROWS(tp), rownum)
end

# tbfapi -- put an integer element

procedure tbfapi (tp, cp, rownum, buffer, first, nelem)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
int	rownum		# i: row number
int	buffer[ARB]	# i: buffer for values to be written
int	first		# i: number of first array element to write
int	nelem		# i: maximum number of elements to write
#--
pointer sp
pointer cbuf		# for writing to a string
bool	bbuf
bool	anyf		# true if any input value is INDEF
int	status		# zero is OK
int	i, j
errchk	tbferr

begin
	status = 0

	if (COL_DTYPE(cp) < 0) {		# text string

	    call smark (sp)
	    call salloc (cbuf, SZ_FNAME, TY_CHAR)
	    j = first
	    do i = 1, nelem {
		if (IS_INDEFI(buffer[i])) {
		    call fspclu (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, status)
		} else {
		    call sprintf (Memc[cbuf], SZ_FNAME, "%-10d")
			call pargi (buffer[i])
		    call fspcls (TB_FILE(tp), COL_NUMBER(cp), rownum, j, 1,
				Memc[cbuf], SZ_FNAME, status)
		}
		j = j + 1
	    }
	    call sfree (sp)

	} else if (COL_DTYPE(cp) == TBL_TY_BOOL) {

	    j = first
	    do i = 1, nelem {
		if (IS_INDEFI(buffer[i])) {
		    call fspclu (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, status)
		} else {
		    bbuf = (buffer[i] != 0)
		    call fspcll (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, bbuf, status)
		}
		j = j + 1
	    }

	} else {

	    # Check for INDEF values.
	    anyf = false
	    do i = 1, nelem {
		if (IS_INDEFI(buffer[i])) {
		    anyf = true
		    break
		}
	    }

	    if (anyf) {
		j = first
		do i = 1, nelem {
		    if (IS_INDEFI(buffer[i])) {
			call fspclu (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, status)
		    } else {
			call fspclj (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, buffer[i], status)
		    }
		    j = j + 1
		}
	    } else {
		call fspclj (TB_FILE(tp), COL_NUMBER(cp), rownum, first, nelem,
			buffer, status)
	    }
	}

	if (status != 0)
	    call tbferr (status)

	TB_NROWS(tp) = max (TB_NROWS(tp), rownum)
end

# tbfaps -- put short integer elements

procedure tbfaps (tp, cp, rownum, buffer, first, nelem)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
int	rownum		# i: row number
short	buffer[ARB]	# i: buffer for values to be written
int	first		# i: number of first array element to write
int	nelem		# i: maximum number of elements to write
#--
pointer sp
pointer cbuf		# for writing to a string
bool	bbuf
bool	anyf		# true if any input value is INDEF
int	status		# zero is OK
int	i, j
errchk	tbferr

begin
	status = 0

	if (COL_DTYPE(cp) < 0) {		# text string

	    call smark (sp)
	    call salloc (cbuf, SZ_FNAME, TY_CHAR)
	    j = first
	    do i = 1, nelem {
		if (IS_INDEFS(buffer[i])) {
		    call fspclu (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, status)
		} else {
		    call sprintf (Memc[cbuf], SZ_FNAME, "%-10d")
			call pargs (buffer[i])
		    call fspcls (TB_FILE(tp), COL_NUMBER(cp), rownum, j, 1,
				Memc[cbuf], SZ_FNAME, status)
		}
		j = j + 1
	    }
	    call sfree (sp)

	} else if (COL_DTYPE(cp) == TBL_TY_BOOL) {

	    j = first
	    do i = 1, nelem {
		if (IS_INDEFS(buffer[i])) {
		    call fspclu (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, status)
		} else {
		    bbuf = (buffer[i] != 0)
		    call fspcll (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, bbuf, status)
		}
		j = j + 1
	    }

	} else {

	    # Check for INDEF values.
	    anyf = false
	    do i = 1, nelem {
		if (IS_INDEFS(buffer[i])) {
		    anyf = true
		    break
		}
	    }

	    if (anyf) {
		j = first
		do i = 1, nelem {
		    if (IS_INDEFS(buffer[i])) {
			call fspclu (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, status)
		    } else {
			call fspcli (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, buffer[i], status)
		    }
		    j = j + 1
		}
	    } else {
		call fspcli (TB_FILE(tp), COL_NUMBER(cp), rownum, first, nelem,
			buffer, status)
	    }
	}

	if (status != 0)
	    call tbferr (status)

	TB_NROWS(tp) = max (TB_NROWS(tp), rownum)
end

# tbfapb -- put boolean elements

procedure tbfapb (tp, cp, rownum, buffer, first, nelem)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
int	rownum		# i: row number
bool	buffer[ARB]	# i: buffer for values to be written
int	first		# i: number of first array element to write
int	nelem		# i: maximum number of elements to write
#--
pointer sp
pointer cbuf		# for writing to a string
double	dbuf
int	status		# zero is OK
int	i, j
errchk	tbferr

begin
	status = 0

	if (COL_DTYPE(cp) < 0) {		# text string

	    call smark (sp)
	    call salloc (cbuf, SZ_FNAME, TY_CHAR)
	    j = first
	    do i = 1, nelem {
		if (buffer[i])
		    call strcpy ("yes", Memc[cbuf], SZ_FNAME)
		else
		    call strcpy ("no", Memc[cbuf], SZ_FNAME)
		call fspcls (TB_FILE(tp), COL_NUMBER(cp), rownum, j, 1,
				Memc[cbuf], SZ_FNAME, status)
		j = j + 1
	    }
	    call sfree (sp)

	} else if (COL_DTYPE(cp) == TBL_TY_BOOL) {

	    call fspcll (TB_FILE(tp), COL_NUMBER(cp), rownum, first, nelem,
			buffer, status)

	} else {

	    # FITSIO should be able to do any other type conversion.
	    j = first
	    do i = 1, nelem {
		if (buffer[i])
		    dbuf = 1.d0
		else
		    dbuf = 0.d0
		call fspcld (TB_FILE(tp), COL_NUMBER(cp), rownum, j, 1,
			dbuf, status)
		j = j + 1
	    }
	}

	if (status != 0)
	    call tbferr (status)

	TB_NROWS(tp) = max (TB_NROWS(tp), rownum)
end

# tbfapt -- put text-string elements

procedure tbfapt (tp, cp, rownum, cbuf, maxch, first, nelem)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
int	rownum		# i: row number
char	cbuf[maxch,ARB]	# i: buffer for values to be written
int	maxch		# i: max number of char in input string
int	first		# i: number of first array element to write
int	nelem		# i: maximum number of elements to write
#--
int	status		# zero is OK
int	i, j		# loop indexes
int	nscan()

# The following are for putting non-text type values
double	dbuf
real	rbuf
int	ibuf
short	sbuf
bool	bbuf

errchk	tbferr

begin
	status = 0

	if (COL_DTYPE(cp) < 0) {

	    call fspcls (TB_FILE(tp), COL_NUMBER(cp), rownum, first, nelem,
		cbuf, maxch, status)

	} else {

	    j = first				# initial value for loop on i

	    switch (COL_DTYPE(cp)) {
	    case TBL_TY_REAL:
		do i = 1, nelem {
		    call sscan (cbuf[1,i])
			call gargr (rbuf)
		    if (nscan() < 1 || IS_INDEF(rbuf)) {
			call fspclu (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, status)
		    } else {
			call fspcle (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, rbuf, status)
		    }
		    j = j + 1
		}
	    case TBL_TY_DOUBLE:
		do i = 1, nelem {
		    call sscan (cbuf[1,i])
			call gargd (dbuf)
		    if (nscan() < 1 || IS_INDEFD(dbuf)) {
			call fspclu (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, status)
		    } else {
			call fspcld (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, dbuf, status)
		    }
		    j = j + 1
		}
	    case TBL_TY_INT:
		do i = 1, nelem {
		    call sscan (cbuf[1,i])
			call gargi (ibuf)
		    if (nscan() < 1 || IS_INDEFI(ibuf)) {
			call fspclu (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, status)
		    } else {
			call fspclj (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, ibuf, status)
		    }
		    j = j + 1
		}
	    case TBL_TY_SHORT:
		do i = 1, nelem {
		    call sscan (cbuf[1,i])
			call gargs (sbuf)
		    if (nscan() < 1 || IS_INDEFS(sbuf)) {
			call fspclu (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, status)
		    } else {
			call fspcli (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, sbuf, status)
		    }
		    j = j + 1
		}
	    case TBL_TY_BOOL:
		do i = 1, nelem {
		    call sscan (cbuf[1,i])
			call gargb (bbuf)
		    if (nscan() < 1) {
			call fspclu (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, status)
		    } else {
			call fspcll (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, bbuf, status)
		    }
		    j = j + 1
		}
	    default:
		call error (1, "bad data type in table")
	    }
	}

	if (status != 0)
	    call tbferr (status)

	TB_NROWS(tp) = max (TB_NROWS(tp), rownum)
end
