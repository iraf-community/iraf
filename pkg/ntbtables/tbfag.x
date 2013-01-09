include <tbset.h>
include "tbtables.h"

# tbfag[tbirds] -- get an array of elements from a FITS table
#
# Phil Hodge,  6-Jul-1995  Subroutine created
# Phil Hodge, 14-Apr-1998  Use COL_FMT directly, instead of calling tbcftg.
# Phil Hodge, 18-Jun-1998  Use fsgcfl instead of fsgcl to get boolean.
# Phil Hodge, 19-Mar-1999  Don't try to get nelem elements if there are
#	not that many in the array, starting at first; get nret instead.
# Phil Hodge,  5-Aug-1999  Use COL_NELEM instead of tbalen to get array length.

# tbfagd -- get double-precision elements

int procedure tbfagd (tp, cp, rownum, buffer, first, nelem)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
int	rownum		# i: row number
double	buffer[ARB]	# o: buffer for values to be gotten
int	first		# i: number of first array element to read
int	nelem		# i: maximum number of elements to read
#--
pointer sp
pointer cbuf		# for getting string
bool	bbuf		# for getting boolean
bool	flagvals	# set to true if the value is undefined
int	i, j		# loop indexes
int	status		# zero is OK
double	nulval		# INDEFD
bool	anyf		# set to true if any value is undefined
int	ntotal		# total number of elements in array
int	nret		# actual number of elements to read
int	nscan()
errchk	tbferr

begin
	status = 0

	ntotal = COL_NELEM(cp)
	nret = min (nelem, ntotal-first+1)

	if (COL_DTYPE(cp) < 0) {		# text string

	    call smark (sp)
	    call salloc (cbuf, SZ_LINE, TY_CHAR)
	    j = first
	    do i = 1, nret {
		call fsgcvs (TB_FILE(tp), COL_NUMBER(cp), rownum,
			    j, 1, "", Memc[cbuf], SZ_LINE, anyf, status)
		call sscan (Memc[cbuf])
		    call gargd (buffer[i])
		if (nscan() < 1)
		    buffer[i] = INDEFD
		j = j + 1
	    }
	    call sfree (sp)

	} else if (COL_DTYPE(cp) == TBL_TY_BOOL) {

	    j = first
	    do i = 1, nret {
		call fsgcfl (TB_FILE(tp), COL_NUMBER(cp), rownum, j, 1,
			bbuf, flagvals, anyf, status)
		if (flagvals)
		    buffer[i] = INDEFD
		else if (bbuf)
		    buffer[i] = 1.d0
		else
		    buffer[i] = 0.d0
		j = j + 1
	    }

	} else {

	    # FITSIO should be able to do any other type conversion.
	    nulval = INDEFD
	    call fsgcvd (TB_FILE(tp), COL_NUMBER(cp), rownum, first, nret,
			nulval, buffer, anyf, status)
	}

	if (status != 0)
	    call tbferr (status)

	return (nret)
end

# tbfagr -- get single-precision elements

int procedure tbfagr (tp, cp, rownum, buffer, first, nelem)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
int	rownum		# i: row number
real	buffer[ARB]	# o: buffer for values to be gotten
int	first		# i: number of first array element to read
int	nelem		# i: maximum number of elements to read
#--
pointer sp
pointer cbuf		# for getting string
bool	bbuf		# for getting boolean
bool	flagvals	# set to true if the value is undefined
int	i, j		# loop indexes
int	status		# zero is OK
real	nulval		# INDEFR
bool	anyf		# set to true if any value is undefined
int	ntotal		# total number of elements in array
int	nret		# actual number of elements to read
int	nscan()
errchk	tbferr

begin
	status = 0

	ntotal = COL_NELEM(cp)
	nret = min (nelem, ntotal-first+1)

	if (COL_DTYPE(cp) < 0) {		# text string

	    call smark (sp)
	    call salloc (cbuf, SZ_LINE, TY_CHAR)
	    j = first
	    do i = 1, nret {
		call fsgcvs (TB_FILE(tp), COL_NUMBER(cp), rownum,
			    j, 1, "", Memc[cbuf], SZ_LINE, anyf, status)
		call sscan (Memc[cbuf])
		    call gargr (buffer[i])
		if (nscan() < 1)
		    buffer[i] = INDEFR
		j = j + 1
	    }
	    call sfree (sp)

	} else if (COL_DTYPE(cp) == TBL_TY_BOOL) {

	    j = first
	    do i = 1, nret {
		call fsgcfl (TB_FILE(tp), COL_NUMBER(cp), rownum, j, 1,
			bbuf, flagvals, anyf, status)
		if (flagvals)
		    buffer[i] = INDEFR
		else if (bbuf)
		    buffer[i] = 1.
		else
		    buffer[i] = 0.
		j = j + 1
	    }

	} else {

	    nulval = INDEFR
	    call fsgcve (TB_FILE(tp), COL_NUMBER(cp), rownum, first, nret,
			nulval, buffer, anyf, status)
	}

	if (status != 0)
	    call tbferr (status)

	return (nret)
end

# tbfagi -- get an integer element

int procedure tbfagi (tp, cp, rownum, buffer, first, nelem)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
int	rownum		# i: row number
int	buffer[ARB]	# o: buffer for values to be gotten
int	first		# i: number of first array element to read
int	nelem		# i: maximum number of elements to read
#--
pointer sp
pointer cbuf		# for getting string
bool	bbuf		# for getting boolean
bool	flagvals	# set to true if the value is undefined
int	i, j		# loop indexes
int	status		# zero is OK
int	nulval		# INDEFI
bool	anyf		# set to true if any value is undefined
int	ntotal		# total number of elements in array
int	nret		# actual number of elements to read
int	nscan()
errchk	tbferr

begin
	status = 0

	ntotal = COL_NELEM(cp)
	nret = min (nelem, ntotal-first+1)

	if (COL_DTYPE(cp) < 0) {		# text string

	    call smark (sp)
	    call salloc (cbuf, SZ_LINE, TY_CHAR)
	    j = first
	    do i = 1, nret {
		call fsgcvs (TB_FILE(tp), COL_NUMBER(cp), rownum,
			    j, 1, "", Memc[cbuf], SZ_LINE, anyf, status)
		call sscan (Memc[cbuf])
		    call gargi (buffer[i])
		if (nscan() < 1)
		    buffer[i] = INDEFI
		j = j + 1
	    }
	    call sfree (sp)

	} else if (COL_DTYPE(cp) == TBL_TY_BOOL) {

	    j = first
	    do i = 1, nret {
		call fsgcfl (TB_FILE(tp), COL_NUMBER(cp), rownum, j, 1,
			bbuf, flagvals, anyf, status)
		if (flagvals)
		    buffer[i] = INDEFI
		else if (bbuf)
		    buffer[i] = 1
		else
		    buffer[i] = 0
		j = j + 1
	    }

	} else {

	    nulval = INDEFI
	    call fsgcvj (TB_FILE(tp), COL_NUMBER(cp), rownum, first, nret,
			nulval, buffer, anyf, status)
	}

	if (status != 0)
	    call tbferr (status)

	return (nret)
end

# tbfags -- get short integer elements

int procedure tbfags (tp, cp, rownum, buffer, first, nelem)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
int	rownum		# i: row number
short	buffer[ARB]	# o: buffer for values to be gotten
int	first		# i: number of first array element to read
int	nelem		# i: maximum number of elements to read
#--
pointer sp
pointer cbuf		# for getting string
bool	bbuf		# for getting boolean
bool	flagvals	# set to true if the value is undefined
int	i, j		# loop indexes
int	status		# zero is OK
short	nulval		# INDEFS
bool	anyf		# set to true if any value is undefined
int	ntotal		# total number of elements in array
int	nret		# actual number of elements to read
int	nscan()
errchk	tbferr

begin
	status = 0

	ntotal = COL_NELEM(cp)
	nret = min (nelem, ntotal-first+1)

	if (COL_DTYPE(cp) < 0) {		# text string

	    call smark (sp)
	    call salloc (cbuf, SZ_LINE, TY_CHAR)
	    j = first
	    do i = 1, nret {
		call fsgcvs (TB_FILE(tp), COL_NUMBER(cp), rownum,
			    j, 1, "", Memc[cbuf], SZ_LINE, anyf, status)
		call sscan (Memc[cbuf])
		    call gargs (buffer[i])
		if (nscan() < 1)
		    buffer[i] = INDEFS
		j = j + 1
	    }
	    call sfree (sp)

	} else if (COL_DTYPE(cp) == TBL_TY_BOOL) {

	    j = first
	    do i = 1, nret {
		call fsgcfl (TB_FILE(tp), COL_NUMBER(cp), rownum, j, 1,
			bbuf, flagvals, anyf, status)
		if (flagvals)
		    buffer[i] = INDEFS
		else if (bbuf)
		    buffer[i] = 1
		else
		    buffer[i] = 0
		j = j + 1
	    }

	} else {

	    nulval = INDEFS
	    call fsgcvi (TB_FILE(tp), COL_NUMBER(cp), rownum, first, nret,
			nulval, buffer, anyf, status)
	}

	if (status != 0)
	    call tbferr (status)

	return (nret)
end

# tbfagb -- get boolean elements

int procedure tbfagb (tp, cp, rownum, buffer, first, nelem)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
int	rownum		# i: row number
bool	buffer[ARB]	# o: buffer for values to be gotten
int	first		# i: number of first array element to read
int	nelem		# i: maximum number of elements to read
#--
pointer sp
pointer cbuf		# for getting string
pointer flags		# scratch for array of null flags
double	dbuf
double	nulval		# INDEFD
int	i, j		# loop indexes
int	status		# zero is OK
bool	anyf		# set to true if any value is undefined
int	ntotal		# total number of elements in array
int	nret		# actual number of elements to read
bool	streq()
errchk	tbferr

begin
	status = 0

	ntotal = COL_NELEM(cp)
	nret = min (nelem, ntotal-first+1)

	if (COL_DTYPE(cp) < 0) {		# text string

	    call smark (sp)
	    call salloc (cbuf, SZ_LINE, TY_CHAR)
	    j = first
	    do i = 1, nret {
		call fsgcvs (TB_FILE(tp), COL_NUMBER(cp), rownum,
			j, 1, "", Memc[cbuf], SZ_LINE, anyf, status)
		call strlwr (Memc[cbuf])
		if (streq (Memc[cbuf], "yes") || streq (Memc[cbuf], "y") ||
		    streq (Memc[cbuf], "true") || streq (Memc[cbuf], "t"))
		    buffer[i] = true
		else
		    buffer[i] = false
		j = j + 1
	    }
	    call sfree (sp)

	} else if (COL_DTYPE(cp) == TBL_TY_BOOL) {

	    call smark (sp)
	    call salloc (flags, nret, TY_CHAR)
	    do i = 1, nret
		buffer[i] = false
	    call fsgcfl (TB_FILE(tp), COL_NUMBER(cp), rownum, first, nret,
			buffer, Memc[flags], anyf, status)
	    # We can't actually use Memc[flags] because bool has no INDEF.
	    call sfree (sp)

	} else {

	    nulval = INDEFD
	    j = first
	    do i = 1, nret {
		call fsgcvd (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, nulval, dbuf, anyf, status)
		if (anyf)
		    buffer[i] = false
		else
		    buffer[i] = (dbuf != 0.d0)
		j = j + 1
	    }
	}

	if (status != 0)
	    call tbferr (status)

	return (nret)
end

# tbfagt -- get text-string elements

int procedure tbfagt (tp, cp, rownum, cbuf, maxch, first, nelem)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
int	rownum		# i: row number
char	cbuf[maxch,ARB]	# o: buffer for values to be gotten
int	maxch		# i: max number of char in output string
int	first		# i: number of first array element to read
int	nelem		# i: maximum number of elements to read
#--
int	status		# zero is OK
int	i, j		# loop indexes
bool	anyf		# set to true if any value is undefined
int	ntotal		# total number of elements in array
int	nret		# actual number of elements to read
# The following are for getting non-text type values and converting to text
double	dbuf
double	dnulval		# INDEFD
real	rbuf
real	rnulval		# INDEFR
int	ibuf
int	inulval		# INDEFI
short	sbuf
short	snulval		# INDEFS
bool	bbuf
bool	flagvals	# set to true if the value is undefined
errchk	tbferr

begin
	status = 0

	ntotal = COL_NELEM(cp)
	nret = min (nelem, ntotal-first+1)

	if (COL_DTYPE(cp) < 0) {		# text-string column?

	    call fsgcvs (TB_FILE(tp), COL_NUMBER(cp), rownum, first, nret,
		"", cbuf, maxch, anyf, status)

	} else {

	    # Not a text-string column.  Get the value and sprintf it.

	    j = first

	    switch (COL_DTYPE(cp)) {
	    case TBL_TY_DOUBLE:
		dnulval = INDEFD
		do i = 1, nret {
		    call fsgcvd (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, dnulval, dbuf, anyf, status)
		    call sprintf (cbuf[1,i], maxch, COL_FMT(cp))
			call pargd (dbuf)
		    j = j + 1
		}
	    case TBL_TY_REAL:
		rnulval = INDEFR
		do i = 1, nret {
		    call fsgcve (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, rnulval, rbuf, anyf, status)
		    call sprintf (cbuf[1,i], maxch, COL_FMT(cp))
			call pargr (rbuf)
		    j = j + 1
		}
	    case TBL_TY_INT:
		inulval = INDEFI
		do i = 1, nret {
		    call fsgcvj (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, inulval, ibuf, anyf, status)
		    call sprintf (cbuf[1,i], maxch, COL_FMT(cp))
			call pargi (ibuf)
		    j = j + 1
		}
	    case TBL_TY_SHORT:
		snulval = INDEFS
		do i = 1, nret {
		    call fsgcvi (TB_FILE(tp), COL_NUMBER(cp), rownum,
				j, 1, snulval, sbuf, anyf, status)
		    call sprintf (cbuf[1,i], maxch, COL_FMT(cp))
			call pargs (sbuf)
		    j = j + 1
		}
	    case TBL_TY_BOOL:
		do i = 1, nret {
		    call fsgcfl (TB_FILE(tp), COL_NUMBER(cp), rownum, j, 1,
				bbuf, flagvals, anyf, status)
		    if (flagvals) {
			call strcpy ("INDEF", cbuf[1,i], maxch)
		    } else {
			call sprintf (cbuf[1,i], maxch, COL_FMT(cp))
			    call pargb (bbuf)
		    }
		    j = j + 1
		}
	    default:
		call error (1, "bad data type in table")
	    }
	}
	if (status != 0)
	    call tbferr (status)

	return (nret)
end
