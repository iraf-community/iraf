include <mach.h>		# for MAX_INT and MAX_SHORT
include <tbset.h>
include "tbtables.h"

# tbzgt[tbirds] -- get a single element
# This procedure gets a single element from an internal buffer corresponding
# to a value in a text file.
#
# Phil Hodge, 14-Jan-1992  Subroutines created.
# Phil Hodge, 10-Feb-1993  Change "NO" to "false" in tbzgtb.
# Phil Hodge, 31-Mar-1993  Include short datatype.
# Phil Hodge, 12-Aug-1993  Use ctol instead of ctoi to allow leading "+" sign.
# Phil Hodge, 14-Apr-1998  Use COL_FMT directly, instead of calling tbcftg.

procedure tbzgtb (tp, cp, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
int	rownum			# i: row number
bool	buffer			# o: buffer for value to be gotten
#--
pointer sp
pointer cbuf			# buffer for copying character elements
int	lenstr			# length of a string table element
int	ip			# offset for extracting a string in Memc
int	ctowrd()
bool	streq()

begin
	if (COL_DTYPE(cp) == TBL_TY_DOUBLE) {
	    buffer = (nint (Memd[COL_OFFSET(cp) + rownum - 1]) != NO)

	} else if (COL_DTYPE(cp) == TBL_TY_INT) {
	    buffer = (Memi[COL_OFFSET(cp) + rownum - 1] != NO)

	} else {				# string
	    call smark (sp)
	    call salloc (cbuf, SZ_FNAME, TY_CHAR)
	    lenstr = -COL_DTYPE(cp) + 1		# one for EOS
	    ip = (rownum - 1) * lenstr + 1
	    if (ctowrd (Memc[COL_OFFSET(cp)], ip, Memc[cbuf], SZ_FNAME) < 1) {
		buffer = false			# bug fix 10-Feb-1993 PEH
	    } else {
		call strlwr (Memc[cbuf])
		buffer = streq (Memc[cbuf], "yes") ||
			 streq (Memc[cbuf], "true")
	    }
	    call sfree (sp)
	}
end

procedure tbzgtd (tp, cp, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
int	rownum			# i: row number
double	buffer			# o: buffer for value to be gotten
#--
int	ival			# buffer for integer value
int	lenstr			# length of a string table element
int	ip			# offset for extracting a string in Memc
int	ctod()

begin
	if (COL_DTYPE(cp) == TBL_TY_DOUBLE) {
	    buffer = Memd[COL_OFFSET(cp) + rownum - 1]

	} else if (COL_DTYPE(cp) == TBL_TY_INT) {
	    ival = Memi[COL_OFFSET(cp) + rownum - 1]
	    if (IS_INDEFI(ival))
		buffer = INDEFD
	    else
		buffer = ival

	} else {				# string
	    lenstr = -COL_DTYPE(cp) + 1		# one for EOS
	    ip = (rownum - 1) * lenstr + 1
	    if (ctod (Memc[COL_OFFSET(cp)], ip, buffer) < 1)
		buffer = INDEFD
	}
end

procedure tbzgtr (tp, cp, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
int	rownum			# i: row number
real	buffer			# o: buffer for value to be gotten
#--
double	dval			# buffer for double precision
int	ival			# buffer for integer value
int	lenstr			# length of a string table element
int	ip			# offset for extracting a string in Memc
int	ctor()

begin
	if (COL_DTYPE(cp) == TBL_TY_DOUBLE) {
	    dval = Memd[COL_OFFSET(cp) + rownum - 1]
	    if (IS_INDEFD(dval))
		buffer = INDEFR
	    else
		buffer = dval

	} else if (COL_DTYPE(cp) == TBL_TY_INT) {
	    ival = Memi[COL_OFFSET(cp) + rownum - 1]
	    if (IS_INDEFI(ival))
		buffer = INDEFR
	    else
		buffer = ival

	} else {				# string
	    lenstr = -COL_DTYPE(cp) + 1		# one for EOS
	    ip = (rownum - 1) * lenstr + 1
	    if (ctor (Memc[COL_OFFSET(cp)], ip, buffer) < 1)
		buffer = INDEFR
	}
end

procedure tbzgti (tp, cp, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
int	rownum			# i: row number
int	buffer			# o: buffer for value to be gotten
#--
double	dval			# buffer for double precision
int	lenstr			# length of a string table element
int	ip			# offset for extracting a string in Memc
long	lval			# so we can use ctol
int	ctol()

begin
	if (COL_DTYPE(cp) == TBL_TY_DOUBLE) {
	    dval = Memd[COL_OFFSET(cp) + rownum - 1]
	    if (IS_INDEFD(dval) || (abs (dval) > MAX_INT))
		buffer = INDEFI
	    else
		buffer = nint (dval)

	} else if (COL_DTYPE(cp) == TBL_TY_INT) {
	    buffer = Memi[COL_OFFSET(cp) + rownum - 1]

	} else {				# string
	    lenstr = -COL_DTYPE(cp) + 1		# one for EOS
	    ip = (rownum - 1) * lenstr + 1
	    if (ctol (Memc[COL_OFFSET(cp)], ip, lval) > 0)
		buffer = lval
	    else
		buffer = INDEFI
#***	    if (ctoi (Memc[COL_OFFSET(cp)], ip, buffer) < 1)
#***		buffer = INDEFI
	}
end

procedure tbzgts (tp, cp, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
int	rownum			# i: row number
short	buffer			# o: buffer for value to be gotten
#--
double	dval			# buffer for double precision
int	lenstr			# length of a string table element
int	ip			# offset for extracting a string in Memc
int	ival
long	lval			# so we can use ctol
int	ctol()

begin
	if (COL_DTYPE(cp) == TBL_TY_DOUBLE) {
	    dval = Memd[COL_OFFSET(cp) + rownum - 1]
	    if (IS_INDEFD(dval) || (abs (dval) > MAX_SHORT))
		buffer = INDEFS
	    else
		buffer = nint (dval)

	} else if (COL_DTYPE(cp) == TBL_TY_INT) {
	    ival = Memi[COL_OFFSET(cp) + rownum - 1]
	    if (IS_INDEFI(ival) || (abs (ival) > MAX_SHORT))
		buffer = INDEFS
	    else
		buffer = ival

	} else {				# string
	    lenstr = -COL_DTYPE(cp) + 1		# one for EOS
	    ip = (rownum - 1) * lenstr + 1
	    if (ctol (Memc[COL_OFFSET(cp)], ip, lval) > 0) {
		if (abs (lval) > MAX_SHORT)
		    buffer = INDEFS
		else
		    buffer = lval
	    } else {
		buffer = INDEFS
	    }
	}
end

procedure tbzgtt (tp, cp, rownum, buffer, maxch)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
int	rownum			# i: row number
char	buffer[ARB]		# o: buffer for value to be gotten
int	maxch			# i: size of buffer
#--
double	dval			# buffer for double precision
int	ival			# buffer for integer value
int	lenstr			# length of a string table element
int	ip			# offset for extracting a string in Memc

begin
	if (COL_DTYPE(cp) == TBL_TY_DOUBLE) {
	    dval = Memd[COL_OFFSET(cp) + rownum - 1]
	    if (IS_INDEFD(dval)) {
		call strcpy ("INDEF", buffer, maxch)
	    } else {
		call sprintf (buffer, maxch, COL_FMT(cp))
		    call pargd (dval)
	    }

	} else if (COL_DTYPE(cp) == TBL_TY_INT) {
	    ival = Memi[COL_OFFSET(cp) + rownum - 1]
	    if (IS_INDEFI(ival)) {
		call strcpy ("INDEF", buffer, maxch)
	    } else {
		call sprintf (buffer, maxch, COL_FMT(cp))
		    call pargi (ival)
	    }

	} else {				# string
	    lenstr = -COL_DTYPE(cp) + 1		# one for EOS
	    ip = (rownum - 1) * lenstr
	    call strcpy (Memc[COL_OFFSET(cp) + ip], buffer, maxch)
	}
end
