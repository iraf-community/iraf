include "tbtables.h"

# tbzpt[tbirds] -- put a single element
# This procedure puts a single element into an internal buffer corresponding
# to a value in a text file.
#
# Phil Hodge, 14-Jan-1992  Subroutines created.
# Phil Hodge, 31-Mar-1993  Include short datatype.
# Phil Hodge, 12-Aug-1993  Use ctol instead of ctoi to allow leading "+" sign.
# Phil Hodge,  4-Mar-1998  Remove calls to tbtwer.

procedure tbzptb (tp, cp, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
int	rownum			# i: row number
bool	buffer			# i: value to be put
#--
int	lenstr			# length of a string table element
int	ip			# offset to a string in Memc

begin
	if (COL_DTYPE(cp) == TBL_TY_DOUBLE) {
	    if (buffer)
		Memd[COL_OFFSET(cp) + rownum - 1] = YES
	    else
		Memd[COL_OFFSET(cp) + rownum - 1] = NO

	} else if (COL_DTYPE(cp) == TBL_TY_INT) {
	    if (buffer)
		Memi[COL_OFFSET(cp) + rownum - 1] = YES
	    else
		Memi[COL_OFFSET(cp) + rownum - 1] = NO

	} else {				# string
	    lenstr = -COL_DTYPE(cp)		# not including EOS
	    ip = (rownum - 1) * (lenstr + 1)	# including EOS
	    if (buffer)
		call strcpy ("yes", Memc[COL_OFFSET(cp) + ip], lenstr)
	    else
		call strcpy ("no", Memc[COL_OFFSET(cp) + ip], lenstr)
	}
end

procedure tbzptd (tp, cp, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
int	rownum			# i: row number
double	buffer			# i: value to be put
#--
char	cbuf[SZ_FNAME]		# buffer for character elements
int	lenstr			# length of a string table element
int	ip			# offset to a string in Memc

begin
	if (COL_DTYPE(cp) == TBL_TY_DOUBLE) {
	    Memd[COL_OFFSET(cp) + rownum - 1] = buffer

	} else if (COL_DTYPE(cp) == TBL_TY_INT) {
	    if (IS_INDEFD(buffer))
		Memi[COL_OFFSET(cp) + rownum - 1] = INDEFI
	    else
		Memi[COL_OFFSET(cp) + rownum - 1] = nint (buffer)

	} else {				# string
	    if (IS_INDEFD(buffer)) {
		call strcpy ("INDEF", cbuf, SZ_FNAME)
	    } else {
		call sprintf (cbuf, SZ_FNAME, "%.16g")
		    call pargd (buffer)
	    }
	    lenstr = -COL_DTYPE(cp)		# not including EOS
	    ip = (rownum - 1) * (lenstr + 1)	# including EOS
	    call strcpy (cbuf, Memc[COL_OFFSET(cp) + ip], lenstr)
	}
end

procedure tbzptr (tp, cp, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
int	rownum			# i: row number
real	buffer			# i: value to be put
#--
char	cbuf[SZ_FNAME]		# buffer for character elements
int	lenstr			# length of a string table element
int	ip			# offset to a string in Memc

begin
	if (COL_DTYPE(cp) == TBL_TY_DOUBLE) {
	    if (IS_INDEF(buffer))
		Memd[COL_OFFSET(cp) + rownum - 1] = INDEFD
	    else
		Memd[COL_OFFSET(cp) + rownum - 1] = buffer

	} else if (COL_DTYPE(cp) == TBL_TY_INT) {
	    if (IS_INDEF(buffer))
		Memi[COL_OFFSET(cp) + rownum - 1] = INDEFI
	    else
		Memi[COL_OFFSET(cp) + rownum - 1] = nint (buffer)

	} else {				# string
	    if (IS_INDEF(buffer)) {
		call strcpy ("INDEF", cbuf, SZ_FNAME)
	    } else {
		call sprintf (cbuf, SZ_FNAME, "%.6g")
		    call pargr (buffer)
	    }
	    lenstr = -COL_DTYPE(cp)		# not including EOS
	    ip = (rownum - 1) * (lenstr + 1)	# including EOS
	    call strcpy (cbuf, Memc[COL_OFFSET(cp) + ip], lenstr)
	}
end

procedure tbzpti (tp, cp, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
int	rownum			# i: row number
int	buffer			# i: value to be put
#--
char	cbuf[SZ_FNAME]		# buffer for character elements
int	lenstr			# length of a string table element
int	ip			# offset to a string in Memc

begin
	if (COL_DTYPE(cp) == TBL_TY_DOUBLE) {
	    if (IS_INDEFI(buffer))
		Memd[COL_OFFSET(cp) + rownum - 1] = INDEFD
	    else
		Memd[COL_OFFSET(cp) + rownum - 1] = buffer

	} else if (COL_DTYPE(cp) == TBL_TY_INT) {
	    Memi[COL_OFFSET(cp) + rownum - 1] = buffer

	} else {				# string
	    if (IS_INDEFI(buffer)) {
		call strcpy ("INDEF", cbuf, SZ_FNAME)
	    } else {
		call sprintf (cbuf, SZ_FNAME, "%d")
		    call pargi (buffer)
	    }
	    lenstr = -COL_DTYPE(cp)		# not including EOS
	    ip = (rownum - 1) * (lenstr + 1)	# including EOS
	    call strcpy (cbuf, Memc[COL_OFFSET(cp) + ip], lenstr)
	}
end

procedure tbzpts (tp, cp, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
int	rownum			# i: row number
short	buffer			# i: value to be put
#--
char	cbuf[SZ_FNAME]		# buffer for character elements
int	lenstr			# length of a string table element
int	ip			# offset to a string in Memc

begin
	if (COL_DTYPE(cp) == TBL_TY_DOUBLE) {
	    if (IS_INDEFS(buffer))
		Memd[COL_OFFSET(cp) + rownum - 1] = INDEFD
	    else
		Memd[COL_OFFSET(cp) + rownum - 1] = buffer

	} else if (COL_DTYPE(cp) == TBL_TY_INT) {
	    if (IS_INDEFS(buffer))
		Memi[COL_OFFSET(cp) + rownum - 1] = INDEFI
	    else
		Memi[COL_OFFSET(cp) + rownum - 1] = buffer

	} else {				# string
	    if (IS_INDEFS(buffer)) {
		call strcpy ("INDEF", cbuf, SZ_FNAME)
	    } else {
		call sprintf (cbuf, SZ_FNAME, "%d")
		    call pargs (buffer)
	    }
	    lenstr = -COL_DTYPE(cp)		# not including EOS
	    ip = (rownum - 1) * (lenstr + 1)	# including EOS
	    call strcpy (cbuf, Memc[COL_OFFSET(cp) + ip], lenstr)
	}
end

procedure tbzptt (tp, cp, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to column descriptor
int	rownum			# i: row number
char	buffer[ARB]		# i: value to be put
#--
int	lenstr			# length of a string table element
int	ip			# offset to a string in Memc
long	lval			# so we can use ctol
int	ctod(), ctol()

begin
	if (COL_DTYPE(cp) == TBL_TY_DOUBLE) {
	    ip = 1
	    if (ctod (buffer, ip, Memd[COL_OFFSET(cp) + rownum - 1]) < 1)
		Memd[COL_OFFSET(cp) + rownum - 1] = INDEFD

	} else if (COL_DTYPE(cp) == TBL_TY_INT) {
	    ip = 1
	    if (ctol (buffer, ip, lval) > 0)
		Memi[COL_OFFSET(cp) + rownum - 1] = lval
	    else
		Memi[COL_OFFSET(cp) + rownum - 1] = INDEFI
#***	    if (ctoi (buffer, ip, Memi[COL_OFFSET(cp) + rownum - 1]) < 1)
#***		Memi[COL_OFFSET(cp) + rownum - 1] = INDEFI

	} else {				# string
	    lenstr = -COL_DTYPE(cp)		# not including EOS
	    ip = (rownum - 1) * (lenstr + 1)	# including EOS
	    call strcpy (buffer, Memc[COL_OFFSET(cp) + ip], lenstr)
	}
end
