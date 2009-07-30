include <mach.h>		# for unpacking strings in tbegpt
include "tbtables.h"

# tbegp[tbirds] -- primitive get element
# This procedure reads one element (i.e. row, column) from a table.
# No type conversion is performed, and there is no checking for INDEF
# values.  Character strings will be unpacked by tbegpt, however.
#
# Phil Hodge, 15-Sep-1987  Subroutine created.
# Phil Hodge, 31-Mar-1993  Include short datatype.
# Phil Hodge, 13-Sep-1994  In tbegpt, use tbeszt for length of string to read.

procedure tbegpb (tp, cptr, offset, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
long	offset			# i: offset in char to location for reading
long	rownum			# i: row number
bool	buffer			# o: buffer to receive value
#--
size_t	sz_val
long	read()
errchk	seek, read

begin
	call seek (TB_FILE(tp), offset)
	sz_val = SZ_BOOL
	# arg2: incompatible pointer
	if (read (TB_FILE(tp), buffer, sz_val) < SZ_BOOL)
		call error (1, "tbegpb:  unexpected end of file")
end

procedure tbegpd (tp, cptr, offset, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
long	offset			# i: offset in char to location for reading
long	rownum			# i: row number
double	buffer			# o: buffer to receive value
#--
size_t	sz_val
long	read()
errchk	seek, read

begin
	call seek (TB_FILE(tp), offset)
	sz_val = SZ_DOUBLE
	# arg2: incompatible pointer
	if (read (TB_FILE(tp), buffer, sz_val) < SZ_DOUBLE)
		call error (1, "tbegpd:  unexpected end of file")
end

procedure tbegpr (tp, cptr, offset, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
long	offset			# i: offset in char to location for reading
long	rownum			# i: row number
real	buffer			# o: buffer to receive value
#--
size_t	sz_val
long	read()
errchk	seek, read

begin
	call seek (TB_FILE(tp), offset)
	sz_val = SZ_REAL
	# arg2: incompatible pointer
	if (read (TB_FILE(tp), buffer, sz_val) < SZ_REAL)
		call error (1, "tbegpr:  unexpected end of file")
end

procedure tbegpi (tp, cptr, offset, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
long	offset			# i: offset in char to location for reading
long	rownum			# i: row number
int	buffer			# o: buffer to receive value
#--
size_t	sz_val
long	read()
errchk	seek, read

begin
	call seek (TB_FILE(tp), offset)
	sz_val = SZ_INT
	# arg2: incompatible pointer
	if (read (TB_FILE(tp), buffer, sz_val) < SZ_INT)
		call error (1, "tbegpi:  unexpected end of file")
end

procedure tbegps (tp, cptr, offset, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
long	offset			# i: offset in char to location for reading
long	rownum			# i: row number
short	buffer			# o: buffer to receive value
#--
size_t	sz_val
long	read()
errchk	seek, read

begin
	call seek (TB_FILE(tp), offset)
	sz_val = SZ_SHORT
	if (read (TB_FILE(tp), buffer, sz_val) < SZ_SHORT)
		call error (1, "tbegps:  unexpected end of file")
end

procedure tbegpt (tp, cptr, offset, rownum, buffer, maxch)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
long	offset			# i: offset in char to location for reading
long	rownum			# i: row number
char	buffer[ARB]		# o: buffer to receive value
int	maxch			# i: max number of char to read
#--
size_t	sz_val
char	cbuf[SZ_LINE]		# buffer for reading from table
size_t	nchar			# number of char to read
long	read(), tbeszt()
errchk	seek, read

begin
	nchar = min (tbeszt (cptr), SZ_LINE)
	call seek (TB_FILE(tp), offset)
	if (read (TB_FILE(tp), cbuf, nchar) < nchar)
	    call error (1, "tbegpt:  unexpected end of file")
	# It may be that no EOS was read from the entry in the table.
	cbuf[nchar+1] = EOS
	sz_val = maxch
	call strupk (cbuf, buffer, sz_val)
end
