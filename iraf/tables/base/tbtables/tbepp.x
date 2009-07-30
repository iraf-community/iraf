include <mach.h>		# for packing strings in tbeppt
include "tbtables.h"

# tbepp[tbirds] -- primitive put element
# This procedure writes one element (i.e. row, column) into a table.
# No type conversion is performed, and the location in the output file
# must already exist (i.e. must not be beyond the EOF).  Character
# strings will be packed by tbeptt, however.
#
# Phil Hodge, 15-Sep-1987  Subroutine created.
# Phil Hodge, 31-Mar-1993  Include short datatype.
# Phil Hodge, 13-Sep-1994  In tbegpt, use tbeszt for length of string to write.

procedure tbeppb (tp, cptr, offset, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
long	offset			# i: offset in char to location for reading
long	rownum			# i: row number
bool	buffer			# i: buffer containing value
#--
size_t	sz_val
errchk	seek, write

begin
	call seek (TB_FILE(tp), offset)
	sz_val = SZ_BOOL
	# arg2: incompatible pointer
	call write (TB_FILE(tp), buffer, sz_val)
end

procedure tbeppd (tp, cptr, offset, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
long	offset			# i: offset in char to location for reading
long	rownum			# i: row number
double	buffer			# i: buffer containing value
#--
size_t	sz_val
errchk	seek, write

begin
	call seek (TB_FILE(tp), offset)
	sz_val = SZ_DOUBLE
	# arg2: incompatible pointer
	call write (TB_FILE(tp), buffer, sz_val)
end

procedure tbeppr (tp, cptr, offset, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
long	offset			# i: offset in char to location for reading
long	rownum			# i: row number
real	buffer			# i: buffer containing value
#--
size_t	sz_val
errchk	seek, write

begin
	call seek (TB_FILE(tp), offset)
	sz_val = SZ_REAL
	# arg2: incompatible pointer
	call write (TB_FILE(tp), buffer, sz_val)
end

procedure tbeppi (tp, cptr, offset, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
long	offset			# i: offset in char to location for reading
long	rownum			# i: row number
int	buffer			# i: buffer containing value
#--
size_t	sz_val
errchk	seek, write

begin
	call seek (TB_FILE(tp), offset)
	sz_val = SZ_INT
	# arg2: incompatible pointer
	call write (TB_FILE(tp), buffer, sz_val)
end

procedure tbepps (tp, cptr, offset, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
long	offset			# i: offset in char to location for reading
long	rownum			# i: row number
short	buffer			# i: buffer containing value
#--
size_t	sz_val
errchk	seek, write

begin
	call seek (TB_FILE(tp), offset)
	sz_val = SZ_SHORT
	call write (TB_FILE(tp), buffer, sz_val)
end

procedure tbeppt (tp, cptr, offset, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
long	offset			# i: offset in char to location for reading
long	rownum			# i: row number
char	buffer[ARB]		# i: buffer containing value
#--
char	cbuf[SZ_LINE]		# buffer for packed string
size_t	nchar			# number of char to write
size_t	sz_val
long	tbeszt()
errchk	seek, write

begin
	nchar = min (tbeszt (cptr), SZ_LINE)
	sz_val = SZ_LINE
	call strpak (buffer, cbuf, sz_val)	# pack the string
	call seek (TB_FILE(tp), offset)
	call write (TB_FILE(tp), cbuf, nchar)
end
