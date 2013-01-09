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
int	rownum			# i: row number
bool	buffer			# i: buffer containing value
#--
errchk	seek, write

begin
	call seek (TB_FILE(tp), offset)
	call write (TB_FILE(tp), buffer, SZ_BOOL)
end

procedure tbeppd (tp, cptr, offset, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
long	offset			# i: offset in char to location for reading
int	rownum			# i: row number
double	buffer			# i: buffer containing value
#--
errchk	seek, write

begin
	call seek (TB_FILE(tp), offset)
	call write (TB_FILE(tp), buffer, SZ_DOUBLE)
end

procedure tbeppr (tp, cptr, offset, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
long	offset			# i: offset in char to location for reading
int	rownum			# i: row number
real	buffer			# i: buffer containing value
#--
errchk	seek, write

begin
	call seek (TB_FILE(tp), offset)
	call write (TB_FILE(tp), buffer, SZ_REAL)
end

procedure tbeppi (tp, cptr, offset, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
long	offset			# i: offset in char to location for reading
int	rownum			# i: row number
int	buffer			# i: buffer containing value
#--
errchk	seek, write

begin
	call seek (TB_FILE(tp), offset)
        if (SZ_INT != SZ_INT32)
            call ipak32 (buffer, buffer, 1)
	call write (TB_FILE(tp), buffer, SZ_INT32)
end

procedure tbepps (tp, cptr, offset, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
long	offset			# i: offset in char to location for reading
int	rownum			# i: row number
short	buffer			# i: buffer containing value
#--
errchk	seek, write

begin
	call seek (TB_FILE(tp), offset)
	call write (TB_FILE(tp), buffer, SZ_SHORT)
end

procedure tbeppt (tp, cptr, offset, rownum, buffer)

pointer tp			# i: pointer to table descriptor
pointer cptr			# i: pointer to column descriptor
long	offset			# i: offset in char to location for reading
int	rownum			# i: row number
char	buffer[ARB]		# i: buffer containing value
#--
char	cbuf[SZ_LINE]		# buffer for packed string
int	nchar			# number of char to write
int	tbeszt()
errchk	seek, write

begin
	nchar = min (tbeszt (cptr), SZ_LINE)
	call strpak (buffer, cbuf, SZ_LINE)	# pack the string
	call seek (TB_FILE(tp), offset)
	call write (TB_FILE(tp), cbuf, nchar)
end
