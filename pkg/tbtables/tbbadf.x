include <mach.h>

# tbbadf -- assign default format
# This procedure assigns a default print format if none was given or if
# the first character is not '%'.
# The actual arguments corresponding to colfmt and pformat may be the same.
#
# Phil Hodge, 12-Aug-1987  Lendata is now number of char in table.
# Phil Hodge, 12-Oct-1987  Change defaults for double, boolean, and string.
# Phil Hodge,  6-Mar-1989  Accept datatype < 0.
# Phil Hodge, 30-Mar-1993  Include short datatype.

procedure tbbadf (colfmt, datatype, lendata, pformat, maxchar)

char	colfmt[ARB]		# i: the print format for the column or null
int	datatype		# i: the SPP data type of the column
int	lendata			# i: storage requirement in table (unit=char)
char	pformat[maxchar]	# o: the print format for the column
int	maxchar			# i: maximum length of the string pformat
#--
begin
	if (colfmt[1] != '%') {		# bad format; assign default
	    switch (datatype) {
	    case TY_REAL:
		call strcpy ("%15.7g", pformat, maxchar)
	    case TY_DOUBLE:
		call strcpy ("%25.16g", pformat, maxchar)
	    case TY_INT:
		call strcpy ("%11d", pformat, maxchar)
	    case TY_BOOL:
		call strcpy ("%6b", pformat, maxchar)
	    case TY_SHORT:
		call strcpy ("%11d", pformat, maxchar)
	    case TY_CHAR:
		pformat[1] = '%'
		call sprintf (pformat[2], maxchar-1, "-%ds")
		    call pargi (lendata * SZB_CHAR)
	    default:
		# datatype < 0 for character type
		pformat[1] = '%'
		call sprintf (pformat[2], maxchar-1, "-%ds")
		    call pargi (-datatype)
	    }
	} else {				# just copy it
	    call strcpy (colfmt, pformat, maxchar)
	}
end
