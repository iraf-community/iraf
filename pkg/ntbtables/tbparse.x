include	<ctype.h>
include <tbset.h>

# tbparse -- extract different portions of table name
# For a table in a QPOE file, the user may give a table name such as
# stuff.qp[abc], where "stuff.qp" is the file name, and "abc" is the name
# of the QPOE parameter containing the table.
#
# For a FITS file, the user may give either the extension name (the value
# of the EXTNAME keyword) or the extension number.  The convention for
# extension number is that the first extension after the primary HDU is
# number one.  This differs from the convention in the FITSIO interface,
# where the primary HDU is number one, so other routines in the table I/O
# interface (tbfopn and tbfnew) will add one to that number.  If the
# extension was specified by name rather than number, the HDU number will
# be set to -1.  If no extension name or number was specified, the value
# returned as the HDU number will be -1.
#
# If the input name has one or more bracketed expressions at the end of the
# name (extension name, etc, for a FITS file, or row or column selectors),
# the bracketed expressions will be returned in the extname string.  Note
# that even if this is just an EXTNAME, the string will include the brackets.
#
# Phil Hodge, 22-Dec-1994  Subroutine created based on qp_parse.
# Phil Hodge,  7-Sep-1995  Allow ".??f" as an extension for a FITS file.
# Phil Hodge, 22-Jan-1996  Allow escaped [ within the file name.
# Phil Hodge,  2-Feb-1996  Move the guts of this routine to tbnparse.
# Phil Hodge, 30-Sep-1997  Change calling sequence of tbnparse.
# Phil Hodge, 12-Apr-1999  Remove type from calling sequence;
#			use SZ_FNAME instead of SZ_LINE for local buffers.

int procedure tbparse (tablename, fname, extname, maxch, hdu)

char	tablename[ARB]	# i: name as specified by user
char	fname[ARB]	# o: name of file containing table
char	extname[ARB]	# o: CDF name, or null if none
int	maxch		# i: size of fname and extname strings
int	hdu		# o: HDU number for FITS file, or -1 if none
#--
pointer sp
pointer brackets	# scratch for expression in brackets
pointer rowselect, colselect	# ignored (selector strings)
pointer scratch
int	localmax, nchar
int	extver, overwrite	# ignored
int	tbnparse()
errchk	tbnparse

begin
	localmax = max (SZ_FNAME, maxch)

	call smark (sp)
	call salloc (brackets, localmax, TY_CHAR)
	call salloc (scratch, localmax, TY_CHAR)
	call salloc (rowselect, SZ_FNAME, TY_CHAR)
	call salloc (colselect, SZ_FNAME, TY_CHAR)

	nchar = tbnparse (tablename, fname, Memc[scratch], Memc[brackets],
		localmax, extver, hdu, overwrite,
		Memc[rowselect], Memc[colselect], SZ_FNAME)

	call strcpy (Memc[brackets], extname, maxch)

	call sfree (sp)

	return (nchar)
end
