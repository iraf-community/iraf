include <mach.h>	# for SZB_CHAR
include "tbtables.h"

# tbfsiz -- get FITSIO buffer size
# This function returns the buffer size (SPP char) that CFITSIO has
# available for the current table.
#
# The buffer size is 25*2880 bytes (currently) if only one table is open
# (see NIOBUF and IOBUFLEN in fitsio2.h).
# See the CFITSIO documentation for further information.
#
# Phil Hodge, 25-May-2000  Function created.

long procedure tbfsiz (tp)

pointer tp		# i: pointer to table struct
#--
size_t	sz_val
pointer sp
pointer comment		# ignored
long	rowlen		# length of row (NAXIS1), then convert to char
long	maxrows		# max number of rows that fit in cfitsio buffers
int	status
long	bufsize		# the function value, the buffer size
errchk	tbferr

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (comment, sz_val, TY_CHAR)

	# Get the row length in bytes.
	call fsgkyk (TB_FILE(tp), "NAXIS1",
		     rowlen, Memc[comment], status)
	if (status != 0)
	    call tbferr (status)
	call sfree (sp)

	rowlen = rowlen / SZB_CHAR	# convert bytes to char

	status = 0
	call fsgrsz (TB_FILE(tp), maxrows, status)
	if (status != 0)
	    call tbferr (status)

	if (rowlen > 1)
	    bufsize = rowlen * maxrows
	else
	    bufsize = maxrows

	return (bufsize)
end
