include <fset.h>
include "fitsio.h"

# This was added for compatibility with CFITSIO.

procedure fsgrsz (iunit, maxrows, status)


int     iunit           # i input file pointer
int     maxrows         # o number of rows that fit in buffer
int     status          # o error status
#--
int	fd
int	bufsize
int	naxis1
char	comm[SZ_FCOMMENT]
int	fstati()
include	"../fitsspp.com"	# in order to get fd from iunit

begin
	call fsgkyj (iunit, "NAXIS1", naxis1, comm, status)
	if (status != 0)
	    return
	naxis1 = naxis1 / 2		# convert from bytes to SPP char

	fd = bufid[iunit]

	bufsize = fstati (fd, F_BUFSIZE)
	if (naxis1 > 0) {
	    maxrows = bufsize / naxis1
	    maxrows = max (1, maxrows)
	} else {
	    maxrows = bufsize
	}
end
