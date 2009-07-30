include "fitsio.h"

procedure fsptbb(iunit,frow,fchar,nchars,value,status)

# write a consecutive string of bytes to an ascii or binary
# table. This will span multiple rows of the table if NCHARS+FCHAR is
# greater than the length of a row.

int     iunit           # i input file pointer
long	frow            # i first row
long	fchar           # i first character
long	nchars          # i number of bytes
char	value[ARB]      # i data value
int     status          # o error status

int	i_frow
int	i_fchar
int	i_nchars

begin
	i_frow = frow
	i_fchar = fchar
	i_nchars = nchars
	call ftptbb(iunit,i_frow,i_fchar,i_nchars,value,status)
end
