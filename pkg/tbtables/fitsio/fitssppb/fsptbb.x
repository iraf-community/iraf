include "fitsio.h"

procedure fsptbb(iunit,frow,fchar,nchars,value,status)

# write a consecutive string of bytes to an ascii or binary
# table. This will span multiple rows of the table if NCHARS+FCHAR is
# greater than the length of a row.

int     iunit           # i input file pointer
int     frow            # i first row
int     fchar           # i first character
int     nchars          # i number of bytes
int     value[ARB]      # i data value
int     status          # o error status

begin

call ftptbb(iunit,frow,fchar,nchars,value,status)
end
