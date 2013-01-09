include "fitsio.h"

procedure fsgtbb(iunit,frow,fchar,nchars,value,status)

# read a consecutive string of bytes from an ascii or binary
# table. This will span multiple rows of the table if NCHARS+FCHAR is
# greater than the length of a row.

int     iunit           # i input file pointer
int     frow            # i first row
int     fchar           # i first character
int     nchars          # i number of bytes
int     value[ARB]      # o data value
int     status          # o error status

begin

call ftgtbb(iunit,frow,fchar,nchars,value,status)
end
