include "fitsio.h"

procedure fsgcx(iunit,colnum,frow,fbit,nbit,lray,status)

# read an array of logical values from a specified bit or byte
# column of the binary table.  A logical .true. value is returned
# if the corresponding bit is 1, and a logical .false. value is
# returned if the bit is 0.
# The binary table column being read from must have datatype 'B'
# or 'X'. This routine ignores any undefined values in the 'B' array.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     fbit            # i first bit
int     nbit            # i number of bits
bool    lray[ARB]       # o logical array
int     status          # o error status

begin

call ftgcx(iunit,colnum,frow,fbit,nbit,lray,status)
end
