include "fitsio.h"

procedure fsgcxj(iunit,colnum,frow,nrow,fbit,nbit,jvalue,status)

# read consecutive bits from 'X' or 'B' column as an unsigned integer

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     nrow            # i number of rows
int     fbit            # i first bit
int     nbit            # i number of bits
int     jvalue[ARB]     # o integer array
int     status          # o error status

begin

call ftgcxj(iunit,colnum,frow,nrow,fbit,nbit,jvalue,status)
end
