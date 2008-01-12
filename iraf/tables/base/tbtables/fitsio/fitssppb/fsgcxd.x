include "fitsio.h"

procedure fsgcxd(iunit,colnum,frow,nrow,fbit,nbit,dvalue,status)

# read consecutive bits from 'X' or 'B' column as an unsigned integer

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     nrow            # i number of rows
int     fbit            # i first bit
int     nbit            # i number of bits
double  dvalue[ARB]     # o double integer array
int     status          # o error status

begin

call ftgcxd(iunit,colnum,frow,nrow,fbit,nbit,dvalue,status)
end
