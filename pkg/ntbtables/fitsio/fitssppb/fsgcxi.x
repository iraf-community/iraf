include "fitsio.h"

procedure fsgcxi(iunit,colnum,frow,nrow,fbit,nbit,ivalue,status)

# read consecutive bits from 'X' or 'B' column as an unsigned integer

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     nrow            # i number of rows
int     fbit            # i first bit
int     nbit            # i number of bits
short   ivalue[ARB]     # o short integer array
int     status          # o error status

begin

call ftgcxi(iunit,colnum,frow,nrow,fbit,nbit,ivalue,status)
end
