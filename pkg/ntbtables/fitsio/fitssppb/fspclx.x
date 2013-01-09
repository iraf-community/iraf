include "fitsio.h"

procedure fspclx(iunit,colnum,frow,fbit,nbit,lray,status)

# write an array of logical values to a specified bit or byte
# column of the binary table.   If the LRAY parameter is .true.,
# then the corresponding bit is set to 1, otherwise it is set
# to 0.
# The binary table column being written to must have datatype 'B'
# or 'X'.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     fbit            # i first bit
int     nbit            # i number of bits
bool    lray[ARB]       # i logical array
int     status          # o error status

begin

call ftpclx(iunit,colnum,frow,fbit,nbit,lray,status)
end
