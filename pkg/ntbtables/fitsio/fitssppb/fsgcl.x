include "fitsio.h"

procedure fsgcl(iunit,colnum,frow,felem,nelem,lray,status)

# read an array of logical values from a specified column of the table.
# The binary table column being read from must have datatype 'L'
# and no datatype conversion will be perform if it is not.
# This routine ignores any undefined values in the logical array.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
bool    lray[ARB]       # o logical array
int     status          # o error status

begin

call ftgcl(iunit,colnum,frow,felem,nelem,lray,status)
end
