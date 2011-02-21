include "fitsio.h"

procedure fspcli(ounit,colnum,frow,felem,nelem,array,status)

# write an array of integer*2 data values to the specified column of
# the table.

int     ounit           # i output file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
short   array[ARB]      # i array of values
int     status          # o error status

begin

call ftpcli(ounit,colnum,frow,felem,nelem,array,status)
end
