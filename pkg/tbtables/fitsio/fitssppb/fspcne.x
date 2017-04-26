include "fitsio.h"

procedure fspcne(ounit,colnum,frow,felem,nelem,array,nulval,status)

# write an array of real data values to the specified column of
# the table.

int     ounit           # i output file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
real    array[ARB]      # r array of values
real    nulval          # r value representing a null
int     status          # o error status

begin

call ftpcne(ounit,colnum,frow,felem,nelem,array,nulval,status)
end
