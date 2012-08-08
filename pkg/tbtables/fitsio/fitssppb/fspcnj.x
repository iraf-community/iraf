include "fitsio.h"

procedure fspcnj(ounit,colnum,frow,felem,nelem,array,nulval,status)

# write an array of integer data values to the specified column of
# the table.

int     ounit           # i output file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
int     array[ARB]      # i array of values
int     nulval          # i value representing a null
int     status          # o error status

begin

call ftpcnj(ounit,colnum,frow,felem,nelem,array,nulval,status)
end
