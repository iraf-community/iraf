include "fitsio.h"

procedure fspcld(ounit,colnum,frow,felem,nelem,array,status)

# write an array of double precision data values to the specified column
# of the table.

int     ounit           # i output file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
double  array[ARB]      # i array of values
int     status          # o error status

begin

call ftpcld(ounit,colnum,frow,felem,nelem,array,status)
end
