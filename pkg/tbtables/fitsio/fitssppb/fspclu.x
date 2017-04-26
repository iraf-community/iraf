include "fitsio.h"

procedure fspclu(ounit,colnum,frow,felem,nelem,status)

# set elements of a table to be undefined

int     ounit           # i output file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
int     status          # o error status

begin

call ftpclu(ounit,colnum,frow,felem,nelem,status)
end
