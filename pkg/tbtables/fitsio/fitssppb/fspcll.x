include "fitsio.h"

procedure fspcll(ounit,colnum,frow,felem,nelem,lray,status)

# write an array of logical values to the  specified column of the table.
# The binary table column being written to must have datatype 'L'
# and no datatype conversion will be perform if it is not.

int     ounit           # i output file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
bool    lray[ARB]       # i logical array
int     status          # o error status

begin

call ftpcll(ounit,colnum,frow,felem,nelem,lray,status)
end
