include "fitsio.h"

procedure fspclc(ounit,colnum,frow,felem,nelem,array,status)

# write an array of single precision complex data values to the
# specified column of the table.
# The binary table column being written to must have datatype 'C'
# and no datatype conversion will be perform if it is not.

int     ounit           # i output file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
real    array[ARB]      # i array of values
int     status          # o error status

begin

call ftpclc(ounit,colnum,frow,felem,nelem,array,status)
end
