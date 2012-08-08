include "fitsio.h"

procedure fsppne(ounit,group,felem,nelem,array,nulval,status)

# Write an array of r*4 values into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
real    array[ARB]      # r array of values
real    nulval          # r value used for null pixels 
int     status          # o error status

begin

call ftppne(ounit,group,felem,nelem,array,nulval,status)
end
