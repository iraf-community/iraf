include "fitsio.h"

procedure fsp3de(ounit,group,dim1,dim2,nx,ny,nz,array,status)

# Write a 3-d cube of r*4 values into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     dim1            # i size of 1st dimension
int     dim2            # i size of 2nd dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
int     nz              # i size of z axis
real    array[ARB]      # i array of values
int     status          # o error status

begin

call ftp3de(ounit,group,dim1,dim2,nx,ny,nz,array,status)
end
