include "fitsio.h"

procedure fsp2di(ounit,group,dim1,nx,ny,array,status)

# Write a 2-d image of i*2 values into the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being written).

int     ounit           # i output file pointer
int     group           # i group number
int     dim1            # i size of 1st dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
short   array[ARB]      # i array of values
int     status          # o error status

begin

call ftp2di(ounit,group,dim1,nx,ny,array,status)
end
