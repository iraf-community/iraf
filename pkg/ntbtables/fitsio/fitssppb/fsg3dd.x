include "fitsio.h"

procedure fsg3dd(ounit,group,nulval,dim1,dim2,nx,ny,nz,
                   array,anyflg,status)

# Read a 3-d cube of byte values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     ounit           # i output file pointer
int     group           # i group number
double  nulval          # i value for undefined pi
int     dim1            # i size of 1st dimension
int     dim2            # i size of 2nd dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
int     nz              # i size of z axis
double  array[ARB]      # o array of values
bool    anyflg          # o any null values?
int     status          # o error status

begin

call ftg3dd(ounit,group,nulval,dim1,dim2,nx,ny,nz,
                    array,anyflg,status)
end
