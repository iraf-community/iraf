include "fitsio.h"

procedure fsg2de(ounit,group,nulval,dim1,nx,ny,array,anyflg,status)

# Read a 2-d image of real values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     ounit           # i output file pointer
int     group           # i group number
real    nulval          # i value for undefined pixels
int     dim1            # i size of 1st dimension
int     nx              # i size of x axis
int     ny              # i size of y axis
real    array[ARB]      # o array of values
bool    anyflg          # o any null values?
int     status          # o error status

begin

call ftg2de(ounit,group,nulval,dim1,nx,ny,array,anyflg,status)
end
