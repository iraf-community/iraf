include "fitsio.h"

procedure fsgcvd(iunit,colnum,frow,felem,nelem,nulval,array,
         anynul,status)

# read an array of r*8 values from a specified column of the table.
# Any undefined pixels will be set equal to the value of NULVAL,
# unless NULVAL=0, in which case no checks for undefined pixels
# will be made.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
double  nulval          # i value for undefined pixels
double  array[ARB]      # o array of values
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgcvd(iunit,colnum,frow,felem,nelem,nulval,array,
         anynul,status)
end
