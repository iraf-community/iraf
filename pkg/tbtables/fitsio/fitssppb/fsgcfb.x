include "fitsio.h"

procedure fsgcfb(iunit,colnum,frow,felem,nelem,array,
          flgval,anynul,status)

# read an array of byte values from a specified column of the table.
# Any undefined pixels will be have the corresponding value of FLGVAL
# set equal to .true., and ANYNUL will be set equal to .true. if
# any pixels are undefined.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
int     array[ARB]      # o array of values
bool    flgval[ARB]     # o is corresponding value undefined?
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgcfb(iunit,colnum,frow,felem,nelem,array,
          flgval,anynul,status)
end
