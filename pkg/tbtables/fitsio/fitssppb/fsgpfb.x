include "fitsio.h"

procedure fsgpfb(iunit,group,felem,nelem,
                   array,flgval,anynul,status)

# Read an array of byte values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).
# Undefined elements will have the corresponding element of
# FLGVAL set equal to .true.
# ANYNUL is return with a value of .true. if any pixels were undefined.

int     iunit           # i input file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
int     array[ARB]      # o array of values
bool    flgval[ARB]     # o is corresponding element undefined?
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgpfb(iunit,group,felem,nelem,
                   array,flgval,anynul,status)
end
