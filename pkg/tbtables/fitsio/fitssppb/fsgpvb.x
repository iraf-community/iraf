include "fitsio.h"

procedure fsgpvb(iunit,group,felem,nelem,nulval,
                   array,anynul,status)

# Read an array of byte values from the primary array.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).
# Undefined elements will be set equal to NULVAL, unless NULVAL=0
# in which case no checking for undefined values will be performed.
# ANYNUL is return with a value of .true. if any pixels were undefined.

int     iunit           # i input file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
int     nulval          # i value for undefined pixel
int     array[ARB]      # o array of values
bool    anynul          # o any null values?
int     status          # o error status

begin

call ftgpvb(iunit,group,felem,nelem,nulval,
                   array,anynul,status)
end
