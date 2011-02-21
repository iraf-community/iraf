include "fitsio.h"

procedure fsgsfb(iunit,colnum,naxis,naxes,fpixel,lpixel,inc,
                 array,flgval,anyflg,status)
# Read a subsection of byte values from the primary array.

int     iunit           # i input file pointer
int     colnum          # i colnum number
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     fpixel[ARB]     # i first pixel
int     lpixel[ARB]     # i last pixel
int     inc[ARB]        # i increment
int     array[ARB]      # o array of values
bool    flgval[ARB]     # o is corresponding value undefined?
bool    anyflg          # o any null values?
int     status          # o error status

begin

call ftgsfb(iunit,colnum,naxis,naxes,fpixel,lpixel,inc,
                 array,flgval,anyflg,status)
end
