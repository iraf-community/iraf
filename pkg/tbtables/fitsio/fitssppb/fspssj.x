include "fitsio.h"

procedure fspssj(iunit,group,naxis,naxes,fpixel,lpixel,array,status)

# Write a subsection of integer values to the primary array.
# A subsection is defined to be any contiguous rectangular
# array of pixels within the n-dimensional FITS data file.
# Data conversion and scaling will be performed if necessary
# (e.g, if the datatype of the FITS array is not the same
# as the array being read).

int     iunit           # i input file pointer
int     group           # i group number
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     fpixel[ARB]     # i first pixel
int     lpixel[ARB]     # i last pixel
int     array[ARB]      # i array of values
int     status          # o error status

begin

call ftpssj(iunit,group,naxis,naxes,fpixel,lpixel,array,status)
end
