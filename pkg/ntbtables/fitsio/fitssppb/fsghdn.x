include "fitsio.h"

procedure fsghdn(iunit,hdunum)

# return the number of the current header data unit.  The
# first HDU (the primary array) is number 1.

int     iunit           # i input file pointer
int     hdunum          # o returned number of the current HDU

begin

call ftghdn(iunit,hdunum)
end
