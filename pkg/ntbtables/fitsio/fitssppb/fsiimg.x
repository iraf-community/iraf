include "fitsio.h"

procedure fsiimg(ounit,bitpix,naxis,naxes,status)

# insert an IMAGE extension

int     ounit           # i output file pointer
int     bitpix          # i bits per pixel
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     status          # o error status

begin

call ftiimg(ounit,bitpix,naxis,naxes,status)
end
