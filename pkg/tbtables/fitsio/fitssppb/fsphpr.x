include "fitsio.h"

procedure fsphpr(ounit,simple,bitpix,naxis,naxes,
                 pcount,gcount,extend,status)

# write required primary header keywords

int     ounit           # i output file pointer
bool    simple          # i simple FITS file?
int     bitpix          # i bits per pixel
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     pcount          # i no. of group parameters
int     gcount          # i no. of groups
bool    extend          # i EXTEND keyword = TRUE?
int     status          # o error status

begin

call ftphpr(ounit,simple,bitpix,naxis,naxes,
                 pcount,gcount,extend,status)
end
