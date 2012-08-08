include "fitsio.h"

procedure fsghpr(iunit,maxdim,simple,bitpix,naxis,naxes,
                    pcount,gcount,extend,status)

# get the required primary header or image extension keywords

int     iunit           # i input file pointer
int     maxdim          # i max. number of dimensions
bool    simple          # o simple FITS file?
int     bitpix          # o bits per pixel
int     naxis           # o number of axes
int     naxes[ARB]      # o dimension of each axis
int     pcount          # o no. of group parameters
int     gcount          # o no. of groups
bool    extend          # o EXTEND keyword = TRUE?
int     status          # o error status

begin

call ftghpr(iunit,maxdim,simple,bitpix,naxis,naxes,
                    pcount,gcount,extend,status)
end
