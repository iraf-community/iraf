include "fitsio.h"

procedure fspdef(ounit,bitpix,naxis,naxes,pcount,gcount,status)

# Primary data DEFinition
# define the structure of the primary data unit or an IMAGE extension

int     ounit           # i output file pointer
int     bitpix          # i bits per pixel
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     pcount          # i number of group parame
int     gcount          # i number of groups
int     status          # o error status

begin

call ftpdef(ounit,bitpix,naxis,naxes,pcount,gcount,status)
end
