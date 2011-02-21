include "fitsio.h"

procedure fsptdm(ounit,colnum,naxis,naxes,status)

# write the TDIMnnn keyword

int     ounit           # i output file pointer
int     colnum          # i column number
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     status          # o error status

begin

call ftptdm(ounit,colnum,naxis,naxes,status)
end
