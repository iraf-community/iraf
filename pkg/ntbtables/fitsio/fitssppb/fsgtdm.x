include "fitsio.h"

procedure fsgtdm(iunit,colnum,maxdim,naxis,naxes,status)

# read the TDIMnnn keyword

int     iunit           # i input file pointer
int     colnum          # i column number
int     maxdim          # i maximum number of dimensions to return
int     naxis           # i number of axes
int     naxes[ARB]      # i dimension of each axis
int     status          # o error status

begin

call ftgtdm(iunit,colnum,maxdim,naxis,naxes,status)
end
