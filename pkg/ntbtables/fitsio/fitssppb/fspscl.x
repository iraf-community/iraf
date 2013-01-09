include "fitsio.h"

procedure fspscl(ounit,bscale,bzero,status)

# Primary SCaLing factor definition
# Define the scaling factor for the primary header data.
# This must be the first HDU of the FITS file.

int     ounit           # i output file pointer
double  bscale          # i scaling factor
double  bzero           # i scaling zeropoint
int     status          # o error status

begin

call ftpscl(ounit,bscale,bzero,status)
end
