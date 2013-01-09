include "fitsio.h"

procedure fspnul(ounit,blank,status)

# Primary Null value definition
# Define the null value for an integer primary array.
# This must be the first HDU of the FITS file.

int     ounit           # i output file pointer
int     blank           # i value used to represent undefined values
int     status          # o error status

begin

call ftpnul(ounit,blank,status)
end
