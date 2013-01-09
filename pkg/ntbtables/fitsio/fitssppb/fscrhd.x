include "fitsio.h"

procedure fscrhd(iunit,status)

# 'CReate Header Data unit'
# create, initialize, and move the i/o pointer to a new extension at
# the end of the FITS file.

int     iunit           # i input file pointer
int     status          # o error status

begin

call ftcrhd(iunit,status)
end
