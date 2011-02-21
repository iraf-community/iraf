include "fitsio.h"

procedure fsphis(ounit,histry,status)

# write a HISTORY record to the FITS header

int     ounit           # i output file pointer
char    histry[SZ_FLONGCOMM]     # i history keyword
%       character fhistr*72
int     status          # o error status

begin

call f77pak(histry,fhistr,SZ_FLONGCOMM)

call ftphis(ounit,fhistr,status)
end
