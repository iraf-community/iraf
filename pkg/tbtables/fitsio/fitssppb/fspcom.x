include "fitsio.h"

procedure fspcom(ounit,commnt,status)

# write a COMMENT record to the FITS header

int     ounit           # i output file pointer
char    commnt[SZ_FLONGCOMM]     # i comment keyword
%       character fcommn*72
int     status          # o error status

begin

call f77pak(commnt,fcommn,SZ_FLONGCOMM)

call ftpcom(ounit,fcommn,status)
end
