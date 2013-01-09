include "fitsio.h"

procedure fscpdt(iunit,ounit,status)

# copies the data from IUNIT to the CHDU of OUNIT.


int     iunit           # i input file pointer
int     ounit           # i output file pointer
int     status          # o error status

begin

call ftcpdt(iunit,ounit,status)
end
