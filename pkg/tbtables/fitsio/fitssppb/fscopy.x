include "fitsio.h"

procedure fscopy(iunit,ounit,moreky,status)

# copies the CHDU from IUNIT to the CHDU of OUNIT.
# This will also reserve space in the header for MOREKY keywords
# if MOREKY > 0.

int     iunit           # i input file pointer
int     ounit           # i output file pointer
int     moreky          # i how many more keywords
int     status          # o error status

begin

call ftcopy(iunit,ounit,moreky,status)
end
