include "fitsio.h"

procedure fspdat(ounit,status)

# write the current date to the DATE keyword in the ounit CHU

int     ounit           # i output file pointer
int     status          # o error status

begin

call ftpdat(ounit,status)
end
