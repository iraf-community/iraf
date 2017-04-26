include "fitsio.h"

procedure fsplsw(iunit,status)

# write keywords to warn users that longstring convention may be used

int     iunit           # i input file pointer
int     status          # o error status

begin

call ftplsw(iunit,status)
end
