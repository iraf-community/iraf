include "fitsio.h"

procedure fsghps(ounit,nexist,keyno,status)

# return the current position in the header

int     ounit           # i output file pointer
int     nexist          # o how many exist?
int     keyno           # o position of the last keyword that was read + 1
int     status          # o error status

begin

call ftghps(ounit,nexist,keyno,status)
end
