include "fitsio.h"

procedure fspcks(iunit,status)

int     iunit          
int     status          # o error status

begin

call ftpcks(iunit,status)
end
