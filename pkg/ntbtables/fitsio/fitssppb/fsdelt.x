include "fitsio.h"

procedure fsdelt(iunit,status)

# close and delete a FITS file that was previously opened with ftopen or ftinit

int     iunit           # i input file pointer
int     status          # o error status

begin

call ftdelt(iunit,status)
end
