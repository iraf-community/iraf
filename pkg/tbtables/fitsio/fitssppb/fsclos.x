include "fitsio.h"

procedure fsclos(iunit,status)

# close a FITS file that was previously opened with ftopen or ftinit

int     iunit           # i input file pointer
int     status          # o error status

begin

call ftclos(iunit,status)
end
