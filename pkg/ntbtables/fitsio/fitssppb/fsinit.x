include "fitsio.h"

procedure fsinit(funit,fname,block,status)

# open a new FITS file with write access

int     funit           # i file I/O pointer
char    fname[ARB]      # i file name
%       character ffname*255
int     block           # i FITS blocking factor
int     status          # o error status

begin

call f77pak(fname ,ffname,255)

call ftinit(funit,ffname,block,status)
end
