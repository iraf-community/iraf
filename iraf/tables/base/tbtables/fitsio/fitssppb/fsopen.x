include "fitsio.h"

procedure fsopen(funit,fname,rwmode,block,status)

# open an existing FITS file with readonly or read/write access

int     funit           # i file I/O pointer
char    fname[ARB]      # i file name
%       character ffname*255
int     rwmode          # i file read/write mode
int     block           # i FITS blocking factor
int     status          # o error status

begin

call f77pak(fname ,ffname,255)

call ftopen(funit,ffname,rwmode,block,status)
end
