include "fitsio.h"

procedure fsghsp(ounit,nexist,nmore,status)

# Get Header SPace
# return the number of additional keywords that will fit in the header

int     ounit           # i output file pointer
int     nexist          # o how many exist?
int     nmore           # o this many more will fit
int     status          # o error status

begin

call ftghsp(ounit,nexist,nmore,status)
end
