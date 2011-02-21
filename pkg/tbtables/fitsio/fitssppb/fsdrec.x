include "fitsio.h"

procedure fsdrec(iunit,pos,status)

# delete a header keyword

int     iunit           # i input file pointer
int     pos             # i position of the keyword to be deleted
int     status          # o error status

begin

call ftdrec(iunit,pos,status)
end
