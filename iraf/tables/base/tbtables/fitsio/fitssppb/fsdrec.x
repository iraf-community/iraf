include "fitsio.h"

procedure fsdrec(iunit,keyno,status)

# delete a header keyword

int     iunit           # i input file pointer
int     keyno           # i position of the keyword to be deleted
int     status          # o error status

begin

call ftdkey(iunit,keyno,status)
end
