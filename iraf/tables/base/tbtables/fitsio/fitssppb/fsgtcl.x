include "fitsio.h"

procedure fsgtcl(iunit,colnum,tcode,rpeat,wdth,status)

int     iunit,colnum,tcode,rpeat,wdth
int     status          # o error status

begin

call ftgtcl(iunit,colnum,tcode,rpeat,wdth,status)

end
