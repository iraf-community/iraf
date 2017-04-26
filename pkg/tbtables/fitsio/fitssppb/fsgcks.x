include "fitsio.h"

procedure fsgcks(iunit,datasum,hdusum,status)

int     iunit          
double  datasum
double  hdusum
int     status          # o error status

begin

call ftgcks(iunit,datasum,hdusum,status)
end
