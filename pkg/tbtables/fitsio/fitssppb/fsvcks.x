include "fitsio.h"

procedure fsvcks(iunit,dataok,hduok,status)

int     iunit          
int     dataok
int     hduok
int     status          # o error status

begin

call ftvcks(iunit,dataok,hduok,status)
end
