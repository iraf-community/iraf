include "fitsio.h"

procedure fsdhdu(iunit,hdutyp,status)

# delete the CHDU

int     iunit           # i input file pointer
int     hdutyp          # o type of the new CHDU
int     status          # o error status

begin

call ftdhdu(iunit,hdutyp,status)
end
