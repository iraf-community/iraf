include "fitsio.h"

procedure fsghad(iunit,curadd,nxtadd)

# delete the CHDU

int     iunit           # i input file pointer
int     curadd          # o starting byte address of the CHDU
int     nxtadd          # o starting byte address of the next HDU

begin

call ftghad(iunit,curadd,nxtadd)
end
