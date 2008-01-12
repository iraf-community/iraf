include "fitsio.h"

procedure fsprec(ounit,record,status)

# write a 80 character record to the FITS header

int     ounit           # i output file pointer
char    record[SZ_FCARD]     # i 80-char header record
%       character frecor*80
int     status          # o error status

begin

call f77pak(record,frecor,SZ_FCARD)

call ftprec(ounit,frecor,status)
end
