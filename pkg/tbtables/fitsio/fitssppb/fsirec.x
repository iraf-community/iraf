include "fitsio.h"

procedure fsirec(ounit,keyno,record,status)

# insert a character string card record to a header

int     ounit           # i output file pointer
int     keyno           # i number of the keyword to insert before
char    record[SZ_FCARD]     # i 80-char header record
%       character frecor*80
int     status          # o error status

begin

call f77pak(record,frecor,SZ_FCARD)

call ftirec(ounit,keyno,frecor,status)
end
