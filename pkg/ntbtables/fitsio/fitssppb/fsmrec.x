include "fitsio.h"

procedure fsmrec(ounit,nkey,record,status)

# modify the nth keyword in the CHU, by replacing it with the
# input 80 character string.

int     ounit           # i output file pointer
int     nkey            # i number of keyword to be modified
char    record[SZ_FCARD]     # i 80-char header record
%       character frecor*80
int     status          # o error status

begin

call f77pak(record,frecor,SZ_FCARD)

call ftmrec(ounit,nkey,frecor,status)
end
