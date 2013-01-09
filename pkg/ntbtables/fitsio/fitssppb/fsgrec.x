include "fitsio.h"

procedure fsgrec(iunit,nrec,record,status)

# Read the Nth 80-byte header record
# This routine is useful for reading the entire header, one
# record at a time.

int     iunit           # i input file pointer
int     nrec            # i number of keywords
char    record[SZ_FCARD]     # o 80-char header record
%       character frecor*80
int     status          # o error status

begin

call ftgrec(iunit,nrec,frecor,status)

call f77upk(frecor,record,SZ_FCARD)
end
