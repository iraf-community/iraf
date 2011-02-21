include "fitsio.h"

procedure fsucrd(ounit,keywrd,card,status)

# update a given header record specified by keyword name.
# new record is appended to header if it doesn't exist.

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
char    card[SZ_FCARD]       # i 80-char header record
%       character fcard*80
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(card  ,fcard, SZ_FCARD)

call ftucrd(ounit,fkeywr,fcard,status)
end
