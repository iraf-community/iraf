include "fitsio.h"

procedure fsmcrd(ounit,keywrd,card,status)

# modify (overwrite) a given header record specified by keyword name.
# This can be used to overwrite the name of the keyword as well as
# the value and comment fields.

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
char    card[SZ_FCARD]       # i 80-char header record
%       character fcard*80
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(card  ,fcard, SZ_FCARD)

call ftmcrd(ounit,fkeywr,fcard,status)
end
