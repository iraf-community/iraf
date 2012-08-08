include "fitsio.h"

procedure fsgcrd(iunit,keywrd,card,status)

# Read the 80 character card image of a specified header keyword record

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
char    card[SZ_FCARD]       # o 80-char header record
%       character fcard*80
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgcrd(iunit,fkeywr,fcard,status)

call f77upk(fcard  ,card ,SZ_FCARD)
end
