include "fitsio.h"

procedure fsdkey(iunit,keywrd,status)

# delete a header keyword

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftdkey(iunit,fkeywr,status)
end
