include "fitsio.h"

procedure fstkey(keywrd,status)

# test that keyword name contains only legal characters:
#   uppercase letters, numbers, hyphen, underscore, or space

char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call fttkey(fkeywr,status)
end
