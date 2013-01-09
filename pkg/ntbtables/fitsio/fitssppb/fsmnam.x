include "fitsio.h"

procedure fsmnam(ounit,oldkey,newkey,status)

# modify the name of a header keyword

int     ounit           # i output file pointer
char    oldkey[SZ_FKEYWORD]     # i keyword name
%       character fokey*8
char    newkey[SZ_FKEYWORD]     # i keyword name
%       character fnkey*8
int     status          # o error status

begin

call f77pak(oldkey,fokey,SZ_FKEYWORD)
call f77pak(newkey,fnkey,SZ_FKEYWORD)

call ftmnam(ounit,fokey,fnkey,status)
end
