include "fitsio.h"

procedure fsmcom(ounit,keywrd,comm,status)

# modify the comment string in a header record

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
char    comm[SZ_FLONGCOMM]       # i keyword comment
%       character fcomm*72
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FLONGCOMM)

call ftmcom(ounit,fkeywr,fcomm,status)
end
