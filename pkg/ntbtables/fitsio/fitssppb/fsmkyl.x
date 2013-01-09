include "fitsio.h"

procedure fsmkyl(ounit,keywrd,logval,comm,status)

# modify a logical value header record

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
bool    logval          # i logical value
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftmkyl(ounit,fkeywr,logval,fcomm,status)
end
