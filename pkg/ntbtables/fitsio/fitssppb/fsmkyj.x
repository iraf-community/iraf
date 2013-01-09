include "fitsio.h"

procedure fsmkyj(ounit,keywrd,intval,comm,status)

# modify an integer value header record

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     intval          # i integer value
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftmkyj(ounit,fkeywr,intval,fcomm,status)
end
