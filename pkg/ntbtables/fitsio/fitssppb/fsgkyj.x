include "fitsio.h"

procedure fsgkyj(iunit,keywrd,intval,comm,status)

# read an integer value and the comment string from a header record

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     intval          # o integer value
char    comm[SZ_FCOMMENT]       # o keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgkyj(iunit,fkeywr,intval,fcomm,status)

call f77upk(fcomm  ,comm ,SZ_FCOMMENT)
end
