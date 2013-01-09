include "fitsio.h"

procedure fsgkyl(iunit,keywrd,logval,comm,status)

# read a logical value and the comment string from a header record

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
bool    logval          # o logical value
char    comm[SZ_FCOMMENT]       # o keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgkyl(iunit,fkeywr,logval,fcomm,status)

call f77upk(fcomm  ,comm ,SZ_FCOMMENT)
end
