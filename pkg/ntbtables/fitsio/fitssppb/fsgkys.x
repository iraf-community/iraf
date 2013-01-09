include "fitsio.h"

procedure fsgkys(iunit,keywrd,strval,comm,status)

# read a character string value and comment string from a header record

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
char    strval[SZ_FSTRVAL]     # o string value
%       character fstrva*70
char    comm[SZ_FCOMMENT]       # o keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgkys(iunit,fkeywr,fstrva,fcomm,status)

call f77upk(fstrva,strval,SZ_FSTRVAL)
call f77upk(fcomm ,comm  ,SZ_FCOMMENT)

end
