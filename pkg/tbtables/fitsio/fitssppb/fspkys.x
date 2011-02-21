include "fitsio.h"

procedure fspkys(ounit,keywrd,strval,comm,status)

# write a character string value to a header record

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
char    strval[SZ_FSTRVAL]     # i string value
%       character fstrva*70
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(strval,fstrva,SZ_FSTRVAL)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftpkys(ounit,fkeywr,fstrva,fcomm,status)
end
