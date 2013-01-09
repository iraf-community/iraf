include "fitsio.h"

procedure fsgkey(iunit,keywrd,value,comm,status)

# Read value and comment of a header keyword from the keyword buffer

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
char    value[SZ_FSTRVAL]           # o keyword value
%       character fvalue*70
char    comm[SZ_FCOMMENT]       # o keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgkey(iunit,fkeywr,fvalue,fcomm,status)

call f77upk(fvalue  ,value ,SZ_FSTRVAL)
call f77upk(fcomm  ,comm ,SZ_FCOMMENT)

end
