include "fitsio.h"

procedure fsgkye(iunit,keywrd,rval,comm,status)

# read a real*4 value and the comment string from a header record

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
real    rval            # o real*4 value
char    comm[SZ_FCOMMENT]       # o keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgkye(iunit,fkeywr,rval,fcomm,status)

call f77upk(fcomm  ,comm ,SZ_FCOMMENT)
end
