include "fitsio.h"

procedure fsgkyd(iunit,keywrd,dval,comm,status)

# read a double precision value and comment string from a header record

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
double  dval            # o real*8 value
char    comm[SZ_FCOMMENT]       # o keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgkyd(iunit,fkeywr,dval,fcomm,status)

call f77upk(fcomm  ,comm ,SZ_FCOMMENT)
end
