include "fitsio.h"

procedure fsgkyt(iunit,keywrd,intval,dval,comm,status)

# read an integer value and fractional parts of a keyword value
# and the comment string from a header record

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     intval          # o integer value
double  dval            # o real*8 value
char    comm[SZ_FCOMMENT]       # o keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftgkyt(iunit,fkeywr,intval,dval,fcomm,status)

call f77upk(fcomm  ,comm ,SZ_FCOMMENT)
end
