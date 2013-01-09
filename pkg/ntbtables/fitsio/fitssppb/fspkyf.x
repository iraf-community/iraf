include "fitsio.h"

procedure fspkyf(ounit,keywrd,rval,decim,comm,status)

# write a real*4 value to a header record in F format

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
real    rval            # i real*4 value
int     decim           # i number of decimal plac
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftpkyf(ounit,fkeywr,rval,decim,fcomm,status)
end
