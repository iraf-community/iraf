include "fitsio.h"

procedure fspkyt(iunit,keywrd,intval,dval,comm,status)

# concatinate a integer value with a double precision fraction
# and write it to the FITS header along with the comment string
# The value will be displayed in F28.16 format

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     intval          # i integer value
double  dval            # i real*8 value
char    comm[SZ_FCOMMENT]       # o keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)

call ftpkyt(iunit,fkeywr,intval,dval,fcomm,status)
end
