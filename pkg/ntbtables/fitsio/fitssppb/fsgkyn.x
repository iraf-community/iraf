include "fitsio.h"

procedure fsgkyn(iunit,nkey,keywrd,value,comm,status)

# Read the name, value, and comment of the NKEYth header record
# This routine is useful for reading the entire header, one
# record at a time.

int     iunit           # i input file pointer
int     nkey            # i number of keywords
char    keywrd[SZ_FKEYWORD]     # o keyword name
%       character fkeywr*8
char    value[SZ_FSTRVAL]      # o data value
%       character fvalue*70
char    comm[SZ_FLONGCOMM]       # o keyword comment
%       character fcomm*72
int     status          # o error status

begin

call ftgkyn(iunit,nkey,fkeywr,fvalue,fcomm,status)

call f77upk(fkeywr  ,keywrd ,SZ_FKEYWORD)
call f77upk(fvalue  ,value ,SZ_FSTRVAL)
call f77upk(fcomm  ,comm ,SZ_FLONGCOMM)
end
