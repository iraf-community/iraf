include "fitsio.h"

procedure fspsvc(keyrec,value,comm,status)

# parse the header record to find value and comment strings

char    keyrec[SZ_FCARD]     # i header keyword string
%       character fkeyre*80
char    value[SZ_FSTRVAL]      # o data value
%       character fvalue*70
char    comm[SZ_FCOMMENT]       # o keyword comment
%       character fcomm*48
int     status          # o error status

begin

call f77pak(keyrec,fkeyre,SZ_FCARD)

call ftpsvc(fkeyre,fvalue,fcomm,status)

call f77upk(fvalue ,value,SZ_FSTRVAL)
call f77upk(fcomm  ,comm ,SZ_FCOMMENT)
end
