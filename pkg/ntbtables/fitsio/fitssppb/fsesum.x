include "fitsio.h"

procedure fsesum(sum,comp,chksum)

double  sum          
bool    comp
char    chksum[16]
%       character fsum*16

begin

call ftesum(sum,comp,fsum)
call f77upk(fsum,chksum,16)
end
