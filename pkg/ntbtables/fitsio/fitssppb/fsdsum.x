include "fitsio.h"

procedure fsdsum(chksum,comp,sum)

char    chksum[16]
bool    comp
double  sum          
%       character fsum*16

begin

call f77pak(chksum,fsum,16)
call ftdsum(fsum,comp,sum)
end
