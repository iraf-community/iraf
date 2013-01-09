include "fitsio.h"

procedure fsasfm(tform,code,width,decims,status)

char    tform[SZ_FTTYPE]
%       character ftform*24
int     code,width,decims
int     status          # o error status

begin

call f77pak(tform,ftform,4)
call ftasfm(ftform,code,width,decims,status)

end
