include "fitsio.h"

procedure fscmps(templ,strng,casesn,match,exact)

char    templ[SZ_FTTYPE]     # i column name template
%       character ftemp*24
char    strng[SZ_FTTYPE]     # i column name 
%       character fstrng*24
bool    casesn           # i require same case?
bool    match            # o do the strings match?
bool    exact            # o is it an exact match?

begin

call f77pak(templ,ftemp,SZ_FTTYPE)
call f77pak(strng,fstrng,SZ_FTTYPE)
call ftcmps(ftemp,fstrng,casesn,match,exact)
end
