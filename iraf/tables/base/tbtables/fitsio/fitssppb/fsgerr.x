include "fitsio.h"

procedure fsgerr(errnum,text)

# Return a descriptive error message corresponding to the error number

int     errnum          # i error number
char    text[SZ_FERRTXT]       # i text string
%       character ftext*30

begin

call ftgerr(errnum,ftext)

call f77upk(ftext  ,text ,SZ_FERRTXT)
end
