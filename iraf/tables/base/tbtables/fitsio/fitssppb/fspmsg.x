include "fitsio.h"

procedure fspmsg(text)

# write a 80 character record to the FITSIO error stack

char    text[SZ_FCARD]     # i 80-char message
%       character ftext*80

begin

call f77pak(text,ftext,SZ_FCARD)

call ftpmsg(ftext)
end
