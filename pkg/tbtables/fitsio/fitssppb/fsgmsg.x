include "fitsio.h"

procedure fsgmsg(text)

# Return oldest error message from the FITSIO error stack

char    text[SZ_FCARD]       # o text string
%       character ftext*80

begin

call ftgmsg(ftext)

call f77upk(ftext  ,text ,SZ_FCARD)
end
