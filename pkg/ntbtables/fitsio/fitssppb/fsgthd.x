include "fitsio.h"

procedure fsgthd(tmplat,card,hdtype,status)

# 'Get Template HeaDer'
# parse a template header line and create a formated
# 80-character string which is suitable for appending to a FITS header

char    tmplat[ARB]     # i template string
%       character ftmpla*100
char    card[SZ_FCARD]       # o 80-char header record
%       character fcard*80
int     hdtype          # o hdu type code
int     status          # o error status

begin

call f77pak(tmplat,ftmpla,100)

call ftgthd(ftmpla,fcard,hdtype,status)

call f77upk(fcard  ,card ,SZ_FCARD)
end
