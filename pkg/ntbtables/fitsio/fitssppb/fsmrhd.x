include "fitsio.h"

procedure fsmrhd(iunit,extmov,xtend,status)

# Move Relative Header Data unit
# move the i/o pointer to the specified HDU and initialize all
# the common block parameters which describe the extension

int     iunit           # i input file pointer
int     extmov          # i relative extension number
int     xtend           # o type of extension
int     status          # o error status

begin

call ftmrhd(iunit,extmov,xtend,status)
end
