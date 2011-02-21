include "fitsio.h"

procedure fsmahd(iunit,extno,xtend,status)

# Move to Absolute Header Data unit
# move the i/o pointer to the specified HDU and initialize all
# the common block parameters which describe the extension

int     iunit           # i input file pointer
int     extno           # i extension number
int     xtend           # o type of extension
int     status          # o error status

begin

call ftmahd(iunit,extno,xtend,status)
end
