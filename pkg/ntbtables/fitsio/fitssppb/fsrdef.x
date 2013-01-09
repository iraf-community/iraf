include "fitsio.h"

procedure fsrdef(ounit,status)

# Data DEFinition
# re-define the length of the data unit
# this simply redefines the start of the next HDU

int     ounit           # i output file pointer
int     status          # o error status

begin

call ftrdef(ounit,status)
end
