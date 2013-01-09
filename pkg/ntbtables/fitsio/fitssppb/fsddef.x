include "fitsio.h"

procedure fsddef(ounit,bytlen,status)

# Data DEFinition
# re-define the length of the data unit
# this simply redefines the start of the next HDU

int     ounit           # i output file pointer
int     bytlen          # i length in bytes
int     status          # o error status

begin

call ftddef(ounit,bytlen,status)
end
