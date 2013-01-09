include "fitsio.h"

procedure fsgiou(iounit,status)

# Returns an unused I/O unit number which may then be used as input
# to the fsinit or fsopen procedures.

int     iounit          # o unused I/O unit number
int     status          # o error status

begin
call ftgiou(iounit,status)
end
