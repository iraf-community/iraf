include "fitsio.h"

procedure fsfiou(iounit,status)

# Returns an unused I/O unit number which may then be used as input
# to the fsinit or fsopen procedures.

int     iounit          # i I/O unit number
int     status          # o error status

begin
call ftfiou(iounit,status)
end
