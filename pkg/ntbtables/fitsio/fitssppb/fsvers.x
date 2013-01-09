include "fitsio.h"

procedure fsvers(vernum)

# Returns the current revision number of the FITSIO package.
# The revision number will be incremented whenever any modifications,
# bug fixes, or enhancements are made to the package

real    vernum          # o FITSIO version number

begin

call ftvers(vernum)
end
