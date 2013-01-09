include "fitsio.h"

procedure fspthp(ounit,heap,status)

# Define the starting address for the heap for a binary table.
# The default address is NAXIS1 * NAXIS2.  It is in units of
# bytes relative to the beginning of the regular binary table data.
# This routine also writes the appropriate THEAP keyword to the
# FITS header.

int     ounit           # i output file pointer
int     heap            # i heap starting address
int     status          # o error status

begin

call ftpthp(ounit,heap,status)
end
