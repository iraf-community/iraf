include "fitsio.h"

procedure fsdcol(ounit,colnum,status)

# delete column in a table

int     ounit           # i output file pointer
int     colnum          # i column to be deleted
int     status          # o error status

begin

call ftdcol(ounit,colnum,status)
end
