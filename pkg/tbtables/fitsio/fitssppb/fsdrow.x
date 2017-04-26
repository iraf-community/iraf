include "fitsio.h"

procedure fsdrow(ounit,frow,nrows,status)

# delete rows in a table

int     ounit           # i output file pointer
int     frow            # first row to delete
int     nrows           # number of rows
int     status          # o error status

begin

call ftdrow(ounit,frow,nrows,status)
end
