include "fitsio.h"

procedure fsirow(ounit,frow,nrows,status)

# insert rows in a table

int     ounit           # i output file pointer
int     frow            # insert rows after this row
int     nrows           # number of rows
int     status          # o error status

begin

call ftirow(ounit,frow,nrows,status)
end
