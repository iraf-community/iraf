include "fitsio.h"

procedure fstscl(ounit,colnum,bscale,bzero,status)

# Table column SCaLing factor definition
# Define the scaling factor for a table column.

int     ounit           # i output file pointer
int     colnum          # i column number
double  bscale          # i scaling factor
double  bzero           # i scaling zeropoint
int     status          # o error status

begin

call fttscl(ounit,colnum,bscale,bzero,status)
end
