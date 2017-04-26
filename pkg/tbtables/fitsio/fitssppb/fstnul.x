include "fitsio.h"

procedure fstnul(ounit,colnum,inull,status)

# Table column NULl value definition
# Define the null value for an integer binary table column

int     ounit           # i output file pointer
int     colnum          # i column number
int     inull           # integer null value
int     status          # o error status

begin

call fttnul(ounit,colnum,inull,status)
end
