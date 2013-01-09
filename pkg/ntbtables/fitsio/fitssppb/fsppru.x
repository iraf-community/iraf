include "fitsio.h"

procedure fsppru(ounit,group,felem,nelem,status)

# set elements of the primary array equal to the undefined value

int     ounit           # i output file pointer
int     group           # i group number
int     felem           # i first element in row
int     nelem           # i number of elements
int     status          # o error status

begin

call ftppru(ounit,group,felem,nelem,status)
end
