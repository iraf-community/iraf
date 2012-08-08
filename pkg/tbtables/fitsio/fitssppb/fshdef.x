include "fitsio.h"

procedure fshdef(ounit,moreky,status)

# Header DEFinition
# define the size of the current header unit; this simply lets
# us determine where the data unit will start

int     ounit           # i output file pointer
int     moreky          # i reserve space for this many more keywords
int     status          # o error status

begin

call fthdef(ounit,moreky,status)
end
