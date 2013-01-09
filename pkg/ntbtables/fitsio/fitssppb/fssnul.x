include "fitsio.h"

procedure fssnul(ounit,colnum,nulval,status)

# ascii table Column NULl value definition
# Define the null value for an ASCII table column.

int     ounit           # i output file pointer
int     colnum          # i column number
char    nulval          # i value for undefined pixels
%       character*16 fnulva
int     status          # o error status

begin

call f77pak(nulval,fnulva,16)

call ftsnul(ounit,colnum,fnulva,status)
end
