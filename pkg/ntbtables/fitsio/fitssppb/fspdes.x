include "fitsio.h"

procedure fspdes(ounit,colnum,rownum,nelem,offset,status)

# write the descriptor values to a binary table.  This is only
# used for column which have TFORMn = 'P', i.e., for variable
# length arrays.

int     ounit           # i output file pointer
int     colnum          # i column number
int     rownum          # i row number
int     nelem           # i number of elements
int     offset          # i offset
int     status          # o error status

begin

call ftpdes(ounit,colnum,rownum,nelem,offset,status)
end
