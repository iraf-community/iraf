include "fitsio.h"

procedure fsgdes(iunit,colnum,rownum,nelem,offset,status)

# read the descriptor values from a binary table.  This is only
# used for column which have TFORMn = 'P', i.e., for variable
# length arrays.

int     iunit           # i input file pointer
int     colnum          # i column number
int     rownum          # i row number
int     nelem           # o number of elements
int     offset          # o offset
int     status          # o error status

begin

call ftgdes(iunit,colnum,rownum,nelem,offset,status)
end
